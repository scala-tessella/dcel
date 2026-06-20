package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.*

import scala.collection.mutable

/** ADR-0025 de-risking spike: BOUNDED-`V` dart assembly for a single vertex-type bucket.
  *
  * This is the genuinely new code ADR-0025 asks for — a fixed-`V` combinatorial-map assembler, NOT a patch
  * grower. Given a bucket `T` (a set of vertex types) it:
  *
  *   1. lays out, for `V` typed vertices, their **darts** (half-edges), each carrying a **port** — the
  *      unordered pair of polygons it separates (the precomputed [[VertexTypes]] alphabet);
  *   2. enumerates the **port-matched perfect matchings** of those darts (a finite CSP — every dart paired,
  *      ports compatible), which fixes the edge involution `α`; faces fall out as the cycles of `φ = σ∘α`;
  *   3. keeps the matchings that close into a CONNECTED genus-1 (torus) map — verified, keyed and
  *      deduplicated by [[DelaneySymbols.classifyClosedMap]] (ADR-0025 enabler #4: reuse the proven oracle
  *      for verify / canonical key / the Krötenheerdt `n orbits = n types` test).
  *
  * Only COMPLETE `V`-vertex tori are searched — there are no partial patches to scatter over, which is the
  * whole point versus the rejected restricted-growth proxy. The spike's question (ADR-0025 risk #1): is the
  * inner assembly SMALL — does `states` track `V` rather than the patch-growth ~850? [[enumerateBucket]]
  * reports it.
  *
  * Soundness is structural: a closed, orientable (built from a rotation system), all-360° map by regular
  * polygons is a genuine flat tiling (ADR-0022), so no overlap test is needed — the 3.3.6.6 / 3.4.4.6
  * angle-valid non-tilings simply cannot be assembled.
  */
object BucketAssembly:

  /** One assembled tiling: vertex-orbit count, vertex-type set, and minimal-symbol canonical key (dedup id).
    */
  final case class Found(n: Int, types: Set[VertexSignature], key: String)

  /** Outcome of assembling a bucket: the distinct Krötenheerdt tilings of that bucket, plus the search cost
    * (`states` = port-matched edges tried — the headline de-risk number; `mapsClosed` = connected closed maps
    * reached; `budgetHit` = the state budget was exhausted, so coverage is partial).
    */
  final case class BucketResult(
      tilings: List[Found],
      states: Long,
      expanded: Long,
      mapsClosed: Long,
      budgetHit: Boolean
  ):
    def keys: Set[String] = tilings.map(_.key).toSet

  /** Assemble every Krötenheerdt tiling whose vertex-type set is exactly `bucket`, by bounded-`V` dart
    * assembly over `V = bucket.size .. maxV`. `stateBudget` caps the total port-matched edges tried.
    */
  def enumerateBucket(
      bucket: Set[VertexSignature],
      maxV: Int,
      stateBudget: Long = 20_000_000L
  ): BucketResult =
    val k          = bucket.size
    val typesList  = bucket.toVector.map(normalize)
    val results    = mutable.Map.empty[String, Found]
    // GLOBAL canonical partial-map dedup set: prunes isomorphic partial matchings across every assignment.
    val seen       = mutable.HashSet.empty[String]
    var states     = 0L
    var expanded   = 0L
    var mapsClosed = 0L
    var budgetHit  = false
    var v          = k
    while v <= maxV && !budgetHit do
      for assignment <- orientedAssignments(typesList, v) if !budgetHit do
        val asm = new Assembler(assignment, stateBudget - states, seen)
        asm.run()
        states += asm.states
        expanded += asm.expanded
        mapsClosed += asm.mapsClosed
        if asm.budgetHit then budgetHit = true
        for (key, f) <- asm.results do results.getOrElseUpdate(key, f)
      v += 1
    val tilings    = results.values.filter(f => f.n == k && f.types == bucket).toList
    BucketResult(tilings, states, expanded, mapsClosed, budgetHit)

  // ---- assignment generation -------------------------------------------------------------------------

  /** Oriented `V`-vertex assignments over the bucket: surjective multisets of the `k` types (each type used ≥
    * 1, so the type set can equal the bucket), each placed in a definite cyclic (oriented) order. A CHIRAL
    * type (whose reverse is not a rotation of itself) contributes both orientations — except vertex 0, fixed
    * to the forward reading to drop the global mirror duplicate. Achiral types (the common case here) add no
    * branching, keeping `states` a clean measurement.
    */
  private def orientedAssignments(types: Vector[VertexSignature], v: Int): List[Vector[Vector[Int]]] =
    // reps(t) = the distinct oriented readings of type t
    val reps: Map[VertexSignature, Vector[Vector[Int]]]                   =
      types.map { t =>
        val fwd = t.toVector
        val rev = t.reverse.toVector
        t -> (if normalize(t.reverse) == normalize(t) then Vector(fwd) else Vector(fwd, rev))
      }.toMap
    val out                                                               = List.newBuilder[Vector[Vector[Int]]]
    // surjective count-vectors (c_0..c_{k-1}), each ≥ 1, summing to v
    def counts(idx: Int, remaining: Int, acc: List[Int]): List[List[Int]] =
      if idx == types.size - 1 then
        if remaining >= 1 then List((remaining :: acc).reverse) else Nil
      else
        (1 to remaining - (types.size - 1 - idx)).toList.flatMap(c =>
          counts(idx + 1, remaining - c, c :: acc)
        )
    for cv <- counts(0, v, Nil) do
      val typeSeq: Vector[VertexSignature]             =
        cv.zip(types).flatMap((c, t) => Vector.fill(c)(t)).toVector
      // expand each vertex's orientation; fix vertex 0 to its forward reading
      def expand(i: Int, acc: List[Vector[Int]]): Unit =
        if i == typeSeq.size then out += acc.reverse.toVector
        else
          val choices = if i == 0 then reps(typeSeq(i)).take(1) else reps(typeSeq(i))
          for ch <- choices do expand(i + 1, ch :: acc)
      expand(0, Nil)
    out.result()

  // ---- the fixed-V dart assembler --------------------------------------------------------------------

  /** Enumerates the port-matched perfect matchings of the darts of one oriented `V`-vertex assignment, and
    * collects the closed torus tilings. `types(i)` is vertex `i`'s oriented cyclic polygon sequence.
    */
  final private class Assembler(types: Vector[Vector[Int]], stateBudget: Long, seen: mutable.HashSet[String]):
    private val nV    = types.size
    private val deg   = types.map(_.size).toArray
    private val start = deg.scanLeft(0)(_ + _) // start(i) = first dart of vertex i; start(nV) = D
    private val D     = start(nV)

    private val vertexOf = Array.tabulate(D) { g =>
      var i = 0; while start(i + 1) <= g do i += 1; i
    }
    private val posOf    = Array.tabulate(D)(g => g - start(vertexOf(g)))

    /** polygon in the corner just CCW-after dart g (between dart g and σ(g)). */
    private def after(g: Int): Int = types(vertexOf(g))(posOf(g))

    /** polygon in the corner just CW-before dart g (between σ⁻¹(g) and g). */
    private def before(g: Int): Int = { val t = types(vertexOf(g)); t((posOf(g) + t.size - 1) % t.size) }

    private def sigmaNext(g: Int): Int = { val i = vertexOf(g); start(i) + (posOf(g) + 1) % deg(i) }
    private def sigmaPrev(g: Int): Int = { val i = vertexOf(g); start(i) + (posOf(g) + deg(i) - 1) % deg(i) }

    // face perm φ = σ∘α and its inverse, evaluated on the CURRENT partial α (−1 where α is not yet decided).
    private def phiOf(d: Int): Int    = { val a = alpha(d); if a >= 0 then sigmaNext(a) else -1 }
    private def phiInvOf(d: Int): Int = alpha(sigmaPrev(d)) // φ⁻¹(d) = α(σ⁻¹(d)), or −1 if undecided

    /** Fail-fast face check. Along a face the corner label `before` is INVARIANT — the ordered port match
      * forces `before(φ(d)) == before(d)` — so a regular face must close at EXACTLY `before(d)` darts. Reject
      * the moment the face fragment through `d` closes at the wrong length, or an open fragment overshoots.
      */
    private def faceOk(d: Int): Boolean =
      val target = before(d)
      var cur    = d
      var back   = 0
      var closed = false
      var p      = phiInvOf(cur)
      while p >= 0 && !closed do
        cur = p; back += 1
        if cur == d then closed = true else p = phiInvOf(cur)
      if closed then back == target // a closed face must be a regular `target`-gon
      else
        var len = 1 // open fragment: count darts from the head `cur`, reject once it cannot be a `target`-gon
        var f   = phiOf(cur)
        while f >= 0 && len <= target do { cur = f; len += 1; f = phiOf(cur) }
        len <= target

    // ORDERED antiparallel port match: gluing dart g to h identifies the face after g with the face before h
    // and vice versa, so a valid edge needs after(g)==before(h) && before(g)==after(h). Stronger than the
    // unordered pair (it also fixes orientation), so it prunes the matching far harder.
    private def compatible(g: Int, h: Int): Boolean =
      after(g) == before(h) && before(g) == after(h)

    private val alpha = Array.fill(D)(-1)
    var states        = 0L
    var expanded      = 0L
    var mapsClosed    = 0L
    var budgetHit     = false
    val results       = mutable.Map.empty[String, Found]

    def run(): Unit = if D % 2 == 0 then matchFrom()

    private def firstUnmatched(): Int =
      var g = 0; while g < D && alpha(g) >= 0 do g += 1; if g < D then g else -1

    private def matchFrom(): Unit =
      if budgetHit then ()
      else
        val g = firstUnmatched()
        if g < 0 then onComplete()
        else
          var h = g + 1
          while h < D && !budgetHit do
            if alpha(h) < 0 && compatible(g, h) then
              states += 1
              if states > stateBudget then budgetHit = true
              else
                alpha(g) = h; alpha(h) = g
                // (1) fail-fast: the just-affected faces (through g and h) must stay able to close as regular
                // polygons; (2) partial-map canonical dedup: recurse only into an isomorphism class not yet
                // visited (the iso maps unmatched darts to unmatched, so every completion is found via the
                // first-seen representative — sound for completeness).
                if faceOk(g) && faceOk(h) && seen.add(canonKey()) then { expanded += 1; matchFrom() }
                alpha(g) = -1; alpha(h) = -1
            h += 1

    // ---- canonical form of the (possibly disconnected) partial map, orientation-preserving --------------
    // Key = the SORTED multiset of per-component canonical strings, so isomorphic partial maps (under vertex
    // relabeling + dart-cycle rotation, NOT reflection — chirality is preserved) collide. Reflection is
    // excluded so the two enantiomorphs of a chiral tiling are not wrongly merged.

    private def canonKey(): String =
      val comp = Array.fill(D)(-1)
      var c    = 0
      var s    = 0
      while s < D do
        if comp(s) < 0 then
          comp(s) = c
          val st = mutable.Stack(s)
          while st.nonEmpty do
            val d = st.pop()
            for nb <- Array(sigmaNext(d), sigmaPrev(d), alpha(d)) if nb >= 0 && comp(nb) < 0 do
              comp(nb) = c; st.push(nb)
          c += 1
        s += 1
      val keys = Array.fill(c)("")
      var ci   = 0
      while ci < c do
        var best: String = null
        var root         = 0
        while root < D do
          if comp(root) == ci then
            val str = bfsString(root)
            if best == null || str < best then best = str
          root += 1
        keys(ci) = best
        ci += 1
      keys.sorted.mkString("#")

    /** Orientation-preserving BFS relabeling from `root` (follow σ then matched α), emitting per dart its
      * corner labels and its neighbours' new labels — a string that is identical for σ/relabel-isomorphic
      * components and distinct otherwise.
      */
    private def bfsString(root: Int): String =
      val lab   = mutable.HashMap(root -> 0)
      val order = mutable.ArrayBuffer(root)
      var head  = 0
      val sb    = new StringBuilder
      while head < order.size do
        val d  = order(head); head += 1
        val sn = sigmaNext(d)
        if !lab.contains(sn) then { lab(sn) = order.size; order += sn }
        val am = alpha(d)
        if am >= 0 && !lab.contains(am) then { lab(am) = order.size; order += am }
        sb.append(after(d)).append('.').append(before(d)).append(':').append(lab(sn)).append(',')
        sb.append(if am >= 0 then lab(am).toString else "x").append(';')
      sb.toString

    private def onComplete(): Unit =
      if !connected() then return
      mapsClosed += 1
      // φ = σ ∘ α (next dart around a face); chambers a(g)=2g+1, b(g)=2g+2 (1-based for DSymbols).
      val phi    = Array.tabulate(D)(g => sigmaNext(alpha(g)))
      val phiInv = Array.fill(D)(-1)
      var g0     = 0
      while g0 < D do { phiInv(phi(g0)) = g0; g0 += 1 }
      val op     = Array.ofDim[Int](2 * D + 1, 3)
      var g      = 0
      while g < D do
        val a = 2 * g + 1; val b = 2 * g + 2
        op(a)(0) = b; op(b)(0) = a   // r0: a ↔ b              (cross vertex)
        op(a)(2) = 2 * alpha(g) + 2  // r2: a(g) ↔ b(α g)      (cross face)
        op(b)(2) = 2 * alpha(g) + 1  //     b(g) ↔ a(α g)
        op(a)(1) = 2 * phiInv(g) + 2 // r1: a(g) ↔ b(φ⁻¹ g)    (cross edge)
        op(b)(1) = 2 * phi(g) + 1    //     b(g) ↔ a(φ g)
        g += 1
      DelaneySymbols.classifyClosedMap(op).foreach { (n, sigs, key) =>
        results.getOrElseUpdate(key, Found(n, sigs.toSet, key))
      }

    /** The whole map is one component (else χ = 0 could be two disjoint tori, not a single tiling). */
    private def connected(): Boolean =
      val seen  = Array.fill(D)(false)
      val stack = mutable.Stack(0)
      seen(0) = true
      var cnt   = 1
      while stack.nonEmpty do
        val d = stack.pop()
        for n2 <- Array(sigmaNext(d), sigmaPrev(d), alpha(d)) if !seen(n2) do
          seen(n2) = true; cnt += 1; stack.push(n2)
      cnt == D
