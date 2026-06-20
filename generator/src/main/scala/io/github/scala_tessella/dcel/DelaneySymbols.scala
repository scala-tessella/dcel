package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.*

import scala.collection.mutable

/** Enumeration of the Krotenheerdt tilings (OEIS A068600) as **2-dimensional Delaney–Dress symbols** — the
  * intrinsic, coordinate-free combinatorial-map approach decided in ADR-0022. A direct Scala port of Olaf
  * Delgado-Friedrichs' `genDSyms` (github.com/odf/julia-dsymbols), restricted to dim 2, plus a
  * regular-polygon / Krotenheerdt filter.
  *
  * A 2D Delaney–Dress symbol is the barycentric subdivision of a tiling into **chambers** (vertex·edge·face
  * flags), quotiented by the symmetry group, carrying three involutions `op(0), op(1), op(2)` (cross the
  * vertex / edge / face of the flag) and `v`-values per orbit. The combinatorial dictionary:
  *   - a **01-orbit** (fix the face) is a TILE; its `m₀₁` = number of edges = polygon side-count.
  *   - a **12-orbit** (fix the vertex) is a VERTEX; its `m₁₂` = vertex degree.
  *   - a **02-orbit** (fix the edge) is an EDGE; `m₀₂` = 2 always (the 2-manifold condition).
  *
  * Why this is the right tool (ADR-0022): construction is intrinsic (no guessed lattice, no plane residues),
  * so an enumerated symbol that is euclidean (curvature 0), orientable and all-360° is a GENUINE flat tiling
  * by the developing-map theorem — the 3.3.6.6 / 3.4.4.6 false-period overlaps cannot arise. The minimal
  * symbol is a canonical key (no type-set duplication), and cost is bounded by chamber count, not covolume.
  */
object DelaneySymbols:

  // ---- exact rationals (curvature only needs +, -, sign) ----------------------------------------------

  /** Minimal normalized rational (Long), enough for the curvature bookkeeping. */
  final case class Frac(num: Long, den: Long):
    def +(o: Frac): Frac = Frac.make(num * o.den + o.num * den, den * o.den)
    def -(o: Frac): Frac = Frac.make(num * o.den - o.num * den, den * o.den)
    def signum: Int      = java.lang.Long.signum(num) * java.lang.Long.signum(den)
    def isZero: Boolean  = num == 0
  object Frac:
    def make(n: Long, d: Long): Frac        =
      val s = if d < 0 then -1 else 1
      val g = gcd(math.abs(n), math.abs(d))
      if g == 0 then Frac(0, 1) else Frac(s * n / g, s * d / g)
    private def gcd(a: Long, b: Long): Long = if b == 0 then a else gcd(b, a % b)

  // ---- Delaney SET (the σ-involution structure; no v-values yet) --------------------------------------

  private val Dim = 2

  /** A Delaney set: `op(D)(i)` is the involution `σ_i` on chambers `1..size` (0 = "undefined/boundary").
    * Immutable; the generator copies on branch (mirrors the Julia `DelaneySetUnderConstruction`).
    */
  final class DSet(val op: Array[Array[Int]]):
    def size: Int                = op.length - 1 // index 0 unused (chambers are 1-based)
    def get(i: Int, d: Int): Int = if d >= 1 && d <= size then op(d)(i) else 0

    /** A fresh copy with one extra (empty) chamber appended. */
    def grown: DSet =
      val n = size
      val a = Array.ofDim[Int](n + 2, Dim + 1)
      var d = 1
      while d <= n do { System.arraycopy(op(d), 0, a(d), 0, Dim + 1); d += 1 }
      new DSet(a)

    def copy: DSet =
      val a = Array.ofDim[Int](size + 1, Dim + 1)
      var d = 1
      while d <= size do { System.arraycopy(op(d), 0, a(d), 0, Dim + 1); d += 1 }
      new DSet(a)

    /** Set the involution `σ_i` to pair `d ↔ e` (in place; used on a freshly copied set). */
    def set(i: Int, d: Int, e: Int): Unit =
      op(d)(i) = e
      if e >= 1 && e <= size then op(e)(i) = d

  object DSet:
    def empty1: DSet = new DSet(Array.ofDim[Int](2, Dim + 1)) // one chamber, all undefined

  /** A `(i, i+1)`-orbit of chambers (a tile, vertex, or edge), with whether it is a "chain" (touches a
    * boundary / has a fixed point) — `r` is its rotational length, `minV` the smallest legal v-value.
    */
  final case class Orbit(i: Int, j: Int, elements: Vector[Int], isChain: Boolean):
    def length: Int = elements.length
    def r: Int      = if isChain then length else (length + 1) / 2
    def minV: Int   = math.ceil(3.0 / r).toInt

  /** All `(i, j)`-orbits, where `j = i+1`. Mirrors `orbits(ds, i, j)`. */
  private def orbits(ds: DSet, i: Int, j: Int): Vector[Orbit] =
    val seen = Array.fill(ds.size + 1)(false)
    val out  = Vector.newBuilder[Orbit]
    var d    = 1
    while d <= ds.size do
      if !seen(d) then
        val orb     = mutable.ArrayBuffer(d)
        seen(d) = true
        var isChain = false
        var e       = d
        var k       = i
        var go      = true
        while go do
          val ek = ds.get(k, e)
          isChain = isChain || ek == e
          e = if ek == 0 then e else ek
          k = i + j - k
          if !seen(e) then { seen(e) = true; orb += e }
          if e == d && k == i then go = false
        out += Orbit(i, j, orb.toVector, isChain)
      d += 1
    out.result()

  private def partialOrientation(ds: DSet): Array[Int] =
    val ori = Array.fill(ds.size + 1)(0)
    if ds.size >= 1 then
      ori(1) = 1
      val q = mutable.Stack(1)
      while q.nonEmpty do
        val d = q.pop()
        var i = 0
        while i <= Dim do
          val di = ds.get(i, d)
          if di != 0 && ori(di) == 0 then { ori(di) = -ori(d); q.push(di) }
          i += 1
    ori

  private def isLoopless(ds: DSet): Boolean =
    var i = 0
    while i <= Dim do
      var d = 1
      while d <= ds.size do { if ds.get(i, d) == d then return false; d += 1 }
      i += 1
    true

  private def isWeaklyOriented(ds: DSet): Boolean =
    val ori = partialOrientation(ds)
    var i   = 0
    while i <= Dim do
      var d = 1
      while d <= ds.size do
        val di = ds.get(i, d)
        if di != d && di != 0 && ori(di) == ori(d) then return false
        d += 1
      i += 1
    true

  private def isOriented(ds: DSet): Boolean = isLoopless(ds) && isWeaklyOriented(ds)

  /** A morphism fixing the structure and sending chamber 1 → `d0`, or `None` if none (used for
    * automorphisms).
    */
  private def morphism(ds: DSet, d0: Int): Option[Array[Int]] =
    val m = Array.fill(ds.size + 1)(0)
    m(1) = d0
    val q = mutable.Queue((1, d0))
    while q.nonEmpty do
      val (d, e) = q.dequeue()
      var i      = 0
      while i <= Dim do
        val di = ds.get(i, d)
        val ei = ds.get(i, e)
        if di > 0 || ei > 0 then
          if m(di) == 0 then { m(di) = ei; q.enqueue((di, ei)) }
          else if m(di) != ei then return None
        i += 1
    Some(m)

  private def automorphisms(ds: DSet): Vector[Array[Int]] = (1 to ds.size).iterator.flatMap(d =>
    morphism(ds, d)
  ).toVector

  // ---- generic backtracking (DFS, streamed via callback) ----------------------------------------------

  private trait BackTracker[R, S]:
    def root: S
    def children(st: S): List[S]
    def extract(st: S): Option[R]
    def foreach(f: R => Unit): Unit =
      def go(st: S): Unit =
        extract(st).foreach(f)
        children(st).foreach(go)
      go(root)

  // ---- D-SET generator (enumerate the involution structures up to maxSize chambers) -------------------

  final private case class DSetGenState(ds: DSet, isRemapStart: Array[Boolean])

  final private class DSetGenerator(maxSize: Int) extends BackTracker[DSet, DSetGenState]:
    def root: DSetGenState = DSetGenState(DSet.empty1, Array.fill(maxSize + 1)(false))

    def extract(st: DSetGenState): Option[DSet] =
      if firstUndefined(st.ds).isEmpty then Some(st.ds) else None

    def children(st: DSetGenState): List[DSetGenState] =
      firstUndefined(st.ds) match
        case None         => Nil
        case Some((d, i)) =>
          val out = List.newBuilder[DSetGenState]
          var e   = d
          val cap = math.min(st.ds.size + 1, maxSize)
          while e <= cap do
            if st.ds.get(i, e) == 0 then
              val isRemapStart         = st.isRemapStart.clone()
              val dset                 = if e > st.ds.size then { isRemapStart(e) = true; st.ds.grown }
              else st.ds.copy
              dset.set(i, d, e)
              val (head, tail, gap, k) = scan02Orbit(dset, d)
              var ok                   = true
              if gap == 1 then dset.set(k, head, tail)
              else if gap == 0 && head != tail then ok = false
              if ok && regularFeasible(dset) && checkCanonicity(dset, isRemapStart) then
                out += DSetGenState(dset, isRemapStart)
            e += 1
          out.result()

  private def firstUndefined(ds: DSet): Option[(Int, Int)] =
    var d = 1
    while d <= ds.size do
      var i = 0
      while i <= Dim do { if ds.get(i, d) == 0 then return Some((d, i)); i += 1 }
      d += 1
    None

  // Admissible orbit r-values for a euclidean tiling by regular {3,4,6,8,12}-gons: a 01-orbit (a TILE) has
  // m₀₁ = r·v ∈ {3,4,6,8,12} ⇒ r₀₁ divides one of those ⇒ r₀₁ ∈ {1,2,3,4,6,8,12}; a 12-orbit (a VERTEX) has
  // degree m₁₂ = r·v ∈ {3,4,5,6} ⇒ r₁₂ ∈ {1,2,3,4,5,6}. A CLOSED orbit is final, so a partial D-set with a
  // closed orbit of inadmissible r can never become such a tiling and is pruned (kills the hyperbolic /
  // high-degree branches — a degree-7 vertex orbit closing is dropped at once).
  private val admissibleR01 = Set(1, 2, 3, 4, 6, 8, 12)
  private val admissibleR12 = Set(1, 2, 3, 4, 5, 6)

  private def regularFeasible(ds: DSet): Boolean =
    feasibleOrbits(ds, 0, 1, admissibleR01, 12) && feasibleOrbits(ds, 1, 2, admissibleR12, 6)

  /** Sound only if every (i,j)-orbit can still become a regular tile / valid vertex: a CLOSED orbit must have
    * an admissible `r`; an OPEN orbit already past `maxR` (it can only grow) will close inadmissibly, so it
    * is pruned too. `maxR` is the largest admissible `r` (12 for tiles, 6 for vertices).
    */
  private def feasibleOrbits(ds: DSet, i: Int, j: Int, admissible: Set[Int], maxR: Int): Boolean =
    val seen = Array.fill(ds.size + 1)(false)
    var d    = 1
    while d <= ds.size do
      if !seen(d) then
        var e        = d
        var k        = i
        var len      = 0
        var isChain  = false
        var complete = true
        var go       = true
        while go do
          if !seen(e) then { seen(e) = true; len += 1 }
          val ek = ds.get(k, e)
          if ek == 0 then { complete = false; go = false } // hit an undefined op ⇒ orbit not yet closed
          else
            if ek == e then isChain = true
            e = ek
            k = i + j - k
            if e == d && k == i then go = false
        if complete then
          val r = if isChain then len else (len + 1) / 2
          if !admissible(r) then return false
        else if len > 2 * maxR then return false // open orbit already too long to ever be admissible
      d += 1
    true

  /** Scan the alternating 0,2-orbit from `d`; returns (head, tail, gap, k) — the manifold-closure helper. */
  private def scan02Orbit(ds: DSet, d: Int): (Int, Int, Int, Int) =
    val (head, i) = scan(ds, Array(0, 2, 0, 2), d, 4)
    val (tail, j) = scan(ds, Array(2, 0, 2, 0), d, 4 - i)
    (head, tail, 4 - i - j, 2 * (i % 2))

  private def scan(ds: DSet, w: Array[Int], d: Int, limit: Int): (Int, Int) =
    var e = d
    var k = 0
    while k < limit && ds.get(w(k), e) != 0 do { e = ds.get(w(k), e); k += 1 }
    (e, k)

  /** Keep only the lexicographically-minimal chamber numbering (mirrors `checkCanonicity!`). */
  private def checkCanonicity(ds: DSet, isRemapStart: Array[Boolean]): Boolean =
    val n2o = Array.fill(ds.size + 1)(0)
    val o2n = Array.fill(ds.size + 1)(0)
    var d   = 1
    while d <= ds.size do
      if isRemapStart(d) then
        val cmp = compareRenumberedFrom(ds, d, n2o, o2n)
        if cmp < 0 then return false
        else if cmp > 0 then isRemapStart(d) = false
      d += 1
    true

  private def compareRenumberedFrom(ds: DSet, d0: Int, n2o: Array[Int], o2n: Array[Int]): Int =
    java.util.Arrays.fill(n2o, 0)
    java.util.Arrays.fill(o2n, 0)
    n2o(1) = d0
    o2n(d0) = 1
    var next = 2
    var d    = 1
    while d <= ds.size do
      var i = 0
      while i <= Dim do
        val ei = ds.get(i, n2o(d))
        if ei == 0 then return 0
        if o2n(ei) == 0 then { o2n(ei) = next; n2o(next) = ei; next += 1 }
        val di = ds.get(i, d)
        if di == 0 then return 0
        else if o2n(ei) != di then return o2n(ei) - di
        i += 1
      d += 1
    0

  // ---- D-SYMBOL (a D-set with v-values per 01- and 12-orbit) -------------------------------------------

  final class DSymbol(
      val dset: DSet,
      val orbs: Vector[Orbit],
      val orbitIndex: Array[Array[Int]],
      val vs: Array[Int]
  ):
    def size: Int                = dset.size
    def get(i: Int, d: Int): Int = dset.get(i, d)

    /** v-value of the (i,j)-orbit through `d` (1 for the two non-adjacent index pairs). */
    def v(i: Int, j: Int, d: Int): Int =
      if d < 1 || d > size then 0
      else if j == i + 1 then vs(orbitIndex(j)(d))
      else if i == j + 1 then vs(orbitIndex(i)(d))
      else if i != j && get(i, d) == get(j, d) then 2
      else 1

    private def rOf(i: Int, j: Int, d: Int): Int =
      if j == i + 1 then orbs(orbitIndex(j)(d)).r
      else if i == j + 1 then orbs(orbitIndex(i)(d)).r
      else if i != j && get(i, d) == get(j, d) then 1
      else 2

    /** `m_{i,i+1}` through `d` — the polygon side-count (i=0) or vertex degree (i=1). */
    def m(i: Int, j: Int, d: Int): Int = rOf(i, j, d) * v(i, j, d)

  private def collectOrbits(ds: DSet): (Vector[Orbit], Array[Array[Int]]) =
    val all   = Vector.newBuilder[Orbit]
    val index = Array.fill(Dim + 1, ds.size + 1)(0)
    var built = Vector.empty[Orbit]
    var i     = 1
    while i <= Dim do
      for orb <- orbits(ds, i - 1, i) do
        built = built :+ orb
        all += orb
        for d <- orb.elements do index(i)(d) = built.length - 1
      i += 1
    (built, index)

  private def mkSymbol(ds: DSet, vs: Array[Int]): DSymbol =
    val (orbs, index) = collectOrbits(ds)
    new DSymbol(ds, orbs, index, vs)

  // ---- D-SYMBOL generator (assign v-values; keep euclidean & spherical via curvature) -----------------

  final private case class DSymGenState(vs: Array[Int], curv: Frac, next: Int)

  final private class DSymGenerator(ds: DSet) extends BackTracker[DSymbol, DSymGenState]:
    private val orbs: Vector[Orbit]       = orbits(ds, 0, 1) ++ orbits(ds, 1, 2)
    private val orbMaps: Set[Vector[Int]] =
      automorphisms(ds).map(m => onOrbits(m)).toSet

    def root: DSymGenState =
      val vs = orbs.map(_.minV).toArray
      DSymGenState(vs, curvature(vs), 1)

    def extract(st: DSymGenState): Option[DSymbol] =
      if st.next > orbs.length && goodResult(st) && isCanonical(st) then Some(mkSymbol(ds, st.vs))
      else None

    def children(st: DSymGenState): List[DSymGenState] =
      if st.next > orbs.length then Nil
      else if st.curv.signum < 0 then List(DSymGenState(st.vs, st.curv, orbs.length + 1))
      else
        val orb = orbs(st.next - 1)
        val k   = if orb.isChain then 1 else 2
        val out = List.newBuilder[DSymGenState]
        var v   = st.vs(st.next - 1)
        var go  = true
        while v <= 7 && go do
          val vs   = st.vs.clone()
          vs(st.next - 1) = v
          val curv = st.curv - Frac(k, st.vs(st.next - 1)) + Frac(k, v)
          if curv.signum >= 0 || isMinimallyHyperbolic(curv, vs) then
            out += DSymGenState(vs, curv, st.next + 1)
          if curv.signum < 0 then go = false
          v += 1
        out.result()

    private def curvature(vs: Array[Int]): Frac =
      var result = Frac(-ds.size, 2)
      var idx    = 0
      while idx < orbs.length do
        result = result + Frac(if orbs(idx).isChain then 1 else 2, vs(idx))
        idx += 1
      result

    private def isMinimallyHyperbolic(curv: Frac, vs: Array[Int]): Boolean =
      if curv.signum >= 0 then false
      else
        var idx = 0
        while idx < orbs.length do
          val k = if orbs(idx).isChain then 1 else 2
          val v = vs(idx)
          if v > orbs(idx).minV && (curv - Frac(k, v) + Frac(k, v - 1)).signum < 0 then return false
          idx += 1
        true

    private def isCanonical(st: DSymGenState): Boolean =
      // canonical iff no automorphism-induced relabeling makes vs lexicographically smaller: vs[m] <= vs
      orbMaps.forall: m =>
        var idx = 0
        var cmp = 0
        while idx < st.vs.length && cmp == 0 do
          cmp = st.vs(m(idx)) - st.vs(idx)
          idx += 1
        cmp <= 0

    private def onOrbits(map: Array[Int]): Vector[Int] =
      val inOrb  = Array.fill(Dim + 1, Dim + 1, ds.size + 1)(0)
      for k <- orbs.indices; d <- orbs(k).elements do
        inOrb(orbs(k).i)(orbs(k).j)(d) = k
      val orbMap = Array.fill(orbs.length)(0)
      var d      = 1
      while d <= ds.size do
        var i = 0
        while i < Dim do { orbMap(inOrb(i)(i + 1)(d)) = inOrb(i)(i + 1)(map(d)); i += 1 }
        d += 1
      orbMap.toVector

    /** Euclidean (curv 0) always passes; spherical (curv > 0) must be a genuine spherical orbifold. */
    private def goodResult(st: DSymGenState): Boolean =
      if st.curv.signum <= 0 then true
      else
        val cones   = mutable.ArrayBuffer.empty[Int]
        val corners = mutable.ArrayBuffer.empty[Int]
        for orb <- orbits(ds, 0, 2) do
          if orb.isChain then { if orb.length == 1 then corners += 2 }
          else if orb.length == 2 then cones += 2
        var idx     = 0
        while idx < orbs.length do
          if st.vs(idx) > 1 then (if orbs(idx).isChain then corners else cones) += st.vs(idx)
          idx += 1
        val front   = cones.sorted.reverse.mkString
        val middle  = if isLoopless(ds) then "" else "*"
        val back    = corners.sorted.reverse.mkString
        val cross   = if isWeaklyOriented(ds) then "" else "x"
        goodKeys.contains(front + middle + back + cross)

  private val goodKeys: Set[String] =
    Set(
      "",
      "*",
      "x",
      "532",
      "432",
      "332",
      "422",
      "322",
      "222",
      "44",
      "33",
      "22",
      "*532",
      "*432",
      "*332",
      "3*2",
      "*422",
      "*322",
      "*222",
      "2*4",
      "2*3",
      "2*2",
      "*44",
      "*33",
      "*22",
      "4*",
      "3*",
      "2*",
      "4x",
      "3x",
      "2x"
    )

  // ---- the Krotenheerdt / regular-polygon filter ------------------------------------------------------

  /** The cyclic sequence of polygon side-counts around the vertex whose 12-orbit contains chamber `d`: step
    * `d := op₂(op₁(d))` walks face-by-face around the vertex; each step's `m₀₁` is a polygon.
    */
  private def vertexConfig(ds: DSymbol, d: Int): Option[List[Int]] =
    val frag = mutable.ArrayBuffer.empty[Int]
    var cur  = d
    var go   = true
    while go do
      frag += ds.m(0, 1, cur)
      val nxt = ds.get(2, ds.get(1, cur))
      cur = if nxt == 0 then cur else nxt
      if cur == d then go = false
      else if frag.length > 24 then return None // runaway guard
    // the walk traverses only the quotient fragment (r₁₂ faces); the genuine vertex degree is m₁₂ = r₁₂·v₁₂,
    // so the geometric vertex is the fragment repeated to length m₁₂ (the symbol's symmetry folds the vertex).
    val m12  = ds.m(1, 2, d)
    if frag.isEmpty || m12 % frag.length != 0 then None
    else Some(List.fill(m12 / frag.length)(frag.toList).flatten)

  /** True iff the symbol is a euclidean tiling by regular polygons `{3,4,6,8,12}` with every vertex a valid
    * 360° type. Returns the vertex type signatures (one per 12-orbit) when valid, else None.
    */
  private def regularPolygonVertices(ds: DSymbol): Option[List[VertexSignature]] =
    // tiles: every 01-orbit's m₀₁ must be an admissible polygon
    val faceOK = orbits(ds.dset, 0, 1).forall(o => polygonSides.contains(ds.m(0, 1, o.elements.head)))
    if !faceOK then None
    else
      val sigs = orbits(ds.dset, 1, 2).map(o => vertexConfig(ds, o.elements.head))
      if sigs.forall(_.exists(isCompleteVertex)) then Some(sigs.flatten.map(normalize).toList) else None

  /** One enumerated Krotenheerdt tiling: its `n` (= vertex-orbit count = distinct-type count), the per-orbit
    * vertex configurations (`vertices.length == n`), the type set, and the chamber count of its minimal
    * Delaney–Dress symbol.
    */
  final case class Tiling(n: Int, vertices: List[VertexSignature], chambers: Int):
    def types: Set[VertexSignature] = vertices.toSet

  /** Enumerate the Krotenheerdt tilings (n vertex orbits = n distinct vertex types) with `1 ≤ n ≤ maxN`, over
    * Delaney–Dress symbols of up to `maxSize` chambers — full detail. `maxSize` is the completeness bound (a
    * cell needing more chambers is not reached) — the combinatorial analogue of the fixed-Λ covolume cap, but
    * bounded by cell SIZE, not covolume.
    */
  def enumerateDetailed(maxN: Int, maxSize: Int): List[Tiling] =
    val out = List.newBuilder[Tiling]
    DSetGenerator(maxSize).foreach: dset =>
      // EUCLIDEAN-feasibility gate: with v-values at their minimum the curvature is MAXIMAL; if even that is
      // negative, every v-assignment is hyperbolic and no flat (curvature-0) tiling exists ⇒ skip the whole
      // DSymGenerator for this D-set. This is what restricts the search to the flat world.
      if euclideanFeasible(dset) then
        DSymGenerator(dset).foreach: dsym =>
          // Filters cheapest-first: euclidean (curvature 0) ⇒ regular-polygon tiling with valid 360° vertices
          // ⇒ MINIMAL (maximal-symmetry symbol; the O(size²) check, so run it LAST, after the cheap ones have
          // discarded the many euclidean-but-not-regular symbols).
          if isEuclidean(dsym) then
            regularPolygonVertices(dsym).foreach: sigs =>
              val orbitCount = sigs.length
              val typeCount  = sigs.toSet.size
              if orbitCount == typeCount && orbitCount <= maxN && isMinimal(dsym) then
                out += Tiling(orbitCount, sigs, dsym.size)
    out.result()

  /** True iff a flat (curvature-0) tiling is achievable on this D-set: the MAXIMAL curvature (every v at its
    * minimum `minV`) is ≥ 0. Raising any v only lowers the curvature, so `maxCurv < 0` ⇒ purely hyperbolic.
    */
  private def euclideanFeasible(ds: DSet): Boolean =
    var c = Frac.make(-ds.size, 2)
    for orb <- orbits(ds, 0, 1) ++ orbits(ds, 1, 2) do
      c = c + Frac.make(if orb.isChain then 1 else 2, orb.minV)
    c.signum >= 0

  /** Bucketed `(n, vertex-type-set)` view of [[enumerateDetailed]]. */
  def enumerate(maxN: Int, maxSize: Int): List[(Int, Set[VertexSignature])] =
    enumerateDetailed(maxN, maxSize).map(t => (t.n, t.types))

  /** A D-symbol is MINIMAL iff it does not properly cover a smaller symbol. For each candidate `d0`, form the
    * coarsest op-congruence that identifies chamber 1 with `d0` (close under `a~b ⇒ op_i(a)~op_i(b)`); if it
    * has fewer classes than chambers AND every class is m-constant (so the quotient preserves the m-values, a
    * genuine covering), the symbol covers that smaller quotient and is therefore NOT minimal. A non-minimal
    * symbol is the same geometric tiling carrying a subgroup of its symmetry (more vertex orbits than n).
    */
  private def isMinimal(ds: DSymbol): Boolean =
    val n  = ds.size
    var d0 = 2
    while d0 <= n do
      val parent                         = Array.tabulate(n + 1)(identity)
      def find(x: Int): Int              = {
        var r = x; while parent(r) != r do r = parent(r); var c = x;
        while parent(c) != c do { val p = parent(c); parent(c) = r; c = p }; r
      }
      def union(a: Int, b: Int): Boolean =
        val (ra, rb) = (find(a), find(b))
        if ra == rb then false else { parent(ra) = rb; true }
      val queue                          = mutable.Queue((1, d0))
      union(1, d0)
      while queue.nonEmpty do
        val (a, b) = queue.dequeue()
        var i      = 0
        while i <= Dim do
          val (ai, bi) = (ds.get(i, a), ds.get(i, b))
          if union(ai, bi) then queue.enqueue((ai, bi))
          i += 1
      val classes                        = (1 to n).map(find).toSet
      if classes.size < n then
        // m-constant on every class ⇒ the quotient is a valid covering ⇒ not minimal
        val m01 = Array.fill(n + 1)(-1)
        val m12 = Array.fill(n + 1)(-1)
        var ok  = true
        var d   = 1
        while d <= n && ok do
          val rep = find(d)
          if m01(rep) < 0 then { m01(rep) = ds.m(0, 1, d); m12(rep) = ds.m(1, 2, d) }
          else if m01(rep) != ds.m(0, 1, d) || m12(rep) != ds.m(1, 2, d) then ok = false
          d += 1
        if ok then return false
      d0 += 1
    true

  private def isEuclidean(ds: DSymbol): Boolean =
    var result = Frac(-ds.size, 2)
    val all    = orbits(ds.dset, 0, 1) ++ orbits(ds.dset, 1, 2)
    var idx    = 0
    while idx < all.length do
      val orb = all(idx)
      result = result + Frac(if orb.isChain then 1 else 2, ds.v(orb.i, orb.j, orb.elements.head))
      idx += 1
    result.isZero

  // ---- bridge for externally-built CLOSED maps (ADR-0025 bucketed-assembly verify / key / dedup) -------

  /** Wrap the three chamber involutions of a CLOSED oriented 2-manifold map as a `v = 1` Delaney–Dress
    * symbol. `op(d)` holds `(σ₀, σ₁, σ₂)` for chamber `d`; chambers are 1-based, `op(0)` is unused, and every
    * involution must be total (a closed map has no boundary chambers). Because the map is the full
    * barycentric subdivision (no symmetry quotient), each 01-orbit's `m₀₁` is directly the polygon side-count
    * and each 12-orbit's `m₁₂` the vertex degree (`r·1`).
    */
  def closedMapSymbol(op: Array[Array[Int]]): DSymbol =
    val n             = op.length - 1
    val a             = Array.ofDim[Int](n + 1, Dim + 1)
    var d             = 1
    while d <= n do
      var i = 0
      while i <= Dim do { a(d)(i) = op(d)(i); i += 1 }
      d += 1
    val ds            = new DSet(a)
    val (orbs, index) = collectOrbits(ds)
    new DSymbol(ds, orbs, index, Array.fill(orbs.length)(1))

  /** The MINIMAL (maximal-symmetry) Delaney–Dress symbol covered by `ds`: quotient by a proper m-preserving
    * op-congruence, iterated to a fixed point. Every torus cover of one tiling reduces to the SAME minimal
    * symbol (Delaney–Dress: it is a complete invariant), so it is the canonical identity for dedup, and its
    * `n` vertex orbits / distinct types are the Krötenheerdt quantities — independent of the chosen cell.
    */
  def minimalSymbol(ds: DSymbol): DSymbol =
    var cur  = ds
    var step = reduceOnce(cur)
    while step.isDefined do { cur = step.get; step = reduceOnce(cur) }
    cur

  /** One quotient step: the coarsest m-constant op-congruence identifying chamber 1 with some `d0`, or `None`
    * if the symbol is already minimal. Mirrors [[isMinimal]] but BUILDS the quotient symbol.
    */
  private def reduceOnce(ds: DSymbol): Option[DSymbol] =
    val n  = ds.size
    var d0 = 2
    while d0 <= n do
      val parent                         = Array.tabulate(n + 1)(identity)
      def find(x: Int): Int              = {
        var r = x; while parent(r) != r do r = parent(r); var c = x;
        while parent(c) != c do { val p = parent(c); parent(c) = r; c = p }; r
      }
      def union(a: Int, b: Int): Boolean =
        val (ra, rb) = (find(a), find(b))
        if ra == rb then false else { parent(ra) = rb; true }
      val queue                          = mutable.Queue((1, d0))
      union(1, d0)
      while queue.nonEmpty do
        val (a, b) = queue.dequeue()
        var i      = 0
        while i <= Dim do
          val (ai, bi) = (ds.get(i, a), ds.get(i, b))
          if union(ai, bi) then queue.enqueue((ai, bi))
          i += 1
      if (1 to n).map(find).toSet.size < n then
        val m01 = Array.fill(n + 1)(-1)
        val m12 = Array.fill(n + 1)(-1)
        var ok  = true
        var d   = 1
        while d <= n && ok do
          val rep = find(d)
          if m01(rep) < 0 then { m01(rep) = ds.m(0, 1, d); m12(rep) = ds.m(1, 2, d) }
          else if m01(rep) != ds.m(0, 1, d) || m12(rep) != ds.m(1, 2, d) then ok = false
          d += 1
        if ok then return Some(quotient(ds, Array.tabulate(n + 1)(find)))
      d0 += 1
    None

  /** Quotient `ds` by the class map `cls` (an m-constant op-congruence). v-values are recomputed so the
    * polygon side-counts and vertex degrees (`m₀₁`, `m₁₂`) are preserved: `v_new = m_original / r_new`.
    */
  private def quotient(ds: DSymbol, cls: Array[Int]): DSymbol =
    val n             = ds.size
    val label         = mutable.LinkedHashMap.empty[Int, Int] // class rep -> new 1-based label
    val repOf         = mutable.ArrayBuffer(0)                // new label -> a representative original chamber
    var d             = 1
    while d <= n do
      val r = cls(d)
      if !label.contains(r) then { label(r) = label.size + 1; repOf += r }
      d += 1
    val c             = label.size
    val a             = Array.ofDim[Int](c + 1, Dim + 1)
    var lab           = 1
    while lab <= c do
      val orig = repOf(lab)
      var i    = 0
      while i <= Dim do { a(lab)(i) = label(cls(ds.get(i, orig))); i += 1 }
      lab += 1
    val qds           = new DSet(a)
    val (orbs, index) = collectOrbits(qds)
    val vs            = Array.tabulate(orbs.length): k =>
      val orb   = orbs(k)
      val mOrig =
        if orb.i == 0 then ds.m(0, 1, repOf(orb.elements.head)) else ds.m(1, 2, repOf(orb.elements.head))
      mOrig / orb.r
    new DSymbol(qds, orbs, index, vs)

  /** A canonical key for a CLOSED symbol: the lexicographically minimal BFS-renumbered trace of the three
    * involutions plus `(m₀₁, m₁₂)` per chamber, over every start chamber. Two symbols are isomorphic iff
    * their keys are equal — the coordinate-free dedup id ADR-0025 asks for.
    */
  def canonicalKey(ds: DSymbol): String =
    val n            = ds.size
    var best: String = null
    var s            = 1
    while s <= n do
      val o2n   = Array.fill(n + 1)(0)
      val n2o   = Array.fill(n + 1)(0)
      o2n(s) = 1; n2o(1) = s
      var next  = 2
      val trace = new StringBuilder
      var d     = 1
      while d <= n do
        val orig = n2o(d)
        var i    = 0
        while i <= Dim do
          val ei = ds.get(i, orig)
          if o2n(ei) == 0 then { o2n(ei) = next; n2o(next) = ei; next += 1 }
          trace.append(o2n(ei)).append(',')
          i += 1
        trace.append(ds.m(0, 1, orig)).append('|').append(ds.m(1, 2, orig)).append(';')
        d += 1
      val t     = trace.toString
      if best == null || t < best then best = t
      s += 1
    best

  /** Classify a CLOSED oriented map (given its chamber involutions) as a regular-polygon torus tiling.
    * `Some((n, vertices, key))` iff it is euclidean (curvature 0 — a torus, since the construction is
    * orientable), a `{3,4,6,8,12}` regular-polygon tiling with valid 360° vertices: `n` = vertex orbits of
    * the MINIMAL symbol, `vertices` their configs, `key` the minimal symbol's canonical key. The caller
    * applies the Krötenheerdt condition (`n` orbits = `n` distinct types). No overlap test is needed — an
    * intrinsic closed all-360° map is a genuine flat tiling (ADR-0022).
    */
  def classifyClosedMap(op: Array[Array[Int]]): Option[(Int, List[VertexSignature], String)] =
    val full = closedMapSymbol(op)
    if !isEuclidean(full) || regularPolygonVertices(full).isEmpty then None
    else
      val min = minimalSymbol(full)
      regularPolygonVertices(min).map(sigs => (sigs.length, sigs, canonicalKey(min)))

  /** [[enumerateDetailed]] augmented with each tiling's minimal-symbol canonical key, so an external
    * enumerator (e.g. the ADR-0025 bucketed assembler) can be cross-checked key-for-key, not just by count.
    */
  def keyedTilings(maxN: Int, maxSize: Int): List[(Int, Set[VertexSignature], String)] =
    val out = List.newBuilder[(Int, Set[VertexSignature], String)]
    DSetGenerator(maxSize).foreach: dset =>
      if euclideanFeasible(dset) then
        DSymGenerator(dset).foreach: dsym =>
          if isEuclidean(dsym) then
            regularPolygonVertices(dsym).foreach: sigs =>
              if sigs.length == sigs.toSet.size && sigs.length <= maxN && isMinimal(dsym) then
                out += ((sigs.length, sigs.toSet, canonicalKey(dsym)))
    out.result()

  /** [[enumerateDetailed]] returning the minimal Delaney SYMBOL itself (not just its type-set) — so an
    * orbifold-directed enumerator can inspect the symbols it must reproduce.
    */
  def enumerateSymbols(maxN: Int, maxSize: Int): List[(Int, List[VertexSignature], DSymbol)] =
    val out = List.newBuilder[(Int, List[VertexSignature], DSymbol)]
    DSetGenerator(maxSize).foreach: dset =>
      if euclideanFeasible(dset) then
        DSymGenerator(dset).foreach: dsym =>
          if isEuclidean(dsym) then
            regularPolygonVertices(dsym).foreach: sigs =>
              if sigs.length == sigs.toSet.size && sigs.length <= maxN && isMinimal(dsym) then
                out += ((sigs.length, sigs, dsym))
    out.result()

  /** Quantifies the orbifold approach's potential: how many COMPLETE D-sets the generate-all generator walks
    * vs how many are euclidean-feasible (curvature ≥ 0 achievable). The euclidean fraction is the slice an
    * orbifold-directed generator would visit; `1 - fraction` is the hyperbolic universe it would skip.
    * Returns `(totalDSets, euclideanFeasibleDSets, regularEuclideanSymbols)`.
    */
  def generationStats(maxN: Int, maxSize: Int): (Long, Long, Long) =
    var total = 0L
    var eucl  = 0L
    var reg   = 0L
    DSetGenerator(maxSize).foreach: dset =>
      total += 1
      if euclideanFeasible(dset) then
        eucl += 1
        DSymGenerator(dset).foreach: dsym =>
          if isEuclidean(dsym) then
            regularPolygonVertices(dsym).foreach: sigs =>
              if sigs.length == sigs.toSet.size && sigs.length <= maxN && isMinimal(dsym) then reg += 1
    (total, eucl, reg)

  /** The orbifold "shape" of a minimal symbol — what an orbifold-directed generator would FIX before
    * enumerating. `D` = chambers, `t/v/e` = number of tile / vertex / edge orbits, orientability, whether it
    * has mirror boundaries (loops), and the cone/corner orders (orbit v-values > 1 = rotation orders). Two
    * minimal symbols sharing this signature are triangulations of the same euclidean orbifold.
    */
  def orbifoldSignature(ds: DSymbol): String =
    val o01     = orbits(ds.dset, 0, 1)
    val o12     = orbits(ds.dset, 1, 2)
    val o02     = orbits(ds.dset, 0, 2)
    val ori     = isOriented(ds.dset)
    val mir     = !isLoopless(ds.dset)
    val cones01 = o01.toList.map(o => ds.v(0, 1, o.elements.head)).filter(_ > 1).sorted
    val cones12 = o12.toList.map(o => ds.v(1, 2, o.elements.head)).filter(_ > 1).sorted
    s"D=${ds.size} t=${o01.length} v=${o12.length} e=${o02.length} ori=$ori mir=$mir " +
      s"cone-tile=[${cones01.mkString(",")}] cone-vert=[${cones12.mkString(",")}]"

  // ---- ADR-0023 Stage 1: ORIENTED-slice generator (rotation orbifolds o/2222/333/442/632) --------------

  /** Like [[DSetGenerator]] but restricted to CLOSED, ORIENTED D-sets — no `σ_i` fixed points (no mirror
    * boundaries) and a consistent 2-colouring (orientable). This is the generation slice of the rotation
    * orbifolds. A mirror tiling is still recovered here as its ORIENTED double cover (≤ 2× the chambers of
    * its mirror minimal symbol); the A068600 vertex-orbit count is then taken on the FULL [[minimalSymbol]]
    * (whose automorphisms include the orientation-reversing reflections), so nothing is lost.
    */
  final private class OrientedDSetGenerator(maxSize: Int) extends BackTracker[DSet, DSetGenState]:
    def root: DSetGenState = DSetGenState(DSet.empty1, Array.fill(maxSize + 1)(false))

    def extract(st: DSetGenState): Option[DSet] =
      if firstUndefined(st.ds).isEmpty && isLoopless(st.ds) && isWeaklyOriented(st.ds) then Some(st.ds)
      else None

    def children(st: DSetGenState): List[DSetGenState] =
      firstUndefined(st.ds) match
        case None         => Nil
        case Some((d, i)) =>
          val out = List.newBuilder[DSetGenState]
          var e   = d + 1 // NO fixed point: never pair a chamber with itself (that would be a mirror)
          val cap = math.min(st.ds.size + 1, maxSize)
          while e <= cap do
            if st.ds.get(i, e) == 0 then
              val isRemapStart         = st.isRemapStart.clone()
              val dset                 = if e > st.ds.size then { isRemapStart(e) = true; st.ds.grown }
              else st.ds.copy
              dset.set(i, d, e)
              val (head, tail, gap, k) = scan02Orbit(dset, d)
              var ok                   = true
              if gap == 1 then { if head == tail then ok = false else dset.set(k, head, tail) }
              else if gap == 0 && head != tail then ok = false
              if ok && isLoopless(dset) && regularFeasible(dset) && isWeaklyOriented(dset)
                && checkCanonicity(dset, isRemapStart)
              then out += DSetGenState(dset, isRemapStart)
            e += 1
          out.result()

  /** Enumerate the regular-polygon euclidean tilings via the ORIENTED slice (ADR-0023 Stage 1): generate
    * oriented closed D-sets, assign euclidean v-values, keep regular `{3,4,6,8,12}`-gon tilings, then key and
    * bucket by the FULL [[minimalSymbol]] (so `n` = vertex orbits under the complete symmetry, incl.
    * mirrors). Deduped by minimal canonical key. Returns `(n, vertices, key)`.
    */
  def orientedRegularSymbols(maxN: Int, maxSize: Int): List[(Int, List[VertexSignature], String)] =
    val out  = List.newBuilder[(Int, List[VertexSignature], String)]
    val seen = mutable.Set.empty[String]
    OrientedDSetGenerator(maxSize).foreach: dset =>
      if euclideanFeasible(dset) then
        DSymGenerator(dset).foreach: dsym =>
          if isEuclidean(dsym) && regularPolygonVertices(dsym).isDefined then
            val mn = minimalSymbol(dsym)
            regularPolygonVertices(mn).foreach: msigs =>
              if msigs.length == msigs.toSet.size && msigs.length <= maxN then
                val key = canonicalKey(mn)
                if seen.add(key) then out += ((msigs.length, msigs, key))
    out.result()

  /** Generation cost of the oriented slice: `(orientedDSets, euclideanFeasible, regularSymbols)` — to compare
    * against [[generationStats]] (the generate-all tree) and see whether restricting to the oriented rotation
    * orbifolds shrinks the search.
    */
  def orientedGenerationStats(maxN: Int, maxSize: Int): (Long, Long, Long) =
    var total = 0L
    var eucl  = 0L
    var reg   = 0L
    val seen  = mutable.Set.empty[String]
    OrientedDSetGenerator(maxSize).foreach: dset =>
      total += 1
      if euclideanFeasible(dset) then
        eucl += 1
        DSymGenerator(dset).foreach: dsym =>
          if isEuclidean(dsym) && regularPolygonVertices(dsym).isDefined then
            val mn = minimalSymbol(dsym)
            regularPolygonVertices(mn).foreach: msigs =>
              if msigs.length == msigs.toSet.size && msigs.length <= maxN && seen.add(canonicalKey(mn)) then
                reg += 1
    (total, eucl, reg)
