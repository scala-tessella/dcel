package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.KrotenheerdtTorusMapSearch as G
import io.github.scala_tessella.dcel.KrotenheerdtTorusMapSearch.FaceZ
import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

import scala.collection.mutable

/** The profile-state engine for the banded family (ADR-0038). The state is the explicit COMBINATORIAL profile
  * — the advancing top boundary on a cylinder of circumference `c` — represented per vertex by the partial
  * fan it has already consumed (the open arc faces up; *below the profile is external/done*, NOT empty
  * faces). This is what the patch-based spike could not express, and why its state space was unbounded.
  *
  * A transition fills the lowest profile vertex (`fillVertex`): branch over the fans that complete its open
  * arc to a valid 360° vertex, splice the fan's upper boundary into the polyline, and update the two
  * neighbours' below-fans — local surgery, the completed vertex forgotten. Banded tilings are CYCLES (a
  * canonical profile recurring at vertical period Δ), closed/keyed by the spike-validated
  * [[CylinderAutomaton.close]].
  *
  * Grown at circumference `c ≥ 4` so no unit polygon's edge wraps the cylinder onto itself; `primitiveBasis`
  * recovers the true small period when closing. Geometry is exact ℤ[ζ₁₂]; primitives reused from the grower.
  */
object ProfileAutomaton:

  /** A profile vertex: its exact position and the partial fan already consumed, as `(startSlot, size)`
    * entries. The OPEN arc (where the next polygons go) is the complement of the covered slots.
    */
  final case class PV(pos: ZetaPoint, fan: List[(Int, Int)]):
    def covered: Set[Int] = G.coveredSlots(fan)
    def complete: Boolean = covered.sizeIs == 12

  /** A profile = circumference vector `c` (horizontal) + one period of vertices (positions folded to x ∈
    * [0,c)).
    */
  final case class Profile(c: ZetaPoint, verts: Vector[PV]):
    def widthX: Double = c.toBigPoint.x.toDouble

  // ---- geometry helpers (reusing grower primitives) ---------------------------------------------------

  private def cslot(poly: Vector[ZetaPoint], i: Int): Int =
    G.slotOfUnit(poly((i + 1) % poly.length) - poly(i))

  /** Fold a position to x ∈ [0, |c|) by integer multiples of `c` (c horizontal). */
  private def foldPos(p: ZetaPoint, c: ZetaPoint): ZetaPoint =
    val w = c.toBigPoint.x.toDouble
    var q = p
    while q.toBigPoint.x.toDouble >= w - 1e-7 do q = q - c
    while q.toBigPoint.x.toDouble < -1e-7 do q = q + c
    q

  private def samePos(a: ZetaPoint, b: ZetaPoint): Boolean =
    val d = a - b; d.a0 == 0 && d.a1 == 0 && d.a2 == 0 && d.a3 == 0

  /** The open arc `(startSlot, width)` of a partial fan — the contiguous run of uncovered slots, starting at
    * the first free slot whose predecessor is covered.
    */
  private def openArc(fan: List[(Int, Int)]): (Int, Int) =
    val cov  = G.coveredSlots(fan)
    val free = 12 - cov.size
    val b    = (0 until 12).find(s => cov((s + 11) % 12) && !cov(s)).getOrElse(0)
    (b, free)

  /** True iff the open (uncovered) arc faces upward (positive net y) — the filled side is below. */
  private def upFacing(fan: List[(Int, Int)]): Boolean =
    val cov = G.coveredSlots(fan)
    (0 until 12).iterator.filterNot(cov.contains).map(s => math.sin(math.toRadians(30.0 * s))).sum > 1e-9

  /** The lowest profile vertex (min y, then min x) — the taut scanline choice. */
  private def lowestIndex(p: Profile): Int =
    p.verts.indices.minBy(i => (p.verts(i).pos.toBigPoint.y.toDouble, p.verts(i).pos.toBigPoint.x.toDouble))

  // ---- the transition: fill the lowest vertex ---------------------------------------------------------

  /** Fill the lowest profile vertex: for each fan `F` that completes its open arc to a valid 360° vertex,
    * splice `F`'s upper boundary into the profile (the completed vertex is forgotten; the two neighbours'
    * below-fans grow). Returns, per fill: the successor profile, the completed vertex type, and the placed
    * faces (for closing). `c ≥ 4` ⇒ no wrap, so a polygon never contributes two corners to one cylinder
    * vertex.
    */
  def fillLowest(p: Profile): List[(Profile, VertexSignature, List[FaceZ])] =
    val j                   = lowestIndex(p)
    val L                   = p.verts(j)
    val (aboveStart, width) = openArc(L.fan)
    val orderedNow          = L.fan.sortBy((s, _) => (s - aboveStart + 12) % 12).map(_._2)
    G.completions(orderedNow, width).flatMap: f =>
      val fullType = normalize(orderedNow ++ f)
      // place F's polygons above L
      var slot     = aboveStart
      val faces    = f.map: m =>
        val poly = G.polygon(L.pos, slot, m)
        slot += G.gSlots(m)
        FaceZ(m, poly)
      // collect the fan entries each non-L corner gains from F (folded to one period; translates of L = L's own
      // completion, dropped)
      val adds     = mutable.Map.empty[ZetaPoint, List[(Int, Int)]]
      faces.foreach: face =>
        face.corners.indices.foreach: i =>
          val c = face.corners(i)
          if !samePos(c, L.pos) then
            val fp = foldPos(c, p.c)
            if !samePos(fp, L.pos) then
              adds(fp) = adds.getOrElse(fp, Nil) :+ (cslot(face.corners, i), face.size)
      // merge into the surviving vertices (all but L)
      val merged   = mutable.Map.empty[ZetaPoint, List[(Int, Int)]]
      p.verts.iterator.zipWithIndex.foreach: (pv, idx) =>
        if idx != j then merged(pv.pos) = pv.fan
      adds.foreach: (pos, es) =>
        merged.get(pos) match
          case Some(existing) => merged(pos) = existing ++ es
          case None           =>
            // a new vertex may coincide (mod c) with an existing one under a different exact rep — match by fold
            merged.keysIterator.find(k => samePos(foldPos(k, p.c), foldPos(pos, p.c))) match
              case Some(k) => merged(k) = merged(k) ++ es
              case None    => merged(pos) = es
      // keep the still-open up-facing vertices (completed ones are interior); reject overlapping fans
      if merged.values.exists(fan =>
          fan.flatMap((s, m) => (0 until G.gSlots(m)).map(k => (s + k) % 12)).pipe(slots =>
            slots.size != slots.distinct.size
          )
        )
      then None
      else
        val keep = merged.toList.collect:
          case (pos, fan) if G.coveredSlots(fan).sizeIs < 12 && upFacing(fan) => PV(pos, fan.sortBy(_._1))
        Some((Profile(p.c, keep.toVector), fullType, faces))

  extension [A](a: A) private def pipe[B](f: A => B): B = f(a)

  // ---- cycle detection + closing ----------------------------------------------------------------------

  /** The anchor of a profile: its lexicographically-min vertex position (for translation-normalisation). */
  private def anchor(p: Profile): ZetaPoint = p.verts.map(_.pos).min

  /** Translation-invariant canonical key: each vertex as `(pos − anchor, fan)`, sorted. Two profiles
    * congruent by translation (e.g. the same shape one period higher) key identically — the basis of cycle
    * detection.
    */
  private def canonKey(p: Profile): List[(Long, Long, Long, Long, List[Int])] =
    import scala.math.Ordering.Implicits.seqOrdering
    val a = anchor(p)
    p.verts.map { v =>
      val d = v.pos - a; (d.a0, d.a1, d.a2, d.a3, v.fan.map(_._2).sorted)
    }.sorted.toList

  private def dedupFaces(faces: List[FaceZ]): List[FaceZ] =
    val seen = mutable.HashSet.empty[List[(Long, Long, Long, Long)]]
    faces.filter(f => seen.add(f.corners.map(z => (z.a0, z.a1, z.a2, z.a3)).sorted.toList))

  /** Drive the automaton from `seed`, filling the lowest vertex (DFS, branching over completions) until a
    * canonical profile RECURS on the path — a cycle = a banded tiling of vertical period `Δ`. Close it via
    * the spike-validated [[CylinderAutomaton.close]] (`verifyCell` gate + `torusMapClassify` D-symbol key),
    * keeping only cells with exactly `maxN` vertex types. `maxSteps` bounds the stack height (a completeness
    * caveat).
    */
  def enumerateFrom(seed: Profile, maxN: Int, maxSteps: Int = 60): Map[String, (Int, Set[VertexSignature])] =
    val out = mutable.Map.empty[String, (Int, Set[VertexSignature])]
    // seen: canonKey -> (anchor at first occurrence, accumulated face count at first occurrence)
    def dfs(
        prof: Profile,
        faces: List[FaceZ],
        types: Set[VertexSignature],
        seen: Map[List[(Long, Long, Long, Long, List[Int])], (ZetaPoint, Int)],
        steps: Int
    ): Unit =
      if steps > maxSteps || prof.verts.isEmpty then ()
      else
        val ck = canonKey(prof)
        seen.get(ck) match
          case Some((anchor0, fc0)) =>
            val delta       = anchor(prof) - anchor0
            val periodFaces = dedupFaces(faces.drop(fc0))
            if !delta.isOrigin && periodFaces.nonEmpty then
              CylinderAutomaton.close(periodFaces, prof.c, delta, maxN).foreach: (n, ts, key) =>
                if ts.sizeIs == maxN then out.getOrElseUpdate(key, (n, ts))
          case None                 =>
            val seen2 = seen + (ck -> (anchor(prof), faces.length))
            fillLowest(prof).foreach: (next, t, newFaces) =>
              val types2 = types + t
              if types2.sizeIs <= maxN then dfs(next, faces ++ newFaces, types2, seen2, steps + 1)
    dfs(seed, Nil, Set.empty, Map.empty, 0)
    out.toMap

  // ---- seeds (the StripBand catalogue IS the set of lower structures) ---------------------------------

  private def mul(p: ZetaPoint, k: Int): ZetaPoint = ZetaPoint(p.a0 * k, p.a1 * k, p.a2 * k, p.a3 * k)

  /** The seed profile carried by a band of `faces` below it: its UP-FACING incomplete vertices (the top
    * boundary) with their below-fans, positions folded to one period of `c`. The down-facing bottom of the
    * band is external. `None` if the faces expose no up-facing frontier.
    */
  def seedFromFaces(faces: List[FaceZ], c: ZetaPoint): Option[Profile] =
    val w     = c.toBigPoint.x.toDouble
    val byPos = mutable.Map.empty[ZetaPoint, List[(Int, Int)]]
    G.vertexFansBy(faces, identity[ZetaPoint]).foreach: (_, repFan) =>
      val (p, fan) = repFan
      if G.coveredSlots(fan).sizeIs < 12 && upFacing(fan) then
        val fp = foldPos(p, c)
        byPos.keysIterator.find(k => samePos(k, fp)) match
          case Some(k) => byPos(k) = byPos(k) ++ fan
          case None    => byPos(fp) = fan
    val verts = byPos.toList.collect:
      case (p, fan) if G.coveredSlots(fan).sizeIs < 12 && upFacing(fan) => PV(p, fan.sortBy(_._1))
    Option.when(verts.nonEmpty)(Profile(c, verts.toVector))

  /** Seed profiles at circumference `c` (any horizontal ℤ[ζ₁₂] vector — integer or √3-family): each
    * [[StripBand]] band whose period divides `c` is replicated to width `c` and its top profile taken as a
    * seed (the band = the lower structure to grow above).
    */
  def seedsC(c: ZetaPoint, maxLen: Int = 4): List[Profile] =
    val cx = c.toBigPoint.x.toDouble
    StripBand
      .catalogue(maxLen)
      .flatMap: band =>
        val bp   = band.period.toBigPoint.x.toDouble
        val reps = math.round(cx / bp).toInt
        if bp > 0.5 && math.abs(reps * bp - cx) < 1e-6 && reps >= 1 then
          val facesC = (0 until reps).toList.flatMap(k =>
            band.faces.map(f => FaceZ(f.size, f.corners.map(_ + mul(band.period, k))))
          )
          seedFromFaces(facesC, c)
        else None
      .distinctBy(canonKey)

  /** Integer-circumference convenience. */
  def seeds(cInt: Int, maxLen: Int = 4): List[Profile] = seedsC(ZetaPoint(cInt.toLong, 0, 0, 0), maxLen)

  // ---- efficient transfer-matrix cycle-finder, restricted per type-set (the n≥3 driver) ---------------

  // Per TARGET n-type-set `ts`: build the PLAIN profile graph restricted to fills completing a vertex of `ts`
  // (finite & small — only those polygons), then find cycles COVERING all n types (the tiling's period). A
  // single graph keyed by canonical profile; cycles found by BFS over (profile, types-used-this-cycle).
  private type CK = List[(Long, Long, Long, Long, List[Int])]
  final private case class PEdge(to: CK, delta: ZetaPoint, faces: List[FaceZ], typ: VertexSignature)

  private def anchored(p: Profile): Profile =
    val a = anchor(p); Profile(p.c, p.verts.map(v => PV(v.pos - a, v.fan)))

  private def shiftFaces(faces: List[FaceZ], s: ZetaPoint): List[FaceZ] =
    if s.isOrigin then faces else faces.map(f => FaceZ(f.size, f.corners.map(_ + s)))

  /** BFS the FINITE profile graph restricted to fills whose completed vertex type is in `ts` (each canonical
    * profile expanded once). Edges carry the vertical shift `δ`, the placed faces (source-anchored frame),
    * and the completed type. Capped at `maxNodes`.
    */
  private def buildGraphForC(
      c: ZetaPoint,
      ts: Set[VertexSignature],
      maxNodes: Int
  ): mutable.Map[CK, List[PEdge]] =
    val graph               = mutable.Map.empty[CK, List[PEdge]]
    val queue               = mutable.Queue.empty[(CK, Profile)]
    def enq(p: Profile): CK =
      val r = anchored(p); val k = canonKey(r)
      if !graph.contains(k) then { graph(k) = Nil; queue += ((k, r)) }
      k
    seedsC(c).foreach(enq)
    while queue.nonEmpty && graph.size < maxNodes do
      val (k, rep) = queue.dequeue()
      val edges    = fillLowest(rep).flatMap: (q, t, f) =>
        Option.when(ts.contains(t)):
          val to = if graph.size < maxNodes then enq(q) else canonKey(anchored(q))
          PEdge(to, anchor(q), f, t)
      graph(k) = edges
    graph

  /** All simple cycles from `start` back to `start` that COVER `target` (DFS, path-local visited ⇒ simple,
    * depth ≤ `maxLen`, ≤ `cap`), each as `(Δ, period faces)`. Multiple cycles ⇒ the several distinct tilings
    * of one type-set (shortest-only found just one).
    */
  private def coveringCyclesFrom(
      graph: mutable.Map[CK, List[PEdge]],
      start: CK,
      target: Set[VertexSignature],
      maxLen: Int,
      cap: Int
  ): List[(ZetaPoint, List[FaceZ])] =
    val out = mutable.ListBuffer.empty[(ZetaPoint, List[FaceZ])]
    def dfs(
        k: CK,
        used: Set[VertexSignature],
        sh: ZetaPoint,
        fs: List[FaceZ],
        depth: Int,
        path: Set[CK]
    ): Unit =
      if out.sizeIs < cap && depth <= maxLen then
        graph.getOrElse(k, Nil).foreach: e =>
          val used2 = used + e.typ
          if used2.subsetOf(target) then
            val nf = fs ++ shiftFaces(e.faces, sh)
            val ns = sh + e.delta
            if e.to == start then { if used2 == target && !ns.isOrigin then out += ((ns, nf)) }
            else if !path.contains(e.to) then dfs(e.to, used2, ns, nf, depth + 1, path + e.to)
    dfs(start, Set.empty, ZetaPoint.origin, Nil, 0, Set(start))
    out.toList

  private def faceSetKey(faces: List[FaceZ]): List[(Int, List[(Long, Long, Long, Long)])] =
    import scala.math.Ordering.Implicits.seqOrdering
    faces.map(f => (f.size, f.corners.map(z => (z.a0, z.a1, z.a2, z.a3)).sorted.toList)).sorted

  /** All banded tilings of EXACTLY the type-set `ts` at circumference `c`: build the restricted graph, then
    * enumerate covering cycles from each seed node, close each via the proven [[CylinderAutomaton.close]],
    * keep cells whose types are exactly `ts`. (`primitiveBasis` reduces a period-`p|c` cell to its
    * primitive.)
    */
  def enumerateForTypeSetC(
      c: ZetaPoint,
      ts: Set[VertexSignature],
      maxNodes: Int = 30000,
      maxLen: Int = 48,
      capPerNode: Int = 80
  ): Map[String, (Int, Set[VertexSignature])] =
    val graph    = buildGraphForC(c, ts, maxNodes)
    val out      = mutable.Map.empty[String, (Int, Set[VertexSignature])]
    val seedKeys = seedsC(c).map(s => canonKey(anchored(s))).distinct.filter(graph.contains)
    val seenCyc  = mutable.HashSet.empty[List[(Int, List[(Long, Long, Long, Long)])]]
    seedKeys.foreach: start =>
      coveringCyclesFrom(graph, start, ts, maxLen, capPerNode).foreach: (delta, faces) =>
        val df = dedupFaces(faces)
        if seenCyc.add(faceSetKey(df)) then
          CylinderAutomaton.close(df, c, delta, ts.size).foreach: (n, types, key) =>
            if types == ts then out.getOrElseUpdate(key, (n, types))
    out.toMap

  /** Integer-circumference convenience. */
  def enumerateForTypeSet(
      cInt: Int,
      ts: Set[VertexSignature],
      maxNodes: Int = 30000,
      maxLen: Int = 48
  ): Map[String, (Int, Set[VertexSignature])] =
    enumerateForTypeSetC(ZetaPoint(cInt, 0, 0, 0), ts, maxNodes, maxLen)

  /** Candidate n-type-sets: every `maxN`-subset of the octagon-free valid vertex types. */
  private def candidateTypeSets(maxN: Int): List[Set[VertexSignature]] =
    VertexTypes.validSignatures.filterNot(_.contains(8)).toList.combinations(maxN).map(_.toSet).toList

  /** Enumerate the banded tilings of EXACTLY `maxN` types at circumference `c`: union over every candidate
    * `maxN`-type-set. (For a targeted gate, call [[enumerateForTypeSet]] on specific type-sets directly.)
    */
  def enumerate(
      cInt: Int,
      maxN: Int,
      maxNodes: Int = 30000,
      maxLen: Int = 48
  ): Map[String, (Int, Set[VertexSignature])] =
    val out = mutable.Map.empty[String, (Int, Set[VertexSignature])]
    candidateTypeSets(maxN).foreach: ts =>
      enumerateForTypeSet(cInt, ts, maxNodes, maxLen).foreach((k, v) => out.getOrElseUpdate(k, v))
    out.toMap
