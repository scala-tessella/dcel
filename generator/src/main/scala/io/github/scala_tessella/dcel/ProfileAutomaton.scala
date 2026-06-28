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

  // Fast Double embedding of a ZetaPoint (the hot path — replaces the BigDecimal `toBigPoint` in the graph
  // build, which dominated `fillLowest`). `2x = (2a₀+a₂) + a₁√3`, `2y = (2a₃+a₁) + a₂√3`.
  private val sqrt3                    = math.sqrt(3.0)
  private def xD(z: ZetaPoint): Double = (2.0 * z.a0 + z.a2 + z.a1 * sqrt3) / 2.0
  private def yD(z: ZetaPoint): Double = (2.0 * z.a3 + z.a1 + z.a2 * sqrt3) / 2.0

  /** Fold a position to x ∈ [0, |c|) by integer multiples of `c` (c horizontal) — O(1): the integer count is
    * read off the Double x-ratio, the subtraction stays exact integer (so congruent points fold to the SAME
    * exact ZetaPoint).
    */
  private def foldPos(p: ZetaPoint, c: ZetaPoint): ZetaPoint =
    val k = math.floor(xD(p) / xD(c) + 1e-7).toInt
    if k == 0 then p else p - mul(c, k)

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
    p.verts.indices.minBy(i => (yD(p.verts(i).pos), xD(p.verts(i).pos)))

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
  private[dcel] def canonKey(p: Profile): List[(Long, Long, Long, Long, List[(Int, Int)])] =
    import scala.math.Ordering.Implicits.seqOrdering
    val a = anchor(p)
    // the fan must key by (slot, size) sorted by SLOT — NOT just sorted sizes: two vertices with the same fan
    // SIZES but different slot arrangements are geometrically distinct profiles; keying on sizes alone collapses
    // them, collapsing a cell's period-cycle into a non-simple loop the simple-cycle finder cannot traverse.
    p.verts.map { v =>
      val d = v.pos - a; (d.a0, d.a1, d.a2, d.a3, v.fan.sortBy(_._1))
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
        seen: Map[CK, (ZetaPoint, Int)],
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
      .allBands(maxLen)
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

  // ---- the COMPLETE profile-seed enumerator -----------------------------------------------------------

  private def cartesian(alphabet: List[Int], len: Int): List[List[Int]] =
    if len == 0 then List(Nil) else for h <- alphabet; t <- cartesian(alphabet, len - 1) yield h :: t

  /** EVERY valid profile at circumference `c` whose polygons are drawn from `sizes` — not just band-top
    * replications. A profile = a periodic polyline (edge slots summing to `c`, x-monotone so the open arc
    * faces up) + a per-vertex BELOW-fan; enumerate all polylines up to `maxEdges` and all edge-consistent
    * below-fan assignments (the below polygon shared on each edge: `belowFan(i).last == belowFan(i+1).head`).
    * This reaches the irreducible period-`c` profiles that `StripBand.fillAbove` + replication (sub-period
    * band tops) miss — the cut-profiles of the harder banded cells. Finite; deduped by canonical profile.
    */
  def enumerateProfiles(c: ZetaPoint, sizes: Set[Int], maxEdges: Int): List[Profile] =
    val alphabet                               = List(10, 11, 0, 1, 2) // x-monotone edges (cos ≥ ½) ⇒ the open arc faces up
    // 1. polylines: slot sequences summing to c, length 1..maxEdges, deduped up to cyclic rotation
    val seen                                   = mutable.HashSet.empty[Vector[Int]]
    val polys                                  = mutable.ListBuffer.empty[Vector[Int]]
    def rots(v: Vector[Int]): Set[Vector[Int]] = (0 until v.length).map(i => v.drop(i) ++ v.take(i)).toSet
    for len <- 1 to maxEdges; seq <- cartesian(alphabet, len) do
      val v = seq.toVector
      if v.foldLeft(ZetaPoint.origin)((p, s) => p + ZetaPoint.step(s)) == c &&
        rots(v).forall(!seen.contains(_))
      then { seen ++= rots(v); polys += v }
    // 2. per polyline: enumerate edge-consistent below-fans, build the profile
    polys.toList.flatMap: edges =>
      val len   = edges.length
      val verts = edges.scanLeft(ZetaPoint.origin)((p, s) => p + ZetaPoint.step(s)).init
      // below arc (start, width) per vertex: complement of the (up-facing) above arc
      val arcs  = (0 until len).map: i =>
        val sIn    = edges((i - 1 + len) % len); val sOut = edges(i)
        val aboveW = ((sIn + 6 - sOut) % 12 + 12) % 12
        ((sIn + 6) % 12, 12 - aboveW)
      val perV  = arcs.map((_, w) => StripBand.fanOptions(w).filter(_.forall(sizes.contains)))
      if perV.exists(_.isEmpty) then Nil
      else
        // choose a below-fan per vertex with the cyclic edge glue belowFan(i).last == belowFan(i+1).head
        def choose(i: Int, acc: Vector[List[Int]]): List[Vector[List[Int]]] =
          if i == len then if acc(len - 1).last == acc(0).head then List(acc) else Nil
          else
            perV(i).flatMap(fan =>
              if i == 0 || acc(i - 1).last == fan.head then choose(i + 1, acc :+ fan) else Nil
            )
        choose(0, Vector.empty).flatMap: fans =>
          val pvs  = (0 until len).map: i =>
            var slot = arcs(i)._1
            val ents = fans(i).map { m =>
              val e = (slot, m); slot += StripBand.gSlots(m); e
            }
            PV(foldPos(verts(i), c), ents)
          val prof = Profile(c, pvs.toVector)
          Option.when(prof.verts.forall(v => upFacing(v.fan)))(prof)
    .distinctBy(canonKey)

  /** The complete seed set for a type-set at `c`: every profile whose polygons are the type-set's polygons.
    */
  def completeSeeds(c: ZetaPoint, ts: Set[VertexSignature]): List[Profile] =
    val sizes    = ts.flatten.toSet
    val maxEdges = math.ceil(xD(c)).toInt + 4
    enumerateProfiles(c, sizes, maxEdges)

  // ---- cut-and-feed diagnostic: localise the recall ceiling for a KNOWN cell --------------------------
  //
  // For a known cell (its closed-map `op` + oracle key), this answers WHY the engine does/doesn't reach it by
  // constructing the cell's OWN cut profile and observing the engine on it:
  //   (representable?) the band axis is a 30°-multiple direction ⇒ expressible at a horizontal ℤ[ζ₁₂]
  //       circumference at all (else the profile automaton is fundamentally blind to it — a REPRESENTATION gap);
  //   (fedEmitsKey?)   feeding the cut as a SEED, the engine emits the cell's key ⇒ the cut+grow+close pipeline
  //       reproduces the cell. This is the TRUSTWORTHY signal (the actual engine path).
  // (The earlier greedy `traceClosesKey`/`traceCycles` were unreliable — the trace picks the FIRST cell-consistent
  //  successor, which can follow a wrong sub-cycle — so they were deleted; `feedDebug`/`graphForensics` are the
  //  decomposition diagnostics.)

  final case class CutFeedResult(
      realized: Boolean,
      representable: Boolean,
      bandAxisHorizontal: Boolean,
      c: Option[ZetaPoint],
      cutProfiles: Int,
      fedEmitsKey: Boolean,
      note: String
  )

  /** Rotate by `ζ^k` (k·30° CCW), exact. */
  private[dcel] def rotZk(z: ZetaPoint, k: Int): ZetaPoint =
    var r = z; var i = ((k % 12) + 12) % 12; while i > 0 do { r = r.timesZeta; i -= 1 }; r

  /** Euclidean length of the planar embedding. */
  private[dcel] def lenD(z: ZetaPoint): Double = math.hypot(xD(z), yD(z))

  /** True iff `u,v` are parallel (zero 2-D cross product). */
  private[dcel] def collinear(u: ZetaPoint, v: ZetaPoint): Boolean = math.abs(xD(u) * yD(v) - yD(u) * xD(v)) <
    1e-7

  /** Sum of a face's corners (= `size ×` its centroid; exact). */
  private[dcel] def centroidZ(f: FaceZ): ZetaPoint = f.corners.reduce(_ + _)

  /** Exact division of every component by `d` (None unless all divide). */
  private[dcel] def divExactZ(z: ZetaPoint, d: Int): Option[ZetaPoint] =
    if d != 0 && z.a0 % d == 0 && z.a1 % d == 0 && z.a2 % d == 0 && z.a3 % d == 0
    then Some(ZetaPoint(z.a0 / d, z.a1 / d, z.a2 / d, z.a3 / d))
    else None

  /** True iff two faces have the same size and the same corner multiset (order-independent). */
  private[dcel] def sameFaceZ(f: FaceZ, g: FaceZ): Boolean =
    f.size == g.size && {
      val a = f.corners.map(z => (z.a0, z.a1, z.a2, z.a3)).sorted
      val b = g.corners.map(z => (z.a0, z.a1, z.a2, z.a3)).sorted
      a == b
    }

  /** An exact ℤ[ζ₁₂] basis of the lattice generated by `gens` (the deck vectors). In 2-D the two shortest
    * independent lattice vectors are a basis (successive minima ⇒ basis), so brute-force the small integer
    * combos and pick them. `pts` (all combos within range) is also returned for the horizontal-vector search.
    */
  private[dcel] def latticeBasisZ(
      gens: List[ZetaPoint],
      R: Int = 6
  ): Option[(ZetaPoint, ZetaPoint, List[ZetaPoint])] =
    val g = gens.filterNot(_.isOrigin)
    if g.isEmpty then None
    else
      val seen                              = mutable.HashSet.empty[(Long, Long, Long, Long)]
      val pts                               = mutable.ListBuffer.empty[ZetaPoint]
      def rec(i: Int, acc: ZetaPoint): Unit =
        if i == g.length then {
          if !acc.isOrigin && seen.add((acc.a0, acc.a1, acc.a2, acc.a3)) then pts += acc
        } else { var m = -R; while m <= R do { rec(i + 1, acc + mul(g(i), m)); m += 1 } }
      rec(0, ZetaPoint.origin)
      if pts.isEmpty then None
      else
        val all = pts.toList.filter(p => lenD(p) < (R + 0.5) * g.map(lenD).min * 2)
        val v1  = all.minBy(lenD)
        all.filterNot(p => collinear(p, v1)).minByOption(lenD).map(v2 => (v1, v2, all))

  /** The unique integer `(m,n)` with `t = m·a + n·b` (exact-verified), or None if `t ∉ ⟨a,b⟩`. */
  private[dcel] def latticeSolve(t: ZetaPoint, a: ZetaPoint, b: ZetaPoint): Option[(Int, Int)] =
    val det = xD(a) * yD(b) - yD(a) * xD(b)
    if math.abs(det) < 1e-9 then None
    else
      val m = math.round((xD(t) * yD(b) - yD(t) * xD(b)) / det).toInt
      val n = math.round((xD(a) * yD(t) - yD(a) * xD(t)) / det).toInt
      Option.when((mul(a, m) + mul(b, n)) == t)((m, n))

  /** True iff face `f` is a lattice-translate of one of the cell's fundamental faces (membership in the cell
    * tiling, mod Λ = ⟨a,b⟩).
    */
  private[dcel] def faceInCell(f: FaceZ, funds: List[FaceZ], a: ZetaPoint, b: ZetaPoint): Boolean =
    funds.exists: f0 =>
      f0.size == f.size && divExactZ(centroidZ(f) - centroidZ(f0), f.size).exists: t =>
        latticeSolve(t, a, b).isDefined && sameFaceZ(FaceZ(f0.size, f0.corners.map(_ + t)), f)

  /** A profile is a VALID cut of the cell iff every face implied by every vertex's below-fan is a cell face:
    * reconstruct `FaceZ(m, polygon(v.pos, s, m))` for each `(s,m)` entry and check [[faceInCell]]. The
    * malformed high-aspect cut (a deep slab keeping spurious floor/interior rows) violates this — its floor
    * vertices' fans reconstruct to off-lattice faces — so this is the invariant the cut construction must
    * satisfy.
    */
  private[dcel] def profileCellConsistent(
      p: Profile,
      funds: List[FaceZ],
      a: ZetaPoint,
      b: ZetaPoint
  ): Boolean =
    p.verts.forall(v => v.fan.forall((s, m) => faceInCell(FaceZ(m, G.polygon(v.pos, s, m)), funds, a, b)))

  /** Cut profiles of the cell: tile the fundamentals over a vertical window (folding mod `c` happens in
    * `seedFromFaces`), then cut at several interior heights — the up-facing vertices on each cut line form a
    * candidate seed profile.
    */
  /** Fold a face mod `c` so its LEFTMOST corner sits in `[0,|c|)` (exact integer shift). Used to dedup
    * c-translates that would otherwise double-count into a vertex's fan when `seedFromFaces` accumulates.
    */
  private[dcel] def foldFaceModC(f: FaceZ, c: ZetaPoint): FaceZ =
    val j = math.floor(f.corners.map(xD).min / xD(c) + 1e-7).toInt
    if j == 0 then f else FaceZ(f.size, f.corners.map(_ - mul(c, j)))

  /** The cell's faces tiled over a `(2K+1)²` window of lattice translates, each folded mod `c` and DEDUPED by
    * corner-set — one representative per cylinder face-orbit (no horizontal double-cover).
    */
  private[dcel] def tiledBandFaces(
      rFaces: List[FaceZ],
      rv1: ZetaPoint,
      rv2: ZetaPoint,
      c: ZetaPoint,
      K: Int = 5
  ): List[FaceZ] =
    val seenF = mutable.HashSet.empty[List[(Long, Long, Long, Long)]]
    (for m <- -K to K; n <- -K to K; f <- rFaces
    yield foldFaceModC(FaceZ(f.size, f.corners.map(_ + mul(rv1, m) + mul(rv2, n))), c))
      .filter(f => seenF.add(f.corners.map(z => (z.a0, z.a1, z.a2, z.a3)).sorted.toList))
      .toList

  /** One cut profile of `tiled` at height `Y`: feed the slab of faces fully BELOW `Y` to `seedFromFaces` (so
    * interior vertices complete and drop out), then keep EXACTLY the vertices that STRADDLE `Y` — those also
    * incident to a face reaching above `Y`. This precise straddle test (no depth/window heuristic) is what
    * makes the cut a genuine monotone front: a y-window keeps spurious non-straddling interior vertices for
    * slanted / high-aspect cells, whose partial fans then have no valid completion (the `fillLowest`
    * dead-end).
    */
  private[dcel] def cutProfileAt(tiled: List[FaceZ], c: ZetaPoint, Y: Double): Option[Profile] =
    val below       = tiled.filter(_.corners.forall(z => yD(z) < Y - 1e-9))
    // folded corners of faces that reach above Y — the vertices a cut vertex must also touch to truly straddle
    val aboveFolded = tiled.filter(_.corners.exists(z => yD(z) > Y + 1e-9))
      .flatMap(_.corners.map(z => foldPos(z, c)))
      .map(z => (z.a0, z.a1, z.a2, z.a3)).toSet
    seedFromFaces(below, c)
      .map(p =>
        Profile(c, p.verts.filter(v => aboveFolded.contains((v.pos.a0, v.pos.a1, v.pos.a2, v.pos.a3))))
      )
      .filter(_.verts.nonEmpty)

  private[dcel] def buildCutProfiles(
      rFaces: List[FaceZ],
      rv1: ZetaPoint,
      rv2: ZetaPoint,
      c: ZetaPoint,
      periodY: Double,
      K: Int = 5
  ): List[Profile] =
    val tiled = tiledBandFaces(rFaces, rv1, rv2, c, K)
    val ysAll = tiled.flatMap(_.corners.map(yD)).distinct.sorted
    if ysAll.length < 4 || periodY <= 1e-6 then Nil
    else
      // interior cut heights: midpoints between consecutive vertex rows in the central third
      val lo   = ysAll(ysAll.length / 3); val hi = ysAll(2 * ysAll.length / 3)
      val rows = ysAll.filter(y => y >= lo && y <= hi)
      val cuts = rows.sliding(2).collect { case Seq(p, q) if q - p > 1e-6 => (p + q) / 2 }.toList
      cuts.flatMap(Y => cutProfileAt(tiled, c, Y)).distinctBy(canonKey)

  /** The decisive diagnostic — see the section header. `targetKey` = the oracle's
    * `DelaneySymbols.canonicalKey` (the same key space the engine emits).
    */
  def cutFeedDiagnose(
      op: Array[Array[Int]],
      targetKey: String,
      ts: Set[VertexSignature],
      maxNodes: Int = 12000,
      maxLen: Int = 64,
      maxBand: Int = 1
  ): CutFeedResult =
    representFrame(op) match
      case None                                       =>
        CutFeedResult(
          false,
          false,
          false,
          None,
          0,
          false,
          "not representable (realize/basis/30°-aligned failed)"
        )
      case Some(Frame(c, _, _, _, hLen, cLen, profs)) =>
        val bandH = math.abs(cLen - hLen) < 1e-6 // the GLOBAL shortest vector is itself horizontalizable
        // feed the cut profiles as seeds and check the engine emits the cell's key (the trustworthy signal)
        val fed   = profs.nonEmpty &&
          enumerateFromSeeds(c, ts, profs, maxNodes, maxLen, maxBand = maxBand).keySet.contains(targetKey)
        CutFeedResult(
          true,
          true,
          bandH,
          Some(c),
          profs.size,
          fed,
          f"|h|=${hLen}%.3f c=${cLen}%.3f profiles=${profs.size}"
        )

  /** The representable cylinder frame for a cell: a horizontal circumference `c`, the rotated fundamental
    * faces + lattice basis, the band-axis length, and the cut-profile seeds. `None` if it can't be realized
    * or its band axis is not a 30°-multiple direction (a REPRESENTATION gap). Shared by [[cutFeedDiagnose]]
    * and debug.
    */
  final private[dcel] case class Frame(
      c: ZetaPoint,
      rFaces: List[FaceZ],
      rv1: ZetaPoint,
      rv2: ZetaPoint,
      hLen: Double,
      cLen: Double,
      profs: List[Profile]
  )

  /** Among lattice points `pts`, the SHORTEST that becomes horizontal (+x) under some `ζ^k` rotation, as
    * `(length, k, vector)`. None if no lattice vector is a 30°-multiple direction (a representation gap).
    */
  private[dcel] def shortestHorizontal(pts: List[ZetaPoint]): Option[(Double, Int, ZetaPoint)] =
    (for p <- pts; k <- 0 until 12 if math.abs(yD(rotZk(p, k))) < 1e-7 && xD(rotZk(p, k)) > 1e-9
    yield (lenD(p), k, p)).minByOption(_._1)

  private[dcel] def representFrame(op: Array[Array[Int]]): Option[Frame] =
    G.realizeCellZ(op).flatMap: (faces, deck, _, _) =>
      latticeBasisZ(deck).flatMap: (v1, v2, pts) =>
        val hLen = pts.map(lenD).min
        shortestHorizontal(pts).map: (cLen, k, p) =>
          // the band period is |p|=cLen; the engine needs |c| ≥ 2 (a unit edge must not wrap the cylinder),
          // so use the smallest integer multiple of the period with |c| ≥ 2.
          val reps    = math.max(1, math.ceil(2.0 / cLen - 1e-9).toInt)
          val c       = rotZk(mul(p, reps), k)
          val rFaces  = faces.map(f => FaceZ(f.size, f.corners.map(z => rotZk(z, k))))
          val rv1     = rotZk(v1, k); val rv2 = rotZk(v2, k)
          // vertical period = covolume / primitive-horizontal-length (independent of how many times c wraps)
          val covol   = math.abs(xD(rv1) * yD(rv2) - yD(rv1) * xD(rv2))
          val periodY = covol / cLen
          Frame(c, rFaces, rv1, rv2, hLen, cLen, buildCutProfiles(rFaces, rv1, rv2, c, periodY))

  // ---- efficient transfer-matrix cycle-finder, restricted per type-set (the n≥3 driver) ---------------

  // Per TARGET n-type-set `ts`: build the PLAIN profile graph restricted to fills completing a vertex of `ts`
  // (finite & small — only those polygons), then find cycles COVERING all n types (the tiling's period). A
  // single graph keyed by canonical profile; cycles found by BFS over (profile, types-used-this-cycle).
  private type CK = List[(Long, Long, Long, Long, List[(Int, Int)])]
  private[dcel] case class PEdge(to: CK, delta: ZetaPoint, faces: List[FaceZ], typ: VertexSignature)

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
      maxNodes: Int,
      seedProfiles: List[Profile]
  ): mutable.Map[CK, List[PEdge]] =
    val graph               = mutable.Map.empty[CK, List[PEdge]]
    val queue               = mutable.Queue.empty[(CK, Profile)]
    def enq(p: Profile): CK =
      val r = anchored(p); val k = canonKey(r)
      if !graph.contains(k) then { graph(k) = Nil; queue += ((k, r)) }
      k
    seedProfiles.foreach(enq)
    while queue.nonEmpty && graph.size < maxNodes do
      val (k, rep) = queue.dequeue()
      val edges    = fillLowest(rep).flatMap: (q, t, f) =>
        Option.when(ts.contains(t)):
          val to = if graph.size < maxNodes then enq(q) else canonKey(anchored(q))
          PEdge(to, anchor(q), f, t)
      graph(k) = edges
    graph

  /** Generic COVERING-WALK finder: every closed walk `start → … → start` whose edge COLORS union to EXACTLY
    * `colors`, as the ordered edge list. A WALK (not a simple cycle) — it may revisit non-start nodes, which
    * is ESSENTIAL: a homogeneous row (e.g. a pure-square `4⁴` band) maps the profile to a translate of
    * itself, a SELF-LOOP in the graph; a simple-cycle finder forbids that revisit and so can never cover such
    * a colour, and a TALL band of `k` such rows needs the self-loop traversed `k` times. So a `(node,
    * colorset)` state is allowed up to `maxRepeat` visits PER BRANCH — enough for a band of that height,
    * while bounding each branch (no infinite self-loop spinning) and staying NON-LOSSY across branches (the
    * old `(node, colorset)` BFS visited each state once GLOBALLY and lost distinct walks). Different repeat
    * counts ⇒ different vertical periods ⇒ genuinely different tilings, all enumerated (deduped downstream by
    * face-set). `maxLen`/`cap`/`budget` are hard backstops. Pure graph algorithm ⇒ unit-testable on synthetic
    * `Int` graphs.
    */
  private[dcel] def coveringWalks[N, E, C](
      start: N,
      edgesOf: N => List[E],
      toOf: E => N,
      colorOf: E => C,
      colors: Set[C],
      maxLen: Int,
      cap: Int,
      maxRepeat: Int = 1,
      budget: Int = 500000
  ): List[List[E]] =
    val out                                                                               = mutable.ListBuffer.empty[List[E]]
    var steps                                                                             = 0
    def dfs(node: N, used: Set[C], visits: Map[(N, Set[C]), Int], pathRev: List[E]): Unit =
      edgesOf(node).foreach: e =>
        if out.sizeIs < cap && steps < budget && pathRev.lengthIs < maxLen then
          steps += 1
          val nxt   = toOf(e)
          val used2 = used + colorOf(e)
          if used2.subsetOf(colors) then
            if nxt == start then { if used2 == colors then out += (e :: pathRev).reverse }
            else
              val v = visits.getOrElse((nxt, used2), 0)
              if v < maxRepeat then dfs(nxt, used2, visits.updated((nxt, used2), v + 1), e :: pathRev)
    dfs(start, Set.empty, Map.empty, Nil)
    out.toList

  /** The REPEATABLE BAND segments of a closed walk `start → e0 → … → start`, as edge-index ranges `[i..j]`:
    * the SIMPLE sub-cycles (a node recurs, with no inner recurrence) — each a homogeneous band of one period
    * that maps the profile to a translate of itself and so can be repeated `k` times for a `k`-row band. A
    * self-loop is the period-1 case; a 2-cycle (e.g. a `3⁶` triangle band node↔node) the period-2 case, etc.
    * The outermost cycle spanning the whole walk (the period "spine", traversed once) is EXCLUDED.
    * Walk-decomposition by last-occurrence positions; pure ⇒ unit-testable.
    */
  private[dcel] def bandSegments[N, E](start: N, path: List[E], toOf: E => N): List[(Int, Int)] =
    val nodes  = (start :: path.map(toOf)).toVector // nodes(i) = node BEFORE edge i; last = return to start
    val pos    = mutable.Map.empty[N, Int]
    val cycles = mutable.ListBuffer.empty[(Int, Int)]
    nodes.indices.foreach: i =>
      pos.get(nodes(i)) match
        case Some(p) => cycles += ((p, i - 1)); (p + 1 until i).foreach(k => pos.remove(nodes(k)))
        case None    => pos(nodes(i)) = i
    cycles.toList.filterNot((i, j) => i == 0 && j == path.length - 1) // drop the whole-walk spine

  /** Band-HEIGHT variants of a closed walk: each repeatable band ([[bandSegments]]) traversed `1..maxRepeat`
    * times (cartesian over the disjoint bands). A walk with no band expands to just itself. Replaying a
    * variant gives the `(Δ,faces)` of a multi-row-band tiling; `close`/`primitiveBasis` then resolves which
    * heights form a valid cell. Bounded by `maxRepeat^(#bands)`. Pure ⇒ unit-testable.
    */
  private[dcel] def expandBands[N, E](start: N, path: List[E], toOf: E => N, maxRepeat: Int): List[List[E]] =
    val bands = bandSegments(start, path, toOf).sortBy(_._1) // disjoint, left-to-right
    if bands.isEmpty then List(path)
    else
      def repsOf(bs: List[(Int, Int)], acc: List[Int]): List[List[Int]] = bs match
        case Nil       => List(acc.reverse)
        case _ :: rest => (1 to maxRepeat).toList.flatMap(k => repsOf(rest, k :: acc))
      repsOf(bands, Nil).map: reps =>
        val buf = mutable.ListBuffer.empty[E]
        var idx = 0
        bands.zip(reps).foreach { case ((i, j), k) =>
          buf ++= path.slice(idx, i)
          (0 until k).foreach(_ => buf ++= path.slice(i, j + 1))
          idx = j + 1
        }
        buf ++= path.slice(idx, path.length)
        buf.toList

  /** Up to `cap` covering cycles from `start` back to `start`, each as `(Δ, period faces)`: a BFS over
    * `(profile, types-used)` that records every edge back to `start` whose accumulated types == `target` and
    * Δ≠0. Each state visited ONCE — so it is O(states)=O(nodes·2^|target|) per call (FAST on the gate's large
    * band-top graphs), and it DOES traverse self-loops (a homogeneous row adds its colour, a NEW state). It
    * is slightly LOSSY (one path per state — distinct cycles sharing a `(node, colorset)` can be missed),
    * which costs COMPLETENESS not the recall ceiling: the generic non-lossy [[coveringWalks]] is correct but
    * O(paths) per node and TIMED OUT the gate (15min vs ~4min), without finding more cells — the missing
    * cells are multi-row bands needing `maxRepeat>1`, expensive for either finder. So the engine uses this
    * BFS; the tested `coveringWalks` is kept for small-graph diagnostics / future completeness work.
    * (ADR-0038.)
    */
  private def coveringCyclesFrom(
      graph: mutable.Map[CK, List[PEdge]],
      start: CK,
      target: Set[VertexSignature],
      maxLen: Int,
      cap: Int
  ): List[List[PEdge]] =
    val out     = mutable.ListBuffer.empty[List[PEdge]]
    val visited = mutable.HashSet.empty[(CK, Set[VertexSignature])]
    // carry the EDGE PATH (references — cheap) so the multi-row finder can spot self-loops and vary band height
    val queue   = mutable.Queue.empty[(CK, Set[VertexSignature], ZetaPoint, List[PEdge], Int)]
    queue += ((start, Set.empty, ZetaPoint.origin, Nil, 0)); visited += ((start, Set.empty))
    while queue.nonEmpty && out.sizeIs < cap do
      val (k, used, sh, edgesRev, d) = queue.dequeue()
      if d < maxLen then
        graph.getOrElse(k, Nil).foreach: e =>
          val used2 = used + e.typ
          if used2.subsetOf(target) then
            val ns = sh + e.delta
            if e.to == start && used2 == target && !ns.isOrigin then out += (e :: edgesRev).reverse
            if visited.add((e.to, used2)) then queue += ((e.to, used2, ns, e :: edgesRev, d + 1))
    out.toList

  /** Accumulate `(Δ, period faces)` from a cycle's edge path: each edge's faces are placed in the
    * source-anchored frame, shifted up by the cumulative Δ so far. (Replaying an [[expandBands]] variant
    * gives the faces+period of a multi-row-band tiling.)
    */
  private[dcel] def replayCycle(edges: List[PEdge]): (ZetaPoint, List[FaceZ]) =
    var sh    = ZetaPoint.origin
    var faces = List.empty[FaceZ]
    edges.foreach: e =>
      faces = faces ++ shiftFaces(e.faces, sh)
      sh = sh + e.delta
    (sh, faces)

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
      capPerNode: Int = 16,
      maxBand: Int = 1
  ): Map[String, (Int, Set[VertexSignature])] =
    // band-top seeds (fast); the COMPLETE enumerator `completeSeeds` was measured NOT to improve recall and
    // explodes at large c — the recall ceiling is growth/cycle-finding for high-aspect cells, not seeds.
    enumerateFromSeeds(c, ts, seedsC(c), maxNodes, maxLen, capPerNode, maxBand)

  /** Like [[enumerateForTypeSetC]] but from an EXPLICIT seed-profile set (e.g. for diagnosing seed coverage:
    * feed a known cell's own cut-profile and check whether the engine closes it).
    */
  def enumerateFromSeeds(
      c: ZetaPoint,
      ts: Set[VertexSignature],
      seedProfiles: List[Profile],
      maxNodes: Int = 30000,
      maxLen: Int = 48,
      capPerNode: Int = 16,
      maxBand: Int = 1
  ): Map[String, (Int, Set[VertexSignature])] =
    val graph         = buildGraphForC(c, ts, maxNodes, seedProfiles)
    val out           = mutable.Map.empty[String, (Int, Set[VertexSignature])]
    val seenCyc       = mutable.HashSet.empty[List[(Int, List[(Long, Long, Long, Long)])]]
    var nCyc, nClosed = 0
    // start covering cycles from EVERY node: a cell's cycle need not pass through a band-top seed profile, so
    // seed-only starts miss cells. Each cycle is expanded to its band-HEIGHT variants (a self-loop = a
    // homogeneous row, repeated 1..maxBand times — the MULTI-ROW finder), replayed, deduped by face-set before
    // the (costly) close. `close`/`primitiveBasis` resolves which height `k` is a valid cell.
    graph.keysIterator.foreach: start =>
      coveringCyclesFrom(graph, start, ts, maxLen, capPerNode).foreach: cycle =>
        expandBands[CK, PEdge](start, cycle, _.to, maxBand).foreach: variant =>
          val (delta, faces) = replayCycle(variant)
          if !delta.isOrigin then
            val df = dedupFaces(faces)
            if seenCyc.add(faceSetKey(df)) then
              nCyc += 1
              CylinderAutomaton.close(df, c, delta, ts.size).foreach: (n, types, key) =>
                nClosed += 1
                if types == ts then out.getOrElseUpdate(key, (n, types))
    if sys.props.contains("pa.debug") then
      println(
        s"    [pa] c.x=${xD(c)}, seeds=${seedProfiles.size}, nodes=${graph.size}, distinctCycles=$nCyc, closedOk=$nClosed, emitted=${out.size}"
      )
    out.toMap

  /** Forensics for a feed failure: dump the reachable graph from `seeds` — per node, its out-edge types and
    * `to`-nodes — plus the UNION of all edge types in the graph and each node's self-reachability. The
    * decisive fact is whether the reachable graph even CONTAINS all `target` types (else no covering cycle
    * can exist — the growth from these seeds never produces a vertex of some target type).
    */
  private[dcel] def graphForensics(
      c: ZetaPoint,
      ts: Set[VertexSignature],
      seeds: List[Profile],
      maxNodes: Int = 40000
  ): String =
    val graph    = buildGraphForC(c, ts, maxNodes, seeds)
    val allTypes = graph.values.flatten.map(_.typ).toSet
    val lines    = graph.toList.zipWithIndex.map: (kv, i) =>
      val (k, edges) = kv
      val outs       = edges.map(e => s"${e.typ.mkString(".")}→${graph.keysIterator.indexOf(e.to)}").mkString(", ")
      s"  node$i (verts=${k.size}): [$outs]"
    s"nodes=${graph.size} allEdgeTypes={${allTypes.map(_.mkString(".")).mkString("; ")}} " +
      s"coversTarget=${ts.subsetOf(allTypes)}\n${lines.mkString("\n")}"

  /** Feed diagnostic: from explicit seeds, report graph size, #distinct cycles found, how many CLOSE at all,
    * the distinct keys they close to, and whether `targetKey` is among them. Decomposes a feed failure into
    * graph-build vs cycle-search vs close.
    */
  private[dcel] def feedDebug(
      c: ZetaPoint,
      ts: Set[VertexSignature],
      seedProfiles: List[Profile],
      targetKey: String,
      maxNodes: Int = 30000,
      maxLen: Int = 64,
      capPerNode: Int = 16,
      maxBand: Int = 1
  ): String =
    val graph   = buildGraphForC(c, ts, maxNodes, seedProfiles)
    val seenCyc = mutable.HashSet.empty[List[(Int, List[(Long, Long, Long, Long)])]]
    var nCyc    = 0
    val keys    = mutable.Set.empty[String]
    var closes  = 0
    graph.keysIterator.foreach: start =>
      coveringCyclesFrom(graph, start, ts, maxLen, capPerNode).foreach: cycle =>
        expandBands[CK, PEdge](start, cycle, _.to, maxBand).foreach: variant =>
          val (delta, faces) = replayCycle(variant)
          if !delta.isOrigin then
            val df = dedupFaces(faces)
            if seenCyc.add(faceSetKey(df)) then
              nCyc += 1
              CylinderAutomaton.close(df, c, delta, ts.size).foreach: (_, _, key) =>
                closes += 1; keys += key
    s"seeds=${seedProfiles.size} nodes=${graph.size} cycles=$nCyc closed=$closes distinctKeys=${keys.size} " +
      s"hasTarget=${keys.contains(targetKey)}"

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
