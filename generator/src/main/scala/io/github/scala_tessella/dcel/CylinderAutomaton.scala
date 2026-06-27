package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.KrotenheerdtTorusMapSearch as G
import io.github.scala_tessella.dcel.KrotenheerdtTorusMapSearch.FaceZ
import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import io.github.scala_tessella.dcel.geometry.BigPoint

import scala.collection.mutable

/** SPIKE for ADR-0038: grow a tiling on a **cylinder of fixed circumference `h`** (the in-band period), close
  * the tall strip into a torus cell, and key it in the **shared D-symbol space** — the grower's exact growth
  * and close machinery ([[KrotenheerdtTorusMapSearch]]), reused with the 2-D lattice replaced by a 1-D
  * mod-⟨h⟩ identification. This is a vertical slice to TEST the load-bearing ADR-0038 assumptions before the
  * full engine:
  *
  *   - **A (finiteness):** the set of distinct growth FRONTS at a fixed `h` is bounded (taut, lowest-front
  *     growth) — measured by [[enumerateAtH]]'s `distinctFronts`.
  *   - **B (cylinder soundness):** the shared `isConsistentBy` accepts valid layers, rejects overlaps, with
  *     the mod-⟨h⟩ vertex identity.
  *   - **C (closing ⇒ right key):** a grown strip closes via `verifyCell` (soundness gate) +
  *     `torusMapClassify` (D-symbol key) to the SAME key the oracle assigns that tiling.
  *
  * DRY: every shared primitive (`polygon`, `gSlots`, `coveredSlots`, `vertexFansBy`, `isConsistentBy`,
  * `growByCompletionBy`, `distinctFacesOf`, `verifyCell`, `torusMapClassify`) is the grower's — the cylinder
  * adds only the mod-⟨h⟩ identity, the strip fold, and the close driver.
  */
object CylinderAutomaton:

  // ---- cylinder identification (h horizontal): a vertex is taken mod ⟨h⟩ ------------------------------

  private def widthX(h: ZetaPoint): Double = h.toBigPoint.x.toDouble

  /** Cylinder-vertex identity: position folded mod `h` in x (h horizontal), exact-y. The `vid` handed to the
    * shared `*By` methods so `h`-translates of a vertex are the SAME vertex.
    */
  private def vid(h: ZetaPoint): ZetaPoint => (Long, Long) =
    val w = widthX(h)
    p =>
      val x  = p.toBigPoint.x.toDouble
      val y  = p.toBigPoint.y.toDouble
      val xm = x - math.floor(x / w + 1e-7) * w
      (math.round(xm * 100000.0), math.round(y * 100000.0))

  private def mul(p: ZetaPoint, k: Int): ZetaPoint = ZetaPoint(p.a0 * k, p.a1 * k, p.a2 * k, p.a3 * k)

  /** Fold a face by an integer multiple of `h` so its centroid-x lands in `[0, |h|)` — the canonical strip
    * representative (keeps the strip one period wide; `h`-translates collapse to one face under `.distinct`).
    */
  private def fold(h: ZetaPoint): FaceZ => FaceZ =
    val w = widthX(h)
    f =>
      val cx = f.corners.map(_.toBigPoint.x.toDouble).sum / f.size
      val k  = math.floor(cx / w + 1e-7).toInt
      if k == 0 then f else FaceZ(f.size, f.corners.map(_ - mul(h, k)))

  /** Strict TAUT scanline selection: complete the lowest-y, then leftmost-x incomplete vertex (NOT MRV) —
    * this keeps the front a bounded skyline, so the automaton's state space is finite. (`completions` count
    * is ignored; the trailing key is 0.)
    */
  private def tautSel(p: ZetaPoint, cs: Int, c: BigPoint): (BigDecimal, BigDecimal, BigDecimal) =
    (p.toBigPoint.y, p.toBigPoint.x, BigDecimal(0))

  /** A frontier vertex is grown only if its open (uncovered) arc faces UPWARD — the filled region is below
    * it. Down-facing incomplete vertices are the fixed bottom cut and are never grown, so growth is
    * one-directional and the front stays a bounded skyline (a finite profile state space).
    */
  private def upFacing(p: ZetaPoint, fan: List[(Int, Int)]): Boolean =
    val covered = G.coveredSlots(fan)
    (0 until 12).iterator.filterNot(covered.contains).map(s => math.sin(math.toRadians(30.0 * s))).sum > 1e-9

  // ---- growth (reusing the grower's generalized machinery) --------------------------------------------

  def isConsistentCyl(faces: List[FaceZ], h: ZetaPoint): Boolean = G.isConsistentBy(faces, vid(h))

  def growByCompletionCyl(faces: List[FaceZ], h: ZetaPoint, n: Int): List[List[FaceZ]] =
    G.growByCompletionBy(faces, n, vid(h), fold(h), tautSel, dedup = _.distinct)

  // ---- closing: verifyCell (soundness gate) + torusMapClassify (D-symbol key) --------------------------

  /** Close the strip into a torus cell with periods `(h, Δ)`: `verifyCell` gates soundness,
    * `torusMapClassify` keys it in the shared D-symbol space on the primitive basis — the grower's
    * `closeCell` path, with `h` fixed and `Δ` proposed by front recurrence.
    */
  def close(
      faces: List[FaceZ],
      h: ZetaPoint,
      delta: ZetaPoint,
      maxN: Int
  ): Option[(Int, Set[VertexSignature], String)] =
    G.verifyCell(faces, h, delta, maxN).flatMap: _ =>
      val originB  = BigPoint.origin
      val (g1, g2) = (h.toBigPoint, delta.toBigPoint)
      val (pv, pw) =
        KrotenheerdtLatticeSearch.primitiveBasis(g1, g2, originB, G.distinctFacesOf(faces, g1, g2), Nil)
      G.torusMapClassify(faces, pv, pw, originB).map((n2, sigs, dkey) => (n2, sigs.toSet, dkey))

  // ---- the spike driver -------------------------------------------------------------------------------

  /** Vertical-period candidates: differences of vertically-aligned completed cylinder vertices of the same
    * type — the front-recurrence proposals, gated by `close`/`verifyCell`.
    */
  private def deltaCandidates(faces: List[FaceZ], h: ZetaPoint): List[ZetaPoint] =
    val completed = G.vertexFansBy(faces, vid(h)).toList.flatMap: (_, repFan) =>
      val (p, fan) = repFan
      Option.when(G.coveredSlots(fan).sizeIs == 12)((normalize(fan.map(_._2)), p))
    completed
      .groupBy(_._1)
      .values
      .flatMap: group =>
        val ps = group.map(_._2)
        for
          a <- ps; b <- ps
          d  = b - a
          if !d.isOrigin && math.abs(d.toBigPoint.x.toDouble) < 1e-6 && d.toBigPoint.y.toDouble > 1e-6
        yield d
      .toList
      .distinctBy(d => (math.round(d.toBigPoint.x.toDouble * 1e5), math.round(d.toBigPoint.y.toDouble * 1e5)))
      .sortBy(_.toBigPoint.y.toDouble)

  /** Canonical profile fingerprint = the automaton state: the UP-FACING frontier vertices (the advancing top
    * profile; the down-facing bottom cut is excluded) as EXACT integer offsets from the lexicographically-min
    * frontier vertex plus each vertex's below-fan sizes. Integer offsets ⇒ exactly translation-invariant (a
    * vertically-shifted copy of the same profile keys identically), so the state set is finite.
    */
  private def frontKey(faces: List[FaceZ], h: ZetaPoint): List[(Long, Long, Long, Long, List[Int])] =
    import scala.math.Ordering.Implicits.seqOrdering
    val front = G.vertexFansBy(faces, vid(h)).toList.collect:
      case (_, (p, fan)) if G.coveredSlots(fan).sizeIs < 12 && upFacing(p, fan) => (p, fan.map(_._2).sorted)
    if front.isEmpty then Nil
    else
      val anchor = front.map(_._1).min
      front.map { (p, fs) =>
        val d = p - anchor; (d.a0, d.a1, d.a2, d.a3, fs)
      }.sorted

  /** Exact strip key for visited-dedup: per face `(size, sorted corner integer-tuples)`, faces sorted —
    * corners are sorted so the same folded face reached from a different root vertex keys identically.
    */
  private def faceCanonKey(faces: List[FaceZ]): List[(Int, List[(Long, Long, Long, Long)])] =
    import scala.math.Ordering.Implicits.seqOrdering
    faces.map(f => (f.size, f.corners.map(z => (z.a0, z.a1, z.a2, z.a3)).sorted.toList)).sorted

  /** The number of distinct CANONICAL FRONTS reachable by taut growth at circumference `h`, deduping by front
    * (the transfer-matrix Markov state) rather than by full patch — two patches with the same open front have
    * the same future, so only one is expanded. If this SATURATES as `maxFaces` rises, the automaton's state
    * space is finite (ADR-0038 assumption A). Returns `(distinctFronts, capped)`.
    */
  def reachableFrontCount(h: ZetaPoint, maxN: Int, maxFaces: Int, perCap: Long = 300000L): (Int, Boolean) =
    val visited = mutable.HashSet.empty[List[(Long, Long, Long, Long, List[Int])]]
    val stack   = mutable.Stack.empty[List[FaceZ]]
    List(3, 4, 6, 12).foreach: m =>
      val seed = List(fold(h)(FaceZ(m, G.polygon(ZetaPoint.origin, 0, m)))).distinct
      if isConsistentCyl(seed, h) && visited.add(frontKey(seed, h)) then stack.push(seed)
    var states  = 0L
    while stack.nonEmpty && states < perCap do
      val faces = stack.pop()
      states += 1
      if faces.sizeIs < maxFaces then
        growByCompletionCyl(faces, h, maxN).foreach: child =>
          if visited.add(frontKey(child, h)) then stack.push(child)
    (visited.size, stack.nonEmpty)

  final case class Result(
      emitted: Map[String, (Int, Set[VertexSignature])],
      distinctFronts: Int,
      states: Long,
      capped: Boolean
  )

  /** Grow on the cylinder of circumference `h` from single-polygon seeds, closing every strip that admits a
    * vertical period. Returns the D-symbol keys emitted (banded tilings at this `h`), the count of distinct
    * fronts (assumption A), and the state count.
    */
  def enumerateAtH(h: ZetaPoint, maxN: Int, maxFaces: Int, perCap: Long = 200000L): Result =
    val emitted = mutable.Map.empty[String, (Int, Set[VertexSignature])]
    val fronts  = mutable.HashSet.empty[List[(Long, Long, Long, Long, List[Int])]]
    val visited = mutable.HashSet.empty[List[(Int, List[(Long, Long, Long, Long)])]]
    val stack   = mutable.Stack.empty[List[FaceZ]]
    var states  = 0L
    // single-polygon seeds: a wider corona would self-overlap when folded onto a thin cylinder; one polygon +
    // completion growth reaches the same tilings (a seed wider than the circumference fails the consistency gate)
    List(3, 4, 6, 12).foreach: m =>
      val seed = List(fold(h)(FaceZ(m, G.polygon(ZetaPoint.origin, 0, m)))).distinct
      if isConsistentCyl(seed, h) && visited.add(faceCanonKey(seed)) then stack.push(seed)
    while stack.nonEmpty && states < perCap do
      val faces = stack.pop()
      states += 1
      fronts += frontKey(faces, h)
      deltaCandidates(faces, h).foreach: d =>
        close(faces, h, d, maxN).foreach((nn, sigs, dkey) => emitted.getOrElseUpdate(dkey, (nn, sigs)))
      if faces.sizeIs < maxFaces then
        growByCompletionCyl(faces, h, maxN).foreach: child =>
          if visited.add(faceCanonKey(child)) then stack.push(child)
    Result(emitted.toMap, fronts.size, states, stack.nonEmpty)
