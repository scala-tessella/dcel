package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.*
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}

import scala.collection.mutable

/** Direct combinatorial torus-quotient enumeration (ADR-0021) — the successor that targets the cells the
  * fixed-Λ engines cannot reach (ADR-0020's covolume-exponential wall: n = 4–7 and the dodecagon types).
  *
  * Instead of sweeping candidate lattices Λ and growing the infinite cover (a planar patch), this enumerates
  * the finite QUOTIENT directly: a combinatorial torus map — `{3,4,6,12}` faces glued edge-to-edge into a
  * connected genus-1 surface (`V − E + F = 0`) — in which every vertex is a valid 360° type. Such a map
  * develops to a flat periodic tiling automatically (all-360° vertices ⇒ trivial holonomy ⇒ the universal
  * cover is the plane with a *derived* translation lattice), so:
  *   - there is NO candidate-lattice sweep (Λ falls out of the map), hence no covolume-exponential, and
  *   - there is NO scatter (a finite map is periodic by construction; aperiodic tilings never appear).
  *
  * Architecture (see ADR-0021):
  *   1. ENUMERATE maps by boundary gluing — grow a partial map face by face (MRV vertex completion, as
  *      [[KrotenheerdtTorusSearch]] does), but each new polygon edge may either open a new boundary edge OR
  *      glue to an existing boundary edge (the torus identification). A closed, all-vertices-complete map is
  *      a candidate cell. Bounded by FACE COUNT, not covolume.
  *   2. DEVELOP + check consistency in exact ℤ[ζ₁₂] ([[ZetaPoint]]): each gluing imposes an exact coordinate
  *      identification; a gluing is consistent iff the two identified vertices coincide mod the emergent
  *      lattice (`ZetaPoint.congruentMod`). Inconsistent gluings prune at the branch point.
  *   3. VERIFY + identify by reusing the proven tail: hand a closed, consistent cell to
  *      [[KrotenheerdtLatticeSearch.verifyContentAnyN]] (orbits = types = n) and `torusContentKey`, so
  *      soundness, chirality, sublattice reduction, and the n-bucketing are inherited unchanged — and every
  *      result cross-checks against the fixed-Λ engines on the cells both can reach.
  *
  * STATUS: prototype STARTING. The intended validation path is to reproduce the fixed-Λ counts exactly on the
  * cells they reach (n = 1 small cells, n = 2) before pushing to the dodecagon cells and n = 3–7 beyond the
  * ADR-0020 wall. The polygon-construction and exact-coordinate primitives are shared with
  * [[KrotenheerdtTorusSearch]] / [[ZetaPoint]]; the new element is the edge-gluing branch + map dedup.
  */
object KrotenheerdtTorusMapSearch:

  // ======================================================================================================
  // Shared cell geometry (ADR-0021 "staged duplicate-then-extract"): these mirror the pure helpers of
  // [[KrotenheerdtTorusSearch]] verbatim. They are duplicated, not shared, until the new engine reproduces
  // the fixed-Λ counts; afterwards they extract to a `private[dcel] object TorusCellGeometry`. The originals'
  // `isConsistent`/boundary helpers are NOT duplicated — those hard-assume a fixed rank-2 Λ and are
  // re-implemented rank-aware by the gluing search.
  // ======================================================================================================

  /** Polygons of the 30°-edge world (octagon excluded — its `4.8.8` needs ℤ[ζ₂₄], handled separately). */
  private val sides: List[Int] = List(3, 4, 6, 12)

  /** Interior angle of an `m`-gon in 30° slots (triangle 2, square 3, hexagon 4, dodecagon 5). */
  private val gSlots: Map[Int, Int] =
    sides.map(m => m -> (interiorAngle(m).toRational.toDouble / 30.0).toInt).toMap

  /** CCW exterior-turn between consecutive edges of an `m`-gon, in 30° slots: `δ = 6 − g`. */
  private def delta(m: Int): Int = 6 - gSlots(m)

  private val area: Map[Int, BigDecimal] =
    sides.map(m => m -> BigDecimal(m / (4.0 * math.tan(math.Pi / m)))).toMap

  private val slotOfUnit: Map[ZetaPoint, Int] = (0 until 12).map(s => ZetaPoint.unit(s) -> s).toMap

  private val sqrt3d = math.sqrt(3.0)

  private def cross(a: BigPoint, b: BigPoint): BigDecimal = a.x * b.y - a.y * b.x

  /** Fast Double embedding of a ZetaPoint into the plane (residue grouping only; the verify horizon
    * recomputes exactly in BigDecimal).
    */
  private def dxy(z: ZetaPoint): (Double, Double) =
    ((2 * z.a0 + z.a2 + z.a1 * sqrt3d) / 2.0, (2 * z.a3 + z.a1 + z.a2 * sqrt3d) / 2.0)

  /** The CCW corners of a unit `m`-gon rooted at `p` whose first edge leaves `p` along 30°-slot `s`. */
  private def polygon(p: ZetaPoint, s: Int, m: Int): Vector[ZetaPoint] =
    val buf = Vector.newBuilder[ZetaPoint]
    var cur = p
    var dir = s
    var i   = 0
    while i < m do
      buf += cur
      cur = cur + ZetaPoint.step(dir)
      dir += delta(m)
      i += 1
    buf.result()

  /** A placed unit polygon, developed in the universal-cover plane frame in exact ℤ[ζ₁₂]. */
  final case class FaceZ(size: Int, corners: Vector[ZetaPoint]):
    def centroid: BigPoint = corners.map(_.toBigPoint).toList.centroid

    lazy val cD: (Double, Double) =
      var sx = 0.0
      var sy = 0.0
      corners.foreach: z =>
        val (x, y) = dxy(z)
        sx += x
        sy += y
      (sx / size, sy / size)

  /** The outgoing 30°-slot of face `f` at corner `p` (the edge `p → next` in CCW order). */
  private def outSlot(f: FaceZ, p: ZetaPoint): Int =
    val i = f.corners.indexOf(p)
    slotOfUnit(f.corners((i + 1) % f.size) - p)

  /** The full corona of a vertex type around the origin: each polygon at its cumulative 30°-slot. */
  private def coronaFaces(typeSizes: List[Int]): List[FaceZ] =
    var slot = 0
    typeSizes.map: m =>
      val f = FaceZ(m, polygon(ZetaPoint.origin, slot, m))
      slot += gSlots(m)
      f

  /** Valid vertex types over `{3,4,6,12}`, each as a concrete cyclic order plus its mirror — corona seeds. */
  private val seedTypes: List[List[Int]] =
    validSignatures.filterNot(_.contains(8)).flatMap(sig => List(sig, sig.reverse)).toList.distinct

  private def tkeyD(
      x: Double,
      y: Double,
      vx: Double,
      vy: Double,
      wx: Double,
      wy: Double,
      det: Double
  ): (Long, Long) =
    def snap(t: Double): Long =
      val r = math.round(t * 100000.0)
      ((r % 100000) + 100000) % 100000
    (snap((x * wy - wx * y) / det), snap((vx * y - x * vy) / det))

  private def tkey(p: BigPoint, vB: BigPoint, wB: BigPoint, originB: BigPoint): (Long, Long) =
    val det                       = vB.x * wB.y - vB.y * wB.x
    val d                         = p - originB
    def snap(t: BigDecimal): Long =
      val r = (t * 100000).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
      (((r % 100000) + 100000) % 100000).toLong
    (snap((d.x * wB.y - wB.x * d.y) / det), snap((vB.x * d.y - d.x * vB.y) / det))

  /** Distinct-torus-face area: each face counted once per `(size, centroid mod Λ)`. */
  private def distinctArea(faces: List[FaceZ], vB: BigPoint, wB: BigPoint): BigDecimal =
    val vx  = vB.x.toDouble; val vy = vB.y.toDouble; val wx = wB.x.toDouble; val wy = wB.y.toDouble
    val det = vx * wy - vy * wx
    faces
      .map(f => (f.size, tkeyD(f.cD._1, f.cD._2, vx, vy, wx, wy, det)))
      .distinct
      .map((m, _) => area(m))
      .sum

  /** Dedup a cell's faces by Double residue, then take the exact BigDecimal centroid of the distinct few. */
  private def distinctFacesOf(faces: List[FaceZ], aB: BigPoint, bB: BigPoint): List[(Int, BigPoint)] =
    val ax  = aB.x.toDouble; val ay = aB.y.toDouble; val bx = bB.x.toDouble; val by = bB.y.toDouble
    val det = ax * by - ay * bx
    faces.distinctBy(f => (f.size, tkeyD(f.cD._1, f.cD._2, ax, ay, bx, by, det))).map(f =>
      (f.size, f.centroid)
    )

  /** Reconstruct each torus vertex's full fan (mod the basis aB, bB) by unioning the incident corners of all
    * its planar instances, keyed by angle — `KrotenheerdtTorusSearch.reconstructFans` verbatim.
    */
  private def reconstructFans(
      faces: List[FaceZ],
      aB: BigPoint,
      bB: BigPoint,
      originB: BigPoint
  ): (Map[(Long, Long), List[((Long, Long), ZetaPoint, FaceZ)]], Map[(Long, Long), Map[Long, Int]]) =
    val byTorus = faces
      .flatMap(f => f.corners.map(p => (tkey(p.toBigPoint, aB, bB, originB), p, f)))
      .groupBy(_._1)
    val fans    = byTorus.view.mapValues: incident =>
      incident.map: (_, p, f) =>
        val c = f.centroid
        (
          math.round(math.atan2((c.y - p.toBigPoint.y).toDouble, (c.x - p.toBigPoint.x).toDouble) * 10000),
          f.size
        )
      .toMap
    .toMap
    (byTorus, fans)

  private def fanComplete(fan: Map[Long, Int]): Boolean =
    fan.values.map(interiorAngle).foldLeft(AngleDegree(0))(_ + _) == AngleDegree(360)

  /** True iff the discovered Λ = (pv, pw) is a GENUINE translation period: the cell faces and their
    * Λ-translates have NO interior overlap. Reduce to one fundamental domain, lay out the 3×3 block of
    * Λ-translates, and check no face corner lies STRICTLY inside another face. For convex unit polygons,
    * interiors overlap iff some corner is strictly interior — so this is an exact, complete overlap test.
    *
    * This is the check the slot/fan/edge/area tests all miss: an all-360° closed torus MAP can still have
    * nontrivial rotational holonomy (a flat cone / Klein-type identification — ADR-0021's orientability
    * caveat), in which case the "period" Λ is false and the development overlaps. The angle-valid but
    * non-tiling vertex types `3.3.6.6` and `3.4.4.6` are exactly such cells; they pass `isConsistentMod`,
    * edge pairing and `distinctArea == covol`, yet fail HERE — their Λ-translated faces overlap.
    */
  private def tilesWithoutOverlap(faces: List[FaceZ], pv: ZetaPoint, pw: ZetaPoint): Boolean =
    val vB                                                              = pv.toBigPoint; val wB  = pw.toBigPoint
    val vx                                                              = vB.x.toDouble; val vy  = vB.y.toDouble; val wx = wB.x.toDouble
    val wy                                                              = wB.y.toDouble; val det = vx * wy - vy * wx
    def scale(z: ZetaPoint, k: Int)                                     = ZetaPoint(z.a0 * k, z.a1 * k, z.a2 * k, z.a3 * k)
    val cell                                                            =
      faces.groupBy(f => (f.size, tkeyD(f.cD._1, f.cD._2, vx, vy, wx, wy, det))).values.map(_.head).toList
    val tiled                                                           =
      for f <- cell; i <- -1 to 1; j <- -1 to 1
      yield FaceZ(f.size, f.corners.map(_ + scale(pv, i) + scale(pw, j)))
    // point STRICTLY left of every CCW edge ⇔ strictly inside the convex polygon (a corner on an edge gives a
    // zero cross product, not strictly inside — so shared edges/corners are allowed, only true overlap fails).
    def strictlyInside(poly: Vector[ZetaPoint], pt: ZetaPoint): Boolean =
      val pts = poly.map(_.toBigPoint)
      val p   = pt.toBigPoint
      val n   = pts.size
      (0 until n).forall: i =>
        val a = pts(i); val b = pts((i + 1) % n)
        (b.x - a.x) * (p.y - a.y) - (b.y - a.y) * (p.x - a.x) > BigDecimal("1e-9")
    tiled.forall: fa =>
      tiled.forall: fb =>
        (fa.corners == fb.corners) || fb.corners.forall(c => !strictlyInside(fa.corners, c))

  /** Verify a developed, closed torus cell: faces placed in the plane frame plus the two derived deck vectors
    * `g1, g2` (the discovered lattice Λ). This is `KrotenheerdtTorusSearch.cellData` → `classifyAnyN`
    * (`Right`-branch) inlined, calling the proven `private[dcel]` tail: re-key on the content's PRIMITIVE
    * lattice, require every torus fan complete, count vertex orbits exactly, and accept iff orbits = types =
    * some `n ≤ maxN`. Returns `(n, types, canonical-key, primitiveCovolume)` — the canonical key cross-checks
    * against the fixed-Λ engines on the cells both can reach, and the covolume lets a caller bound the search
    * the same way the fixed-Λ engine does. `None` if the cell is not yet closed (a fan still incomplete or
    * the derived Λ-cell not filled) or it is not a Krotenheerdt tiling for any `n ≤ maxN`.
    */
  def verifyCell(
      faces: List[FaceZ],
      g1: ZetaPoint,
      g2: ZetaPoint,
      maxN: Int
  ): Option[(Int, Set[VertexSignature], String, BigDecimal)] =
    val vB       = g1.toBigPoint
    val wB       = g2.toBigPoint
    val originB  = BigPoint.origin
    val distinct = distinctFacesOf(faces, vB, wB)
    val (pv, pw) = KrotenheerdtLatticeSearch.primitiveBasis(vB, wB, originB, distinct, Nil)
    val pcov     = cross(pv, pw).abs
    if distinctArea(faces, pv, pw) < pcov - BigDecimal("1e-6") then None // Λ-cell not yet filled
    else if !tilesWithoutOverlap(faces, g1, g2) then None                // false period (rotational holonomy)
    else
      val (byTorus, fans) = reconstructFans(faces, pv, pw, originB)
      if fans.exists((_, f) => !fanComplete(f)) then None // a torus fan still incomplete
      else
        val sigs  = fans.view.mapValues(f => VertexTypes.normalize(f.toList.sortBy(_._1).map(_._2))).toMap
        val types = sigs.values.toSet
        if types.sizeIs > maxN then None
        else
          val pDistinct = distinctFacesOf(faces, pv, pw)
          val verts     = sigs.toList.map((tk, sig) => (sig.mkString("."), byTorus(tk).head._2.toBigPoint))
          KrotenheerdtLatticeSearch
            .verifyContentAnyN(pv, pw, originB, pDistinct, verts, types, maxN)
            .map((nn, key) => (nn, types, key, pcov))

  // ======================================================================================================
  // The discovered-Λ propagation search (ADR-0021 step 1). We develop ONE planar patch per seed corona in the
  // universal-cover frame (exact ℤ[ζ₁₂]), growing it by MRV vertex completion under the same valid-vertex /
  // ≤maxN-type soundness prune as the fixed-Λ engine — but with NO candidate lattice. The deck lattice Λ is
  // *discovered* from the patch: a translation between two completed vertices with an identical fan is a
  // candidate deck vector (the edge-gluing of the ADR, read off as a period). As soon as a rank-2 period basis
  // is found whose cell the patch fills with all fans complete, [[verifyCell]] closes and identifies it. No
  // lattice sweep ⇒ no covolume-exponential; the cost is the bounded planar growth, gated by face count.
  // ======================================================================================================

  /** The 24 isometries of ℤ[ζ₁₂] (dihedral order 24): rotations `ζ^k` and their conjugate-reflections, as
    * exact integer maps. The full module group (not just a lattice's point group) — the partial map carries
    * no fixed Λ, so the canonical visited-set must be invariant under every rigid motion of the developed
    * patch.
    */
  private val groupMaps: List[ZetaPoint => ZetaPoint] =
    for
      reflect <- List(false, true)
      k       <- (0 until 12).toList
    yield (z: ZetaPoint) =>
      var p = if reflect then z.conjugate else z
      var i = 0
      while i < k do { p = p.timesZeta; i += 1 }
      p

  /** Geometry-free canonical key of a developed patch, invariant under the module group + translation: for
    * each isometry `g`, transform corners, translation-anchor on the lexicographically minimal corner, sort
    * faces and corners, and take the min over all `g`. The trig-free dedup that makes two growth orders (or
    * two chiral/rotated developments) reaching the same patch collapse to one visited entry. (The ADR's
    * flag/dart canonical labelling, realised on the exact integer coordinates.)
    */
  private def canonicalKey(faces: List[FaceZ]): Vector[Long] =
    import scala.math.Ordering.Implicits.seqOrdering
    groupMaps.iterator.map: g =>
      val tf     = faces.map(f => (f.size, f.corners.map(g)))
      val anchor = tf.iterator.flatMap(_._2).min
      tf.map: (s, cs) =>
        s.toLong +: cs.map(_ - anchor).sorted.flatMap(z => Vector(z.a0, z.a1, z.a2, z.a3))
      .sorted
        .flatten
        .toVector
    .min

  /** Planar fan at vertex `p`: incident faces' `(startSlot, size)`, by ascending start slot. */
  private def planarFan(faces: List[FaceZ], p: ZetaPoint): List[(Int, Int)] =
    faces.filter(_.corners.contains(p)).map(f => (outSlot(f, p), f.size)).sortBy(_._1)

  private def coveredSlots(fan: List[(Int, Int)]): Set[Int] =
    fan.flatMap((start, m) => (0 until gSlots(m)).map(kk => (start + kk) % 12)).toSet

  /** Planar (Λ-free) consistency: at every developed vertex, the incident faces occupy a conflict-free set of
    * 30° slots — the analogue of the fixed-Λ `isConsistent`, but keyed by EXACT ZetaPoint identity (no Λ
    * residue), since here the patch is a genuine non-overlapping planar chunk of the universal cover.
    */
  private def isPlanarConsistent(faces: List[FaceZ]): Boolean =
    val coverage = mutable.Map.empty[ZetaPoint, mutable.Map[Int, (Int, Int)]]
    faces.forall: f =>
      f.corners.forall: p =>
        val slotMap = coverage.getOrElseUpdate(p, mutable.Map.empty)
        val start   = outSlot(f, p)
        (0 until gSlots(f.size)).forall: kk =>
          val slot = (start + kk) % 12
          slotMap.get(slot) match
            case Some(owner) if owner != ((start, f.size)) => false
            case _                                         => slotMap(slot) = (start, f.size); true

  /** Sound iff every developed vertex's fan is a valid completed vertex or an extendable partial fan, AND the
    * patch's *completed* vertices show at most `n` distinct types — `KrotenheerdtTorusSearch.isSound`
    * verbatim (it is already Λ-free). The main prune that keeps the planar growth on real n-uniform tilings.
    */
  private def isSound(faces: List[FaceZ], n: Int): Boolean =
    val completeTypes = mutable.Set.empty[VertexSignature]
    faces.flatMap(_.corners).distinct.forall: p =>
      val fan     = planarFan(faces, p)
      val covered = coveredSlots(fan)
      if covered.sizeIs == 12 then
        isCompleteVertex(fan.map(_._2)) && {
          completeTypes += VertexTypes.normalize(fan.map(_._2)); completeTypes.sizeIs <= n
        }
      else isExtendableFan(fan.map(_._2))

  /** Every way to fill a vertex's remaining `gap` (30° slots) so `fan ++ completion`, read CCW, is a valid
    * complete vertex type — `KrotenheerdtTorusSearch.completions` verbatim.
    */
  private def completions(fan: List[Int], gap: Int): List[List[Int]] =
    if gap == 0 then if isCompleteVertex(fan) then List(Nil) else Nil
    else
      sides.flatMap: m =>
        val g = gSlots(m)
        if g > gap then Nil
        else
          val extended = fan :+ m
          val ok       = if g == gap then isCompleteVertex(extended) else isExtendableFan(extended)
          if !ok then Nil else completions(extended, gap - g).map(m :: _)

  /** Planar constraint-propagation growth: commit the WHOLE most-constrained (MRV) incomplete vertex,
    * branching over its valid completions — `KrotenheerdtTorusSearch.growByCompletion` with the Λ-consistency
    * gate replaced by the Λ-free [[isPlanarConsistent]]. Develops the patch outward in the universal cover.
    *
    * MRV ties are broken by distance to the patch centroid (then by exact coordinates for determinism) so the
    * patch grows as a compact DISK, not a 1-D strip. This matters here in a way it does not for the fixed-Λ
    * engine: with no Λ to bound the patch, Λ is discovered by gluing the patch boundary in two *independent*
    * directions ([[boundaryGlueBases]]); strip growth keeps the boundary collinear, so the rank-2 gluing —
    * and single-face cells like `4⁴`/`6.6.6` — would never appear. Compact growth surfaces it within a disk.
    */
  private def growByCompletionPlanar(faces: List[FaceZ], n: Int): List[List[FaceZ]] =
    val centroid   = faces.flatMap(_.corners).distinct.map(_.toBigPoint).centroid
    val incomplete = faces.flatMap(_.corners).distinct.flatMap: p =>
      val fan     = planarFan(faces, p)
      val covered = coveredSlots(fan)
      Option.when(covered.sizeIs < 12):
        val free     = 12 - covered.size
        val b        = (0 until 12).find(s => covered((s + 11) % 12) && !covered(s)).getOrElse(0)
        val arcStart = (b + free) % 12
        val ordered  = fan.sortBy((start, _) => (start - arcStart + 12) % 12).map(_._2)
        (p, b, completions(ordered, free))
    if incomplete.isEmpty then Nil
    else
      val (p, b, comps) =
        incomplete.minBy((p, _, cs) => (cs.size, p.toBigPoint.distanceTo(centroid), p.a0, p.a1, p.a2, p.a3))
      comps.flatMap: comp =>
        var slot     = b
        val newFaces = comp.map: m =>
          val f = FaceZ(m, polygon(p, slot, m))
          slot += gSlots(m)
          f
        val next     = newFaces ++ faces
        Option.when(isPlanarConsistent(next) && isSound(next, n))(next)

  /** The patch's directed boundary half-edges `(p, slot)` (edge `p → p+step(slot)`, CCW so the patch is on
    * its left): a face edge whose REVERSE half-edge `(p+step(slot), slot+6)` is not also a face edge, i.e.
    * not yet shared with a neighbour. On the torus every edge is shared by two faces, so each boundary
    * half-edge is eventually either extended (a neighbour face placed) or GLUED to another boundary
    * half-edge.
    */
  private def boundaryHalfEdges(faces: List[FaceZ]): List[(ZetaPoint, Int)] =
    val edges   = faces.flatMap: f =>
      f.corners.indices.map: i =>
        val p = f.corners(i)
        (p, slotOfUnit(f.corners((i + 1) % f.size) - p))
    val edgeSet = edges.toSet
    edges.filter((p, s) => !edgeSet.contains((p + ZetaPoint.step(s), (s + 6) % 12)))

  /** Candidate deck-lattice bases by BOUNDARY GLUING — the heart of ADR-0021's early closure. Gluing the
    * boundary half-edge `(p1, b)` to an antiparallel one `(p2, b+6)` identifies them as the same torus edge
    * under the pure translation `t = p1 + step(b) − p2` (orientation-preserving ⇒ a deck vector, never a
    * glide — the orientability condition). Each antiparallel boundary pair yields one candidate `t`; the
    * shortest few, paired into independent rank-2 bases shortest-covolume first, are the candidate Λ's.
    * Unlike reading periods off repeated *vertices* (which needs ~two developed cells), boundary gluing
    * closes at ONE cell — the deck vectors are the opposite-side identifications of a single fundamental
    * domain — so the patch never grows large and the n ≥ 2 scatter is cut. `verifyCell`'s covolume / fan /
    * orbit checks reject a `t` that is not a genuine period, so an over-generous candidate set only costs
    * verify calls.
    */
  private def boundaryGlueBases(faces: List[FaceZ]): List[(ZetaPoint, ZetaPoint)] =
    val boundary = boundaryHalfEdges(faces)
    val ts       =
      (for
        (p1, s1) <- boundary
        (p2, s2) <- boundary
        if s2 == (s1 + 6) % 12 // antiparallel: orientation-preserving translation gluing
        t = p1 + ZetaPoint.step(s1) - p2
        if !t.isOrigin
      yield t).distinct
    bases(ts.sortBy { t =>
      val (x, y) = dxy(t); x * x + y * y
    }.take(8))

  /** Candidate deck-lattice bases read off the patch by REPEATED VERTICES: two completed vertices carrying
    * the IDENTICAL fan (same absolute `(slot, size)` list — a translation preserves edge directions, so an
    * identical fan marks a pure translation period) differ by a candidate deck vector. This is the closure
    * oracle the engine uses: unlike [[boundaryGlueBases]] it is empty until the patch carries a *repeated*
    * complete vertex (i.e. near closure), so the (O(faces²)) `verifyCell` is not paid on every growth state.
    * The shortest few vectors are paired into independent rank-2 bases, shortest-covolume first.
    */
  private def periodBases(faces: List[FaceZ]): List[(ZetaPoint, ZetaPoint)] =
    val byFan = mutable.Map.empty[List[(Int, Int)], mutable.ListBuffer[ZetaPoint]]
    faces.flatMap(_.corners).distinct.foreach: p =>
      val fan = planarFan(faces, p)
      if coveredSlots(fan).sizeIs == 12 then byFan.getOrElseUpdate(fan, mutable.ListBuffer.empty) += p
    val diffs =
      byFan.values.flatMap: buf =>
        val ps = buf.toList
        for i <- ps.indices; j <- ps.indices if j > i; d = ps(j) - ps(i) if !d.isOrigin yield d
    bases(diffs.toList.distinct.sortBy { d =>
      val (x, y) = dxy(d); x * x + y * y
    }.take(6))

  /** Independent rank-2 bases from candidate deck vectors, shortest-covolume first. */
  private def bases(vectors: List[ZetaPoint]): List[(ZetaPoint, ZetaPoint)] =
    (for
      i       <- vectors.indices
      j       <- vectors.indices
      if i < j
      (x1, y1) = dxy(vectors(i))
      (x2, y2) = dxy(vectors(j))
      cov      = math.abs(x1 * y2 - y1 * x2)
      if cov > 1e-9
    yield (vectors(i), vectors(j), cov)).sortBy(_._3).map((g1, g2, _) => (g1, g2)).toList

  /** Try to close the patch into a torus cell: for each candidate period basis, run the proven [[verifyCell]]
    * tail; record any Krotenheerdt tiling it identifies. Returns true iff the patch closed (⇒ stop growing
    * this branch: a complete cell only replicates itself under further growth).
    */
  private def tryClose(
      faces: List[FaceZ],
      maxN: Int,
      maxCovolume: Double,
      results: mutable.Map[String, (Int, Set[VertexSignature])]
  ): Boolean =
    // A connected patch develops ONE tiling, whose period lattice is unique up to basis. `boundaryGlueBases` yields
    // candidates shortest-covolume first, so the FIRST that verifies is the PRIMITIVE cell; a coarser period
    // (a sublattice) is the same tiling seen through an m-times-larger torus and must NOT be recorded as a
    // separate cell. (`primitiveBasis` reduces a sublattice basis derived from a full cell, but a patch that
    // fills only the coarse cell has no finer content to reduce from — so we pick the minimal cell here.)
    var best: Option[(Int, Set[VertexSignature], String, BigDecimal)] = None
    boundaryGlueBases(faces).foreach: (g1, g2) =>
      verifyCell(faces, g1, g2, maxN).foreach: hit =>
        if best.forall(hit._4 < _._4) then best = Some(hit)
    best.foreach: (n, types, key, pcov) =>
      // Closed into a genuine torus cell ⇒ caller stops growing this branch (further growth only replicates
      // it); record only cells within the covolume bound (the search's analogue of the fixed-Λ covol cap).
      if pcov <= BigDecimal(maxCovolume) + BigDecimal("1e-6") then results.getOrElseUpdate(key, (n, types))
    best.isDefined

  /** Residue slot-consistency mod Λ — `KrotenheerdtTorusSearch.isConsistent` verbatim. Every torus vertex's
    * incident faces (grouped by position mod Λ, Double residue) occupy a conflict-free set of 30° slots. THIS
    * is the property that genuinely certifies a valid edge-to-edge tiling: if it holds and each
    * residue-vertex is complete (12 slots) then the Λ-periodic development tiles the plane with no overlap or
    * gap. The angle-bucket fan reconstruction ([[reconstructFans]]) does NOT imply it — that is the soundness
    * gap that lets an angle-valid-but-non-tiling type (3.3.6.6, 3.4.4.6) slip through.
    */
  private def isConsistentMod(faces: List[FaceZ], vB: BigPoint, wB: BigPoint): Boolean =
    val vx       = vB.x.toDouble; val vy = vB.y.toDouble; val wx = wB.x.toDouble; val wy = wB.y.toDouble
    val det      = vx * wy - vy * wx
    val coverage = mutable.Map.empty[(Long, Long), mutable.Map[Int, (Int, Int)]]
    faces.forall: f =>
      f.corners.forall: p =>
        val (px, py) = dxy(p)
        val slotMap  = coverage.getOrElseUpdate(tkeyD(px, py, vx, vy, wx, wy, det), mutable.Map.empty)
        val start    = outSlot(f, p)
        (0 until gSlots(f.size)).forall: kk =>
          val slot = (start + kk) % 12
          slotMap.get(slot) match
            case Some(owner) if owner != ((start, f.size)) => false
            case _                                         => slotMap(slot) = (start, f.size); true

  /** Enumerate the Krotenheerdt tilings with `n ≤ maxN` by direct torus-map construction, bucketed by `n`.
    * Develops one planar patch per seed corona, growing by MRV vertex completion, and closes each branch the
    * moment a discovered rank-2 period lattice makes [[verifyCell]] accept. `maxFaces` bounds the patch (a
    * completeness caveat, like the fixed-Λ `(k, maxCovolume)` bound: a cell needing more faces than the
    * budget is not reached). Returns `(n, types, canonical-key)` per distinct tiling, sorted by `(n, key)`.
    */
  def enumerate(
      maxN: Int,
      maxFaces: Int,
      maxCovolume: Double = Double.MaxValue
  ): List[(Int, Set[VertexSignature], String)] =
    val results = mutable.Map.empty[String, (Int, Set[VertexSignature])]
    val visited = mutable.HashSet.empty[Vector[Long]]
    val stack   = mutable.Stack.empty[List[FaceZ]]
    for sig <- seedTypes do
      val seed = coronaFaces(sig)
      if isPlanarConsistent(seed) && isSound(seed, maxN) && visited.add(canonicalKey(seed)) then
        stack.push(seed)
    while stack.nonEmpty do
      val faces  = stack.pop()
      val closed = tryClose(faces, maxN, maxCovolume, results)
      if !closed && faces.sizeIs < maxFaces then
        growByCompletionPlanar(faces, maxN).foreach: child =>
          if visited.add(canonicalKey(child)) then stack.push(child)
    results.toList.map((key, nt) => (nt._1, nt._2, key)).sortBy((n, _, key) => (n, key))

  // -- hand-built validation cells (rung 1): the single-face torus cells, for the spec ---------------------

  /** The single-square torus cell `4.4.4.4`: one unit square at the origin, deck lattice Λ = ((1,0), (0,1)),
    * covolume 1. (ADR-0021 validation ladder, rung 1.)
    */
  def squareCell: (List[FaceZ], ZetaPoint, ZetaPoint) =
    (List(FaceZ(4, polygon(ZetaPoint.origin, 0, 4))), ZetaPoint(1, 0, 0, 0), ZetaPoint(0, 0, 0, 1))

  /** The single-hexagon torus cell `6.6.6`: one unit hexagon at the origin, deck lattice the triangular
    * lattice spanned by two opposite-edge gluings, covolume 3√3/2 (two torus vertices). (Rung 1.)
    */
  def hexagonCell: (List[FaceZ], ZetaPoint, ZetaPoint) =
    val hex = polygon(ZetaPoint.origin, 0, 6)
    // Opposite-edge gluings: corner i to corner i+3 are antipodal; the deck vectors are the two independent
    // diameters between opposite edge-midpoints, i.e. the differences of opposite corners' positions.
    (List(FaceZ(6, hex)), hex(2) - hex(0), hex(3) - hex(1))

  /** The 2-triangle rhombus torus cell `3⁶` (the triangular tiling): an up-triangle at the origin and the
    * down-triangle across its slot-2 edge, deck lattice the triangular lattice ((1,0), ζ²), covolume √3/2,
    * one torus vertex `3.3.3.3.3.3`. The first MULTI-FACE cell — exercises `distinctFacesOf` /
    * `primitiveBasis` over two same-size faces (rung 2). The remaining multi-face/multi-type/chiral small
    * cells (`3.4.6.4`, `3.3.3.4.4`, `3.6.3.6`, the chiral snub `3.3.3.3.6`) are validated end-to-end by the
    * rung-3 search cross-check against [[KrotenheerdtTorusSearch]], which covers every n = 1 and n = 2 cell
    * key-for-key.
    */
  def triangleCell: (List[FaceZ], ZetaPoint, ZetaPoint) =
    val up   = FaceZ(3, polygon(ZetaPoint.origin, 0, 3))      // (0,0),(1,0),ζ²
    val down = FaceZ(3, polygon(ZetaPoint(1, 0, 0, 0), 2, 3)) // across the up-triangle's slot-2 edge
    (List(up, down), ZetaPoint(1, 0, 0, 0), ZetaPoint(0, 0, 1, 0))
