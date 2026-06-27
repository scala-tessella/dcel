package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.*
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}
import java.util.concurrent.{ConcurrentHashMap, Executors, ForkJoinPool, PriorityBlockingQueue, TimeUnit}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

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
  private[dcel] val gSlots: Map[Int, Int] =
    sides.map(m => m -> (interiorAngle(m).toRational.toDouble / 30.0).toInt).toMap

  /** CCW exterior-turn between consecutive edges of an `m`-gon, in 30° slots: `δ = 6 − g`. */
  private def delta(m: Int): Int = 6 - gSlots(m)

  private val area: Map[Int, BigDecimal] =
    sides.map(m => m -> BigDecimal(m / (4.0 * math.tan(math.Pi / m)))).toMap

  private[dcel] val slotOfUnit: Map[ZetaPoint, Int] = (0 until 12).map(s => ZetaPoint.unit(s) -> s).toMap

  private val sqrt3d = math.sqrt(3.0)

  private def cross(a: BigPoint, b: BigPoint): BigDecimal = a.x * b.y - a.y * b.x

  /** Fast Double embedding of a ZetaPoint into the plane (residue grouping only; the verify horizon
    * recomputes exactly in BigDecimal).
    */
  private def dxy(z: ZetaPoint): (Double, Double) =
    ((2 * z.a0 + z.a2 + z.a1 * sqrt3d) / 2.0, (2 * z.a3 + z.a1 + z.a2 * sqrt3d) / 2.0)

  /** The CCW corners of a unit `m`-gon rooted at `p` whose first edge leaves `p` along 30°-slot `s`. */
  private[dcel] def polygon(p: ZetaPoint, s: Int, m: Int): Vector[ZetaPoint] =
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
  private[dcel] def distinctFacesOf(faces: List[FaceZ], aB: BigPoint, bB: BigPoint): List[(Int, BigPoint)] =
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
      (for f <- cell; i <- -1 to 1; j <- -1 to 1
      yield FaceZ(f.size, f.corners.map(_ + scale(pv, i) + scale(pw, j)))).toVector
    // point STRICTLY left of every CCW edge ⇔ strictly inside the convex polygon (a corner ON an edge gives a
    // zero cross, NOT strictly inside — shared edges/corners allowed, only true overlap fails). EXACT integer
    // orientation (ZetaPoint.crossSign): the predicate the BigDecimal `> 1e-9` approximated, ~10× faster, no ε.
    def strictlyInside(poly: Vector[ZetaPoint], pt: ZetaPoint): Boolean =
      val n = poly.size
      var i = 0
      while i < n do
        if ZetaPoint.crossSign(poly(i), poly((i + 1) % n), pt) <= 0 then return false
        i += 1
      true
    // SPATIAL PRUNING: two unit polygons overlap only if their centroids are within R₁+R₂ ≤ 3.87 (dodecagon
    // circumradius ≈ 1.93), so bucket faces on a size-4 Double grid and test only the 3×3 neighbour buckets —
    // every truly-overlapping pair shares a neighbour bucket; the far pairs (the O(F²) majority) are skipped.
    val cents                                                           = tiled.map: f =>
      var sx = 0.0; var sy = 0.0
      f.corners.foreach: c =>
        val (x, y) = dxy(c); sx += x; sy += y
      (sx / f.corners.size, sy / f.corners.size)
    def bkey(i: Int)                                                    =
      (math.floor(cents(i)._1 / 4.0).toInt, math.floor(cents(i)._2 / 4.0).toInt)
    val buckets                                                         = mutable.HashMap.empty[(Int, Int), mutable.ArrayBuffer[Int]]
    tiled.indices.foreach(i => buckets.getOrElseUpdate(bkey(i), mutable.ArrayBuffer.empty) += i)
    tiled.indices.forall: ia =>
      val fa       = tiled(ia)
      val (bx, by) = bkey(ia)
      var ok       = true
      var di       = -1
      while di <= 1 && ok do
        var dj = -1
        while dj <= 1 && ok do
          buckets.get((bx + di, by + dj)).foreach: bucket =>
            val it = bucket.iterator
            while it.hasNext && ok do
              val fb = tiled(it.next())
              if fa.corners != fb.corners then
                if fb.corners.exists(c => strictlyInside(fa.corners, c)) then ok = false
          dj += 1
        di += 1
      ok

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
    // tilesWithoutOverlap is now exact-integer + spatially pruned (~0.3ms), so run it FIRST: a non-period's
    // Λ-translates overlap, rejecting it before the (BigDecimal, O(F²)) primitiveBasis — which is then paid only
    // on candidates that genuinely tile (≈ only on closing states), not on every committed patch's ~20
    // boundary-glue candidates. Same conjunction of necessary conditions, reordered ⇒ identical accept/reject.
    if !tilesWithoutOverlap(faces, g1, g2) then None // false period (rotational holonomy) / not yet tiling
    else
      val vB       = g1.toBigPoint
      val wB       = g2.toBigPoint
      val originB  = BigPoint.origin
      val distinct = distinctFacesOf(faces, vB, wB)
      val (pv, pw) = KrotenheerdtLatticeSearch.primitiveBasis(vB, wB, originB, distinct, Nil)
      val pcov     = cross(pv, pw).abs
      if distinctArea(faces, pv, pw) < pcov - BigDecimal("1e-6") then None // Λ-cell not yet filled
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

  /** Classify a closed torus cell into the SHARED D-symbol key space (`DelaneySymbols.classifyClosedMap`),
    * instead of the geometric content key. Builds the torus map's barycentric `op` array from the cell: a
    * dart is `(vertexResidue mod Λ, outgoingSlot)`; `α(v,s) = (neighbour, s+6)` (the reverse half-edge),
    * `σ(v,s) = (v, next outgoing slot CCW)` (the rotation system), and `op` is assembled exactly as
    * `BucketAssembly` does (`φ = σ∘α`; chambers `a(g)=2g+1, b(g)=2g+2`). `op` alone encodes polygon sizes
    * (chamber orbits), so this needs no vertex-type bookkeeping. `None` if the map is not cleanly closed (a
    * missing α/σ partner). This is ADR-0032's key-unification step: the grower then dedups in the SAME space
    * as the oracle and the bounded-V assembler, enabling the Phase-3 union and certified counts.
    */
  def torusMapClassify(
      faces: List[FaceZ],
      pvB: BigPoint,
      pwB: BigPoint,
      originB: BigPoint
  ): Option[(Int, List[VertexSignature], String)] =
    cellToOp(faces, pvB, pwB, originB).flatMap(DelaneySymbols.classifyClosedMap)

  /** The barycentric Delaney `op` array of a closed torus cell (faces + Λ): a dart is `(vertexResidue mod Λ,
    * outgoingSlot)`, `α` the reverse half-edge, `σ` the rotation CCW; chambers `a(g)=2g+1, b(g)=2g+2`. The
    * inverse direction is [[realizeCell]]. `None` if the map is not cleanly closed (a missing α/σ partner).
    */
  def cellToOp(
      faces: List[FaceZ],
      pvB: BigPoint,
      pwB: BigPoint,
      originB: BigPoint
  ): Option[Array[Array[Int]]] =
    val (byTorus, _) = reconstructFans(faces, pvB, pwB, originB)
    val outSlots     = byTorus.map((v, inc) => v -> inc.map((_, p, f) => outSlot(f, p)).distinct.sorted).toMap
    val rep          = byTorus.map((v, inc) => v -> inc.head._2).toMap
    val darts        = outSlots.toList.flatMap((v, ss) => ss.map(s => (v, s)))
    val idx          = darts.zipWithIndex.toMap
    val D            = darts.length
    if D == 0 then None
    else
      def neighbour(v: (Long, Long), s: Int): (Long, Long) =
        tkey((rep(v) + ZetaPoint.step(s)).toBigPoint, pvB, pwB, originB)
      val alpha                                            = new Array[Int](D)
      val sigmaNext                                        = new Array[Int](D)
      var ok                                               = true
      darts.foreach: (v, s) =>
        val g  = idx((v, s))
        val ss = outSlots(v)
        val sn = idx.get((v, ss((ss.indexOf(s) + 1) % ss.length)))
        val al = idx.get((neighbour(v, s), (s + 6) % 12))
        (al, sn) match
          case (Some(a), Some(n)) => alpha(g) = a; sigmaNext(g) = n
          case _                  => ok = false
      if !ok then None
      else
        val phi    = Array.tabulate(D)(g => sigmaNext(alpha(g))) // next dart around a face
        val phiInv = Array.fill(D)(-1)
        var g0     = 0
        while g0 < D do { phiInv(phi(g0)) = g0; g0 += 1 }
        val op     = Array.ofDim[Int](2 * D + 1, 3)
        var g      = 0
        while g < D do
          val a = 2 * g + 1; val b = 2 * g + 2
          op(a)(0) = b; op(b)(0) = a   // r0: a ↔ b (cross vertex)
          op(a)(2) = 2 * alpha(g) + 2  // r2: cross face
          op(b)(2) = 2 * alpha(g) + 1
          op(a)(1) = 2 * phiInv(g) + 2 // r1: cross edge
          op(b)(1) = 2 * phi(g) + 1
          g += 1
        Some(op)

  /** D-SYMBOL → GEOMETRY: realize a closed torus map (`op`) as an exact ℤ[ζ₁₂] cell (faces + primitive
    * lattice Λ), the inverse of [[cellToOp]]. Develops the map by BFS: place one face as a regular polygon,
    * then place each α-neighbour as a regular polygon sharing the (reversed) edge — deterministic, no search
    * (the map dictates every gluing). Λ is then read off the developed patch via [[boundaryGlueBases]] +
    * [[verifyCell]]. Lets us obtain a geometric cell — and hence [[rotationCenters]] — for ANY tiling given
    * its D-symbol/op (e.g. the small-cell tilings the symmetry grower captures away), independent of either
    * grower's reach.
    */
  def realizeCell(op: Array[Array[Int]], maxN: Int = 7): Option[(List[FaceZ], BigPoint, BigPoint)] =
    val D = (op.length - 1) / 2
    if D <= 0 then None
    else
      def alpha(g: Int): Int = (op(2 * g + 1)(2) - 2) / 2
      val phi                = Array.tabulate(D)(g => (op(2 * g + 2)(1) - 1) / 2)
      val phiInv             = Array.fill(D)(-1)
      for g <- 0 until D do phiInv(phi(g)) = g
      // The op's orientation may be CW or CCW relative to the CCW `polygon` placement; try the face cycle in
      // both directions (φ and φ⁻¹) and keep whichever develops consistently.
      develop(op, D, alpha, phi, maxN).orElse(develop(op, D, alpha, phiInv, maxN))

  /** One development attempt of [[realizeCell]] with a given face-successor (`succ` = φ or φ⁻¹). */
  private def develop(
      op: Array[Array[Int]],
      D: Int,
      alpha: Int => Int,
      succ: Array[Int],
      maxN: Int
  ): Option[(List[FaceZ], BigPoint, BigPoint)] =
    val faceOf                                                  = Array.fill(D)(-1)
    val faceDarts                                               = mutable.ArrayBuffer.empty[Vector[Int]]
    var g0                                                      = 0
    while g0 < D do
      if faceOf(g0) < 0 then
        val orbit = mutable.ArrayBuffer(g0)
        faceOf(g0) = faceDarts.length
        var h     = succ(g0)
        while h != g0 do { faceOf(h) = faceDarts.length; orbit += h; h = succ(h) }
        faceDarts += orbit.toVector
      g0 += 1
    val tail                                                    = new Array[ZetaPoint](D)
    val slotA                                                   = new Array[Int](D)
    val placed                                                  = Array.fill(D)(false)
    val faceCorners                                             = Array.fill(faceDarts.length)(Vector.empty[ZetaPoint])
    val queue                                                   = mutable.Queue.empty[Int]
    def placeFace(startDart: Int, t0: ZetaPoint, s0: Int): Unit =
      val fi = faceOf(startDart)
      if faceCorners(fi).nonEmpty then () // already placed (reached via another dart)
      else
        val orbit   = faceDarts(fi)
        val k       = orbit.indexOf(startDart)
        val ord     = orbit.drop(k) ++ orbit.take(k) // start the cycle at startDart
        val size    = ord.length
        val corners = polygon(t0, s0, size)
        var i       = 0
        while i < size do
          val d = ord(i)
          tail(d) = corners(i); slotA(d) = (s0 + i * delta(size)) % 12; placed(d) = true
          queue.enqueue(d)
          i += 1
        faceCorners(fi) = corners
    // EXACT deck vectors: at a NON-tree edge (α(g)'s partner already placed at a translated position), the
    // identification translation t = v − tail(α(g)) is a genuine deck vector. These (not boundaryGlueBases'
    // heuristic shortest-pairs, which a spanning-tree patch's irregular boundary defeats) generate Λ exactly.
    val deck                                                    = mutable.ListBuffer.empty[ZetaPoint]
    placeFace(0, ZetaPoint.origin, 0)
    while queue.nonEmpty do
      val g  = queue.dequeue()
      val ag = alpha(g)
      val v  = tail(g) + ZetaPoint.step(slotA(g)) // head of g's edge = tail of α(g)'s (reversed) edge
      if !placed(ag) then placeFace(ag, v, (slotA(g) + 6) % 12)
      else
        val t = v - tail(ag)
        if !t.isOrigin then deck += t
    if placed.exists(!_) then None
    else
      val faces                                          = faceCorners.toList.map(cs => FaceZ(cs.length, cs))
      val originB                                        = BigPoint.origin
      // Λ = the rank-2 lattice the deck vectors generate; the cell FILLS it (distinctArea == covolume) and
      // tiles without overlap (a wrong orientation overlaps ⇒ rejected ⇒ the other orientation closes).
      var best: Option[(BigPoint, BigPoint, BigDecimal)] = None
      bases(deck.distinct.toList).foreach: (g1, g2) =>
        val distinct   = distinctFacesOf(faces, g1.toBigPoint, g2.toBigPoint)
        val (pvB, pwB) =
          KrotenheerdtLatticeSearch.primitiveBasis(g1.toBigPoint, g2.toBigPoint, originB, distinct, Nil)
        val pcov       = cross(pvB, pwB).abs
        if pcov > BigDecimal("1e-9") && distinctArea(faces, pvB, pwB) >= pcov - BigDecimal("1e-6")
          && tilesWithoutOverlap(faces, g1, g2)
        then if best.forall(pcov < _._3) then best = Some((pvB, pwB, pcov))
      best.map((pvB, pwB, _) => (faces, pvB, pwB))

  /** GROUND-TRUTH rotational-symmetry reference for a closed torus cell (faces + lattice Λ = (pv, pw)): the
    * set of `(centre-type, order)` rotation centres, type ∈ {"face","vertex","edge"} (polygon-centre / vertex
    * / edge-midpoint) and order ∈ {2,3,4,6}. A centre passes iff the EXACT integer rotation about it (`r(z) =
    * ζ^(12/m)·z + t`, exact since a tiling-preserving rotation maps lattice points to lattice points) maps
    * the face-set onto itself mod Λ. Per centre the MAX order is kept. This is the reference for which SEEDS
    * can reach a tiling (face↔polygon-centre seed, vertex↔vertex seed, edge↔edge-midpoint seed), and the
    * decisive datum for whether the symmetry engine can reach a tiling at all (and from which centre).
    */
  def rotationCenters(faces: List[FaceZ], pvB: BigPoint, pwB: BigPoint): Set[(String, Int)] =
    val originB                                     = BigPoint.origin
    def rkey(z: ZetaPoint): (Long, Long)            = tkey(z.toBigPoint, pvB, pwB, originB)
    def fkey(f: FaceZ): (Int, Vector[(Long, Long)]) = (f.size, f.corners.map(rkey).sorted)
    val faceSet                                     = faces.map(fkey).toSet
    def rot0(z: ZetaPoint, k: Int): ZetaPoint       =
      var p = z; var i = 0; while i < k do { p = p.timesZeta; i += 1 }; p
    // r is a symmetry iff it maps every face to a face mod Λ (finite + injective ⇒ a bijection of the cell)
    def isSym(r: ZetaPoint => ZetaPoint): Boolean   =
      faces.forall(f => faceSet.contains((f.size, f.corners.map(z => rkey(r(z))).sorted)))
    val out                                         = mutable.Set.empty[(String, Int)]
    val orders                                      = List(6, 4, 3, 2)
    // FACE centres (rotation 360/m about a p-gon centre needs m | p): max order per distinct face mod Λ
    for f <- faces.groupBy(fkey).values.map(_.head) do
      val passing = orders.filter(m =>
        f.size % m == 0 && {
          val k = 12 / m; val t = f.corners(f.size / m) - rot0(f.corners.head, k)
          isSym(z => rot0(z, k) + t)
        }
      )
      if passing.nonEmpty then out += (("face", passing.max))
    // VERTEX centres: max order per distinct vertex mod Λ
    for v <- faces.flatMap(_.corners).groupBy(rkey).values.map(_.head) do
      val passing = orders.filter { m =>
        val k = 12 / m; isSym(z => rot0(z, k) + (v - rot0(v, k)))
      }
      if passing.nonEmpty then out += (("vertex", passing.max))
    // EDGE midpoints (order 2 only): 180° about (a+b)/2 ⇒ r(z) = (a+b) − z
    val edges                                       = faces
      .flatMap(f => f.corners.indices.map(i => (f.corners(i), f.corners((i + 1) % f.size))))
      .groupBy((a, b) => Set(rkey(a), rkey(b)))
      .values
      .map(_.head)
    for (a, b) <- edges do
      if isSym(z => rot0(z, 6) + (a + b)) then out += (("edge", 2))
    out.toSet

  /** Rotation-symmetry REFERENCE over the symmetry grower's reachable tilings: for each tiling it closes,
    * record (D-symbol key → (types, its [[rotationCenters]], the seed label that first reached it)).
    * Validates the capture hypothesis (a reached tiling's centres include the seed-type that reached it) and
    * gives the ground-truth rotation structure per tiling. Self-contained (per-seed DFS, like the production
    * grower).
    */
  def symmetryRotationReference(
      maxN: Int,
      maxFaces: Int,
      log: String => Unit = _ => ()
  ): Map[String, (Set[VertexSignature], Set[(String, Int)], String)] =
    val originB = BigPoint.origin
    val out     = mutable.Map.empty[String, (Set[VertexSignature], Set[(String, Int)], String)]
    val seeds   = allSeeds
    for (seed, si) <- seeds.zipWithIndex do
      val visited                                      = mutable.HashSet.empty[Vector[Long]]
      val stack                                        = mutable.Stack.empty[List[FaceZ]]
      val seedCorners                                  = seed.faces.flatMap(_.corners).toSet
      def coronaCommitted(faces: List[FaceZ]): Boolean =
        seedCorners.forall(p => coveredSlots(planarFan(faces, p)).sizeIs == 12)
      if isPlanarConsistent(seed.faces) && isSound(seed.faces, maxN) && visited.add(canonicalKey(seed.faces))
      then stack.push(seed.faces)
      while stack.nonEmpty do
        val faces  = stack.pop()
        var closed = false
        if coronaCommitted(faces) then
          var best: Option[(ZetaPoint, ZetaPoint, BigDecimal)] = None
          boundaryGlueBases(faces).foreach: (g1, g2) =>
            val vB   = g1.toBigPoint
            val wB   = g2.toBigPoint
            val cov0 = cross(vB, wB).abs
            if cov0 > BigDecimal("1e-9") && distinctArea(faces, vB, wB) >= cov0 - BigDecimal("1e-6") then
              verifyCell(faces, g1, g2, maxN).foreach((_, _, _, pcov) =>
                if best.forall(pcov < _._3) then best = Some((g1, g2, pcov))
              )
          best.foreach: (g1, g2, _) =>
            val distinct   = distinctFacesOf(faces, g1.toBigPoint, g2.toBigPoint)
            val (pvB, pwB) =
              KrotenheerdtLatticeSearch.primitiveBasis(g1.toBigPoint, g2.toBigPoint, originB, distinct, Nil)
            torusMapClassify(faces, pvB, pwB, originB).foreach: (_, sigs, dkey) =>
              out.getOrElseUpdate(dkey, (sigs.toSet, rotationCenters(faces, pvB, pwB), seed.label))
            closed = true
        if !closed && faces.sizeIs < maxFaces then
          growBySymmetry(faces, maxN, seed.rot, seed.m).foreach: child =>
            if visited.add(canonicalKey(child)) then stack.push(child)
      log(s"seed ${si + 1}/${seeds.size} '${seed.label}' done — keys so far: ${out.size}")
    out.toMap

  /** Rotation-symmetry reference via the FREE grower (vertex-corona seeds + free planar growth,
    * seed-INDEPENDENT so `visited` is safely shared). It reaches the SMALL cells — including the ones the
    * symmetry grower's close-and-stop captures away — so it supplies the rotation centres of those (e.g. the
    * square-centred {4⁴;3³.4²}). Returns D-symbol key → (types, [[rotationCenters]]). `maxFaces` bounds the
    * (scatter-prone) growth; small cells close well within a modest bound.
    */
  def freeGrowerRotationReference(
      maxN: Int,
      maxFaces: Int
  ): Map[String, (Set[VertexSignature], Set[(String, Int)])] =
    val originB = BigPoint.origin
    val out     = mutable.Map.empty[String, (Set[VertexSignature], Set[(String, Int)])]
    val visited = mutable.HashSet.empty[Vector[Long]]
    val stack   = mutable.Stack.empty[List[FaceZ]]
    for sig <- seedTypes do
      val seed = coronaFaces(sig)
      if isPlanarConsistent(seed) && isSound(seed, maxN) && visited.add(canonicalKey(seed)) then
        stack.push(seed)
    while stack.nonEmpty do
      val faces                                            = stack.pop()
      var best: Option[(ZetaPoint, ZetaPoint, BigDecimal)] = None
      boundaryGlueBases(faces).foreach: (g1, g2) =>
        val vB   = g1.toBigPoint
        val wB   = g2.toBigPoint
        val cov0 = cross(vB, wB).abs
        if cov0 > BigDecimal("1e-9") && distinctArea(faces, vB, wB) >= cov0 - BigDecimal("1e-6") then
          verifyCell(faces, g1, g2, maxN).foreach((_, _, _, pcov) =>
            if best.forall(pcov < _._3) then best = Some((g1, g2, pcov))
          )
      best.foreach: (g1, g2, _) =>
        val distinct   = distinctFacesOf(faces, g1.toBigPoint, g2.toBigPoint)
        val (pvB, pwB) =
          KrotenheerdtLatticeSearch.primitiveBasis(g1.toBigPoint, g2.toBigPoint, originB, distinct, Nil)
        torusMapClassify(faces, pvB, pwB, originB).foreach: (_, sigs, dkey) =>
          out.getOrElseUpdate(dkey, (sigs.toSet, rotationCenters(faces, pvB, pwB)))
      if best.isEmpty && faces.sizeIs < maxFaces then
        growByCompletionPlanar(faces, maxN).foreach: child =>
          if visited.add(canonicalKey(child)) then stack.push(child)
    out.toMap

  // ======================================================================================================
  // The discovered-Λ propagation search (ADR-0021 step 1). We develop ONE planar patch per seed corona in the
  // universal-cover frame (exact ℤ[ζ₁₂]), growing it by MRV vertex completion under the same valid-vertex /
  // ≤maxN-type soundness prune as the fixed-Λ engine — but with NO candidate lattice. The deck lattice Λ is
  // *discovered* from the patch: a translation between two completed vertices with an identical fan is a
  // candidate deck vector (the edge-gluing of the ADR, read off as a period). As soon as a rank-2 period basis
  // is found whose cell the patch fills with all fans complete, [[verifyCell]] closes and identifies it. No
  // lattice sweep ⇒ no covolume-exponential; the cost is the bounded planar growth, gated by face count.
  // ======================================================================================================

  /** The visited-set key: an EXACT face-set fingerprint (128-bit hash), position- and orientation-SENSITIVE —
    * NOT quotiented by any isometry. The grower's growth is ANCHORED: it rotates about a FIXED centre
    * (origin) by the seed's FIXED `rot`, so it is equivariant only under the seed's own C_m (which keeps the
    * patch invariant ⇒ already a fixed-point of the dedup), NOT under arbitrary rigid motions. The old key
    * quotiented by the full 24-element module group + translation, which OVER-collapsed distinct anchored
    * patches (two patches differing by a non-seed isometry got the same key yet grow differently under the
    * fixed `rot`), silently pruning valid growth paths — a non-deterministic completeness bug
    * (DeterminismProbe: sequential lost a reachable tiling; parallel found it racily). Keying on the literal
    * face set fixes that: it dedups ONLY genuinely-identical patches (the same faces reached via a different
    * growth ORDER), never different ones. Each face is `(size, its corner coords sorted)`; faces sorted;
    * flattened; hashed to 128 bits (so `visited` stays flat in memory — birthday collision at ~10⁷ states ≈
    * 10⁻²⁵).
    */
  private def canonicalKey(faces: List[FaceZ]): Vector[Long] =
    import scala.math.Ordering.Implicits.seqOrdering
    val perFace: List[Vector[Long]] = faces.map: f =>
      val cs = f.corners.toList.map(z => Vector(z.a0, z.a1, z.a2, z.a3)).sorted // corner SET, canonical order
      (f.size.toLong +: cs.flatten).toVector
    var h1                          = 1125899906842597L               // odd prime seed (polynomial rolling hash, ×31)
    var h2                          = -3750763034362895579L           // FNV-1a 64-bit offset basis (14695981039346656037 as signed Long)
    val it                          = perFace.sorted.iterator.flatten // faces in canonical order
    while it.hasNext do
      val x = it.next()
      h1 = h1 * 31L + x
      h2 = (h2 ^ x) * 1099511628211L // FNV-1a 64-bit prime
    Vector(h1, h2)

  /** Planar fan at vertex `p`: incident faces' `(startSlot, size)`, by ascending start slot. */
  private def planarFan(faces: List[FaceZ], p: ZetaPoint): List[(Int, Int)] =
    faces.filter(_.corners.contains(p)).map(f => (outSlot(f, p), f.size)).sortBy(_._1)

  /** Incident fans per vertex, keyed by a vertex-identity `vid` (`identity` ⇒ exact universal-cover vertex; a
    * mod-⟨h⟩ fold ⇒ a cylinder vertex). One groupBy pass — `(representative corner, sorted (slot, size))` per
    * identified vertex. Shared by [[isSoundBy]]/[[growByCompletionBy]] (replacing the per-vertex `planarFan`
    * filter, so soundness/growth are O(faces) not O(faces²)) and the cylinder automaton.
    */
  private[dcel] def vertexFansBy[K](
      faces: List[FaceZ],
      vid: ZetaPoint => K
  ): Map[K, (ZetaPoint, List[(Int, Int)])] =
    faces
      .flatMap(f => f.corners.map(p => (vid(p), p, f)))
      .groupBy(_._1)
      .view
      .mapValues(inc => (inc.map(_._2).min, inc.map((_, p, f) => (outSlot(f, p), f.size)).sortBy(_._1)))
      .toMap

  private[dcel] def coveredSlots(fan: List[(Int, Int)]): Set[Int] =
    fan.flatMap((start, m) => (0 until gSlots(m)).map(kk => (start + kk) % 12)).toSet

  /** Planar (Λ-free) consistency: at every developed vertex, the incident faces occupy a conflict-free set of
    * 30° slots — the analogue of the fixed-Λ `isConsistent`, but keyed by EXACT ZetaPoint identity (no Λ
    * residue), since here the patch is a genuine non-overlapping planar chunk of the universal cover.
    */
  /** Consistency keyed by a vertex-identity `vid`: at every identified vertex the incident faces occupy a
    * conflict-free 30°-slot set. `vid = identity` is the Λ-free planar check ([[isPlanarConsistent]]); a
    * mod-⟨h⟩ fold is the cylinder check. Generalises the original (which keyed coverage by the exact
    * ZetaPoint) — identical for the identity case, one extra trivial `vid(p)` call per corner.
    */
  private[dcel] def isConsistentBy[K](faces: List[FaceZ], vid: ZetaPoint => K): Boolean =
    val coverage = mutable.Map.empty[K, mutable.Map[Int, (Int, Int)]]
    faces.forall: f =>
      f.corners.forall: p =>
        val slotMap = coverage.getOrElseUpdate(vid(p), mutable.Map.empty)
        val start   = outSlot(f, p)
        (0 until gSlots(f.size)).forall: kk =>
          val slot = (start + kk) % 12
          slotMap.get(slot) match
            case Some(owner) if owner != ((start, f.size)) => false
            case _                                         => slotMap(slot) = (start, f.size); true

  private def isPlanarConsistent(faces: List[FaceZ]): Boolean = isConsistentBy(faces, identity[ZetaPoint])

  /** Sound iff every developed vertex's fan is a valid completed vertex or an extendable partial fan, AND the
    * patch's *completed* vertices show at most `n` distinct types — `KrotenheerdtTorusSearch.isSound`
    * verbatim (it is already Λ-free). The main prune that keeps the planar growth on real n-uniform tilings.
    */
  /** Soundness keyed by a vertex-identity `vid`: every identified vertex is a complete valid vertex or an
    * extendable partial fan, and completed vertices show ≤ `n` distinct types. `vid = identity` is the planar
    * check ([[isSound]]); a mod-⟨h⟩ fold is the cylinder check. Routes through [[vertexFansBy]] (one groupBy)
    * instead of a `planarFan` filter per vertex — same fans, O(faces) instead of O(faces²).
    */
  private[dcel] def isSoundBy[K](faces: List[FaceZ], n: Int, vid: ZetaPoint => K): Boolean =
    val completeTypes = mutable.Set.empty[VertexSignature]
    vertexFansBy(faces, vid).forall: (_, repFan) =>
      val fan     = repFan._2
      val covered = coveredSlots(fan)
      if covered.sizeIs == 12 then
        isCompleteVertex(fan.map(_._2)) && {
          completeTypes += VertexTypes.normalize(fan.map(_._2)); completeTypes.sizeIs <= n
        }
      else isExtendableFan(fan.map(_._2))

  private def isSound(faces: List[FaceZ], n: Int): Boolean = isSoundBy(faces, n, identity[ZetaPoint])

  /** Every way to fill a vertex's remaining `gap` (30° slots) so `fan ++ completion`, read CCW, is a valid
    * complete vertex type — `KrotenheerdtTorusSearch.completions` verbatim.
    */
  private[dcel] def completions(fan: List[Int], gap: Int): List[List[Int]] =
    if gap == 0 then if isCompleteVertex(fan) then List(Nil) else Nil
    else
      sides.flatMap: m =>
        val g = gSlots(m)
        if g > gap then Nil
        else
          val extended = fan :+ m
          val ok       = if g == gap then isCompleteVertex(extended) else isExtendableFan(extended)
          if !ok then Nil else completions(extended, gap - g).map(m :: _)

  /** Constraint-propagation growth keyed by a vertex-identity `vid`, placed faces normalised by `canon`, and
    * the incomplete vertex chosen by `selKey` (smallest first; a 3-key tuple, then exact coords for
    * determinism). Generalises [[growByCompletionPlanar]]:
    *   - `vid = identity`, `canon = identity`, `selKey = (completions, distance-to-centroid, _)` ⇒ the
    *     compact-DISK MRV planar grower (byte-identical: same vertex, same children, same order);
    *   - `vid = fold mod ⟨h⟩`, `canon = fold into the strip`, `selKey = (y, x, _)` (strict lowest-leftmost) ⇒
    *     the cylinder automaton's TAUT scanline front, whose skyline stays bounded ⇒ a finite state space
    *     (the MRV order would leave a ragged, unbounded boundary). See [[CylinderAutomaton]].
    * Incomplete-vertex detection routes through [[vertexFansBy]] (one groupBy, O(faces)) rather than a
    * `planarFan` filter per vertex (O(faces²)). The original disk-growth rationale: with no Λ to bound the
    * patch, the period is discovered by gluing the boundary in two independent directions
    * ([[boundaryGlueBases]]); a compact disk surfaces single-face cells (`4⁴`/`6.6.6`) a 1-D strip never
    * would.
    */
  private[dcel] def growByCompletionBy[K](
      faces: List[FaceZ],
      n: Int,
      vid: ZetaPoint => K,
      canon: FaceZ => FaceZ,
      selKey: (ZetaPoint, Int, BigPoint) => (BigDecimal, BigDecimal, BigDecimal),
      admit: (ZetaPoint, List[(Int, Int)]) => Boolean = (_, _) => true,
      dedup: List[FaceZ] => List[FaceZ] = identity
  ): List[List[FaceZ]] =
    val centroid   = faces.flatMap(_.corners).distinct.map(_.toBigPoint).centroid
    val incomplete = vertexFansBy(faces, vid).toList.flatMap: (_, repFan) =>
      val (p, fan) = repFan
      val covered  = coveredSlots(fan)
      // `admit` restricts which incomplete vertices may be grown (default: all). The cylinder admits only
      // UP-FACING frontier vertices, so growth advances in one direction and the bottom cut is left fixed —
      // keeping the front a bounded skyline (a finite profile state space).
      Option.when(covered.sizeIs < 12 && admit(p, fan)):
        val free     = 12 - covered.size
        val b        = (0 until 12).find(s => covered((s + 11) % 12) && !covered(s)).getOrElse(0)
        val arcStart = (b + free) % 12
        val ordered  = fan.sortBy((start, _) => (start - arcStart + 12) % 12).map(_._2)
        (p, b, completions(ordered, free))
    if incomplete.isEmpty then Nil
    else
      val (p, b, comps) = incomplete.minBy: (p, _, cs) =>
        val (k1, k2, k3) = selKey(p, cs.size, centroid)
        (k1, k2, k3, p.a0, p.a1, p.a2, p.a3)
      comps.flatMap: comp =>
        var slot     = b
        val newFaces = comp.map: m =>
          val f = canon(FaceZ(m, polygon(p, slot, m)))
          slot += gSlots(m)
          f
        // dedup defaults to identity (no-op, zero cost on the planar disk where faces are never duplicated);
        // the cylinder passes `_.distinct` so a face folded onto an existing h-translate collapses to one.
        val next     = dedup(newFaces ++ faces)
        Option.when(isConsistentBy(next, vid) && isSoundBy(next, n, vid))(next)

  private def growByCompletionPlanar(faces: List[FaceZ], n: Int): List[List[FaceZ]] =
    growByCompletionBy(
      faces,
      n,
      identity[ZetaPoint],
      identity[FaceZ],
      (p, cs, c) => (BigDecimal(cs), p.toBigPoint.distanceTo(c), BigDecimal(0))
    )

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
    // SHORT-CIRCUIT: candidates are covolume-ascending and every verifying candidate reduces (primitiveBasis)
    // to the SAME unique period Λ — so the FIRST that verifies is the primitive cell and has minimal pcov.
    // Stop there instead of running the (now-cheaper but still non-trivial) verifyCell on all ~20 candidates.
    val hit = boundaryGlueBases(faces).iterator.flatMap { case (g1, g2) =>
      verifyCell(faces, g1, g2, maxN)
    }.nextOption()
    hit.foreach: (n, types, key, pcov) =>
      // Closed into a genuine torus cell ⇒ caller stops growing this branch (further growth only replicates
      // it); record only cells within the covolume bound (the search's analogue of the fixed-Λ covol cap).
      if pcov <= BigDecimal(maxCovolume) + BigDecimal("1e-6") then results.getOrElseUpdate(key, (n, types))
    hit.isDefined

  /** PURE closure: the min-covolume torus cell the patch closes into (or `None`), with the cheap
    * per-candidate gate (`distinctArea ≥ covolume`, skipping the expensive `verifyCell`/`primitiveBasis` on
    * candidates the patch cannot fill — the measured 99.8% hotspot). SOUND: `primitiveBasis` only shrinks a
    * candidate when the patch already contains a full cell (closeable), and then the primitive period is
    * itself a (shortest) boundary-glue candidate that fills its raw cell and passes the gate. The returned
    * key is the SHARED D-symbol key ([[torusMapClassify]]), with `verifyCell` kept as the soundness gate. No
    * shared state ⇒ safe to call from any thread (the basis of the parallel driver).
    */
  private def closeCell(
      faces: List[FaceZ],
      maxN: Int
  ): Option[(Int, Set[VertexSignature], String, BigDecimal)] =
    val originB = BigPoint.origin
    // SHORT-CIRCUIT at the first verifying candidate (covolume-ascending ⇒ primitive ⇒ minimal pcov). The cheap
    // raw-area gate still skips candidates the patch cannot fill before the (now overlap-first) verifyCell.
    boundaryGlueBases(faces).iterator.flatMap { case (g1, g2) =>
      val vB   = g1.toBigPoint
      val wB   = g2.toBigPoint
      val cov0 = cross(vB, wB).abs
      if cov0 > BigDecimal("1e-9") && distinctArea(faces, vB, wB) >= cov0 - BigDecimal("1e-6") then
        // verifyCell is the SOUNDNESS gate (its tilesWithoutOverlap rejects false-period non-tilings like
        // 3.3.6.6 / 3.4.4.6 that the purely-combinatorial classifyClosedMap would accept). Once it confirms a
        // genuine tiling, key it in the SHARED D-symbol space via the barycentric op (torusMapClassify), so the
        // grower dedups with the oracle and the bounded-V assembler (ADR-0032).
        verifyCell(faces, g1, g2, maxN).flatMap: (_, _, _, pcov) =>
          val distinct = distinctFacesOf(faces, vB, wB)
          val (pv, pw) = KrotenheerdtLatticeSearch.primitiveBasis(vB, wB, originB, distinct, Nil)
          torusMapClassify(faces, pv, pw, originB).map((n2, sigs2, dkey) => (n2, sigs2.toSet, dkey, pcov))
      else None
    }.nextOption()

  /** Like [[closeCell]] but also returns the closed cell's [[rotationCenters]] (computed once, on the chosen
    * minimal-covolume basis). The basis for the cheap parallel rotation-symmetry reference driver
    * ([[symmetryRotationReferenceParallel]]) — same soundness gate (`verifyCell`) + same D-symbol key
    * (`torusMapClassify`), so its results dedup in the shared space.
    */
  private def closeCellWithCentres(
      faces: List[FaceZ],
      maxN: Int
  ): Option[(Int, Set[VertexSignature], String, Set[(String, Int)], BigDecimal)] =
    val originB = BigPoint.origin
    // SHORT-CIRCUIT at the first verifying candidate (covolume-ascending ⇒ primitive), then read its centres.
    boundaryGlueBases(faces).iterator.flatMap { case (g1, g2) =>
      val vB   = g1.toBigPoint
      val wB   = g2.toBigPoint
      val cov0 = cross(vB, wB).abs
      if cov0 > BigDecimal("1e-9") && distinctArea(faces, vB, wB) >= cov0 - BigDecimal("1e-6") then
        verifyCell(faces, g1, g2, maxN).flatMap: (_, _, _, pcov) =>
          val distinct = distinctFacesOf(faces, vB, wB)
          val (pv, pw) = KrotenheerdtLatticeSearch.primitiveBasis(vB, wB, originB, distinct, Nil)
          torusMapClassify(faces, pv, pw, originB).map: (n2, sigs2, dkey) =>
            (n2, sigs2.toSet, dkey, rotationCenters(faces, pv, pw), pcov)
      else None
    }.nextOption()

  private def tryCloseFast(
      faces: List[FaceZ],
      maxN: Int,
      maxCovolume: Double,
      results: mutable.Map[String, (Int, Set[VertexSignature])]
  ): Boolean =
    closeCell(faces, maxN) match
      case Some((n, types, key, pcov)) =>
        if pcov <= BigDecimal(maxCovolume) + BigDecimal("1e-6") then results.getOrElseUpdate(key, (n, types))
        true
      case None                        => false

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

  // ======================================================================================================
  // SYMMETRY-FIRST GROWER (Phase 2 / ADR-0023 architecture B, the never-built one). The free planar grower
  // [[enumerate]] scatters (ADR-0028: aperiodic 1-D stackings grow to the face cap before Λ is found). The fix
  // is to COMMIT to a rotation symmetry up front: seed a configuration with a C_m centre at the origin and, at
  // every growth step, place the FULL C_m orbit of each completion. The patch is then symmetric (hence
  // periodic-by-construction) and grows as a compact disk whose deck lattice is discovered fast — no scatter.
  // The branch factor is cut by ~m (only fundamental-sector choices branch). It is euclidean-by-construction
  // (regular tiles only ⇒ none of the oriented-slice's 98% non-euclidean D-set waste). Bounded by the
  // FUNDAMENTAL DOMAIN, not the covolume nor the D-set tree.
  //
  // De-risk spike (m=6): the only exact-arithmetic central config that needs no half-integer centre is a
  // HEXAGON centred at the origin — its 6 corners are ζ^0,ζ²,…,ζ^10 (unit vectors, integral ℤ[ζ₁₂]) and the
  // 60° rotation about the origin is exactly `timesZeta²`. It seeds 6³, 3.6.3.6, 3.4.6.4 and the both-walled
  // 4.6.12. GATE: reproduce the m=6 n≤3 tilings exactly (key-for-key vs the oracle) with states bounded by
  // domain size, and reach 4.6.12 cheaply. (Other m / face-centred configs follow only if the gate is GO.)
  // ======================================================================================================

  /** Rotation by `360/m` degrees about the ORIGIN (the C_m centre), exact: `m` divides 12 so the turn is a
    * whole number `12/m` of 30°-slots, i.e. `timesZeta` applied `12/m` times.
    */
  private def rotateBy(m: Int): ZetaPoint => ZetaPoint =
    val k = 12 / m
    (z: ZetaPoint) =>
      var p = z
      var i = 0
      while i < k do { p = p.timesZeta; i += 1 }
      p

  private def rotateFace(rot: ZetaPoint => ZetaPoint, f: FaceZ): FaceZ =
    FaceZ(f.size, f.corners.map(rot))

  /** Drop geometric duplicates (same size + same corner SET — a face placed from two orbit elements that
    * share it has two corner orderings but one geometry); keeps boundary-edge bookkeeping correct.
    */
  private def dedupFaces(faces: List[FaceZ]): List[FaceZ] =
    val seen = mutable.HashSet.empty[(Int, Set[ZetaPoint])]
    faces.filter(f => seen.add((f.size, f.corners.toSet)))

  /** The hexagon centred at the origin — corners ζ^0,ζ²,…,ζ^10 (each a unit ℤ[ζ₁₂] vector; adjacent corners
    * differ by a unit step, so all the slot/boundary machinery applies). The m∈{2,3,6} central seed.
    */
  private def centeredHexagon: FaceZ =
    FaceZ(6, Vector(0, 2, 4, 6, 8, 10).map(ZetaPoint.unit))

  /** A symmetric central SEED: the placed faces, the exact order-`m` rotation about its centre (an integer
    * ℤ[ζ₁₂] affine map — the centre's irrational coordinates are never needed), and a human label. The grower
    * commits to this rotation and keeps the patch invariant under it.
    */
  final case class Seed(faces: List[FaceZ], rot: ZetaPoint => ZetaPoint, m: Int, label: String)

  /** POLYGON-CENTRE seed: a `p`-gon placed corner-first at the origin, with the exact order-`m` rotation
    * about its centre. The rotation by `360/m` maps corner `0` to corner `p/m` (CCW), so as an affine map
    * `r(z) = ζ^(12/m)·z + t` with `t = corner_{p/m} − ζ^(12/m)·corner_0 = corner_{p/m}` (corner_0 = origin).
    * Integral throughout, though the geometric centre is not. Requires `m | p` and `m ≤ 6`.
    */
  def polygonCenterSeed(p: Int, m: Int): Seed =
    require(p % m == 0 && m <= 6, s"polygonCenterSeed: need m|p and m<=6, got p=$p m=$m")
    val corners                      = polygon(ZetaPoint.origin, 0, p)
    val k                            = 12 / m
    val t                            = corners(p / m)
    def rot(z: ZetaPoint): ZetaPoint =
      var r = z; var i = 0; while i < k do { r = r.timesZeta; i += 1 }; r + t
    Seed(List(FaceZ(p, corners)), rot, m, s"poly$p/m$m")

  /** VERTEX-CENTRE seed: the corona of vertex type `typeSizes` around the origin VERTEX, with the pure
    * `ζ^(12/m)` rotation about the origin (`t = 0`). Valid only when the corona is genuinely C_m-symmetric
    * (the cyclic type's rotational order is a multiple of `m`) — caller/test must check.
    */
  def vertexCenterSeed(typeSizes: List[Int], m: Int): Seed =
    Seed(coronaFaces(typeSizes), rotateBy(m), m, s"vtx${typeSizes.mkString(".")}/m$m")

  /** EDGE-MIDPOINT seed (m = 2): two congruent `p`-gons sharing the edge `origin → ζ^0`, swapped by the 180°
    * rotation about the edge midpoint, `r(z) = ζ^0 − z` (= `ζ^6·z + ζ^0`, exact).
    */
  def edgeMidSeed(p: Int): Seed =
    val a                            = FaceZ(p, polygon(ZetaPoint.origin, 0, p))
    val t                            = ZetaPoint.unit(0)
    def rot(z: ZetaPoint): ZetaPoint = t - z
    Seed(List(a, FaceZ(p, a.corners.map(rot))), rot, 2, s"edge$p")

  /** The full enumerable seed catalogue over m ∈ {2,3,4,6}: every polygon-centre (`m | p`), every
    * vertex-centre whose corona is C_m-symmetric, and the edge-midpoint dominoes (m = 2). The same tiling is
    * reached from several of these (it has several inequivalent rotation centres); the cross-seed dedup is
    * the canonical key (see ADR-0032).
    */
  def allSeeds: List[Seed] =
    val polys = for p <- sides; m <- List(6, 4, 3, 2) if p % m == 0 yield polygonCenterSeed(p, m)
    val edges = sides.map(edgeMidSeed)
    // vertex-centre seeds: each valid {3,4,6,12} vertex type, at each rotation order its corona admits
    val verts =
      for
        sig <- seedTypes
        m   <- List(6, 4, 3, 2)
        if isCoronaSymmetric(sig, m)
      yield vertexCenterSeed(sig, m)
    (polys ++ verts ++ edges).distinctBy(_.label)

  /** True iff the vertex corona `sig` is C_m-symmetric — invariant under rotation by `360/m` (= `12/m` slots)
    * about the vertex. Polygons subtend UNEQUAL angles, so this is an ANGULAR (slot) check, not a
    * rotate-the-sequence-by-k-positions check: each polygon sits at a cumulative start slot, and the corona
    * is C_m-symmetric iff the `(startSlot, size)` set maps onto itself under `+12/m`.
    */
  private def isCoronaSymmetric(sig: List[Int], m: Int): Boolean =
    val shift   = 12 / m
    val starts  = sig.scanLeft(0)((acc, p) => acc + gSlots(p)).init // cumulative start slot of each polygon
    val polySet = starts.zip(sig).map((s, size) => (s % 12, size)).toSet
    polySet.forall((s, size) => polySet.contains(((s + shift) % 12, size)))

  /** True iff the partial fan `arc` (polygon sizes in arc order around a vertex) can still be completed
    * within `targets`: it occurs as a contiguous cyclic arc of some target vertex figure, in either
    * orientation. SOUND prune for the constrained grower — a valid `targets`-tiling's every partial vertex
    * fan IS a contiguous arc of one of its (∈ targets) figures, so this never removes a path to such a
    * tiling. Empty targets ⇒ unconstrained (always true).
    */
  private def isArcOfSomeTarget(arc: List[Int], targets: Set[VertexSignature]): Boolean =
    targets.isEmpty || arc.isEmpty || targets.exists: t =>
      arc.lengthIs <= t.length && {
        val n = arc.length
        (t ++ t).sliding(n).exists(_ == arc) || (t.reverse ++ t.reverse).sliding(n).exists(_ == arc)
      }

  /** One symmetric growth step: place a SINGLE next polygon at the most-constrained (MRV) incomplete vertex's
    * open arc, plus its full C_m orbit (the same face rotated by `360/m` about the centre, `m` copies), so
    * the patch stays C_m-symmetric. Single-tile (not whole-vertex) placement is the fix for the
    * wedge-boundary clash: a face shared between adjacent fundamental sectors is then placed ONCE and
    * deduped, instead of two whole-vertex completions disagreeing on it. Branches over the polygon size; kept
    * iff the extended fan is a valid (complete or extendable) vertex AND the orbit-augmented patch is
    * planar-consistent and sound.
    */
  private def growBySymmetry(
      faces: List[FaceZ],
      n: Int,
      rot: ZetaPoint => ZetaPoint,
      m: Int,
      targetTypes: Set[VertexSignature] = Set.empty
  ): List[List[FaceZ]] =
    val centroid   = faces.flatMap(_.corners).distinct.map(_.toBigPoint).centroid
    val incomplete = faces.flatMap(_.corners).distinct.flatMap: p =>
      val fan     = planarFan(faces, p)
      val covered = coveredSlots(fan)
      Option.when(covered.sizeIs < 12):
        val free     = 12 - covered.size
        val b        = (0 until 12).find(s => covered((s + 11) % 12) && !covered(s)).getOrElse(0)
        val arcStart = (b + free) % 12
        val ordered  = fan.sortBy((start, _) => (start - arcStart + 12) % 12).map(_._2)
        (p, b, free, ordered)
    if incomplete.isEmpty then Nil
    else
      val (p, b, free, ordered) =
        incomplete.minBy((p, _, free, _) => (free, p.toBigPoint.distanceTo(centroid), p.a0, p.a1, p.a2, p.a3))
      sides.flatMap: mm =>
        val g = gSlots(mm)
        if g > free then Nil
        else
          val extended = ordered :+ mm
          // PARTIAL-FAN ARC PRUNE (ADR-0033): besides the generic extendable/complete check, when constrained
          // require the fan being built at the MRV vertex to stay a contiguous ARC of some TARGET vertex figure
          // (or, on completion, BE a target type). Cuts the per-step branching over {3,4,6,12} from ~4 to ~1-2
          // — exponential tree shrink at depth — at the SOURCE (before building the orbit). Sound: a valid
          // T-tiling's every partial vertex fan is an arc of one of its (∈T) figures, so no valid path is lost.
          val ok       =
            if g == free then
              isCompleteVertex(extended) &&
              (targetTypes.isEmpty || targetTypes.contains(VertexTypes.normalize(extended)))
            else isExtendableFan(extended) && isArcOfSomeTarget(extended, targetTypes)
          if !ok then Nil
          else
            // the single new face at the open arc start, plus its C_m orbit (rotate 360/m, m copies)
            val orbit = mutable.ListBuffer.empty[FaceZ]
            var acc   = List(FaceZ(mm, polygon(p, b, mm)))
            var i     = 0
            while i < m do { orbit ++= acc; acc = acc.map(f => rotateFace(rot, f)); i += 1 }
            val next  = dedupFaces(orbit.toList ++ faces)
            // TYPE-SET CONSTRAINT (when targetTypes non-empty): drop a child the moment it COMPLETES a vertex
            // whose type is outside the target set. Sound — a valid tiling of that type-set never completes an
            // off-target vertex — and it collapses the otherwise-exponential growth tree to the paths that can
            // actually build a tiling of `targetTypes` (the bounded-V-style pruning, kept with the cell/m win).
            Option.when(
              isPlanarConsistent(next) && isSound(next, n) &&
                (targetTypes.isEmpty || completeVertexTypes(next).subsetOf(targetTypes))
            )(next)

  /** Diagnostic: the distinct COMPLETE vertex types (360°-covered) present in a patch — to see, during a
    * symmetric grow, which vertex configurations are actually forming.
    */
  def completeVertexTypes(faces: List[FaceZ]): Set[VertexSignature] =
    faces
      .flatMap(_.corners)
      .distinct
      .iterator
      .map(p => planarFan(faces, p))
      .filter(fan => coveredSlots(fan).sizeIs == 12)
      .map(fan => VertexTypes.normalize(fan.map(_._2)))
      .toSet

  /** Outcome of [[enumerateBySymmetry]]: the distinct tilings found, plus the search cost (`states` = patches
    * popped) and whether the face budget was hit on any branch (⇒ coverage may be partial).
    */
  final case class SymResult(
      tilings: List[(Int, Set[VertexSignature], String)],
      states: Long,
      budgetHit: Boolean
  )

  /** Grow ONE seed into its tilings, writing into the SHARED `results` (cross-seed dedup by geometric content
    * key) and `visited` (cross-seed dedup of isometric partial patches — `canonicalKey` is frame-invariant).
    * Returns `(statesPopped, budgetHit)`. Closure is GATED on the seed's own corners all being 360°-complete:
    * else a lone hexagon glues into the 6.6.6 cell at face-count 1 and the branch stops, but the centre is
    * the order-m centre of MANY tilings, so deferring closure forces the first ring (neighbour choice) to
    * branch.
    */
  private def enumerateFromSeed(
      seed: Seed,
      maxN: Int,
      maxFaces: Int,
      maxCovolume: Double,
      results: mutable.Map[String, (Int, Set[VertexSignature])],
      visited: mutable.HashSet[Vector[Long]],
      onState: (List[FaceZ], Boolean) => Unit
  ): (Long, Boolean) =
    val stack                                        = mutable.Stack.empty[List[FaceZ]]
    var states                                       = 0L
    var budgetHit                                    = false
    val seedCorners                                  = seed.faces.flatMap(_.corners).toSet
    def coronaCommitted(faces: List[FaceZ]): Boolean =
      seedCorners.forall(p => coveredSlots(planarFan(faces, p)).sizeIs == 12)
    if isPlanarConsistent(seed.faces) && isSound(seed.faces, maxN) && visited.add(canonicalKey(seed.faces))
    then
      stack.push(seed.faces)
    while stack.nonEmpty do
      val faces  = stack.pop()
      states += 1
      val closed = coronaCommitted(faces) && tryCloseFast(faces, maxN, maxCovolume, results)
      onState(faces, closed)
      if !closed then
        if faces.sizeIs >= maxFaces then budgetHit = true
        else
          growBySymmetry(faces, maxN, seed.rot, seed.m).foreach: child =>
            if visited.add(canonicalKey(child)) then stack.push(child)
    (states, budgetHit)

  /** Symmetry-first enumeration from the central HEXAGON (the m∈{2,3,6} polygon-centre spike seed). Kept as a
    * focused entry point (and the de-risk spike's). The de-risk GO is recorded in ADR-0032.
    */
  def enumerateBySymmetry(
      m: Int,
      maxN: Int,
      maxFaces: Int,
      maxCovolume: Double = Double.MaxValue,
      onState: (List[FaceZ], Boolean) => Unit = (_, _) => ()
  ): SymResult =
    val results             = mutable.Map.empty[String, (Int, Set[VertexSignature])]
    val visited             = mutable.HashSet.empty[Vector[Long]]
    val seed                = Seed(List(centeredHexagon), rotateBy(m), m, s"hex/m$m")
    val (states, budgetHit) = enumerateFromSeed(seed, maxN, maxFaces, maxCovolume, results, visited, onState)
    SymResult(
      results.toList.map((key, nt) => (nt._1, nt._2, key)).sortBy((n, _, key) => (n, key)),
      states,
      budgetHit
    )

  /** The FULL symmetry-first enumeration: run every seed in [[allSeeds]] through [[enumerateFromSeed]],
    * sharing ONE `results` (so the same tiling reached from several rotation-centre seeds dedups by canonical
    * content key — the algorithmic cross-seed dedup of ADR-0032's hypothesis) and ONE `visited`. Per-seed
    * cost is reported via `onSeed` for coverage/cost inspection.
    */
  def enumerateAllSeeds(
      maxN: Int,
      maxFaces: Int,
      maxCovolume: Double = Double.MaxValue,
      onSeed: (Seed, Long, Boolean) => Unit = (_, _, _) => (),
      log: String => Unit = _ => (),
      logEveryMs: Long = 10000L
  ): SymResult =
    val results      = mutable.Map.empty[String, (Int, Set[VertexSignature])]
    var totalStates  = 0L
    var anyBudgetHit = false
    // LIVE telemetry (so long runs aren't blind waits): a daemon prints elapsed / current seed / states+rate /
    // current+max patch face-count every `logEveryMs`. All shared counters are atomic; the search is
    // single-threaded so only the daemon reads concurrently.
    val seedList     = allSeeds
    val statesA      = new AtomicLong(0)
    val curFacesA    = new AtomicLong(0)
    val maxFacesA    = new AtomicLong(0)
    val tilingsA     = new AtomicLong(0)
    val curSeed      = new AtomicReference("")
    val seedIdx      = new AtomicLong(0)
    val running      = new AtomicBoolean(true)
    val t0           = System.nanoTime()
    val logger       = new Thread(() =>
      while running.get do
        try Thread.sleep(logEveryMs)
        catch case _: InterruptedException => ()
        if running.get then
          val secs = math.max(1e-3, (System.nanoTime() - t0) / 1e9)
          val st   = statesA.get
          log(
            f"  [${secs}%5.0fs] seed ${seedIdx.get}%2d/${seedList.size} ${curSeed.get}%-22s" +
              f" states=$st%-7d (${(st / secs).toLong}%d/s) faces=${curFacesA.get}/max${maxFacesA.get} tilings=${tilingsA.get}"
          )
    )
    logger.setDaemon(true)
    logger.start()
    try
      var i = 0
      for seed <- seedList do
        i += 1
        curSeed.set(seed.label)
        seedIdx.set(i)
        // PER-SEED visited (growBySymmetry depends on the seed's (rot, m), so partial-patch dedup must NOT be
        // shared across seeds — else a seed's path to its own tiling is pruned; see enumerateAllSeedsParallel).
        val (states, hit) = enumerateFromSeed(
          seed,
          maxN,
          maxFaces,
          maxCovolume,
          results,
          mutable.HashSet.empty[Vector[Long]],
          (faces, _) =>
            statesA.incrementAndGet()
            val fc = faces.size
            curFacesA.set(fc)
            if fc > maxFacesA.get then maxFacesA.set(fc)
        )
        tilingsA.set(results.size)
        totalStates += states
        anyBudgetHit ||= hit
        onSeed(seed, states, hit)
    finally
      running.set(false)
      logger.interrupt()
    SymResult(
      results.toList.map((key, nt) => (nt._1, nt._2, key)).sortBy((n, _, key) => (n, key)),
      totalStates,
      anyBudgetHit
    )

  /** PARALLEL full enumeration with WORK-STEALING over patches: a `ForkJoinPool` runs one task per partial
    * patch and submits each child as a new task, so the pool steals work BOTH across seeds and within a
    * single heavy seed's subtree — no single-seed tail (the load imbalance a seed-per-task split had). Shares
    * a thread-safe `results` (content-/D-symbol-key dedup) and `visited` (atomic `add` ⇒ each patch explored
    * once). The per-patch work (`closeCell`/`growBySymmetry`/`canonicalKey`) is pure ⇒ sound + complete: only
    * WHICH thread explores a patch is nondeterministic; the result SET is identical to [[enumerateAllSeeds]]
    * (validated in SymmetryGrowerSpec). `awaitQuiescence` ends the run. Live daemon telemetry via `log`.
    */
  def enumerateAllSeedsParallel(
      maxN: Int,
      maxFaces: Int,
      maxCovolume: Double = Double.MaxValue,
      parallelism: Int = math.max(1, Runtime.getRuntime.availableProcessors - 1),
      log: String => Unit = _ => (),
      logEveryMs: Long = 10000L,
      maxMillis: Long = Long.MaxValue
  ): SymResult =
    // wall-clock cap (for long unattended runs): past the deadline, tasks stop GROWING (treated as budget hit)
    // but still finish their closeCell, so the frontier drains and awaitQuiescence returns ~promptly. Lets us
    // set maxFaces generously (so n≥3 cells close) while TIME bounds the run — and it can't run away / OOM.
    val deadlineNanos                                                                           = {
      val now = System.nanoTime()
      if maxMillis >= Long.MaxValue / 2000000L then Long.MaxValue else now + maxMillis * 1000000L
    }
    val results                                                                                 = new ConcurrentHashMap[String, (Int, Set[VertexSignature])]()
    // PER-SEED partial-patch dedup (keyed by seed index): growBySymmetry depends on the seed's (rot, m), so a
    // patch grown under one seed's symmetry differs from the same patch under another's — sharing `visited`
    // across seeds would prune a seed's path to its own tiling (e.g. 4.6.12 from the dodecagon seed, pruned by
    // an earlier seed reaching an isometric partial patch first), an order-dependent COMPLETENESS bug. Results
    // (tilings) stay shared — cross-seed dedup by D-symbol key is correct.
    val visited                                                                                 = ConcurrentHashMap.newKeySet[(Int, Vector[Long])]()
    val statesA                                                                                 = new AtomicLong(0)
    val curFacesA                                                                               = new AtomicLong(0)
    val maxFacesA                                                                               = new AtomicLong(0)
    val anyBudgetHit                                                                            = new AtomicBoolean(false)
    val running                                                                                 = new AtomicBoolean(true)
    val t0                                                                                      = System.nanoTime()
    val cov                                                                                     = BigDecimal(maxCovolume) + BigDecimal("1e-6")
    // WORK-STEALING over PATCHES, not seeds: one ForkJoinPool task per partial patch, children submitted as new
    // tasks. The pool steals across seeds AND within the heavy seed's subtree, so there is no single-seed tail
    // (the load-imbalance the seed-per-task version had). The per-patch work is pure (closeCell/growBySymmetry/
    // canonicalKey); shared `results`/`visited` are atomic ⇒ sound + complete, result-set-identical to
    // sequential (validated). awaitQuiescence ends the run when no task is queued or running.
    val pool                                                                                    = new ForkJoinPool(parallelism)
    val logger                                                                                  = new Thread(() =>
      while running.get do
        try Thread.sleep(logEveryMs)
        catch case _: InterruptedException => ()
        if running.get then
          val secs = math.max(1e-3, (System.nanoTime() - t0) / 1e9)
          val st   = statesA.get
          log(
            f"  [${secs}%5.0fs] states=$st%-8d (${(st / secs).toLong}%d/s)  frontier~${pool.getQueuedTaskCount}" +
              f"  faces~${curFacesA.get}/max${maxFacesA.get} tilings=${results.size}"
          )
    )
    logger.setDaemon(true)
    logger.start()
    def submit(seedIdx: Int, seed: Seed, seedCorners: Set[ZetaPoint], faces: List[FaceZ]): Unit =
      pool.execute(() =>
        statesA.incrementAndGet()
        val fc        = faces.size
        curFacesA.set(fc)
        if fc > maxFacesA.get then maxFacesA.set(fc)
        val committed = seedCorners.forall(p => coveredSlots(planarFan(faces, p)).sizeIs == 12)
        val closed    = committed &&
          (closeCell(faces, maxN) match
            case Some((n, types, key, pcov)) =>
              if pcov <= cov then results.putIfAbsent(key, (n, types))
              true
            case None                        => false)
        if !closed then
          if faces.sizeIs >= maxFaces || System.nanoTime() >= deadlineNanos then anyBudgetHit.set(true)
          else
            growBySymmetry(faces, maxN, seed.rot, seed.m).foreach: child =>
              if visited.add((seedIdx, canonicalKey(child))) then submit(seedIdx, seed, seedCorners, child)
      )
    try
      for (seed, seedIdx) <- allSeeds.zipWithIndex do
        val seedCorners = seed.faces.flatMap(_.corners).toSet
        if isPlanarConsistent(seed.faces) && isSound(seed.faces, maxN) &&
          visited.add((seedIdx, canonicalKey(seed.faces)))
        then submit(seedIdx, seed, seedCorners, seed.faces)
      pool.awaitQuiescence(Long.MaxValue, TimeUnit.DAYS)
    finally
      running.set(false)
      logger.interrupt()
      pool.shutdown()
    SymResult(
      results.asScala.toList.map((key, nt) => (nt._1, nt._2, key)).sortBy((n, _, key) => (n, key)),
      statesA.get,
      anyBudgetHit.get
    )

  /** PARALLEL rotation-symmetry reference: the work-stealing twin of [[symmetryRotationReference]] (and the
    * centre-capturing twin of [[enumerateAllSeedsParallel]]). Per closed tiling it records (D-symbol key →
    * (types, [[rotationCenters]])), keyed in the shared D-symbol space. Same ForkJoinPool / per-seed
    * `visited` / deadline machinery as the production driver, so it reaches the large rotational cells
    * (dodecagons) at a generous maxFaces in minutes, not the single-threaded reference's hours. Live
    * telemetry via `log`.
    */
  def symmetryRotationReferenceParallel(
      maxN: Int,
      maxFaces: Int,
      maxCovolume: Double = Double.MaxValue,
      parallelism: Int = math.max(1, Runtime.getRuntime.availableProcessors - 1),
      log: String => Unit = _ => (),
      logEveryMs: Long = 10000L,
      maxMillis: Long = Long.MaxValue,
      targetTypes: Set[VertexSignature] = Set.empty
  ): Map[String, (Set[VertexSignature], Set[(String, Int)])] =
    val deadlineNanos                                                                           =
      val now = System.nanoTime()
      if maxMillis >= Long.MaxValue / 2000000L then Long.MaxValue else now + maxMillis * 1000000L
    val results                                                                                 = new ConcurrentHashMap[String, (Set[VertexSignature], Set[(String, Int)])]()
    val visited                                                                                 = ConcurrentHashMap.newKeySet[(Int, Vector[Long])]()
    val statesA                                                                                 = new AtomicLong(0)
    val curFacesA                                                                               = new AtomicLong(0)
    val maxFacesA                                                                               = new AtomicLong(0)
    val running                                                                                 = new AtomicBoolean(true)
    val t0                                                                                      = System.nanoTime()
    val cov                                                                                     = BigDecimal(maxCovolume) + BigDecimal("1e-6")
    val pool                                                                                    = new ForkJoinPool(parallelism)
    val logger                                                                                  = new Thread(() =>
      while running.get do
        try Thread.sleep(logEveryMs)
        catch case _: InterruptedException => ()
        if running.get then
          val secs = math.max(1e-3, (System.nanoTime() - t0) / 1e9)
          val st   = statesA.get
          log(
            f"  [${secs}%5.0fs] states=$st%-8d (${(st / secs).toLong}%d/s)  frontier~${pool.getQueuedTaskCount}" +
              f"  faces~${curFacesA.get}/max${maxFacesA.get} tilings=${results.size}"
          )
    )
    logger.setDaemon(true)
    logger.start()
    def submit(seedIdx: Int, seed: Seed, seedCorners: Set[ZetaPoint], faces: List[FaceZ]): Unit =
      pool.execute(() =>
        statesA.incrementAndGet()
        val fc        = faces.size
        curFacesA.set(fc)
        if fc > maxFacesA.get then maxFacesA.set(fc)
        val committed = seedCorners.forall(p => coveredSlots(planarFan(faces, p)).sizeIs == 12)
        val closed    = committed &&
          (closeCellWithCentres(faces, maxN) match
            case Some((_, types, key, centres, pcov)) =>
              // record only ON-TARGET closures when constrained: a patch can close into an OFF-target tiling
              // whose extra vertex type completes only via Λ-wraparound (planar-invisible to the growth filter),
              // so this closure-level type-set check is what makes the constrained result leak-free. Still
              // `closed=true` (stop growing — a complete cell only replicates) regardless of on/off target.
              if pcov <= cov && (targetTypes.isEmpty || types.subsetOf(targetTypes)) then
                results.putIfAbsent(key, (types, centres))
              true
            case None                                 => false)
        if !closed && faces.sizeIs < maxFaces && System.nanoTime() < deadlineNanos then
          growBySymmetry(faces, maxN, seed.rot, seed.m, targetTypes).foreach: child =>
            if visited.add((seedIdx, canonicalKey(child))) then submit(seedIdx, seed, seedCorners, child)
      )
    try
      // when constrained, skip seeds whose own germ already completes an off-target vertex (cheap pre-filter)
      val seeds0 =
        if targetTypes.isEmpty then allSeeds
        else allSeeds.filter(s => completeVertexTypes(s.faces).subsetOf(targetTypes))
      for (seed, seedIdx) <- seeds0.zipWithIndex do
        val seedCorners = seed.faces.flatMap(_.corners).toSet
        if isPlanarConsistent(seed.faces) && isSound(seed.faces, maxN) &&
          visited.add((seedIdx, canonicalKey(seed.faces)))
        then submit(seedIdx, seed, seedCorners, seed.faces)
      pool.awaitQuiescence(Long.MaxValue, TimeUnit.DAYS)
    finally
      running.set(false)
      logger.interrupt()
      pool.shutdown()
    results.asScala.toMap

  /** The closure-proximity score of a patch (lower = explore FIRST), for [[symmetryClosureDirectedParallel]].
    * A patch that is about to CLOSE into a torus cell is a compact disk whose boundary is short relative to
    * its area (the boundary edges all glue away into the period identification); a sprawling or thin patch
    * has a long boundary per face. So the boundary-to-area ratio is small for near-closing patches and large
    * for far-from-closing ones. Crucially it REWARDS GROWTH — a naive "few incomplete vertices / short
    * boundary" score is non-monotone (you must grow, temporarily adding boundary, before gluing closes), so
    * it stalls at the seed; the RATIO falls as a patch grows compactly (perimeter ~ √area), driving the
    * search toward a closed cell instead of fanning out.
    */
  private def closureScore(faces: List[FaceZ]): Double =
    boundaryHalfEdges(faces).size.toDouble / (faces.size + 1.0)

  /** CLOSURE-DIRECTED (best-first) twin of [[symmetryRotationReferenceParallel]] (ADR-0034 §4). IDENTICAL
    * search space, soundness gate ([[closeCellWithCentres]]), per-seed `visited` dedup and D-symbol keys —
    * only the EXPLORATION ORDER differs: a global priority frontier ([[closureScore]]) expands the patch
    * nearest to closing first, instead of DFS. Still EXHAUSTIVE under quiescence (a reordering ⇒ identical
    * results to the DFS driver — tested); under a `maxStates` / `maxMillis` / `maxFaces` cut it is a sound
    * LOWER BOUND that should reach the DEEP large-domain C₂ residual cells (n ≥ 3) with far fewer states than
    * the DFS order, which burns the budget on shallow non-closing patches. `maxStates` caps total patches
    * popped (for states-to-reach measurement vs the DFS driver). Workers pull from a `PriorityBlockingQueue`;
    * the search ends when `pending` (queued-or-processing) hits 0 (quiescence) or a budget bound trips.
    */
  def symmetryClosureDirectedParallel(
      maxN: Int,
      maxFaces: Int,
      maxCovolume: Double = Double.MaxValue,
      parallelism: Int = math.max(1, Runtime.getRuntime.availableProcessors - 1),
      log: String => Unit = _ => (),
      logEveryMs: Long = 10000L,
      maxMillis: Long = Long.MaxValue,
      maxStates: Long = Long.MaxValue,
      targetTypes: Set[VertexSignature] = Set.empty
  ): Map[String, (Set[VertexSignature], Set[(String, Int)])] =
    val deadlineNanos =
      val now = System.nanoTime()
      if maxMillis >= Long.MaxValue / 2000000L then Long.MaxValue else now + maxMillis * 1000000L
    val results       = new ConcurrentHashMap[String, (Set[VertexSignature], Set[(String, Int)])]()
    val visited       = ConcurrentHashMap.newKeySet[(Int, Vector[Long])]()
    val statesA       = new AtomicLong(0)
    val pending       = new AtomicLong(0) // items queued-or-processing; search ends when this hits 0
    val maxFacesA     = new AtomicLong(0)
    val running       = new AtomicBoolean(true)
    val t0            = System.nanoTime()
    val cov           = BigDecimal(maxCovolume) + BigDecimal("1e-6")

    final case class Item(
        score: Double,
        seedIdx: Int,
        seed: Seed,
        corners: Set[ZetaPoint],
        faces: List[FaceZ]
    )
    val pq                                                                                = new PriorityBlockingQueue[Item](256, java.util.Comparator.comparingDouble[Item](_.score))
    def push(seedIdx: Int, seed: Seed, corners: Set[ZetaPoint], faces: List[FaceZ]): Unit =
      pending.incrementAndGet()
      pq.put(Item(closureScore(faces), seedIdx, seed, corners, faces))

    val logger = new Thread(() =>
      while running.get do
        try Thread.sleep(logEveryMs)
        catch case _: InterruptedException => ()
        if running.get then
          val secs = math.max(1e-3, (System.nanoTime() - t0) / 1e9)
          val st   = statesA.get
          log(
            f"  [${secs}%5.0fs] states=$st%-8d (${(st / secs).toLong}%d/s)  frontier~${pq.size}" +
              f"  maxfaces=${maxFacesA.get} tilings=${results.size}"
          )
    )
    logger.setDaemon(true)

    def processOne(it: Item): Unit =
      statesA.incrementAndGet()
      val fc        = it.faces.size
      if fc > maxFacesA.get then maxFacesA.set(fc)
      val committed = it.corners.forall(p => coveredSlots(planarFan(it.faces, p)).sizeIs == 12)
      val closed    = committed &&
        (closeCellWithCentres(it.faces, maxN) match
          case Some((_, types, key, centres, pcov)) =>
            if pcov <= cov && (targetTypes.isEmpty || types.subsetOf(targetTypes)) then
              results.putIfAbsent(key, (types, centres))
            true
          case None                                 => false)
      if !closed && it.faces.sizeIs < maxFaces && statesA.get < maxStates && System.nanoTime() < deadlineNanos
      then
        growBySymmetry(it.faces, maxN, it.seed.rot, it.seed.m, targetTypes).foreach: child =>
          if visited.add((it.seedIdx, canonicalKey(child))) then push(it.seedIdx, it.seed, it.corners, child)

    val workers = (0 until parallelism).map: _ =>
      val th = new Thread(() =>
        var done = false
        while !done do
          val it = pq.poll(50, TimeUnit.MILLISECONDS)
          if it == null then { if pending.get == 0 then done = true }
          else
            try processOne(it)
            finally pending.decrementAndGet()
      )
      th.setDaemon(true)
      th
    try
      val seeds0 =
        if targetTypes.isEmpty then allSeeds
        else allSeeds.filter(s => completeVertexTypes(s.faces).subsetOf(targetTypes))
      for (seed, seedIdx) <- seeds0.zipWithIndex do
        val corners = seed.faces.flatMap(_.corners).toSet
        if isPlanarConsistent(seed.faces) && isSound(seed.faces, maxN) &&
          visited.add((seedIdx, canonicalKey(seed.faces)))
        then push(seedIdx, seed, corners, seed.faces)
      logger.start()
      workers.foreach(_.start())
      workers.foreach(_.join())
    finally
      running.set(false)
      logger.interrupt()
    results.asScala.toMap

  /** Profiling variant of [[enumerateFromSeed]] (single seed, own results/visited): runs the SAME DFS but
    * times each per-state phase — `coronaCommitted`, `tryClose`, `growBySymmetry`, the children's
    * `canonicalKey` (visited dedup) — so the per-state-cost bottleneck is MEASURED, not guessed. Returns the
    * tilings (must match the real driver — tested) plus the phase-time breakdown in milliseconds.
    */
  def profileSeed(
      seed: Seed,
      maxN: Int,
      maxFaces: Int,
      maxCovolume: Double = Double.MaxValue
  ): (SymResult, Map[String, Long]) =
    val results                                      = mutable.Map.empty[String, (Int, Set[VertexSignature])]
    val visited                                      = mutable.HashSet.empty[Vector[Long]]
    val stack                                        = mutable.Stack.empty[List[FaceZ]]
    var states                                       = 0L
    var budgetHit                                    = false
    val seedCorners                                  = seed.faces.flatMap(_.corners).toSet
    def coronaCommitted(faces: List[FaceZ]): Boolean =
      seedCorners.forall(p => coveredSlots(planarFan(faces, p)).sizeIs == 12)
    var tCorona                                      = 0L
    var tClose                                       = 0L
    var tGrow                                        = 0L
    var tKey                                         = 0L
    if isPlanarConsistent(seed.faces) && isSound(seed.faces, maxN) && visited.add(canonicalKey(seed.faces))
    then stack.push(seed.faces)
    while stack.nonEmpty do
      val faces = stack.pop()
      states += 1
      val a0    = System.nanoTime(); val cc     = coronaCommitted(faces); tCorona += System.nanoTime() - a0
      val a1    = System.nanoTime(); val closed = cc && tryCloseFast(faces, maxN, maxCovolume, results)
      tClose += System.nanoTime() - a1
      if !closed then
        if faces.sizeIs >= maxFaces then budgetHit = true
        else
          val a2       = System.nanoTime()
          val children = growBySymmetry(faces, maxN, seed.rot, seed.m)
          tGrow += System.nanoTime() - a2
          children.foreach: child =>
            val a3 = System.nanoTime(); val k = canonicalKey(child); tKey += System.nanoTime() - a3
            if visited.add(k) then stack.push(child)
    val res                                          = SymResult(
      results.toList.map((key, nt) => (nt._1, nt._2, key)).sortBy((n, _, key) => (n, key)),
      states,
      budgetHit
    )
    (
      res,
      Map(
        "corona"       -> tCorona / 1000000,
        "tryClose"     -> tClose / 1000000,
        "grow"         -> tGrow / 1000000,
        "canonicalKey" -> tKey / 1000000
      )
    )

  /** Sub-profile of [[tryClose]] (the measured 99.8% hotspot): splits closure into `boundaryGlueBases` vs the
    * per-candidate `verifyCell` loop, and counts candidate bases / verifyCell calls — to know which to cut.
    * Mirrors [[enumerateFromSeed]]'s control flow exactly. Returns ms + counts.
    */
  def profileClose(
      seed: Seed,
      maxN: Int,
      maxFaces: Int,
      maxCovolume: Double = Double.MaxValue
  ): Map[String, Long] =
    val results                                      = mutable.Map.empty[String, (Int, Set[VertexSignature])]
    val visited                                      = mutable.HashSet.empty[Vector[Long]]
    val stack                                        = mutable.Stack.empty[List[FaceZ]]
    val seedCorners                                  = seed.faces.flatMap(_.corners).toSet
    def coronaCommitted(faces: List[FaceZ]): Boolean =
      seedCorners.forall(p => coveredSlots(planarFan(faces, p)).sizeIs == 12)
    var tBgb                                         = 0L
    var tVerify                                      = 0L
    var verifyCalls                                  = 0L
    var states                                       = 0L
    if isPlanarConsistent(seed.faces) && isSound(seed.faces, maxN) && visited.add(canonicalKey(seed.faces))
    then stack.push(seed.faces)
    while stack.nonEmpty do
      val faces  = stack.pop()
      states += 1
      var closed = false
      if coronaCommitted(faces) then
        val b0                                                            = System.nanoTime(); val bases = boundaryGlueBases(faces); tBgb += System.nanoTime() - b0
        var best: Option[(Int, Set[VertexSignature], String, BigDecimal)] = None
        bases.foreach: (g1, g2) =>
          val vB   = g1.toBigPoint
          val wB   = g2.toBigPoint
          val cov0 = cross(vB, wB).abs
          if cov0 > BigDecimal("1e-9") && distinctArea(faces, vB, wB) >= cov0 - BigDecimal("1e-6") then
            val v0 = System.nanoTime(); val hit = verifyCell(faces, g1, g2, maxN)
            tVerify += System.nanoTime() - v0
            verifyCalls += 1
            hit.foreach(h => if best.forall(h._4 < _._4) then best = Some(h))
        best.foreach((n, types, key, pcov) =>
          if pcov <= BigDecimal(maxCovolume) + BigDecimal("1e-6") then
            results.getOrElseUpdate(key, (n, types))
        )
        closed = best.isDefined
      if !closed && faces.sizeIs < maxFaces then
        growBySymmetry(faces, maxN, seed.rot, seed.m).foreach: child =>
          if visited.add(canonicalKey(child)) then stack.push(child)
    Map(
      "boundaryGlueBases_ms" -> tBgb / 1000000,
      "verifyCell_ms"        -> tVerify / 1000000,
      "states"               -> states,
      "verifyCalls"          -> verifyCalls
    )

  // ======================================================================================================
  // EARLY-GLUING CORE (ADR-0023). The interleaved extend-vs-glue map grower: develop one plane frame, and
  // accumulate the deck lattice Λ by gluing antiparallel boundary half-edges — closing at ONE cell (no
  // grow-cover scatter). The deck lattice is a search variable (rank 0→1→2), not a swept parameter.
  // ======================================================================================================

  /** The partial deck lattice — a rank-0/1/2 sublattice of ℤ[ζ₁₂] accumulated by gluing. */
  enum Gens:
    case Rank0
    case Rank1(g: ZetaPoint)
    case Rank2(g1: ZetaPoint, g2: ZetaPoint)

    /** Incorporate a new deck vector `t`; `None` if it is inconsistent (a third independent period ⇒ the
      * identification is not a discrete torus lattice).
      */
    def add(t: ZetaPoint): Option[Gens] = this match
      case Rank0         => if t.isOrigin then Some(Rank0) else Some(Rank1(t))
      case Rank1(g)      =>
        if independentZ(g, t) then Some(Rank2(g, t))
        else if isMultipleZ(t, g) then Some(Rank1(g))
        else None
      case Rank2(g1, g2) =>
        if t.isOrigin || t.congruentMod(ZetaPoint.origin, g1, g2) then Some(Rank2(g1, g2)) else None

  private def independentZ(a: ZetaPoint, b: ZetaPoint): Boolean =
    val (ax, ay) = dxy(a); val (bx, by) = dxy(b)
    math.abs(ax * by - ay * bx) > 1e-9

  private def isMultipleZ(t: ZetaPoint, g: ZetaPoint): Boolean =
    val gs = Array(g.a0, g.a1, g.a2, g.a3)
    val ts = Array(t.a0, t.a1, t.a2, t.a3)
    val i  = gs.indexWhere(_ != 0)
    if i < 0 then t.isOrigin
    else if ts(i) % gs(i) != 0 then false
    else { val k = ts(i) / gs(i); (0 until 4).forall(j => ts(j) == k * gs(j)) }

  /** Candidate single deck vectors: `t = p1 + step(b) − p2` for each antiparallel boundary half-edge pair
    * `(p1,b)`, `(p2,b+6)`. Shortest few — each is one GLUE move (`gens.add(t)`).
    */
  private def candidateGlueVectors(faces: List[FaceZ]): List[ZetaPoint] =
    val boundary = boundaryHalfEdges(faces)
    (for
      (p1, s1) <- boundary
      (p2, s2) <- boundary
      if s2 == (s1 + 6) % 12
      t = p1 + ZetaPoint.step(s1) - p2
      if !t.isOrigin
    yield t).distinct.sortBy { t =>
      val (x, y) = dxy(t); x * x + y * y
    }.take(6)

  /** Canonical visited key for a `(faces, gens)` state (faces key + a loose lattice tag; under-dedup only
    * costs states, never drops cells).
    */
  private def stateKey(faces: List[FaceZ], gens: Gens): Vector[Long] =
    def canon(z: ZetaPoint): Vector[Long] =
      val (x, y) = dxy(z)
      val zz     = if y > 1e-9 || (math.abs(y) <= 1e-9 && x > 0) then z else -z
      Vector(zz.a0, zz.a1, zz.a2, zz.a3)
    val tag                               = gens match
      case Gens.Rank0         => Vector(0L)
      case Gens.Rank1(g)      => 1L +: canon(g)
      case Gens.Rank2(g1, g2) =>
        2L +: List(canon(g1), canon(g2)).sortBy(v => (v(0), v(1), v(2), v(3))).flatten.toVector
    canonicalKey(faces) ++ tag

  /** Enumerate by the early-gluing core: seed coronas, interleave EXTEND (place faces at the MRV vertex) and
    * GLUE (assert a deck vector), close each branch when the discovered rank-2 Λ makes [[verifyCell]] accept.
    * Returns `(n, types, key)` per distinct tiling (bucketed by the verified `n ≤ maxN`).
    */
  def enumerateByGluing(maxN: Int, maxFaces: Int): List[(Int, Set[VertexSignature], String)] =
    val results = mutable.Map.empty[String, (Int, Set[VertexSignature])]
    val visited = mutable.HashSet.empty[Vector[Long]]
    val stack   = mutable.Stack.empty[(List[FaceZ], Gens)]
    for sig <- seedTypes do
      val seed = coronaFaces(sig)
      if isPlanarConsistent(seed) && isSound(seed, maxN) && visited.add(stateKey(seed, Gens.Rank0)) then
        stack.push((seed, Gens.Rank0))
    while stack.nonEmpty do
      val (faces, gens) = stack.pop()
      gens match
        case Gens.Rank2(g1, g2) =>
          // Λ discovered: only EXTEND, Λ-consistently, and close when the cell is filled. No more gluing.
          val vB    = g1.toBigPoint
          val wB    = g2.toBigPoint
          val cov   = cross(vB, wB).abs
          val close =
            distinctArea(faces, vB, wB) >= cov - BigDecimal("1e-6") && // cheap gate before the costly verify
              (verifyCell(faces, g1, g2, maxN) match
                case Some((n, types, key, _)) => results.getOrElseUpdate(key, (n, types)); true
                case None                     => false)
          if !close && faces.sizeIs < maxFaces then
            growByCompletionPlanar(faces, maxN).foreach: child =>
              if isConsistentMod(child, vB, wB) && visited.add(stateKey(child, gens)) then
                stack.push((child, gens))
        case _                  =>
          // Λ not yet rank-2: interleave GLUE (assert a deck vector) and EXTEND (place faces at the MRV vertex).
          if faces.sizeIs < maxFaces then
            candidateGlueVectors(faces).foreach: t =>
              gens.add(t).foreach: g2 =>
                if visited.add(stateKey(faces, g2)) then stack.push((faces, g2))
            growByCompletionPlanar(faces, maxN).foreach: child =>
              if visited.add(stateKey(child, gens)) then stack.push((child, gens))
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
