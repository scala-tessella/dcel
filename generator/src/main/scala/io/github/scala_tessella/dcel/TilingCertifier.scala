package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingEquivalency.groupByBoundaryEquivalency
import io.github.scala_tessella.dcel.TilingLattice.{
  ParallelogonBlock,
  floorToInt,
  gaussReduced,
  maximalRectangle,
  scaled,
  validatedPeriods
}
import io.github.scala_tessella.dcel.VertexTypes.*
import io.github.scala_tessella.dcel.geometry.BigPoint
import io.github.scala_tessella.dcel.structure.Vertex

/** Certification pipeline of the Krotenheerdt enumeration (OEIS A068600): turns a finite horizon patch into
  * evidence of an infinite n-uniform tiling with n vertex types, or a structured rejection.
  *
  *   1. **Lattice** — the patch must reveal translation-period candidates
  *      ([[TilingLattice.validatedPeriods]], tolerant validation); the basis among them is selected by
  *      content evidence ([[selectBasis]]), which is robust both to the signature-rounding noise that breaks
  *      strict validation at scale and to the sublattice false periods that tolerant validation admits.
  *   1. **Parallelogon block** — the selected basis must yield a block of at least 2x2 whole lattice cells of
  *      identical face content. The block's boundary is a parallelogon, which tiles the plane by translation
  *      carrying its faces (ADR-0015), and with >= 2 cells in each direction every seam of that infinite
  *      tiling is witnessed inside the patch — a constructive periodicity proof that, unlike a
  *      translate-and-merge wallpaper test, does not depend on the patch's outline shape.
  *   1. **Block-interior witnessing** — every vertex inside the block must be an interior (fully surrounded)
  *      patch vertex, and the block's vertex types must equal the patch-interior types, exactly n of them.
  *      Without this, a patch can masquerade as a different tiling: e.g. a chunk of a triangle+hexagon tiling
  *      whose interior happens to show only `6.6.6` vertices at this horizon, the triangles' vertices hiding
  *      unmeasured on the patch boundary.
  *   1. **Uniformity** — `uniformityTree.sizeLeaves == n`; since uniformity refines gonality,
  *      `classes == types == n` forces uniformity = gonality = n — the Krotenheerdt condition.
  */
object TilingCertifier:

  enum RejectReason:
    case NoLattice, NoPeriodEvidence, SubTilingPeriod, BlockTooSmall, CellsDiffer, NoWitnessCell,
      TooShallow, WrongTypeCount, WrongClassCount

  final case class Certified(
      n: Int,
      vertexTypes: Set[VertexSignature],
      basis: (BigPoint, BigPoint),
      torusKey: String,
      patch: Tiling
  )

  /** The vertex type (bracelet-normalized fan of incident polygon sizes) of an inner vertex. */
  def vertexTypeOf(tiling: TilingDCEL, vertex: Vertex): VertexSignature =
    normalize(
      vertex.incidentEdgesUnsafe
        .flatMap(_.incidentFace)
        .filter(_ != tiling.outerFace)
        .map(_.halfEdgesUnsafe.size)
    )

  def innerVertexTypes(tiling: TilingDCEL): Set[VertexSignature] =
    tiling.innerVertices.map(vertexTypeOf(tiling, _)).toSet

  /** Block-relative lattice geometry: membership tests for the certified block's content. Faces use the
    * closed range (a face on the seam appears at both reduced positions and dedups after reduction); vertices
    * use the half-open range so each lattice-translate orbit is counted once.
    */
  final case class CellGeometry(basis: (BigPoint, BigPoint), block: TilingLattice.ParallelogonBlock):
    private val (v, w)      = basis
    private val blockOrigin = block.corners.head
    private val det         = v.x * w.y - v.y * w.x
    private val eps         = BigDecimal("1e-9")

    def coords(p: BigPoint): (BigDecimal, BigDecimal) =
      val d = p - blockOrigin
      ((d.x * w.y - w.x * d.y) / det, (v.x * d.y - d.x * v.y) / det)

    def containsFace(p: BigPoint): Boolean =
      val (alpha, beta) = coords(p)
      alpha >= -eps && alpha <= BigDecimal(block.cellsWide) + eps &&
      beta >= -eps && beta <= BigDecimal(block.cellsHigh) + eps

    def containsVertex(p: BigPoint): Boolean =
      val (alpha, beta) = coords(p)
      alpha >= -eps && alpha < BigDecimal(block.cellsWide) - eps &&
      beta >= -eps && beta < BigDecimal(block.cellsHigh) - eps

    /** The block cell holding this position (by half-open lattice coordinates), if inside the block. */
    def cellOf(p: BigPoint): Option[(Int, Int)] =
      val (alpha, beta) = coords(p)
      val i             = (alpha + eps).setScale(0, BigDecimal.RoundingMode.FLOOR).toInt
      val j             = (beta + eps).setScale(0, BigDecimal.RoundingMode.FLOOR).toInt
      Option.when(i >= 0 && i < block.cellsWide && j >= 0 && j < block.cellsHigh)((i, j))

    /** Position reduced into a single cell, rounded. */
    def reduced(p: BigPoint): (BigDecimal, BigDecimal) =
      val (alpha, beta) = coords(p)
      (fracOf(alpha), fracOf(beta))

    private def fracOf(x: BigDecimal): BigDecimal =
      val r        = x.setScale(9, BigDecimal.RoundingMode.HALF_UP)
      val flooring = r.setScale(0, BigDecimal.RoundingMode.FLOOR)
      val f        = (r - flooring).setScale(9, BigDecimal.RoundingMode.HALF_UP)
      if f >= BigDecimal(1) then BigDecimal(0).setScale(9) else f

  def certify(patch: Tiling, n: Int): Either[RejectReason, Certified] =
    for
      // Tolerant candidate detection + structural basis selection. Strict (zero-defect) landing
      // validation is rounding-brittle at scale: one 9-decimal direction-key flip among hundreds of
      // landings kills the true period. A tolerant budget alone instead admits sublattice false
      // periods on the back of sparse minority-type mismatches. The resolution: take every
      // tolerantly validated candidate vector and let content evidence pick the basis
      // ([[selectBasis]]) — a false period cannot reproduce identical full-covolume cell content
      // across a whole block, a true one always does, and the comparison involves a handful of
      // reduced face positions instead of hundreds of signature landings.
      periods      <- patch.validatedPeriods().toRight(RejectReason.NoLattice)
      selected     <- selectBasis(patch, periods._1, periods._2)
      (basis, cell) = selected

      // A witness cell: one whose vertices are all interior, so every type in the cell is measured.
      innerIds     = patch.innerVertices.map(_.id).toSet
      vertexCells  = patch.vertices
                       .flatMap(v => cell.cellOf(v.coords).map(ij => (ij, v)))
                       .groupMap(_._1)(_._2)
      boundaryIds  = patch.boundaryVerticesUnsafe.map(_.id).toSet
      allCells     = (for i <- 0 until cell.block.cellsWide; j <- 0 until cell.block.cellsHigh
                     yield (i, j)).toList
      witnessCell <-
        allCells
          .filter(ij =>
            vertexCells.get(ij).exists(vs => vs.nonEmpty && vs.forall(v => innerIds.contains(v.id)))
          )
          // the deepest such cell: corona refinement needs room around every representative
          .maxByOption(ij => vertexCells(ij).map(v => depthToBoundary(v, boundaryIds)).min)
          .toRight(RejectReason.NoWitnessCell)
      witnessTypes = vertexCells(witnessCell).map(vertexTypeOf(patch, _)).toSet
      _           <- Either.cond(witnessTypes.size == n, (), RejectReason.WrongTypeCount)
      // Uniformity classes measured directly on the witness-cell representatives by corona
      // refinement: the global uniformityTree parks boundary-truncated vertices in extra leaves
      // (patch-shape noise on spiral growth patches). The witness cell holds at least one
      // representative of every vertex orbit of the periodic tiling, so refining the
      // representatives by local-DCEL boundary-equivalence up to the patch's safe depth measures
      // the orbit count (under the documented corona-equivalence assumption).
      classes     <- orbitClasses(patch, vertexCells(witnessCell)).toRight(RejectReason.TooShallow)
      _           <- Either.cond(classes == n, (), RejectReason.WrongClassCount)
    yield Certified(n, witnessTypes, basis, torusKey(patch, basis, cell, witnessCell), patch)

  /** A cell counts as complete when its accumulated face area is within this of one covolume (mirrors
    * [[TilingLattice]]'s tolerance).
    */
  private val CELL_TOL = BigDecimal(1.0e-6)

  /** Selects the translation basis from the tolerantly validated candidate vectors by structural evidence.
    * Gauss-reduced candidate pairs are tried in increasing cell-area order; the first basis to pass wins:
    *
    *   - the anchored cell grid must contain a block of >= 2x2 area-complete cells;
    *   - every block cell must hold the same reduced face content (a false period never reproduces it);
    *   - the cell content must show every face size present among the patch's inner faces — a sublattice
    *     false period that is a true period of a mono-type region (e.g. the triangle sublattice inside a
    *     `3.3.3.3.3.3 + 3.3.3.3.6` patch) can pass the first two checks on a block confined to that region,
    *     but its cell misses the other faces; a true fundamental cell contains every face of the tiling.
    *
    * Area ordering makes the primitive true basis beat non-primitive true bases; the false sublattice bases
    * tried before it (smaller covolume) all fail the content checks.
    */
  private[dcel] def selectBasis(
      patch: Tiling,
      anchor: BigPoint,
      vectors: List[(BigPoint, Int, Int)]
  ): Either[RejectReason, ((BigPoint, BigPoint), CellGeometry)] =
    val eps = BigDecimal("1e-9")

    def key9(p: BigPoint): (BigDecimal, BigDecimal) =
      (
        p.x.setScale(SCALE, BigDecimal.RoundingMode.HALF_UP),
        p.y.setScale(SCALE, BigDecimal.RoundingMode.HALF_UP)
      )
    def canonicalSign(t: BigPoint): BigPoint        =
      val (x, y) = key9(t)
      if y > 0 || (y == 0 && x > 0) then t else BigPoint.origin - t

    // Candidates come in +/- pairs: rank by mismatch fraction first (a true period mismatches only
    // on rounding flips, a tolerated sublattice false period sits near the defect budget), then by
    // norm; keep one sign per vector and bound the pool. Norm alone is not enough: on patches whose
    // dominant signature class merges several true orbits (the triangles of 3.3.3.3.3.3+3.3.3.3.6
    // families), the sublattice floods the shortest ranks with false vectors and the true period
    // would never be tried.
    // A candidate is "low" when its mismatches are explainable by rounding flips alone. Flips scale
    // with the landing count (each landing is one rounded-key comparison), so the bound is hybrid:
    // an absolute floor of 2 for small patches, 2.5% of landings at scale — a true period at v~300
    // can show 3-5 flips and must not be mistaken for a broken sublattice vector.
    def isLow(mismatches: Int, landings: Int): Boolean =
      mismatches <= math.max(2, (landings * 0.025).toInt)

    val pool: List[(BigPoint, Int, Int)] =
      vectors
        .map((t, m, l) => (canonicalSign(t), m, l))
        .sortBy((t, m, l) => (m.toDouble / l, t.dot(t)))
        .distinctBy((t, _, _) => key9(t))
        .take(32)

    val bases: List[((BigPoint, BigPoint), Boolean)] =
      (for
        i <- pool.indices
        j <- (i + 1) until pool.size
        // collinear pairs are not a basis and would zero-divide inside the reduction
        if pool(i)._1.cross(pool(j)._1).abs > eps
      yield
        val (a, b)   = gaussReduced(pool(i)._1, pool(j)._1)
        val (ca, cb) = (canonicalSign(a), canonicalSign(b))
        val ord      = Ordering[(BigDecimal, (BigDecimal, BigDecimal))]
        val pair     =
          if ord.lteq((ca.dot(ca), key9(ca)), (cb.dot(cb), key9(cb))) then (ca, cb) else (cb, ca)
        (pair, isLow(pool(i)._2, pool(i)._3) && isLow(pool(j)._2, pool(j)._3))
      )
        .toList
        .groupMapReduce((pair, _) => (key9(pair._1), key9(pair._2)))(identity)((x, y) =>
          (x._1, x._2 || y._2)
        )
        .values.toList
        .sortBy((pair, _) => (pair._1.cross(pair._2).abs, pair._1.dot(pair._1) + pair._2.dot(pair._2)))

    val faces          =
      patch.innerFaces.map: f =>
        (f.halfEdgesUnsafe.size, f.getVerticesUnsafe.map(_.coords).centroid, f.areaUnsafe)
    val patchFaceSizes = faces.map(_._1).toSet

    var sawBlock                                          = false
    var subTilingPeriod                                   = false
    var hit: Option[((BigPoint, BigPoint), CellGeometry)] = None
    val iterator                                          = bases.iterator
    while hit.isEmpty && iterator.hasNext do
      val ((v, w), low) = iterator.next()
      val det           = v.cross(w)
      val covol         = det.abs

      def cellIndex(p: BigPoint): (Int, Int) =
        val d = p - anchor
        (floorToInt((d.x * w.y - w.x * d.y) / det), floorToInt((v.x * d.y - d.x * v.y) / det))

      val byCell: Map[(Int, Int), List[(Int, BigPoint, BigDecimal)]] =
        faces.groupBy((_, c, _) => cellIndex(c))
      val occupied                                                   =
        byCell.filter((_, fs) => (fs.map(_._3).sum - covol).abs < CELL_TOL).keySet

      maximalRectangle(occupied).foreach: (i0, j0, cellsWide, cellsHigh) =>
        if cellsWide >= 2 && cellsHigh >= 2 then
          sawBlock = true
          val origin = anchor + scaled(v, BigDecimal(i0)) + scaled(w, BigDecimal(j0))
          val sideV  = scaled(v, BigDecimal(cellsWide))
          val sideW  = scaled(w, BigDecimal(cellsHigh))
          val block  = ParallelogonBlock(
            corners = List(origin, origin + sideV, origin + sideV + sideW, origin + sideW),
            cellsWide = cellsWide,
            cellsHigh = cellsHigh,
            area = covol * cellsWide * cellsHigh
          )
          val cell   = CellGeometry((v, w), block)

          val faceCells =
            faces
              .flatMap((sides, c, _) => cell.cellOf(c).map(ij => (ij, (sides, cell.reduced(c)))))
              .groupMap(_._1)(_._2)
              .view.mapValues(_.toSet).toMap
          val allCells  =
            (for i <- 0 until cellsWide; j <- 0 until cellsHigh yield (i, j)).toSet

          if faceCells.keySet == allCells && faceCells.values.toSet.sizeIs == 1 then
            if patchFaceSizes.subsetOf(faceCells.values.head.map(_._1)) then
              hit = Some(((v, w), cell))
            else if low then
              // A rounding-clean basis whose content-equal block misses a face size: the patch's
              // only flawless periodic structure is a sub-tiling of fewer face kinds (a mono-type
              // core decorated at its rim, say). In a genuine n-uniform patch a low basis is a true
              // period and its cells contain every face of the tiling — final, not transient.
              subTilingPeriod = true

    // Failure triage. With two independent near-zero-mismatch candidates the patch shows a true 2D
    // period and just needs more room (transient: the search may grow it). Without them the patch
    // is at best 1D-periodic — the signature of the stacking-aperiodic families (dense decoration
    // rows at random offsets, locally indistinguishable from a genuine n-uniform tiling) that
    // otherwise retry-branch all the way to the hard cap.
    hit.toRight {
      if subTilingPeriod then RejectReason.SubTilingPeriod
      else
        val low            =
          vectors.filter((_, m, l) => isLow(m, l)).map((t, _, _) => canonicalSign(t)).distinctBy(key9)
        val twoDimensional = low.exists(a => low.exists(b => a.cross(b).abs > eps))
        if !twoDimensional then RejectReason.NoPeriodEvidence
        else if sawBlock then RejectReason.CellsDiffer
        else RejectReason.BlockTooSmall
    }

  /** Refines the witness-cell representatives into uniformity classes by corona equivalence at growing
    * radius, bounded by the depth at which every representative's corona is still fully interior to the
    * patch. Returns `None` when even radius 1 is not safely measurable.
    */
  private[dcel] def depthToBoundary(v: Vertex, boundaryIds: Set[structure.VertexId]): Int =
    Iterator.from(1).find(d => v.bfsVertices(d).exists(u => boundaryIds.contains(u.id))).get

  private def orbitClasses(patch: Tiling, reps: List[Vertex]): Option[Int] =
    val boundary = patch.boundaryVerticesUnsafe.map(_.id).toSet

    // A depth is trustworthy only while every compared local DCEL stays clear of the patch
    // boundary. Vertex-distance clearance is not enough: the local DCEL includes whole faces, so
    // its hull can reach the boundary well before the vertex distance does on large polygons (a
    // dodecagon spans graph distance 6 — seen splitting same-orbit 3.12.12 representatives whose
    // depth-5 balls were vertex-interior but face-truncated). Refinement results from a truncated
    // depth are discarded; the measurement stops at the last fully witnessed depth.
    var classes: List[List[Vertex]] = List(reps)
    var depth                       = 0
    var lastSound                   = -1
    var stop                        = false
    while !stop && classes.exists(_.sizeIs > 1) do
      val refinements: List[Option[List[List[Vertex]]]] =
        classes.map: group =>
          if group.sizeIs == 1 then Some(List(group))
          else
            val localTilings = group.map(v => v -> patch.getDcelAtVertex(v.id, depth).toOption.get)
            if localTilings.exists((_, local) => local.vertices.exists(u => boundary.contains(u.id)))
            then None
            else Some(groupByBoundaryEquivalency(localTilings))
      if refinements.forall(_.isDefined) then
        classes = refinements.flatMap(_.get)
        lastSound = depth
        depth += 1
      else stop = true
    // Depth 1 coronas cannot separate orbits that agree on first neighbourhoods (seen: a shifted
    // multi-orbit 3.3.4.3.4 variant certifying as 1-uniform); require radius 2 before judging
    // multi-member classes. All-singleton classes are exact at any depth: every split is genuine.
    if classes.forall(_.sizeIs == 1) || lastSound >= 2 then Some(classes.size) else None

  /** Sound aperiodicity detector: lower-bounds the vertex-orbit count of ANY tiling containing the patch.
    *
    * Inner vertices are grouped by type, then refined by corona equivalence at growing depth, comparing only
    * vertices whose local DCEL at that depth is fully witnessed (contains no patch-boundary vertex, as in
    * [[orbitClasses]]). Vertices with structurally different witnessed coronas can never share an orbit in
    * any continuation, so more than n classes is a final verdict — no heuristic threshold involved. This is
    * what stops the stacking-aperiodic decoration families (type-complete in every ball, periodic along rows
    * but stacked at mismatched offsets), which no local gate can distinguish from a genuine n-uniform patch:
    * vertices at differently shifted seams split at shallow depth.
    */
  private[dcel] def tooManyWitnessedOrbits(
      patch: Tiling,
      n: Int,
      maxDepth: Int = 3,
      skipLargestGroup: Boolean = false
  ): Boolean =
    val boundaryIds                = patch.boundaryVerticesUnsafe.map(_.id).toSet
    val typeGroups                 = patch.innerVertices.groupBy(vertexTypeOf(patch, _)).values.toList
    // With exactly n types showing, types and orbits must be in bijection, so a witnessed split
    // inside ANY single type group is fatal on its own. The dominant group carries almost the whole
    // O(g^2) equivalence cost; growth-time callers skip it (still sound, just weaker — the skipped
    // group counts as one class) and leave its refinement to the certify-time checks.
    val candidates                 =
      if skipLargestGroup && typeGroups.sizeIs > 1 then typeGroups.sortBy(-_.size).tail
      else typeGroups
    val skipped                    = typeGroups.size - candidates.size
    // Depth 0 cannot split beyond the type grouping (same type = congruent star), so start at 1.
    var groups: List[List[Vertex]] = candidates
    var exceeded                   = groups.size + skipped > n
    var depth                      = 1
    while !exceeded && depth <= maxDepth do
      groups = groups.flatMap: group =>
        val measured =
          group
            .map(v => v -> patch.getDcelAtVertex(v.id, depth).toOption.get)
            .filterNot((_, local) => local.vertices.exists(u => boundaryIds.contains(u.id)))
        if measured.sizeIs <= 1 then List(measured.map(_._1)).filter(_.nonEmpty)
        else groupByBoundaryEquivalency(measured)
      exceeded = groups.size + skipped > n
      depth += 1
    exceeded

  // --------------------------------------------------------------------------------------------
  // Torus key: canonical identity of the infinite tiling
  // --------------------------------------------------------------------------------------------

  private val SCALE = 9

  private def frac(x: BigDecimal): BigDecimal =
    val r        = x.setScale(SCALE, BigDecimal.RoundingMode.HALF_UP)
    val flooring = r.setScale(0, BigDecimal.RoundingMode.FLOOR)
    val f        = (r - flooring).setScale(SCALE, BigDecimal.RoundingMode.HALF_UP)
    if f >= BigDecimal(1) then BigDecimal(0).setScale(SCALE) else f

  /** Canonical content of one fundamental cell: face descriptors (sides, centroid mod lattice) and vertex
    * descriptors (type, position mod lattice) of the certified block only — strays outside the block are
    * tolerated defects, not identity. Canonicalized over every equivalent primitive basis (the candidate
    * vectors `±v, ±w, ±(v+w), ±(v−w)` paired with equal cell area, covering rotations and reflections of the
    * tiling) and over the choice of cell origin (each face descriptor anchors in turn). Two patches of the
    * same infinite tiling quotient to the same torus; distinct tilings — including those sharing a
    * vertex-type composition — differ in cell arrangement and get different keys.
    */
  def torusKey(
      tiling: TilingDCEL,
      basis: (BigPoint, BigPoint),
      cell: CellGeometry,
      witnessCell: (Int, Int)
  ): String =
    // Primitive reduction first: strict lattice detection can settle on a doubled cell when a
    // single rounding-borderline landing breaks the true period. If the block content is invariant
    // under a half-vector self-translation (pure arithmetic on reduced coordinates — no signature
    // rounding involved), halve the basis and repeat, so the key always describes the primitive
    // torus and candidate bases are enumerated from primitive vectors.
    val (v, w) = primitiveBasis(tiling, cell, basis)
    val det0   = (v.x * w.y - v.y * w.x).abs

    val candidateVectors = List(v, w, v + w, v - w).flatMap(p => List(p, BigPoint.origin - p))
    val candidateBases   =
      for
        a  <- candidateVectors
        b  <- candidateVectors
        det = a.x * b.y - a.y * b.x
        if (det.abs - det0).abs < BigDecimal("1e-9")
      yield (a, b, det)

    // The witness cell's content only: one fundamental domain, fully measured.
    val faces         =
      tiling.innerFaces
        .map: face =>
          val vertices = face.getVerticesUnsafe.map(_.coords)
          (vertices.size, vertices.centroid)
        .filter((_, c) => cell.cellOf(c).contains(witnessCell))
    val typedVertices =
      tiling.innerVertices
        .map: vertex =>
          (vertexTypeOf(tiling, vertex), vertex.coords)
        .filter((_, c) => cell.cellOf(c).contains(witnessCell))

    // Primitive reduction: strict lattice detection can settle on a doubled cell when a single
    // rounding-borderline landing breaks the true period. If the cell content is invariant under a
    // half-vector self-translation, fold it onto the finer lattice (pure arithmetic on reduced
    // coordinates — no signature rounding involved) so the key always describes the primitive torus.
    def reducePrimitive(
        faces0: List[(Int, (BigDecimal, BigDecimal))],
        verts0: List[(String, (BigDecimal, BigDecimal))]
    ): (List[(Int, (BigDecimal, BigDecimal))], List[(String, (BigDecimal, BigDecimal))]) =
      val half                                                                                             = BigDecimal("0.5")
      def fr(x: BigDecimal): BigDecimal                                                                    =
        val r = x.setScale(SCALE, BigDecimal.RoundingMode.HALF_UP)
        val f = r - r.setScale(0, BigDecimal.RoundingMode.FLOOR)
        if f >= BigDecimal(1) then BigDecimal(0).setScale(SCALE)
        else f.setScale(SCALE, BigDecimal.RoundingMode.HALF_UP)
      val shifts: List[((BigDecimal, BigDecimal), ((BigDecimal, BigDecimal)) => (BigDecimal, BigDecimal))] =
        List(
          ((half, BigDecimal(0)), (ab: (BigDecimal, BigDecimal)) => (fr(ab._1 * 2), ab._2)),
          ((BigDecimal(0), half), (ab: (BigDecimal, BigDecimal)) => (ab._1, fr(ab._2 * 2))),
          ((half, half), (ab: (BigDecimal, BigDecimal)) => (fr(ab._1 - ab._2), fr(ab._2 * 2)))
        )
      def contentKey(fs: List[(Int, (BigDecimal, BigDecimal))])                                            =
        fs.map((sd, ab) => (sd, fr(ab._1), fr(ab._2))).toSet
      shifts.collectFirst {
        case ((sa, sb), transform)
            if contentKey(faces0.map((sd, ab) => (sd, (fr(ab._1 + sa), fr(ab._2 + sb))))) ==
              contentKey(faces0) =>
          reducePrimitive(
            faces0.map((sd, ab) => (sd, transform(ab))).distinctBy((sd, ab) => (sd, fr(ab._1), fr(ab._2))),
            verts0.map((t, ab) => (t, transform(ab))).distinctBy((t, ab) => (t, fr(ab._1), fr(ab._2)))
          )
      }.getOrElse((faces0, verts0))

    val keys =
      for (a, b, det) <- candidateBases
      yield
        def lattice(p: BigPoint): (BigDecimal, BigDecimal) =
          ((p.x * b.y - b.x * p.y) / det, (a.x * p.y - p.x * a.y) / det)

        val faceCells   = faces.map((sides, c) => (sides, lattice(c)))
        val vertexCells = typedVertices.map((t, c) => (t.mkString("."), lattice(c)))

        val anchored =
          faceCells.map(_._2).distinct.map: (anchorAlpha, anchorBeta) =>
            val fs =
              faceCells
                .map((sides, ab) => (sides, frac(ab._1 - anchorAlpha), frac(ab._2 - anchorBeta)))
                .distinct.sorted
            val vs =
              vertexCells
                .map((t, ab) => (t, frac(ab._1 - anchorAlpha), frac(ab._2 - anchorBeta)))
                .distinct.sorted
            (fs.map((s, x, y) => s"$s:$x:$y") ++ vs.map((t, x, y) => s"$t:$x:$y")).mkString("|")
        anchored.min

    keys.min

  /** Reduces a possibly non-primitive basis by detecting half-vector self-translations of the block content
    * (faces only, by reduced position): content invariant under `v/2`, `w/2` or `(v+w)/2` means the true
    * fundamental cell is finer; halve and recurse.
    */
  private def primitiveBasis(
      tiling: TilingDCEL,
      cell: CellGeometry,
      basis: (BigPoint, BigPoint)
  ): (BigPoint, BigPoint) =
    val faces =
      tiling.innerFaces
        .map: face =>
          val vertices = face.getVerticesUnsafe.map(_.coords)
          (vertices.size, vertices.centroid)
        .filter((_, c) => cell.containsFace(c))

    def fr(x: BigDecimal): BigDecimal =
      val r = x.setScale(SCALE, BigDecimal.RoundingMode.HALF_UP)
      val f = r - r.setScale(0, BigDecimal.RoundingMode.FLOOR)
      if f >= BigDecimal(1) then BigDecimal(0).setScale(SCALE)
      else f.setScale(SCALE, BigDecimal.RoundingMode.HALF_UP)

    def reduce(v: BigPoint, w: BigPoint): (BigPoint, BigPoint) =
      val det                                                = v.x * w.y - v.y * w.x
      def coords(p: BigPoint)                                =
        ((p.x * w.y - w.x * p.y) / det, (v.x * p.y - p.x * v.y) / det)
      val content                                            = faces.map((sides, c) => (sides, coords(c)))
      def contentKey(shiftA: BigDecimal, shiftB: BigDecimal) =
        content.map((sd, ab) => (sd, fr(ab._1 + shiftA), fr(ab._2 + shiftB))).toSet
      val base                                               = contentKey(BigDecimal(0), BigDecimal(0))
      val half                                               = BigDecimal("0.5")
      val finer                                              =
        if contentKey(half, BigDecimal(0)) == base then Some((v.scaled(half), w))
        else if contentKey(BigDecimal(0), half) == base then Some((v, w.scaled(half)))
        else if contentKey(half, half) == base then Some((v, (v + w).scaled(half)))
        else None
      finer match
        case Some((fv, fw)) => reduce(fv, fw)
        case None           => (v, w)

    reduce(basis._1, basis._2)
