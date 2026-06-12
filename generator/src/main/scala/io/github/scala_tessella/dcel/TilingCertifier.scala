package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingEquivalency.groupByBoundaryEquivalency
import io.github.scala_tessella.dcel.TilingLattice.{largestContainedParallelogonBlock, translationLattice}
import io.github.scala_tessella.dcel.VertexTypes.*
import io.github.scala_tessella.dcel.geometry.BigPoint
import io.github.scala_tessella.dcel.structure.Vertex

/** Certification pipeline of the Krotenheerdt enumeration (OEIS A068600): turns a finite horizon patch into
  * evidence of an infinite n-uniform tiling with n vertex types, or a structured rejection.
  *
  *   1. **Lattice** — the patch must reveal a primitive translation basis (k-uniform tilings are periodic, so
  *      a large-enough patch of a true tiling always does).
  *   1. **Parallelogon block** — the patch must contain a block of at least 2x2 whole lattice cells
  *      ([[TilingLattice.largestContainedParallelogonBlock]]). The block's boundary is a parallelogon, which
  *      tiles the plane by translation carrying its faces (ADR-0015), and with >= 2 cells in each direction
  *      every seam of that infinite tiling is witnessed inside the patch — a constructive periodicity proof
  *      that, unlike a translate-and-merge wallpaper test, does not depend on the patch's outline shape.
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
    case NoLattice, BlockTooSmall, CellsDiffer, NoWitnessCell, TooShallow, WrongTypeCount, WrongClassCount

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
      // Strict detection (zero defect tolerance): enumeration patches are weld-free, so a true
      // period preserves every interior signature; any tolerance lets sublattice false periods
      // through on the back of sparse minority-type mismatches.
      basis <- patch.translationLattice(maxDefectFraction = 0.0).toRight(RejectReason.NoLattice)
      block <- patch
                 .largestContainedParallelogonBlock(maxDefectFraction = 0.0)
                 .toRight(RejectReason.BlockTooSmall)
      _     <- Either.cond(block.cellsWide >= 2 && block.cellsHigh >= 2, (), RejectReason.BlockTooSmall)
      cell   = CellGeometry(basis, block)

      // Translation-invariance of content: every block cell must hold the same reduced face content.
      // The block's area-completeness deliberately tolerates defects; content equality is what makes
      // "the parallelogon block tiles by translation carrying its faces" airtight.
      faceCells = patch.innerFaces
                    .map(f => (f.halfEdgesUnsafe.size, f.getVerticesUnsafe.map(_.coords).centroid))
                    .flatMap((sides, c) => cell.cellOf(c).map(ij => (ij, (sides, cell.reduced(c)))))
                    .groupMap(_._1)(_._2)
                    .view.mapValues(_.toSet).toMap
      allCells  = (for i <- 0 until block.cellsWide; j <- 0 until block.cellsHigh yield (i, j)).toSet
      _        <- Either.cond(
                    faceCells.keySet == allCells && faceCells.values.toSet.sizeIs == 1,
                    (),
                    RejectReason.CellsDiffer
                  )

      // A witness cell: one whose vertices are all interior, so every type in the cell is measured.
      innerIds     = patch.innerVertices.map(_.id).toSet
      vertexCells  = patch.vertices
                       .flatMap(v => cell.cellOf(v.coords).map(ij => (ij, v)))
                       .groupMap(_._1)(_._2)
      boundaryIds  = patch.boundaryVerticesUnsafe.map(_.id).toSet
      witnessCell <-
        allCells.toList
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

  /** Refines the witness-cell representatives into uniformity classes by corona equivalence at growing
    * radius, bounded by the depth at which every representative's corona is still fully interior to the
    * patch. Returns `None` when even radius 1 is not safely measurable.
    */
  private[dcel] def depthToBoundary(v: Vertex, boundaryIds: Set[structure.VertexId]): Int =
    Iterator.from(1).find(d => v.bfsVertices(d).exists(u => boundaryIds.contains(u.id))).get

  private def orbitClasses(patch: Tiling, reps: List[Vertex]): Option[Int] =
    val boundary  = patch.boundaryVerticesUnsafe.map(_.id).toSet
    val safeDepth = reps.map(depthToBoundary(_, boundary)).min - 1
    // Depth 1 coronas cannot separate orbits that agree on first neighbourhoods (seen: a shifted
    // multi-orbit 3.3.4.3.4 variant certifying as 1-uniform); require radius 2 before judging.
    if safeDepth < 2 then None
    else
      var classes: List[List[Vertex]] = List(reps)
      for d <- 0 to safeDepth do
        classes = classes.flatMap: group =>
          if group.sizeIs == 1 then List(group)
          else
            val localTilings = group.map(v => v -> patch.getDcelAtVertex(v.id, d).toOption.get)
            groupByBoundaryEquivalency(localTilings)
      Some(classes.size)

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
