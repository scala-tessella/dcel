package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint, SimplePolygon}
import io.github.scala_tessella.dcel.structure.{Face, HalfEdge, Vertex, VertexId}

import scala.collection.mutable

/** Largest parallelogon contained in a tiling (ADR-0015) and the translation-lattice detection it builds on.
  *
  * Public API (extensions on `TilingDCEL`): [[largestContainedParallelogon]] (ordered corner vertices, the
  * headline query) and [[translationLattice]] (the reduced primitive basis). The block view (cell dimensions
  * + area) behind the corners is kept package-private until a consumer needs it.
  *
  * Detects the primitive translation lattice `{v, w}` of a finite periodic patch from its interior structure.
  * Vertices are typed by an orientation-aware `signature` (the sorted directions to their neighbours), then
  * the lattice is read off the single most populous signature class — one translation orbit, whose pairwise
  * differences are exactly the period lattice. This is robust to a handful of welded foreign faces (they form
  * minority classes and stay invisible) and naturally excludes cross-orientation vectors (e.g. between
  * 6.6.6's two `[6,6,6]` sublattices). A candidate is kept only if it has enough overlap with the patch,
  * ruling out large vectors with little genuine support.
  *
  * Candidates are validated structurally over the whole interior (signature-preserving on the overlap, a few
  * welded-face defects tolerated), and the result is Lagrange–Gauss reduced to a canonical primitive basis.
  *
  * Residual assumption: a vertex is typed only by its first neighbour ring, so two distinct translation
  * orbits that happen to share a first-ring signature would be conflated. Regular tilings (3⁶, 4⁴, 6⁶, 4.8.8,
  * …) do not exhibit this; a deeper (second-ring) signature would generalise it.
  */
object TilingLattice:

  /** The largest parallelogon contained in a tiling, as a block of whole lattice cells. Internal: the public
    * query is [[TilingLattice.largestContainedParallelogon]] (corner vertices); this quantitative view is
    * kept package-private until a consumer needs the area / cell counts.
    *
    * @param corners
    *   the four ideal lattice-point corners, in order (dominant-orbit positions; the public query snaps these
    *   to the actual whole-face boundary vertices)
    * @param cellsWide
    *   number of fundamental cells along the first lattice direction
    * @param cellsHigh
    *   number of fundamental cells along the second lattice direction
    * @param area
    *   the block area (`cellsWide * cellsHigh * covolume`)
    */
  private[dcel] case class ParallelogonBlock(
      corners: List[BigPoint],
      cellsWide: Int,
      cellsHigh: Int,
      area: BigDecimal
  )

  /** Internal: the maximal cell block — lattice basis, origin, the winning cell rectangle `(i0, j0, width,
    * height)`, and inner faces grouped by lattice cell.
    */
  private case class Block(
      v: BigPoint,
      w: BigPoint,
      origin: BigPoint,
      rect: (Int, Int, Int, Int),
      facesByCell: Map[(Int, Int), List[Face]]
  )

  /** Coordinates carry ~1e-12 float error from polar construction (ADR-0009); 9 decimals folds coincident
    * points together while keeping the ~0.5-apart distinct points separate.
    */
  private val SCALE = 9

  /** A cell counts as complete when its accumulated face area is within this of one covolume. The gap to the
    * next case (a partial cell missing a whole face) is far larger, so the bound is generous.
    */
  private val CELL_TOL = BigDecimal(1.0e-6)

  private type Key = (BigDecimal, BigDecimal)

  private def key(p: BigPoint): Key =
    (
      p.x.setScale(SCALE, BigDecimal.RoundingMode.HALF_UP),
      p.y.setScale(SCALE, BigDecimal.RoundingMode.HALF_UP)
    )

  extension (tiling: TilingDCEL)

    /** All translation-period candidates that pass the (tolerant) landing validation, sorted by norm and
      * paired with their landing-mismatch fraction, together with the dominant-orbit anchor point. Unlike
      * [[translationLattice]] — which commits to the two shortest independent vectors — this exposes the full
      * validated list so a caller can select a basis by stronger structural criteria (e.g. cell-content
      * equality, which is robust to the signature-rounding noise that breaks strict validation on large
      * patches and to the sublattice false periods that tolerant validation admits). The mismatch fraction
      * separates the two populations cheaply: ~0 for true periods (rounding flips only), near the defect
      * budget for tolerated false periods. Each candidate carries `(vector, mismatches, landings)`.
      */
    private[dcel] def validatedPeriods(
        minOverlapFraction: Double = 0.25,
        maxDefectFraction: Double = 0.1
    ): Option[(BigPoint, List[(BigPoint, Int, Int)])] =
      periodCandidates(minOverlapFraction, maxDefectFraction, anchorOnly = true).map((dominant, validated) =>
        (dominant.head.coords, validated)
      )

    /** The local star of a vertex: the sorted, rounded directions to its adjacent vertices. Invariant under
      * translation, so two vertices related by a lattice translation share a signature; mirror/rotation
      * images do not.
      */
    private def signature(v: Vertex): List[Key] =
      v.adjacentVerticesUnsafe
        .map(n => key(n.coords - v.coords))
        .sorted

    /** Interior (fully surrounded) vertices — all vertices minus the boundary cycle. */
    private def interiorVertices: List[Vertex] =
      val boundaryIds = tiling.boundaryVerticesUnsafe.map(_.id).toSet
      tiling.vertices.filterNot(v => boundaryIds.contains(v.id))

    /** Detects the primitive translation lattice, if the patch is periodic.
      *
      * @return
      *   `(v, w)` the Lagrange–Gauss reduced primitive basis (sign-canonicalised), or `None` if no lattice is
      *   found
      */
    def translationLattice(
        minOverlapFraction: Double = 0.25,
        maxDefectFraction: Double = 0.1
    ): Option[(BigPoint, BigPoint)] =
      periodicData(minOverlapFraction, maxDefectFraction).map((_, v, w) => (v, w))

    /** The dominant translation orbit plus its reduced lattice basis — shared by [[translationLattice]] and
      * [[largestContainedParallelogon]].
      *
      * @param minOverlapFraction
      *   a candidate period must map at least this fraction of dominant-orbit vertices onto interior
      *   vertices, ruling out large vectors with little/no genuine overlap.
      */
    private def periodCandidates(
        minOverlapFraction: Double,
        maxDefectFraction: Double,
        anchorOnly: Boolean = false
    ): Option[(List[Vertex], List[(BigPoint, Int, Int)])] =
      val interior                         = interiorVertices
      val sigAll: Map[VertexId, List[Key]] =
        interior.map(v => v.id -> signature(v)).toMap
      // Candidates are differences within the single most populous signature class — one translation orbit, so
      // every such difference is a genuine lattice vector, and cross-orientation vectors (e.g. 6.6.6's two
      // [6,6,6] sublattices) are excluded as cross-class differences.
      val dominant: List[Vertex]           =
        interior.groupBy(v => sigAll(v.id)).values.toList.maxByOption(_.size).getOrElse(Nil)
      if dominant.size < 4 then None
      else
        // Validate over the WHOLE interior, not just the dominant orbit: a true period maps every interior
        // vertex it lands on onto a same-signature vertex. This rejects coincidences that preserve dominant
        // points but break other vertex types, and supersedes a point-in-polygon test (a within-orbit
        // candidate never drops a vertex into a face interior). A few welded-foreign-face vertices that get
        // completed to 360° are tolerated as defects.
        val vertexAt: Map[Key, Vertex] =
          interior.map(v => key(v.coords) -> v).toMap
        val minMatches                 = math.max(3, (dominant.size * minOverlapFraction).toInt)
        // maxDefectFraction = 0 demands exact signature preservation on every landing: the right
        // setting for weld-free patches (e.g. enumeration candidates), where a tolerance would let a
        // sub-period of a dominant-orbit sublattice pass on the back of sparse minority-type
        // mismatches. The default keeps the welded-defect tolerance this detector was built with.
        val maxDefects                 =
          if maxDefectFraction == 0.0 then 0
          else math.max(2, (dominant.size * maxDefectFraction).toInt)

        // anchorOnly trades the O(d^2) full difference set for the O(d) differences from a single
        // well-placed anchor — the dominant vertex nearest the patch centroid, whose visible
        // orbit-mates surround it in every direction. Enumeration certifiers call this on every
        // horizon patch, where candidate generation dominates the certify cost; the anchor is moved
        // to the head of the returned orbit so callers anchor their cell grids on it.
        val anchored: List[Vertex] =
          if anchorOnly then
            val centroid = interior.map(_.coords).centroid
            val anchor   = dominant.minBy(v => (v.coords - centroid).dot(v.coords - centroid))
            anchor :: dominant.filterNot(_.id == anchor.id)
          else dominant

        val candidates: List[BigPoint] =
          val pairs =
            if anchorOnly then anchored.tail.map(b => b.coords - anchored.head.coords)
            else for a <- anchored; b <- anchored if a.id != b.id yield b.coords - a.coords
          pairs
            .filterNot(t => t.almostEquals(BigPoint.origin))
            .groupBy(key).values.map(_.head).toList

        // Each surviving candidate carries its landing statistics: a true period mismatches only on
        // rounding flips (an absolute handful), a tolerated sublattice false period mismatches in
        // proportion to the structure it breaks — callers that select a basis by content evidence
        // rank and threshold on these.
        val validated =
          candidates
            .flatMap: t =>
              val landings   = interior.flatMap(u => vertexAt.get(key(u.coords + t)).map(w => (u, w)))
              val mismatches = landings.count((u, w) => sigAll(u.id) != sigAll(w.id))
              Option.when(landings.size >= minMatches && mismatches <= maxDefects)(
                (t, mismatches, landings.size)
              )
            .sortBy((t, _, _) => t.dot(t))
        Some((anchored, validated))

    private def periodicData(
        minOverlapFraction: Double,
        maxDefectFraction: Double
    ): Option[(List[Vertex], BigPoint, BigPoint)] =
      periodCandidates(minOverlapFraction, maxDefectFraction).flatMap { (dominant, validated) =>
        val vectors = validated.map(_._1)
        vectors.headOption.flatMap { v =>
          vectors
            .find(u => v.cross(u).abs > BigDecimal(ACCURACY)) // shortest independent
            .map(w => gaussReduced(v, w))
            .map((a, b) => (dominant, canonicalSign(a), canonicalSign(b)))
        }
      }

    /** Maximal block of complete lattice cells (shared by the area- and corner-returning methods).
      *
      * Each inner face is assigned to the lattice cell containing its centroid; a cell is *complete* when the
      * face area it accumulates equals the lattice covolume `|v × w|` (one fundamental domain). Foreign
      * boundary faces (e.g. welded triangles) leave their cells short of a full covolume, so they are
      * excluded.
      */
    private def maximalBlock(minOverlapFraction: Double, maxDefectFraction: Double): Option[Block] =
      periodicData(minOverlapFraction, maxDefectFraction).flatMap { (dominant, v, w) =>
        val det    = v.cross(w)
        val covol  = det.abs
        val origin = dominant.head.coords

        // lattice coordinates (α, β) of p relative to origin, then its integer cell (⌊α⌋, ⌊β⌋)
        def cellOf(p: BigPoint): (Int, Int) =
          val d     = p - origin
          val alpha = (d.x * w.y - w.x * d.y) / det
          val beta  = (v.x * d.y - d.x * v.y) / det
          (floorToInt(alpha), floorToInt(beta))

        val facesByCell: Map[(Int, Int), List[Face]] =
          tiling.innerFaces.groupBy(f => cellOf(f.getVerticesUnsafe.map(_.coords).centroid))

        val occupied: Set[(Int, Int)] =
          facesByCell.filter((_, faces) => (faces.map(_.areaUnsafe).sum - covol).abs < CELL_TOL).keySet

        maximalRectangle(occupied).map(rect => Block(v, w, origin, rect, facesByCell))
      }

    /** Internal: the largest contained parallelogon as a block of whole fundamental-domain cells — cell
      * dimensions, area, and ideal lattice-point corners. Lattice-based only (no whole-boundary fast path).
      * The public query is [[largestContainedParallelogon]]; this is package-private pending a consumer that
      * needs the quantitative view.
      *
      * @return
      *   the block, or `None` if no lattice is found or no cell is complete
      */
    private[dcel] def largestContainedParallelogonBlock(
        minOverlapFraction: Double = 0.25,
        maxDefectFraction: Double = 0.1
    ): Option[ParallelogonBlock] =
      maximalBlock(minOverlapFraction, maxDefectFraction).map {
        case Block(v, w, origin, (i0, j0, width, height), _) =>
          def corner(i: Int, j: Int): BigPoint =
            origin + scaled(v, BigDecimal(i)) + scaled(w, BigDecimal(j))
          ParallelogonBlock(
            corners = List(
              corner(i0, j0),
              corner(i0 + width, j0),
              corner(i0 + width, j0 + height),
              corner(i0, j0 + height)
            ),
            cellsWide = width,
            cellsHigh = height,
            area = v.cross(w).abs * width * height
          )
      }

    /** The largest parallelogon contained in the patch (ADR-0015), as its ordered corner vertices (4 or 6) —
      * the corners where the parallelogon's sides meet, and the limit tessellation the patch tends to.
      *
      * Two paths:
      *   1. Whole-boundary fast path — if the patch's own boundary is already a parallelogon, the whole patch
      *      is the answer (nothing larger fits), so its boundary corners are returned directly, no lattice
      *      needed. This also handles single units (a lone square, a regular hexagon, a joined-hexagon
      *      badge).
      *   2. Otherwise, the lattice search: detect the translation lattice, take the maximal block of whole
      *      cells, walk its union boundary, and read off the turning corners via `parallelogonIndices`.
      *
      * Corners are genuine tiling vertices, ordered, rotated to start at the lowest corner for determinism.
      *
      * @return
      *   the corner vertices, or `None` if the patch is neither a parallelogon nor contains one
      */
    def largestContainedParallelogon(
        minOverlapFraction: Double = 0.25,
        maxDefectFraction: Double = 0.1
    ): Option[List[Vertex]] =
      wholeBoundaryCorners.orElse(latticeParallelogonCorners(minOverlapFraction, maxDefectFraction))

    /** Whole-boundary fast path: if the patch boundary itself is a parallelogon, its corner vertices are the
      * answer (the whole patch is the largest contained parallelogon).
      */
    private def wholeBoundaryCorners: Option[List[Vertex]] =
      val boundary = tiling.boundaryEdgesUnsafe
      tiling.boundarySimplePolygonUnsafe.parallelogonIndices match
        case Nil     => None
        case indices => Some(canonicalCorners(indices.map(i => boundary(i).origin)))

    /** Lattice fallback: the maximal cell block's union boundary, with its turning corners read off via
      * `parallelogonIndices`. The block interior angle at each boundary vertex is the sum of the incident
      * block-face corner angles there.
      */
    private def latticeParallelogonCorners(
        minOverlapFraction: Double,
        maxDefectFraction: Double
    ): Option[List[Vertex]] =
      maximalBlock(minOverlapFraction, maxDefectFraction).flatMap {
        case Block(_, _, _, (i0, j0, width, height), facesByCell) =>
          val blockCells: Set[(Int, Int)] =
            (for i <- i0 until i0 + width; j <- j0 until j0 + height yield (i, j)).toSet
          val blockFaces: Set[Face]       =
            blockCells.flatMap(facesByCell.getOrElse(_, Nil))
          val blockEdges: List[HalfEdge]  =
            blockFaces.toList.flatMap(_.halfEdgesUnsafe)

          // boundary = block-face half-edges whose twin lies outside the block (a non-block face, or the outer)
          val boundary: List[HalfEdge] =
            blockEdges.filter(he => !he.twin.flatMap(_.incidentFace).exists(blockFaces.contains))
          val ordered                  = orderCycle(boundary)

          if ordered.size < 4 || ordered.size != boundary.size then None
          else
            // block interior angle at each boundary vertex = Σ corner angles of block faces meeting there
            val angleAt: Map[VertexId, AngleDegree] =
              blockEdges
                .groupBy(_.origin.id)
                .map((vertexId, hes) => vertexId -> hes.flatMap(_.angle).foldLeft(AngleDegree(0))(_ + _))
            val cycleVertices                       = ordered.map(_.origin)
            val polygon                             = SimplePolygon(ordered.map(he => angleAt(he.origin.id)).toVector)
            polygon.parallelogonIndices match
              case Nil     => None
              case indices => Some(canonicalCorners(indices.map(cycleVertices)))
      }

  /** Flip a vector to a canonical half-plane so the basis is deterministic. */
  private def canonicalSign(v: BigPoint): BigPoint =
    val positive = v.x > BigDecimal(ACCURACY) || (v.x.abs <= BigDecimal(ACCURACY) && v.y > 0)
    if positive then v else BigPoint.origin - v

  private[dcel] def scaled(p: BigPoint, k: BigDecimal): BigPoint =
    BigPoint(p.x * k, p.y * k)

  private[dcel] def floorToInt(x: BigDecimal): Int =
    x.setScale(0, BigDecimal.RoundingMode.FLOOR).toInt

  /** Chains boundary half-edges into a single cycle by matching each one's destination to the next one's
    * origin (the block face stays on the left, so the walk is counter-clockwise). Stops early if the set is
    * not a single closed cycle — the caller treats a short result as "not a clean block".
    */
  private def orderCycle(halfEdges: List[HalfEdge]): List[HalfEdge] =
    halfEdges match
      case Nil       => Nil
      case head :: _ =>
        val byOrigin = halfEdges.groupBy(_.origin.id)
        val used     = mutable.HashSet(head)
        val ordered  = mutable.ListBuffer(head)
        var current  = head
        var continue = true
        while continue && ordered.size < halfEdges.size do
          byOrigin.getOrElse(current.destinationUnsafe.id, Nil).find(he => !used.contains(he)) match
            case Some(next) => ordered += next; used += next; current = next
            case None       => continue = false
        ordered.toList

  /** Rotates a corner cycle to start at the geometrically lowest corner (min x, then y), so equal blocks
    * yield the same ordered corner list regardless of which boundary edge the walk began on.
    */
  private def canonicalCorners(corners: List[Vertex]): List[Vertex] =
    if corners.isEmpty then corners
    else
      val start = corners.zipWithIndex.minBy((v, _) => (v.coords.x, v.coords.y))._2
      corners.drop(start) ++ corners.take(start)

  /** Maximal-area rectangle of occupied cells, as `(i0, j0, width, height)` with `(i0, j0)` the lower-left
    * cell. Classic histogram sweep: for each row, grow per-column run-heights and take the largest rectangle
    * in that histogram, tracking the winning position. O(rows · cols).
    */
  private[dcel] def maximalRectangle(occupied: Set[(Int, Int)]): Option[(Int, Int, Int, Int)] =
    if occupied.isEmpty then None
    else
      val minI    = occupied.iterator.map(_._1).min
      val maxI    = occupied.iterator.map(_._1).max
      val minJ    = occupied.iterator.map(_._2).min
      val maxJ    = occupied.iterator.map(_._2).max
      val cols    = maxI - minI + 1
      val heights = Array.fill(cols)(0)

      var best     = Option.empty[(Int, Int, Int, Int)]
      var bestArea = 0

      for j <- minJ to maxJ do
        for c <- 0 until cols do
          heights(c) = if occupied.contains((minI + c, j)) then heights(c) + 1 else 0

        // largest rectangle in the histogram whose bottom row is j
        val stack = scala.collection.mutable.Stack.empty[(Int, Int)] // (startColumn, height)
        for idx <- 0 to cols do
          val h     = if idx < cols then heights(idx) else 0
          var start = idx
          while stack.nonEmpty && stack.top._2 >= h do
            val (s, ph) = stack.pop()
            val width   = idx - s
            if ph > 0 && ph * width > bestArea then
              bestArea = ph * width
              best = Some((minI + s, j - ph + 1, width, ph)) // rows [j-ph+1 .. j]
            start = s
          if h > 0 then stack.push((start, h))
      best

  /** Lagrange–Gauss reduction of a 2D lattice basis: repeatedly subtract the nearest integer multiple of the
    * shorter vector from the longer, swapping so the first stays shortest. Terminates at the reduced
    * (minimal) basis — the two successive minima — which in 2D is guaranteed to be a basis of the same
    * lattice.
    */
  private[dcel] def gaussReduced(v0: BigPoint, w0: BigPoint): (BigPoint, BigPoint) =
    var a        = v0
    var b        = w0
    if a.dot(a) > b.dot(b) then
      val t = a; a = b; b = t
    var continue = true
    while continue do
      val m = (a.dot(b) / a.dot(a)).setScale(0, BigDecimal.RoundingMode.HALF_UP) // nearest integer
      if m == BigDecimal(0) then continue = false
      else
        b = b - scaled(a, m)
        if a.dot(a) > b.dot(b) then
          val t = a; a = b; b = t
    (a, b)
