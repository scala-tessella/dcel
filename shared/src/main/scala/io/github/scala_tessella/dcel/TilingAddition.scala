package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingBoundaryIntersection.checkForBoundaryIntersections
import io.github.scala_tessella.dcel.TilingBuilder.*
import io.github.scala_tessella.dcel.TilingEquivalency.deepCopy
import io.github.scala_tessella.dcel.TilingGrowthWiring.*
import io.github.scala_tessella.dcel.TilingMerge.OldNewVertexPair
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint, RegularPolygon, SimplePolygon}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.ring_seq.RingSeq.rotateRight

import scala.annotation.tailrec

object TilingAddition:

  def calculateNewVertices(sides: Int, p1: BigPoint, p2: BigPoint): List[BigPoint] =
    val angle = RegularPolygon(sides).alpha.conjugate
    calculateVertexPoints(Vector.fill(sides)(angle), p1, p2).drop(2)

  extension (tiling: TilingDCEL)

    private def nextFaceId: FaceId =
      FaceId(tiling.innerFaces.map(_.id.value).maxOption.getOrElse(0) + 1)

    private def nextVertexIndex: Int =
      tiling.vertices.map(_.id.value).maxOption.getOrElse(0) + 1

    private def addElements(
        newVertices: List[Vertex],
        newHalfEdges: List[HalfEdge],
        newFace: Face
    ): TilingDCEL =
      TilingDCEL(
        vertices = tiling.vertices ::: newVertices,
        halfEdges = tiling.halfEdges ::: newHalfEdges,
        innerFaces = tiling.innerFaces :+ newFace,
        outerFace = tiling.outerFace
      )

    private def growthWithHoleCheck(
        startVertex: Vertex,
        endVertex: Vertex,
        edgeToBuildOn: HalfEdge,
        angles: List[AngleDegree],
        points: List[BigPoint],
        boundaryEdges: List[HalfEdge]
    ): Either[TilingError, (TilingDCEL, TilingDCEL, Face, Option[OldNewVertexPair])] =
      val containerFace = edgeToBuildOn.incidentFace.get

      for
        (tempVertices, edgeResults, boundaryAngles) <- additionalVertices(
                                                         startVertex,
                                                         endVertex,
                                                         edgeToBuildOn,
                                                         angles,
                                                         points,
                                                         tiling.nextVertexIndex,
                                                         containerFace
                                                       )
        adjustedTempVertices                         =
          edgeResults.startEdge.destinationUnsafe :: tempVertices.drop(edgeResults.startCounter) ::: List(
            edgeResults.endEdge.origin
          )
        _                                           <- checkForBoundaryIntersections(adjustedTempVertices, boundaryEdges)
        result                                      <-
          val maybeHoleClosure: Option[OldNewVertexPair] =
            findHoleClosure(startVertex, boundaryEdges, tempVertices)
          val deepCopiedOriginal: TilingDCEL             =
            if maybeHoleClosure.isDefined then tiling.deepCopy
            else TilingDCEL.empty
          val (newVertices, newHalfEdges, newFace)       =
            additionalElements(
              edgeToBuildOn,
              angles,
              tiling.nextFaceId,
              tempVertices,
              edgeResults,
              boundaryAngles
            )
          // Return new DCEL with updated components
          val grownTiling                                =
            addElements(newVertices, newHalfEdges, newFace)
          Right((grownTiling, deepCopiedOriginal, containerFace, maybeHoleClosure))
      yield result

    private def growthResult(
        startVertex: Vertex,
        endVertex: Vertex,
        edgeToBuildOn: HalfEdge,
        angles: List[AngleDegree],
        points: List[BigPoint]
    ): Either[TilingError, (TilingDCEL, TilingDCEL, Face, Option[OldNewVertexPair])] =
      val containerFace          = edgeToBuildOn.incidentFace.get
      val containerBoundaryEdges = containerFace.halfEdgesUnsafe
      growthWithHoleCheck(
        startVertex,
        endVertex,
        edgeToBuildOn,
        angles,
        points,
        containerBoundaryEdges
      )

    private def determinePathDirection(
        pathFwd: List[HalfEdge],
        pathBack: List[HalfEdge]
    ): (List[HalfEdge], Boolean) =
      if pathFwd.sizeCompare(pathBack) < 0 then
        (pathFwd, true)
      else
        (pathBack, false)

    private def calculateHolePolygonAngles(holePath: List[HalfEdge]): List[AngleDegree] =
      val holeAngles       = holePath.map: halfEdge =>
        halfEdge.angle.get
      val sumOfOtherAngles = holeAngles.tail.sumExact
      val closingAngle     = SimplePolygon.alphaSum(holeAngles.length) - sumOfOtherAngles
      closingAngle :: holeAngles.tail

    /** Calculates the angles needed to fill a hole in the tiling's boundary, determining the correct starting
      * vertex and direction for the new polygon.
      *
      * @param v_match
      *   the existing vertex closing the hole
      * @param v_new
      *   the new vertex closing the hole
      */
    private def holeAnglesWithDirection(
        v_match: Vertex,
        v_new: Vertex,
        containerFace: Face
    ): (List[AngleDegree], VertexId, VertexId) =
      val boundaryEdges = containerFace.halfEdgesUnsafe

      // 1. Determine the shorter path (the "hole") on the boundary between the two vertices.
      val pathFwd  = boundaryEdges.getPathUnsafe(from = v_match, to = v_new)
      val pathBack = boundaryEdges.getPathUnsafe(from = v_new, to = v_match)

      val (holePath, isForward) =
        determinePathDirection(pathFwd, pathBack)

      // 2. Calculate the internal angles for a new polygon that would fill this hole.
      val polygonAngles = calculateHolePolygonAngles(holePath)

      // 3. Determine the starting vertex and adjust angle order based on the path direction.
      if isForward then
        (polygonAngles, v_match.id, holePath.head.destinationUnsafe.id)
      else
        // For a backward path, the angles must be rotated, and the start vertex is different.
        val lastEdge = holePath.last
        (polygonAngles.rotateRight(1), lastEdge.origin.id, lastEdge.destinationUnsafe.id)

    private def validateBoundaryEdge(startingWithVertexId: VertexId)
        : Either[TilingError, (HalfEdge, Vertex, Vertex, List[HalfEdge])] =
      val boundaryEdges = tiling.boundaryEdgesUnsafe
      for
        edgeToBuildOn            <- boundaryEdges
                                      .find: halfEdge =>
                                        halfEdge.origin.id == startingWithVertexId
                                      .toRight(ValidationError(
                                        s"Edge starting with vertex $startingWithVertexId not found on the boundary."
                                      ))
        (startVertex, endVertex) <- edgeToBuildOn.endpointsAsVertices
                                      .toRight(ValidationError("Edge has no destination vertex."))
      yield (edgeToBuildOn, startVertex, endVertex, boundaryEdges)

    def addUntrustedSimplePolygonToBoundary(
        onEdgeStartingWith: VertexId,
        angles: Vector[AngleDegree]
    ): Either[TilingError, TilingDCEL] =
      SimplePolygon.fromUntrusted(angles).flatMap: simplePolygon =>
        addSimplePolygonToBoundary(onEdgeStartingWith, simplePolygon)

    /** Adds a simple polygon to the outer boundary, defined by the supplied interior angles.
      *
      * Preconditions:
      *   - onEdgeStartingWithVertexId identifies a half-edge that belongs to the current outer boundary.
      *   - angles describes a valid simple polygon: angles.length >= 3 and passes angle-sum validation.
      *   - The polygon’s unit-length sides are implied by the internal geometry builder; the polygon must
      *     close without degeneracy.
      *   - The growth must not cause the boundary to self-intersect.
      *
      * Postconditions on success:
      *   - A new inner face is created and inserted, possibly reusing some boundary segments when angles
      *     allow.
      *   - The outer boundary is updated by replacing the consumed segment with the new exterior edges of the
      *     polygon.
      *   - Vertex leaving edges are updated so each involved vertex points to a valid boundary edge.
      *   - DCEL invariants are preserved.
      *
      * Failure cases:
      *   - Returns a TilingError if the target edge is not on the boundary, angles are invalid, the polygon
      *     wouldn’t close with unit-length edges, or topology/geometry/spatial validation fails (including
      *     boundary intersection).
      */
    private[dcel] def addSimplePolygonToBoundary(
        onEdgeStartingWithVertexId: VertexId,
        simple: SimplePolygon
    ): Either[TilingError, TilingDCEL] =
      for
        (edgeToBuildOn, startVertex, endVertex, boundaryEdges) <-
          validateBoundaryEdge(onEdgeStartingWithVertexId)
        result                                                 <- addSimplePolygon(startVertex.id, endVertex.id, simple)
      yield result

    /** Convenience overload for addSimplePolygonToBoundary using degrees.
      *
      * See addSimplePolygonToBoundary(List[AngleDegree]) for full preconditions and postconditions.
      */
    private[dcel] def addSimplePolygonToBoundary(
        onEdgeStartingWithVertexId: VertexId,
        degrees: Int*
    ): Either[TilingError, TilingDCEL] =
      addSimplePolygonToBoundary(
        onEdgeStartingWithVertexId,
        SimplePolygon(degrees*)
      )

    /** Adds a regular polygon to the outer boundary along the specified boundary edge.
      *
      * Preconditions:
      *   - onEdgeStartingWithVertexId identifies a half-edge that belongs to the current outer boundary.
      *   - sides >= 3.
      *   - The growth must not cause boundary self-intersections.
      *
      * Postconditions on success:
      *   - A new inner face with the given number of sides is added.
      *   - Boundary edges are rewired to include the new outer portion and exclude the consumed portion.
      *   - DCEL invariants remain valid; vertex leaving edges point to boundary edges.
      *
      * Failure cases:
      *   - Returns a TilingError if the edge is not on the boundary, sides are invalid, or the operation
      *     would violate topology/geometry/spatial constraints (including boundary intersection).
      */
    private[dcel] def addRegularPolygonToBoundary(
        onEdgeStartingWithVertexId: VertexId,
        polygon: RegularPolygon
    ): Either[TilingError, TilingDCEL] =
      for
        (_, startVertex, endVertex, _) <- validateBoundaryEdge(onEdgeStartingWithVertexId)
        result                         <- addRegularPolygon(startVertex.id, endVertex.id, polygon)
      yield result

    /** Internal helper that attempts to fill a newly detected boundary hole with a polygon.
      *
      * Preconditions:
      *   - clone is a deep-copied snapshot of the tiling before the latest growth (only required when a
      *     closure is detected).
      *   - maybeHoleClosure contains the vertex pair defining the closure if a hole was formed; otherwise
      *     None.
      *
      * Postconditions on success:
      *   - Returns Some(updatedTiling) where the hole has been filled by adding the appropriate polygon face.
      *   - Returns None when no hole closure is needed.
      *
      * Failure cases:
      *   - This method is best-effort and may assume previous validations; it returns None if not applicable.
      */
    private def maybeFilled(
        clone: TilingDCEL,
        containerFace: Face,
        maybeHoleClosure: Option[OldNewVertexPair]
    ): Option[TilingDCEL] =
      maybeHoleClosure.map: (v_match, v_new) =>
        val (holeAngles, startingVertexId, endingVertexId) =
          tiling.holeAnglesWithDirection(v_match, v_new, containerFace)
        val simplePolygon                                  =
          SimplePolygon(holeAngles.toVector)
        clone.addSimplePolygonUnsafe(startingVertexId, endingVertexId, simplePolygon).get

    def addUntrustedSimplePolygon(
        start: VertexId,
        end: VertexId,
        angles: Vector[AngleDegree]
    ): Either[TilingError, TilingDCEL] =
      SimplePolygon.fromUntrusted(angles).flatMap: simplePolygon =>
        addSimplePolygon(start, end, simplePolygon)

    /** Adds a simple polygon between two boundary vertices.
      *
      * Preconditions:
      *   - startVertexId and endVertexId denote consecutive vertices along the selected boundary direction
      *     that define the edge to grow from.
      *   - angles is a valid simple polygon specification (length >= 3 and valid angle sum).
      *   - Polygon reconstruction from unit-length sides closes without degeneracy.
      *   - No boundary self-intersections are created by this growth.
      *
      * Postconditions on success:
      *   - A new inner face is inserted; any overlapping boundary segments are reused where possible.
      *   - Boundary edges and vertex-leaving edges are updated accordingly.
      *   - DCEL invariants hold (twins, next/prev, incidentFace, angles).
      *
      * Failure cases:
      *   - Returns a TilingError upon invalid inputs, failed geometry checks, or topology/spatial violations.
      *   - If the growth creates a hole, the method may recursively attempt to fill it before returning.
      */
    @tailrec private[dcel] def addSimplePolygon(
        start: VertexId,
        end: VertexId,
        simple: SimplePolygon
    ): Either[TilingError, TilingDCEL] =
      val either =
        for
          (startVertex, endVertex, edgeToBuildOn) <-
            tiling.findVerticesAndEdgeBetween(start, end)
          points                                   = calculateVertexPoints(simple.toAngles, startVertex.coords, endVertex.coords)
          _                                       <- validatePoints(points)
          result                                  <-
            growthResult(
              startVertex,
              endVertex,
              edgeToBuildOn,
              simple.toAngles.toList,
              points
            )
        yield result

      either match
        case Left(value)                                                    => Left(value)
        case Right((revisedTiling, clone, containerFace, maybeHoleClosure)) =>
          revisedTiling.maybeFilled(clone, containerFace, maybeHoleClosure) match
            case None             => Right(revisedTiling)
            case Some(holeFilled) => holeFilled.addSimplePolygon(start, end, simple)

    /** Convenience overload for addSimplePolygon using degrees.
      *
      * See addSimplePolygon(List[AngleDegree]) for full preconditions and postconditions.
      */
    private[dcel] def addSimplePolygon(
        start: VertexId,
        end: VertexId,
        degrees: Int*
    ): Either[TilingError, TilingDCEL] =
      addSimplePolygon(start, end, SimplePolygon(degrees*))

    /** Internal helper that adds a simple polygon without performing guard validations.
      *
      * Preconditions:
      *   - Caller ensures angles produce valid points and that the insertion is topologically safe.
      *   - startVertexId and endVertexId identify a valid boundary edge to grow from.
      *
      * Postconditions:
      *   - Returns Some(updatedTiling) with the new face and boundary in place when construction succeeds.
      *   - Returns None if the initial vertex/edge lookup fails.
      *
      * Failure cases:
      *   - May throw if underlying invariants (e.g., required edge links) are not satisfied due to misuse.
      *     Intended for internal use after prior guards have passed.
      */
    private def addSimplePolygonUnsafe(
        start: VertexId,
        end: VertexId,
        simple: SimplePolygon
    ): Option[TilingDCEL] =
      for
        (startVertex, endVertex, edgeToBuildOn) <-
          tiling.findVerticesAndEdgeBetween(start, end).toOption
        points                                   = calculateVertexPoints(simple.toAngles, startVertex.coords, endVertex.coords)
      yield
        val containerFace = edgeToBuildOn.incidentFace.get

        val (tempVertices, edgeResults, boundaryAngles) =
          additionalVertices(
            startVertex,
            endVertex,
            edgeToBuildOn,
            simple.toAngles.toList,
            points,
            tiling.nextVertexIndex,
            containerFace
          ).toOption.get

        val (newVertices, newHalfEdges, newFace) =
          additionalElements(
            edgeToBuildOn,
            simple.toAngles.toList,
            tiling.nextFaceId,
            tempVertices,
            edgeResults,
            boundaryAngles
          )

        // Return new DCEL with updated components
        addElements(newVertices, newHalfEdges, newFace)

    /** Adds a regular polygon between two boundary vertices.
      *
      * Preconditions:
      *   - startVertexId and endVertexId denote the boundary edge to grow from.
      *   - sides >= 3.
      *   - No boundary self-intersections are introduced by this growth.
      *
      * Postconditions on success:
      *   - A new inner face with the given number of sides is inserted.
      *   - Boundary edges are rewired; vertex leaving edges updated to boundary edges.
      *   - DCEL invariants hold.
      *
      * Failure cases:
      *   - Returns a TilingError when the input is invalid or the operation would violate
      *     topology/geometry/spatial constraints.
      *   - If the growth creates a hole, the method may recursively fill it before returning.
      */
    @tailrec def addRegularPolygon(
        start: VertexId,
        end: VertexId,
        polygon: RegularPolygon
    ): Either[TilingError, TilingDCEL] =
      val either =
        for
          (startVertex, endVertex, edgeToBuildOn) <-
            tiling.findVerticesAndEdgeBetween(start, end)
          angles                                   = polygon.angles
          points                                   = calculateVertexPoints(angles, startVertex.coords, endVertex.coords)
          result                                  <-
            growthResult(
              startVertex,
              endVertex,
              edgeToBuildOn,
              angles.toList,
              points
            )
        yield result

      either match
        case Left(value)                                                    => Left(value)
        case Right((revisedTiling, clone, containerFace, maybeHoleClosure)) =>
          revisedTiling.maybeFilled(clone, containerFace, maybeHoleClosure) match
            case None             => Right(revisedTiling)
            case Some(holeFilled) => holeFilled.addRegularPolygon(start, end, polygon)
