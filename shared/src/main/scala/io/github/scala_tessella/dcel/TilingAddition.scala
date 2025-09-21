package io.github.scala_tessella.dcel

import BigDecimalGeometry._
import Polygon.{RegularPolygon, SimplePolygon}
import TilingBuilder.{calculateVertexPoints, validatePoints, validateSides}
import TilingEquivalency._
import io.github.scala_tessella.ring_seq.RingSeq.{rotateRight, slidingO}

import scala.annotation.tailrec

object TilingAddition:

  def calculateNewVertices(sides: Int, p1: BigPoint, p2: BigPoint): List[BigPoint] =
    val angle = RegularPolygon(sides).alpha.conjugate
    calculateVertexPoints(List.fill(sides)(angle), p1, p2).drop(2)

  private def createVertices(points: List[BigPoint], startingIndex: Int): List[Vertex] =
    points.zipWithIndex.map { (point, index) =>

      Vertex(VertexId(s"V${startingIndex + index}"), point)
    }

  // Extract polygon angle calculation
  private def polygonAngle(sides: Int): AngleDegree =
    RegularPolygon(sides).alpha

  // More descriptive boundary angle calculation
  private def boundaryAngleForVertex(
      vertex: Vertex,
      outerFace: Face,
      additionalInteriorAngle: AngleDegree
  ): AngleDegree =
    val currentInteriorSum = vertex.currentInteriorAngleSumUnsafe(outerFace)
    (currentInteriorSum + additionalInteriorAngle).conjugate

  private def createEdgePairs(
      vertices: List[Vertex],
      outerFace: Face,
      newFace: Face,
      startVertexAngle: AngleDegree,
      polygonAngles: List[AngleDegree]
  ): List[(HalfEdge, HalfEdge)] =
    val edgeVertices = vertices.sliding(2).collect {
      case origin :: destination :: Nil => (origin, destination)
    }.toList

    val subsequentBoundaryAngles = polygonAngles.init.map(_.conjugate)
    val boundaryAngles           = startVertexAngle +: subsequentBoundaryAngles

    edgeVertices.lazyZip(boundaryAngles).lazyZip(polygonAngles).map {
      case ((origin, destination), boundaryAngle, innerAngle) =>
        HalfEdge.createTwinHalfEdges(
          origin,
          destination,
          outerFace,
          newFace,
          boundaryAngle,
          innerAngle
        )
    }

  private case class SharedEdgesResult(
      sharedEdges: List[HalfEdge],
      startCheck: AngleDegree,
      startEdge: HalfEdge,
      startCounter: Int,
      endCheck: AngleDegree,
      endEdge: HalfEdge,
      endCounter: Int
  )

  private def findSharedEdges(
      edgeToBuildOn: HalfEdge,
      boundaryAngles: BoundaryAngles
  )(using outerFace: Face): Either[TilingError, SharedEdgesResult] =

    @tailrec
    def traverse(
        edge: HalfEdge,
        check: AngleDegree,
        angles: List[AngleDegree],
        acc: List[HalfEdge],
        getNext: HalfEdge => HalfEdge,
        getVertex: HalfEdge => Vertex
    ): Either[TilingError, (List[HalfEdge], AngleDegree, HalfEdge)] =
      if check.toRational < 0 then Left(ValidationError("Angle wider than container"))
      else if !check.isFullCircle then Right((acc, check, edge))
      else if angles.isEmpty then Left(ValidationError("Same as container"))
      else
        val nextCheck = boundaryAngleForVertex(getVertex(edge), outerFace, angles.head)
        traverse(getNext(edge), nextCheck, angles.tail, edge :: acc, getNext, getVertex)

    for
      (prepended, startCheck, startEdge) <- traverse(
                                              edgeToBuildOn.prev.get,
                                              boundaryAngles.start,
                                              boundaryAngles.newVertices.reverse,
                                              Nil,
                                              _.prev.get,
                                              _.origin
                                            )
      (appended, endCheck, endEdge)      <- traverse(
                                              edgeToBuildOn.next.get,
                                              boundaryAngles.end,
                                              boundaryAngles.newVertices,
                                              Nil,
                                              _.next.get,
                                              _.destination.get
                                            )
    yield SharedEdgesResult(
      sharedEdges = prepended ::: edgeToBuildOn :: appended.reverse,
      startCheck = startCheck,
      startEdge = startEdge,
      startCounter = prepended.length,
      endCheck = endCheck,
      endEdge = endEdge,
      endCounter = appended.length
    )

  private def checkForBoundaryIntersections(
      adjustedTempVertices: List[Vertex],
      boundaryEdges: List[HalfEdge]
  ): Either[TilingError, Unit] =
    // Create line segments for the new boundary
    val newSides = adjustedTempVertices.sliding(2).toList.map {
      case p1 :: p2 :: Nil => BigLineSegment(p1.coords, p2.coords)
      case _               => BigLineSegment(BigPoint(), BigPoint()) // This should never happen
    }

    val oldSides = boundaryEdges.slidingO(2).map {
      case e1 :: e2 :: Nil => BigLineSegment(e1.origin.coords, e2.origin.coords)
      case _               => BigLineSegment(BigPoint(), BigPoint())
    }.toList

    // Check for intersections
    if oldSides.hasProperIntersections(newSides) then
      Left(ValidationError("Boundary intersection"))
    else
      Right(())

  extension (tiling: TilingDCEL)

    private def nextFaceId: FaceId =
      FaceId("F" + (tiling.innerFaces.map(_.id.value.tail.toInt).max + 1).toString)

    private def nextVertexIndex: Int =
      tiling.vertices.map(_.id.value.tail.toInt).max + 1

    private def growthWithHoleCheck(
        startVertex: Vertex,
        endVertex: Vertex,
        edgeToBuildOn: HalfEdge,
        angles: List[AngleDegree],
        points: List[BigPoint],
        boundaryEdges: List[HalfEdge]
    ): Either[TilingError, (TilingDCEL, TilingDCEL, Face, Option[(Vertex, Vertex)])] =
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
          edgeResults.startEdge.destination.get :: tempVertices.drop(edgeResults.startCounter) ::: List(
            edgeResults.endEdge.origin
          )
        _                                           <- checkForBoundaryIntersections(adjustedTempVertices, boundaryEdges)
        result                                      <-
          val maybeHoleClosure: Option[(Vertex, Vertex)] =
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
            TilingDCEL(
              vertices = tiling.vertices ::: newVertices,
              halfEdges = tiling.halfEdges ::: newHalfEdges,
              innerFaces = tiling.innerFaces :+ newFace,
              outerFace = tiling.outerFace
            )
          Right((grownTiling, deepCopiedOriginal, containerFace, maybeHoleClosure))
      yield result

    private def determinePathDirection(
        pathFwd: List[HalfEdge],
        pathBack: List[HalfEdge]
    ): (List[HalfEdge], Boolean) =
      if pathFwd.sizeCompare(pathBack) < 0 then
        (pathFwd, true)
      else
        (pathBack, false)

    private def calculateHolePolygonAngles(holePath: List[HalfEdge]): List[AngleDegree] =
      val holeAngles       = holePath.map(_.angle.get)
      val sumOfOtherAngles = holeAngles.tail.sum2
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
      val pathFwd  = boundaryEdges.getPath(from = v_match, to = v_new)
      val pathBack = boundaryEdges.getPath(from = v_new, to = v_match)

      val (holePath, isForward) =
        determinePathDirection(pathFwd, pathBack)

      // 2. Calculate the internal angles for a new polygon that would fill this hole.
      val polygonAngles = calculateHolePolygonAngles(holePath)

      // 3. Determine the starting vertex and adjust angle order based on the path direction.
      if isForward then
        (polygonAngles, v_match.id, holePath.head.destination.get.id)
      else
        // For a backward path, the angles must be rotated, and the start vertex is different.
        val lastEdge = holePath.last
        (polygonAngles.rotateRight(1), lastEdge.origin.id, lastEdge.destination.get.id)

    private def validateBoundaryEdge(startingWithVertexId: VertexId)
        : Either[TilingError, (HalfEdge, Vertex, Vertex, List[HalfEdge])] = {
      val boundaryEdges = tiling.boundaryEdges
      for
        edgeToBuildOn            <- boundaryEdges
                                      .find(_.origin.id == startingWithVertexId)
                                      .toRight(ValidationError(
                                        s"Edge starting with vertex $startingWithVertexId not found on the boundary."
                                      ))
        (startVertex, endVertex) <- edgeToBuildOn.endpointsAsVertices
                                      .toRight(ValidationError("Edge has no destination vertex."))
      yield (edgeToBuildOn, startVertex, endVertex, boundaryEdges)
    }

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
    def addSimplePolygonToBoundary(
        onEdgeStartingWithVertexId: VertexId,
        angles: List[AngleDegree]
    ): Either[TilingError, TilingDCEL] =
      for
        _                                                      <- validateSides(angles.length, "simple")
        (edgeToBuildOn, startVertex, endVertex, boundaryEdges) <-
          validateBoundaryEdge(onEdgeStartingWithVertexId)
        result                                                 <- addSimplePolygon(startVertex.id, endVertex.id, angles)
      yield result

    /** Convenience overload for addSimplePolygonToBoundary using degrees.
      *
      * See addSimplePolygonToBoundary(List[AngleDegree]) for full preconditions and postconditions.
      */
    def addSimplePolygonToBoundary(
        onEdgeStartingWithVertexId: VertexId,
        degrees: Int*
    ): Either[TilingError, TilingDCEL] =
      addSimplePolygonToBoundary(onEdgeStartingWithVertexId, degrees.map(AngleDegree(_)).toList)

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
        sides: Int
    ): Either[TilingError, TilingDCEL] =
      for
        _                              <- validateSides(sides, "regular")
        (_, startVertex, endVertex, _) <- validateBoundaryEdge(onEdgeStartingWithVertexId)
        result                         <- addRegularPolygon(startVertex.id, endVertex.id, sides)
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
        maybeHoleClosure: Option[(Vertex, Vertex)]
    ): Option[TilingDCEL] =
      maybeHoleClosure.map((v_match, v_new) =>
        val (holeAngles, startingVertexId, endingVertexId) =
          tiling.holeAnglesWithDirection(v_match, v_new, containerFace)
        clone.addSimplePolygonUnsafe(startingVertexId, endingVertexId, holeAngles).get
      )

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
    @tailrec def addSimplePolygon(
        startVertexId: VertexId,
        endVertexId: VertexId,
        angles: List[AngleDegree]
    ): Either[TilingError, TilingDCEL] =
      val either: Either[TilingError, (TilingDCEL, TilingDCEL, Face, Option[(Vertex, Vertex)])] =
        for
          (startVertex, endVertex, edgeToBuildOn) <-
            tiling.findVerticesAndEdgeBetween(startVertexId, endVertexId)
          _                                       <- validateSides(angles.length, "simple")
          _                                       <- SimplePolygon.validatePolygonAngles(angles)
          points                                   = calculateVertexPoints(angles, startVertex.coords, endVertex.coords)
          _                                       <- validatePoints(points)
          result                                  <-
            val containerFace          = edgeToBuildOn.incidentFace.get
            val containerBoundaryEdges = containerFace.halfEdgesUnsafe
            growthWithHoleCheck(startVertex, endVertex, edgeToBuildOn, angles, points, containerBoundaryEdges)
        yield result

      either match
        case Left(value)                                                    => Left(value)
        case Right((revisedTiling, clone, containerFace, maybeHoleClosure)) =>
          revisedTiling.maybeFilled(clone, containerFace, maybeHoleClosure) match
            case None             => Right(revisedTiling)
            case Some(holeFilled) => holeFilled.addSimplePolygon(startVertexId, endVertexId, angles)

    /** Convenience overload for addSimplePolygon using degrees.
      *
      * See addSimplePolygon(List[AngleDegree]) for full preconditions and postconditions.
      */
    def addSimplePolygon(
        startVertexId: VertexId,
        endVertexId: VertexId,
        degrees: Int*
    ): Either[TilingError, TilingDCEL] =
      addSimplePolygon(startVertexId, endVertexId, degrees.map(AngleDegree(_)).toList)

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
        startVertexId: VertexId,
        endVertexId: VertexId,
        angles: List[AngleDegree]
    ): Option[TilingDCEL] =
      for
        (startVertex, endVertex, edgeToBuildOn) <-
          tiling.findVerticesAndEdgeBetween(startVertexId, endVertexId).toOption
        points                                   = calculateVertexPoints(angles, startVertex.coords, endVertex.coords)
      yield
        val containerFace = edgeToBuildOn.incidentFace.get

        val (tempVertices, edgeResults, boundaryAngles) =
          additionalVertices(
            startVertex,
            endVertex,
            edgeToBuildOn,
            angles,
            points,
            tiling.nextVertexIndex,
            containerFace
          ).toOption.get

        val (newVertices, newHalfEdges, newFace) =
          additionalElements(
            edgeToBuildOn,
            angles,
            tiling.nextFaceId,
            tempVertices,
            edgeResults,
            boundaryAngles
          )

        // Return new DCEL with updated components
        TilingDCEL(
          vertices = tiling.vertices ::: newVertices,
          halfEdges = tiling.halfEdges ::: newHalfEdges,
          innerFaces = tiling.innerFaces :+ newFace,
          outerFace = tiling.outerFace
        )

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
        startVertexId: VertexId,
        endVertexId: VertexId,
        sides: Int
    ): Either[TilingError, TilingDCEL] =
      val either: Either[TilingError, (TilingDCEL, TilingDCEL, Face, Option[(Vertex, Vertex)])] =
        for
          (startVertex, endVertex, edgeToBuildOn) <-
            tiling.findVerticesAndEdgeBetween(startVertexId, endVertexId)
          _                                       <- validateSides(sides, "regular")
          polyAngle                                = polygonAngle(sides)
          angles                                   = List.fill(sides)(polyAngle)
          points                                   = calculateVertexPoints(angles, startVertex.coords, endVertex.coords)
          result                                  <-
            val containerFace          = edgeToBuildOn.incidentFace.get
            val containerBoundaryEdges = containerFace.halfEdgesUnsafe
            growthWithHoleCheck(startVertex, endVertex, edgeToBuildOn, angles, points, containerBoundaryEdges)
        yield result

      either match
        case Left(value)                                                    => Left(value)
        case Right((revisedTiling, clone, containerFace, maybeHoleClosure)) =>
          revisedTiling.maybeFilled(clone, containerFace, maybeHoleClosure) match
            case None             => Right(revisedTiling)
            case Some(holeFilled) => holeFilled.addRegularPolygon(startVertexId, endVertexId, sides)

  // Helper case classes for better structure
  private case class BoundaryAngles(
      start: AngleDegree,
      end: AngleDegree,
      newVertices: List[AngleDegree]
  )

  private case class BoundaryState(
      prev: Option[HalfEdge],
      next: Option[HalfEdge]
  )

  private def additionalVertices(
      startVertex: Vertex,
      endVertex: Vertex,
      edgeToBuildOn: HalfEdge,
      angles: List[AngleDegree],
      points: List[BigPoint],
      vertexIndex: Int,
      outer: Face
  ): Either[TilingError, (List[Vertex], SharedEdgesResult, BoundaryAngles)] =
    given outerFace: Face = outer

    // Calculate boundary angles
    val boundaryAngles = BoundaryAngles(
      start = boundaryAngleForVertex(startVertex, outerFace, angles.head),
      end = boundaryAngleForVertex(endVertex, outerFace, angles(1)),
      newVertices = angles.drop(2)
    )

    for
      edgesResult <- findSharedEdges(edgeToBuildOn, boundaryAngles)

      // Create new components
      vertexPoints        = points.drop(2).reverse
      revisedVertexPoints = vertexPoints.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)
      newVertices         = createVertices(revisedVertexPoints, vertexIndex)
    yield (newVertices, edgesResult, boundaryAngles)

  /** Finds a couple of vertices from the existing and the additional boundary sharing the same coords and
    * thus marking a hole
    *
    * @param startVertex
    *   the boundary vertex from where the new vertices are added
    * @param boundaryEdges
    *   the edges forming the tiling's boundary
    * @param newVertices
    *   the added vertices
    */
  private def findHoleClosure(
      startVertex: Vertex,
      boundaryEdges: List[HalfEdge],
      newVertices: List[Vertex]
  ): Option[(Vertex, Vertex)] =
    // Find pairs of vertices (one from boundary, one new) that share coordinates
    val sharedVertices = newVertices.sameCoords(boundaryEdges.map(_.origin)).map(_.swap)

    sharedVertices match
      case Nil  => None
      case many =>
        // Get the indices of the new vertices that are shared
        val indices = many.map(p => newVertices.indexOf(p._2))

        // Find the last vertex from the initial contiguous block of shared vertices
        val forwardContiguousCount = indices.zip(indices.tail)
          .takeWhile { case (a, b) =>
            a + 1 == b
          }
          .length
        val endOfFirstBlock        = many(forwardContiguousCount)

        // Find the first vertex from the final contiguous block of shared vertices
        val backwardContiguousCount = indices.reverse.zip(indices.reverse.tail)
          .takeWhile { case (a, b) =>
            a - 1 == b
          }
          .length
        val startOfLastBlock        = many(many.length - 1 - backwardContiguousCount)

        // Determine which closure point results in a smaller path on the boundary
        def shortestPathLength(to: Vertex): Int =
          val pathLength = boundaryEdges.getPath(from = startVertex, to = to).length
          math.min(pathLength, boundaryEdges.length - pathLength)

        val forwardPathLength  = shortestPathLength(endOfFirstBlock._1)
        val backwardPathLength = shortestPathLength(startOfLastBlock._1)

        if forwardPathLength < backwardPathLength then
          Some(endOfFirstBlock)
        else
          Some(startOfLastBlock)

  private def additionalElements(
      edgeToBuildOn: HalfEdge,
      angles: List[AngleDegree],
      newFaceId: FaceId,
      newVertices: List[Vertex],
      edgesResult: SharedEdgesResult,
      boundaryAngles: BoundaryAngles
  ): (List[Vertex], List[HalfEdge], Face) =
    val innerFace = edgeToBuildOn.incidentFace.get

    given outerFace: Face = innerFace

    val newFace = Face(newFaceId)

    // Different start and end vertex
    val revisedStartVertex = edgesResult.startEdge.destination.get
    val revisedEndVertex   = edgesResult.endEdge.origin

    // Different boundary angles
    val revisedBoundaryAngles = BoundaryAngles(
      start = edgesResult.startCheck,
      end = edgesResult.endCheck,
      newVertices =
        boundaryAngles.newVertices.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)
    )

    // Different boundary
    val completeBoundary = BoundaryState(Some(edgesResult.startEdge), Some(edgesResult.endEdge))

    val allVertices = revisedStartVertex :: newVertices ::: revisedEndVertex :: Nil

    val revisedAngles = angles.reverse.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)

    val edgePairs                         =
      createEdgePairs(allVertices, outerFace, newFace, revisedBoundaryAngles.start, revisedAngles)
    val (newBoundaryEdges, newInnerEdges) = edgePairs.unzip

    val sharedAngles = angles.takeRight(edgesResult.startCounter) ++ angles.take(edgesResult.endCounter + 1)
    // Update existing structures
    updateExistingStructures(
      edgesResult.sharedEdges,
      newFace,
      sharedAngles,
      newBoundaryEdges,
      completeBoundary,
      revisedBoundaryAngles
    )

    // Link new face edges
    linkNewFaceEdges(edgeToBuildOn, edgesResult.sharedEdges, newInnerEdges.reverse, newFace)

    // Connect to boundary
    connectNewBoundaryEdges(newBoundaryEdges, completeBoundary, outerFace, edgesResult.sharedEdges)

    // Update vertex leaving edges
    updateVertexLeavingEdges(revisedStartVertex :: newVertices, newBoundaryEdges)

    (newVertices, newBoundaryEdges ::: newInnerEdges, newFace)

  private def updateExistingStructures(
      sharedEdges: List[HalfEdge],
      newFace: Face,
      polyAngles: List[AngleDegree],
      newBoundaryEdges: List[HalfEdge],
      originalBoundary: BoundaryState,
      boundaryAngles: BoundaryAngles
  ): Unit =
    // Update shared edges
    sharedEdges.foreach(_.incidentFace = Some(newFace))
    val sharedEdgesFirstAngle = newBoundaryEdges.head.angle
    sharedEdges.zipWithIndex.foreach((edge, index) => edge.angle = Some(polyAngles(index)))

    // Update last boundary edge angle
//    newBoundaryEdges.lastOption.foreach(_.angle = Some(boundaryAngles.newVertices.head.conjugate))

    // Update the existing boundary edge from end vertex
    originalBoundary.next.foreach { nextEdge =>

      if nextEdge.origin.id == sharedEdges.last.destination.map(_.id).getOrElse("") then
        nextEdge.angle = Some(boundaryAngles.end)
    }

    // Update boundary in the special shared edges case
    if sharedEdges.length > 1 && newBoundaryEdges.length == 1 then
      newBoundaryEdges.head.angle = sharedEdgesFirstAngle

  private def linkNewFaceEdges(
      edgeToBuildOn: HalfEdge,
      sharedEdges: List[HalfEdge],
      reversedInnerEdges: List[HalfEdge],
      newFace: Face
  ): Unit =
    val allInnerEdges = sharedEdges ::: reversedInnerEdges
    allInnerEdges.linkInCycle()
    newFace.outerComponent = Some(edgeToBuildOn)

  private def connectNewBoundaryEdges(
      newBoundaryEdges: List[HalfEdge],
      originalBoundary: BoundaryState,
      outerFace: Face,
      sharedEdges: List[HalfEdge]
  ): Unit =
    HalfEdge.insertBoundarySegment(
      originalBoundary.prev.get,
      originalBoundary.next.get,
      newBoundaryEdges
    )

    // Update the outer face component if necessary
    if outerFace.outerComponent.exists(e => sharedEdges.contains(e)) then
      outerFace.outerComponent = newBoundaryEdges.headOption

  private def updateVertexLeavingEdges(
      verticesWithNewEdges: List[Vertex],
      newBoundaryEdges: List[HalfEdge]
  ): Unit =
    verticesWithNewEdges.zip(newBoundaryEdges).foreach { (vertex, edge) =>

      vertex.leaving = Some(edge)
    }
