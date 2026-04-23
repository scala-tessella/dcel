package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingBoundaryIntersection.{
  BoundaryAngles,
  BoundaryState,
  SharedEdgesResult,
  boundaryAngleForVertex,
  checkForBoundaryIntersections,
  findSharedEdges
}
import io.github.scala_tessella.dcel.TilingBuilder.*
import io.github.scala_tessella.dcel.TilingEquivalency.*
import io.github.scala_tessella.dcel.TilingMerge.{OldNewVertexPair, mergeTilings}
import io.github.scala_tessella.dcel.geometry.{
  AngleDegree,
  BigPoint,
  BigRadian,
  RegularPolygon,
  SimplePolygon
}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.ring_seq.RingSeq.rotateRight

import scala.annotation.tailrec

object TilingAddition:

  def calculateNewVertices(sides: Int, p1: BigPoint, p2: BigPoint): List[BigPoint] =
    val angle = RegularPolygon(sides).alpha.conjugate
    calculateVertexPoints(Vector.fill(sides)(angle), p1, p2).drop(2)

  private def createVertices(points: List[BigPoint], startingIndex: Int): List[Vertex] =
    points.zipWithIndex.map: (point, index) =>
      Vertex(VertexId(startingIndex + index), point)

  private def createEdgePairs(
      vertices: List[Vertex],
      outerFace: Face,
      newFace: Face,
      startVertexAngle: AngleDegree,
      polygonAngles: List[AngleDegree]
  ): List[(HalfEdge, HalfEdge)] =
    val edgeVertices =
      vertices.sliding(2)
        .collect:
          case origin :: destination :: Nil => (origin, destination)
        .toList

    val subsequentBoundaryAngles =
      polygonAngles.init.map: angle =>
        angle.conjugate
    val boundaryAngles           = startVertexAngle +: subsequentBoundaryAngles

    edgeVertices.lazyZip(boundaryAngles).lazyZip(polygonAngles).map:
      case ((origin, destination), boundaryAngle, innerAngle) =>
        HalfEdge.createTwinHalfEdges(
          origin,
          destination,
          outerFace,
          newFace,
          boundaryAngle,
          innerAngle
        )

  extension (tiling: TilingDCEL)

    private def nextFaceId: FaceId =
      FaceId(
        tiling.innerFaces
          .map: face =>
            face.id.value
          .max + 1
      )

    private def nextVertexIndex: Int =
      tiling.vertices
        .map: vertex =>
          vertex.id.value
        .max + 1

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

    private def freshIdTranslations(base: TilingDCEL): (VertexId => VertexId, FaceId => FaceId) =
      // Translate vertices: give completely fresh vertex ids
      val vertexIds                                 =
        tiling.vertices.map: vertex =>
          vertex.id
      val maxVertexId                               =
        base.vertices.map(_.id.value).maxOption.getOrElse(0)
      val vertexIdTranslation: VertexId => VertexId =
        vertexIds.indices
          .map: index =>
            val oldId = vertexIds(index)
            oldId -> VertexId(maxVertexId + index + 1)
          .toMap

      // Translate faces: keep outer face id, shift inner ones
      val faceIds                             =
        tiling.faces.map: face =>
          face.id
      val maxFaceId                           =
        base.faces.map(_.id.value).maxOption.getOrElse(0)
      val faceIdTranslation: FaceId => FaceId =
        faceIds.indices
          .map: index =>
            faceIds(index) match
              case faceId if faceId == FaceId.outerId => faceId -> faceId // outer face
              case faceId                             => faceId -> FaceId(maxFaceId + index)
          .toMap

      (vertexIdTranslation, faceIdTranslation)

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

    private[dcel] def rawDouble(origin: Vertex, repeat: Vertex, withInversion: Boolean = false): TilingDCEL =
      // Compute the translation vector from origin to repeat
      val modifier: BigPoint => BigPoint           = if withInversion then _.scaled(-1.0) else identity
      val delta                                    = repeat.coords - modifier(origin.coords)
      val coordsTranslation: BigPoint => BigPoint  = modifier(_) + delta
      val (vertexIdTranslation, faceIdTranslation) = freshIdTranslations(tiling)

      // Second copy, translated in space and with fresh ids
      val translated: TilingDCEL =
        tiling.translatedDouble(
          coordsTranslation,
          vertexIdTranslation,
          faceIdTranslation
        )

      mergeTilings(tiling, translated)

    /** Multiply the tiling as a fan centered at the given vertex.
      *
      * @param origin
      * @return
      */
    private[dcel] def rawFan(origin: Vertex): Either[TilingError, TilingDCEL] =
      if tiling.isEmpty then
        Right(tiling)
      else

        // 1. check if origin is the id of an existing boundary vertex, otherwise return a validation error
        val originId = origin.id

        if !tiling.boundaryVerticesUnsafe.exists(_.id == originId) then
          return Left(ValidationError(s"Vertex ${origin.id.toPrefixedString} is not on the boundary."))

        // 2. calculate max multiplication factor, is the integer part of 360° divided by the sum interior angles, minus 1
        val anglesSum = origin.currentInteriorAngleSumUnsafe(tiling.outerFace)
        val factor    =
          math.floor(AngleDegree(360).toRational.toDouble / anglesSum.toRational.toDouble).toInt - 1

        // Validate a single merged copy before applying all symmetric copies.
        def cannotExpand: Left[ValidationError, TilingDCEL] =
          Left(ValidationError(s"Cannot be expanded around boundary Vertex ${originId.toPrefixedString}."))

        // 3. if it is 0 (the interior angles more than 180°) return the tiling itself
        if factor <= 0 then
          return cannotExpand

        def boundaryEdgesAtOrigin(target: TilingDCEL): Either[TilingError, (HalfEdge, HalfEdge)] =
          val boundaryEdges = target.boundaryEdgesUnsafe
          val edgeOutOpt    =
            boundaryEdges.find: halfEdge =>
              halfEdge.origin.id == originId
          val edgeInOpt     =
            boundaryEdges.find: halfEdge =>
              halfEdge.destination.exists(_.id == originId)
          (edgeInOpt, edgeOutOpt) match
            case (Some(edgeIn), Some(edgeOut)) => Right((edgeIn, edgeOut))
            case _                             =>
              Left(ValidationError(s"Adjacent boundary edges not found for ${originId.toPrefixedString}."))

        // 4. calculate the two perimeter edges adjacent to the origin vertex, and call A the first and Z the second
        val zEdge =
          boundaryEdgesAtOrigin(tiling) match
            case Left(_)             =>
              return Left(ValidationError(
                s"Adjacent boundary edges not found for ${originId.toPrefixedString}."
              ))
            case Right((_, edgeOut)) => edgeOut

        def rotateAround(center: BigPoint, angle: BigRadian)(point: BigPoint): BigPoint =
          val dx   = point.x - center.x
          val dy   = point.y - center.y
          // ADR-0009 candidate A: Math trig on Double.
          val cosA = BigDecimal(Math.cos(angle.toDouble))
          val sinA = BigDecimal(Math.sin(angle.toDouble))
          BigPoint(
            center.x + dx * cosA - dy * sinA,
            center.y + dx * sinA + dy * cosA
          )

        def translatedCopy(base: TilingDCEL, rotation: BigRadian, center: BigPoint): TilingDCEL =
          val (vertexIdTranslation, faceIdTranslation) = freshIdTranslations(base)

          val coordsTransformer: BigPoint => BigPoint =
            rotateAround(center, rotation)

          tiling.translatedDouble(coordsTransformer, vertexIdTranslation, faceIdTranslation)

        def firstCopy(current: TilingDCEL): Option[TilingDCEL] =
          boundaryEdgesAtOrigin(current) match
            case Left(_)           =>
              None
            case Right((bEdge, _)) =>
              current.vertices.find(_.id == originId).map: currentOrigin =>
                val zAngle = currentOrigin.coords.angleTo(zEdge.destinationUnsafe.coords)
                val bAngle = currentOrigin.coords.angleTo(bEdge.origin.coords)
                val delta  = bAngle - zAngle
                translatedCopy(current, delta, currentOrigin.coords)

        val seedCopy   = firstCopy(tiling)
        if seedCopy.isEmpty then return cannotExpand
        val seedMerged = mergeTilings(tiling, seedCopy.get)
        if TilingValidation.validate(seedMerged).isLeft then
          return cannotExpand

        // 5. for the times of the multiplication factor, repeat this process:
        @tailrec
        def loop(current: TilingDCEL, remaining: Int): Either[TilingError, TilingDCEL] =
          if remaining <= 0 then Right(current)
          else
            firstCopy(current) match
              case None         =>
                cannotExpand
              case Some(copied) =>
                val grown = mergeTilings(current, copied)
                loop(grown, remaining - 1)

        // 6. return the grown tiling (or the original one if even the first growth attempt fails)
        loop(seedMerged, factor - 1)

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
  ): Option[OldNewVertexPair] =
    val boundaryVertices =
      boundaryEdges.map: halfEdge =>
        halfEdge.origin

    // Find pairs of vertices (one from boundary, one new) that share coordinates
    val sharedVertices: List[OldNewVertexPair] =
      newVertices.sameCoords(boundaryVertices)
        .map: vertexPair =>
          vertexPair.swap

    if sharedVertices.isEmpty then
      return None

    val oldVertexIndices = sharedVertices.map: pair =>
      boundaryVertices.indexOf(pair.oldVertex)

    val forwardContiguousCount =
      oldVertexIndices.zip(oldVertexIndices.tail)
        .takeWhile: (a, b) =>
          a + 1 == b
        .length
    val endOfFirstBlock        = sharedVertices(forwardContiguousCount)

    // Find the first vertex from the final contiguous block of shared vertices
    val backwardContiguousCount2 =
      oldVertexIndices.reverse.zip(oldVertexIndices.reverse.tail)
        .takeWhile: (a, b) =>
          a - 1 == b
        .length
    val startOfLastBlock         = sharedVertices(sharedVertices.length - 1 - backwardContiguousCount2)

    // Determine which closure point results in a smaller path on the boundary
    def shortestBoundaryPathLength(to: Vertex): Int =
      val pathLength = boundaryEdges.getPathUnsafe(from = startVertex, to = to).length
      math.min(pathLength, boundaryEdges.length - pathLength)

    val forwardPathLength  = shortestBoundaryPathLength(endOfFirstBlock.oldVertex)
    val backwardPathLength = shortestBoundaryPathLength(startOfLastBlock.oldVertex)

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
    val revisedStartVertex = edgesResult.startEdge.destinationUnsafe
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
    sharedEdges.zipWithIndex.foreach: (edge, index) =>
      edge.angle = Some(polyAngles(index))

    // Update the existing boundary edge from end vertex
    val endVertexId =
      sharedEdges.last.destinationUnsafe.id
    originalBoundary.next.foreach: nextEdge =>
      if nextEdge.origin.id == endVertexId then
        nextEdge.angle = Some(boundaryAngles.end)

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
    verticesWithNewEdges.zip(newBoundaryEdges).foreach: (vertex, edge) =>
      vertex.leaving = Some(edge)
