package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingBuilder.*
import io.github.scala_tessella.dcel.TilingEquivalency.*
import io.github.scala_tessella.dcel.geometry.{
  AngleDegree,
  BigLineSegment,
  BigPoint,
  RegularPolygon,
  SimplePolygon
}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.ring_seq.RingSeq.{rotateRight, slidingO}

import scala.annotation.tailrec

object TilingAddition:

  def calculateNewVertices(sides: Int, p1: BigPoint, p2: BigPoint): List[BigPoint] =
    val angle = RegularPolygon(sides).alpha.conjugate
    calculateVertexPoints(Vector.fill(sides)(angle), p1, p2).drop(2)

  private def createVertices(points: List[BigPoint], startingIndex: Int): List[Vertex] =
    points.zipWithIndex.map: (point, index) =>
      Vertex(vertexIdV(startingIndex + index), point)

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
    val verticesPairs = adjustedTempVertices.sliding(2).toVector
    val newSides      = verticesPairs.map:
      case p1 :: p2 :: Nil => BigLineSegment(p1.coords, p2.coords)
      case _               => throw new Error("Edge vertices not in pair")

    val edgesPairs = boundaryEdges.slidingO(2).toVector
    val oldSides   = edgesPairs.map:
      case e1 :: e2 :: Nil => BigLineSegment(e1.origin.coords, e2.origin.coords)
      case _               => throw new Error("Edges not in pair")

    // Check for intersections
    if oldSides.hasProperIntersections(newSides) then
      val intersections = oldSides.properIntersections(newSides)
      val decoded       = intersections.map: (segment1, segment2) =>
        newSides.indexOf(segment1) match
          case -1 => oldSides.indexOf(segment1) match
              case -1 => throw new Error("Intersection not in either list")
              case j  => newSides.indexOf(segment2) match
                  case -1 => throw new Error("Segment 2 not in list")
                  case i  => (verticesPairs(i), edgesPairs(j))
          case i  => oldSides.indexOf(segment2) match
              case -1 => throw new Error("Segment 2 not in list")
              case j  => (verticesPairs(i), edgesPairs(j))
      val vertexIds     =
        decoded.map: (verticesPair, edgesPair) =>
          (
            verticesPair.map: vertex =>
              vertex.id,
            edgesPair.map: halfEdge =>
              halfEdge.origin.id
          )
      val edges         =
        vertexIds.map: (e1, e2) =>
          s"${e1.head}-${e1(1)} with ${e2.head}-${e2(1)}"
      Left(ValidationError(s"Boundary intersection: ${edges.mkString(", ")}"))
    else
      Right(())

  private type OldNewVertexPair = (oldVertex: Vertex, newVertex: Vertex)

  extension (tiling: TilingDCEL)

    private def nextFaceId: FaceId =
      faceIdF(
        tiling.innerFaces
          .map: face =>
            idFromFaceId(face.id)
          .max + 1
      )

    private def nextVertexIndex: Int =
      tiling.vertices
        .map: vertex =>
          idFromVertexId(vertex.id)
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
          edgeResults.startEdge.destination.get :: tempVertices.drop(edgeResults.startCounter) ::: List(
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

//      println(s"v_match.id = ${v_match.id}, v_new.id = ${v_new.id}")
//      println(s"boundaryEdges.map(_.origin.id) = ${boundaryEdges.map(_.origin.id)}")

      // 1. Determine the shorter path (the "hole") on the boundary between the two vertices.
      val pathFwd  = boundaryEdges.getPath(from = v_match, to = v_new)
      val pathBack = boundaryEdges.getPath(from = v_new, to = v_match)

      val (holePath, isForward) =
        determinePathDirection(pathFwd, pathBack)

      // 2. Calculate the internal angles for a new polygon that would fill this hole.
      val polygonAngles = calculateHolePolygonAngles(holePath)

//      if polygonAngles.head.isFullCircle then
//        println(s"polygonAngles = $polygonAngles")
//        throw new Error("First angle is full circle")

      // 3. Determine the starting vertex and adjust angle order based on the path direction.
      if isForward then
        (polygonAngles, v_match.id, holePath.head.destination.get.id)
      else
        // For a backward path, the angles must be rotated, and the start vertex is different.
        val lastEdge = holePath.last
        (polygonAngles.rotateRight(1), lastEdge.origin.id, lastEdge.destination.get.id)

    private def validateBoundaryEdge(startingWithVertexId: VertexId)
        : Either[TilingError, (HalfEdge, Vertex, Vertex, List[HalfEdge])] =
      val boundaryEdges = tiling.boundaryEdges
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
    def addSimplePolygonToBoundary(
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
        simple: SimplePolygon
    ): Either[TilingError, TilingDCEL] =
      val either: Either[TilingError, (TilingDCEL, TilingDCEL, Face, Option[OldNewVertexPair])] =
        for
          (startVertex, endVertex, edgeToBuildOn) <-
            tiling.findVerticesAndEdgeBetween(startVertexId, endVertexId)
          points                                   = calculateVertexPoints(simple.toAngles, startVertex.coords, endVertex.coords)
          _                                       <- validatePoints(points)
          result                                  <-
            val containerFace          = edgeToBuildOn.incidentFace.get
            val containerBoundaryEdges = containerFace.halfEdgesUnsafe
            growthWithHoleCheck(
              startVertex,
              endVertex,
              edgeToBuildOn,
              simple.toAngles.toList,
              points,
              containerBoundaryEdges
            )
        yield result

      either match
        case Left(value)                                                    => Left(value)
        case Right((revisedTiling, clone, containerFace, maybeHoleClosure)) =>
          revisedTiling.maybeFilled(clone, containerFace, maybeHoleClosure) match
            case None             => Right(revisedTiling)
            case Some(holeFilled) => holeFilled.addSimplePolygon(startVertexId, endVertexId, simple)

    /** Convenience overload for addSimplePolygon using degrees.
      *
      * See addSimplePolygon(List[AngleDegree]) for full preconditions and postconditions.
      */
    def addSimplePolygon(
        startVertexId: VertexId,
        endVertexId: VertexId,
        degrees: Int*
    ): Either[TilingError, TilingDCEL] =
      addSimplePolygon(startVertexId, endVertexId, SimplePolygon(degrees*))

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
        simple: SimplePolygon
    ): Option[TilingDCEL] =
      for
        (startVertex, endVertex, edgeToBuildOn) <-
          tiling.findVerticesAndEdgeBetween(startVertexId, endVertexId).toOption
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
        startVertexId: VertexId,
        endVertexId: VertexId,
        polygon: RegularPolygon
    ): Either[TilingError, TilingDCEL] =
      val either: Either[TilingError, (TilingDCEL, TilingDCEL, Face, Option[OldNewVertexPair])] =
        for
          (startVertex, endVertex, edgeToBuildOn) <-
            tiling.findVerticesAndEdgeBetween(startVertexId, endVertexId)
          angles                                   = polygon.angles
          points                                   = calculateVertexPoints(angles, startVertex.coords, endVertex.coords)
          result                                  <-
            val containerFace          = edgeToBuildOn.incidentFace.get
            val containerBoundaryEdges = containerFace.halfEdgesUnsafe
            growthWithHoleCheck(
              startVertex,
              endVertex,
              edgeToBuildOn,
              angles.toList,
              points,
              containerBoundaryEdges
            )
        yield result

      either match
        case Left(value)                                                    => Left(value)
        case Right((revisedTiling, clone, containerFace, maybeHoleClosure)) =>
          revisedTiling.maybeFilled(clone, containerFace, maybeHoleClosure) match
            case None             => Right(revisedTiling)
            case Some(holeFilled) => holeFilled.addRegularPolygon(startVertexId, endVertexId, polygon)

    private[dcel] def rawDouble(origin: Vertex, repeat: Vertex, withInversion: Boolean = false): TilingDCEL =
      // Compute the translation vector from origin to repeat
      val modifier: BigPoint => BigPoint            = if withInversion then _.scaled(-1.0) else identity
      val delta                                     = repeat.coords - modifier(origin.coords)
      val coordsTranslation: BigPoint => BigPoint   = modifier(_) + delta
      // Translate vertices: give completely fresh vertex ids
      val vertexIds                                 =
        tiling.vertices.map: vertex =>
          vertex.id
      val maxVertexId                               =
        vertexIds
          .map: vertexId =>
            idFromVertexId(vertexId)
          .maxOption.get
      val vertexIdTranslation: VertexId => VertexId =
        vertexIds.indices
          .map: index =>
            val oldId = vertexIds(index)
            oldId -> vertexIdV(maxVertexId + index + 1)
          .toMap

      // Translate faces: keep outer face id, shift inner ones
      val faceIds                             =
        tiling.faces.map: face =>
          face.id
      val maxFaceId                           =
        faceIds
          .map: faceId =>
            idFromFaceId(faceId)
          .maxOption.get
      val faceIdTranslation: FaceId => FaceId =
        faceIds.indices
          .map: index =>
            faceIds(index) match
              case faceId if idFromFaceId(faceId) == 0 => faceId -> faceId // outer face
              case faceId                              => faceId -> faceIdF(maxFaceId + index)
          .toMap

      // Second copy, translated in space and with fresh ids
      val translated: TilingDCEL =
        tiling.translatedDouble(
          coordsTranslation,
          vertexIdTranslation,
          faceIdTranslation
        )

      // -----------------------------------------------------------------------
      // 1. Identify coincident vertices between original and translated copy
      // -----------------------------------------------------------------------
      import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY

      val sharedVertexPairs: List[(Vertex, Vertex)] =
        tiling.vertices.sameCoords(translated.vertices, accuracy = ACCURACY)

      // Map: "secondary" (translated) vertex id -> "primary" (original) vertex id
      val substitutionMap: Map[VertexId, VertexId] =
        sharedVertexPairs
          .map: (orig, copied) =>
            copied.id -> orig.id
          .toMap

      // -----------------------------------------------------------------------
      // 2. Build merged vertex set by identifying coincident vertices
      // -----------------------------------------------------------------------
      import scala.collection.mutable

      def repOf(id: VertexId): VertexId =
        substitutionMap.getOrElse(id, id)

      val allOldVertices: List[Vertex] =
        tiling.vertices ++ translated.vertices

      // Representative id -> merged Vertex
      val repToVertex = mutable.HashMap.empty[VertexId, Vertex]

      allOldVertices.foreach: vertex =>
        val r = repOf(vertex.id)
        if !repToVertex.contains(r) then
          // Use coordinates of the first encountered vertex in that equivalence class
          repToVertex(r) = Vertex(r, vertex.coords)

      // Every old vertex id maps to its merged vertex
      val newVertexOfId: Map[VertexId, Vertex] =
        allOldVertices
          .map: vertex =>
            vertex.id -> repToVertex(repOf(vertex.id))
          .toMap

      val newVertices: List[Vertex] =
        repToVertex.values.toList

      // -----------------------------------------------------------------------
      // 3. Clone half-edges and faces for the union of the two tilings
      // -----------------------------------------------------------------------
      val allOldEdges: List[HalfEdge] =
        tiling.halfEdges ++ translated.halfEdges

      val allOldFaces: List[Face] =
        // include both outers and inners, but we will rebuild the single outer face later
        (tiling.outerFace :: tiling.innerFaces) ++ (translated.outerFace :: translated.innerFaces)

      val edgeMap = mutable.HashMap.empty[HalfEdge, HalfEdge]
      // 3.a – create new edges with merged origins and copy angles only
      allOldEdges.foreach: halfEdge =>
        val newOrigin = newVertexOfId(halfEdge.origin.id)
        val ne        = HalfEdge(newOrigin)
        ne.angle = halfEdge.angle
        edgeMap(halfEdge) = ne

      // 3.b – create inner faces only (discard old outers, we will recompute boundary)
      val faceMap = mutable.HashMap.empty[Face, Face]
      allOldFaces.foreach: face =>
        if idFromFaceId(face.id) != 0 then
          faceMap.getOrElseUpdate(face, Face(face.id)): Unit

      // 3.c – wire next / prev / incidentFace from old to new via maps
      allOldEdges.foreach:
        oe =>
          val ne = edgeMap(oe)
          oe.next.foreach: on =>
            ne.next = Some(edgeMap(on))
          oe.prev.foreach: op =>
            ne.prev = Some(edgeMap(op))
          oe.incidentFace.foreach:
            of =>
              if idFromFaceId(of.id) != 0 then
                // inner face
                ne.incidentFace = Some(faceMap(of))
              // else: old outer face, leave as boundary (incidentFace = None)

      // 3.d – set outerComponent / innerComponents on inner faces only
      allOldFaces.foreach: of =>
        if idFromFaceId(of.id) != 0 then
          val nf = faceMap(of)
          of.outerComponent.foreach: startOld =>
            nf.outerComponent = Some(edgeMap(startOld))
          nf.innerComponents =
            of.innerComponents.map: maybeHalfEdge =>
              maybeHalfEdge.map: halfEdge =>
                edgeMap(halfEdge)

      // 3.e – (re)wire twins purely by endpoints in the merged graph
      val dirBuckets =
        mutable.HashMap.empty[(VertexId, VertexId), mutable.ArrayBuffer[HalfEdge]]

      edgeMap.values.foreach: e =>
        e.next.foreach: n =>
          val key = (e.origin.id, n.origin.id)
          val buf = dirBuckets.getOrElseUpdate(key, mutable.ArrayBuffer.empty[HalfEdge])
          buf += e

      dirBuckets.foreach:
        case ((o, d), buf) =>
          val oppKey = (d, o)
          dirBuckets.get(oppKey).foreach: oppBuf =>
            val count = math.min(buf.size, oppBuf.size)
            var i     = 0
            while i < count do
              val e1 = buf(i)
              val e2 = oppBuf(i)
              if e1.twin.isEmpty && e2.twin.isEmpty then
                e1.twinWith(e2)
              i += 1

      // -----------------------------------------------------------------------
      // 3.f – collapse duplicated seam edges (same origin/destination)
      // -----------------------------------------------------------------------
      val allNewEdgesInitial: List[HalfEdge] =
        edgeMap.values.toList

      val toRemove = mutable.Set.empty[HalfEdge]

      // Group by undirected key (min(originId, destId), max(originId, destId))
      val byUndirected: Map[(VertexId, VertexId), List[HalfEdge]] =
        allNewEdgesInitial
          .flatMap: e =>
            e.destination.map: d =>
              val oId = e.origin.id
              val dId = d.id
              if oId.value <= dId.value then ((oId, dId), e) else ((dId, oId), e)
          .groupMap((vertexIdPair, _) => vertexIdPair): (_, halfEdge) =>
            halfEdge

      byUndirected.values.foreach: edges =>
        // Partition by direction (origin,dest)
        val byDir = edges.groupBy: e =>
          (e.origin.id, e.destination.get.id)
        if byDir.size >= 2 then
          // One representative per direction, prefer edges with an incident inner face
          val mainPerDir: Map[(VertexId, VertexId), HalfEdge] =
            byDir.view
              .mapValues: sameDirEdges =>
                sameDirEdges
                  .find:
                    _.incidentFace.isDefined
                  .getOrElse(sameDirEdges.head)
              .toMap

          // Wire the two main directions as twins
          val mains = mainPerDir.values.toList
          if mains.size == 2 then
            val a = mains.head
            val b = mains(1)
            a.twinWith(b)

          // All other edges in each direction are redundant; rewire their neighbours to the main
          byDir.foreach:
            case ((origId, destId), sameDirEdges) =>
              if sameDirEdges.size > 1 then
                val main = mainPerDir((origId, destId))
                sameDirEdges.foreach: redundant =>
                  if redundant ne main then
                    // Redirect prev / next around redundant edge to main
                    redundant.prev.foreach: p =>
                      if p.next.contains(redundant) then p.next = Some(main)
                    redundant.next.foreach: n =>
                      if n.prev.contains(redundant) then n.prev = Some(main)
                    toRemove += redundant

      val allNewEdgesNoDuplicates: List[HalfEdge] =
        allNewEdgesInitial.filterNot(toRemove.contains)

      // -----------------------------------------------------------------------
      // 4. Rebuild outer face and assign boundary edges
      // -----------------------------------------------------------------------
      // Boundary edges are those not incident to any inner face
      val boundaryEdges =
        allNewEdgesNoDuplicates.filter:
          _.incidentFace.isEmpty

      val newOuterFace = Face(FaceId.outerId)

      if boundaryEdges.nonEmpty then
        val ordered = boundaryEdges.orderBoundary
        // Link boundary cycle and assign outer face
        ordered.linkInCycle()
        ordered.foreach: e =>
          e.incidentFace = Some(newOuterFace)
        newOuterFace.outerComponent = ordered.headOption
        // Recompute boundary angles from incident inner angles, as in TilingBuilder.setOuterAngles
        ordered.foreach: outerEdge =>
          val vertex         = outerEdge.origin
          val incidentAtV    = allNewEdgesNoDuplicates.filter:
            _.origin eq vertex
          val innerAnglesSum = incidentAtV.interiorAnglesSum(newOuterFace)
          outerEdge.angle = Some(innerAnglesSum.conjugate)

      // -----------------------------------------------------------------------
      // 5. Ensure each merged vertex has a leaving edge
      // -----------------------------------------------------------------------
      val allNewEdges = allNewEdgesNoDuplicates

      newVertices.foreach: vertex =>
        // Prefer a boundary edge as leaving if available, else any incident edge
        val boundaryLeaving = boundaryEdges.find:
          _.origin eq vertex
        val anyLeaving      = allNewEdges.find:
          _.origin eq vertex
        vertex.leaving = boundaryLeaving.orElse(anyLeaving)

      // -----------------------------------------------------------------------
      // 6. Build merged faces lists and validate
      // -----------------------------------------------------------------------
      val newInnerFaces: List[Face] =
        faceMap.values.toList

      //          TilingDCEL.fromUntrusted(
      //            vertices = newVertices,
      //            halfEdges = allNewEdges,
      //            innerFaces = newInnerFaces,
      //            outerFace = newOuterFace
      //          )
      TilingDCEL(newVertices, allNewEdges, newInnerFaces, newOuterFace)

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
  private[dcel] def findHoleClosure(
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

//    println(s"startVertex: $startVertex")
//    println(s"newVertices: $newVertices")
//    println(s"sharedVertices: $sharedVertices")

    if sharedVertices.isEmpty then
      return None

//    // Get the indices of the new vertices that are shared
//    val newVertexIndices = sharedVertices.map: pair =>
//      newVertices.indexOf(pair.newVertex)

    val oldVertexIndices = sharedVertices.map: pair =>
      boundaryVertices.indexOf(pair.oldVertex)

    val forwardContiguousCount2 =
      oldVertexIndices.zip(oldVertexIndices.tail)
        .takeWhile: (a, b) =>
          a + 1 == b
        .length
    val endOfFirstBlock2 = sharedVertices(forwardContiguousCount2)

//    // Find the last vertex from the initial contiguous block of shared vertices
//    val forwardContiguousCount =
//      newVertexIndices.zip(newVertexIndices.tail)
//        .takeWhile: (a, b) =>
//          a + 1 == b
//        .length
//    val endOfFirstBlock        = sharedVertices(forwardContiguousCount)
//    println(s"endOfFirstBlock: $endOfFirstBlock")
//    println(s"endOfFirstBlock2: $endOfFirstBlock2")

    // Find the first vertex from the final contiguous block of shared vertices
    val backwardContiguousCount2 =
      oldVertexIndices.reverse.zip(oldVertexIndices.reverse.tail)
        .takeWhile: (a, b) =>
          a - 1 == b
        .length
    val startOfLastBlock2 = sharedVertices(sharedVertices.length - 1 - backwardContiguousCount2)

//    // Find the first vertex from the final contiguous block of shared vertices
//    val backwardContiguousCount =
//      newVertexIndices.reverse.zip(newVertexIndices.reverse.tail)
//        .takeWhile: (a, b) =>
//          a - 1 == b
//        .length
//    val startOfLastBlock        = sharedVertices(sharedVertices.length - 1 - backwardContiguousCount)
//    println(s"startOfLastBlock: $startOfLastBlock")
//    println(s"startOfLastBlock2: $startOfLastBlock2")

//    val boundaryVertices = boundaryEdges.map(_.origin)
//    val startIndex = boundaryVertices.indexOf(startVertex)
////        println(s"x: $startIndex")
//    val found =
//      boundaryVertices.indices.find(i => sharedVertices.map(_._1).contains(boundaryVertices.applyO(i + startIndex))).get
////        println(s"found: $found")
//    val isReversed: Boolean =
//      sharedVertices.size > 2 &&
//        startOfLastBlock._1 != boundaryVertices(found) && endOfFirstBlock._1 != boundaryVertices(found)
//    println(s"isReversed: $isReversed")
////        if isReversed then
////          throw new RuntimeException("This is going to fail")

    // Determine which closure point results in a smaller path on the boundary
    def shortestBoundaryPathLength(to: Vertex): Int =
      val pathLength = boundaryEdges.getPath(from = startVertex, to = to).length
      math.min(pathLength, boundaryEdges.length - pathLength)

    val forwardPathLength  = shortestBoundaryPathLength(endOfFirstBlock2.oldVertex)
    val backwardPathLength = shortestBoundaryPathLength(startOfLastBlock2.oldVertex)
//    println(s"forwardPathLength: $forwardPathLength, backwardPathLength: $backwardPathLength")

    if forwardPathLength < backwardPathLength then
      Some(endOfFirstBlock2)
    else
      Some(startOfLastBlock2)

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
    sharedEdges.zipWithIndex.foreach: (edge, index) =>
      edge.angle = Some(polyAngles(index))

    // Update last boundary edge angle
//    newBoundaryEdges.lastOption.foreach(_.angle = Some(boundaryAngles.newVertices.head.conjugate))

    // Update the existing boundary edge from end vertex
    val endVertexId =
      sharedEdges.last.destination
        .map: vertex =>
          vertex.id
        .get
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
