package io.github.scala_tessella
package dcel

import BigDecimalGeometry.*
import Polygon.{RegularPolygon, SimplePolygon}
import TilingBuilder.{calculateVertexPoints, validatePoints, validateSides}
import TilingEquivalency.*

import ring_seq.RingSeq.{rotateRight, slidingO}

import scala.annotation.tailrec

object TilingAddition:

  def calculateNewVertices(sides: Int, p1: BigPoint, p2: BigPoint): List[BigPoint] =
    val angle = RegularPolygon(sides).alpha.conjugate
    calculateVertexPoints(List.fill(sides)(angle), p1, p2).drop(2)

  private def createVertices(points: List[BigPoint], startingIndex: Int): List[Vertex] =
    points.zipWithIndex.map { (point, index) =>
      Vertex(s"V${startingIndex + index}", point)
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
    val currentInteriorSum = vertex.getCurrentInteriorAngleSum(outerFace)
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
    val boundaryAngles = startVertexAngle +: subsequentBoundaryAngles

    edgeVertices.lazyZip(boundaryAngles).lazyZip(polygonAngles).map {
      case ((origin, destination), boundaryAngle, innerAngle) =>
        HalfEdge.createTwinHalfEdges(
          origin, destination, outerFace, newFace,
          boundaryAngle, innerAngle
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
  )(using outerFace: Face): Either[String, SharedEdgesResult] =

    @tailrec
    def traverse(
      edge: HalfEdge,
      check: AngleDegree,
      angles: List[AngleDegree],
      acc: List[HalfEdge],
      getNext: HalfEdge => HalfEdge,
      getVertex: HalfEdge => Vertex
    ): Either[String, (List[HalfEdge], AngleDegree, HalfEdge)] =
      if check.toRational < 0 then Left("Angle wider than container")
      else if !check.isFullCircle then Right((acc, check, edge))
      else if angles.isEmpty then Left("Same as container")
      else
        val nextCheck = boundaryAngleForVertex(getVertex(edge), outerFace, angles.head)
        traverse(getNext(edge), nextCheck, angles.tail, edge :: acc, getNext, getVertex)

    for
      (prepended, startCheck, startEdge) <- traverse(edgeToBuildOn.prev.get, boundaryAngles.start, boundaryAngles.newVertices.reverse, Nil, _.prev.get, _.origin)
      (appended, endCheck, endEdge) <- traverse(edgeToBuildOn.next.get, boundaryAngles.end, boundaryAngles.newVertices, Nil, _.next.get, _.destination.get)
    yield
      SharedEdgesResult(
        sharedEdges = prepended ::: edgeToBuildOn :: appended.reverse,
        startCheck = startCheck,
        startEdge = startEdge,
        startCounter = prepended.length,
        endCheck = endCheck,
        endEdge = endEdge,
        endCounter = appended.length
      )

  extension (tiling: TilingDCEL)

    private def nextFaceId: String =
      "F" + (tiling.innerFaces.map(_.id.tail.toInt).max + 1).toString

    private def nextVertexIndex: Int =
      tiling.vertices.map(_.id.tail.toInt).max + 1

    private def growthWithHoleCheck(
      startVertex: Vertex,
      endVertex: Vertex,
      edgeToBuildOn: HalfEdge,
      angles: List[AngleDegree],
      points: List[BigPoint],
      boundaryEdges: List[HalfEdge]
    ): Either[String, (TilingDCEL, TilingDCEL, Option[(Vertex, Vertex)])] =
      val innerFace = edgeToBuildOn.incidentFace.get

      additionalVertices(startVertex, endVertex, edgeToBuildOn, angles, points, tiling.nextVertexIndex, innerFace) match
        case Left(value) => Left(value)
        case Right((tempVertices, edgeResults, boundaryAngles)) =>

          val adjustedTempVertices =
            edgeResults.startEdge.destination.get :: tempVertices.drop(edgeResults.startCounter) ::: List(edgeResults.endEdge.origin)

          // here we must check if the new boundary intersects with the existing one
          val newSides: List[BigLineSegment] =
            adjustedTempVertices.sliding(2).toList.map {
              (_: @unchecked) match
                case p1 :: p2 :: Nil => BigLineSegment(p1.coords, p2.coords)
            }

          val newBox =
            BigBox.fromPoints(adjustedTempVertices.map(_.coords)).expand(1)

          val oldSides: List[BigLineSegment] =
            boundaryEdges.slidingO(2).toList.map {
              case e1 :: e2 :: Nil => BigLineSegment(e1.origin.coords, e2.origin.coords)
              case _ => BigLineSegment(BigPoint(), BigPoint())
            }.filter(line => newBox.contains(line.p1) || newBox.contains(line.p2))

          val hasIntersection =
            oldSides.exists(oldSide => newSides.exists(newSide => oldSide.properlyIntersects(newSide)))

          if hasIntersection then
            return Left("Boundary intersection")

          val maybeHoleClosure: Option[(Vertex, Vertex)] =
            findHoleClosure(startVertex, boundaryEdges, tempVertices)

          val deepCopiedOriginal: TilingDCEL =
            if maybeHoleClosure.isDefined then tiling.deepCopy
            else TilingDCEL.empty

          val (newVertices, newHalfEdges, newFace) =
            additionalElements(edgeToBuildOn, angles, tiling.nextFaceId, tempVertices, edgeResults, boundaryAngles)

          // Return new DCEL with updated components
          val grownTiling =
            tiling.copy(
              vertices = tiling.vertices ::: newVertices,
              halfEdges = tiling.halfEdges ::: newHalfEdges,
              innerFaces = tiling.innerFaces :+ newFace
            )
          Right((grownTiling, deepCopiedOriginal, maybeHoleClosure))

    /** Calculates the angles needed to fill a hole in the tiling's boundary,
     *  determining the correct starting vertex and direction for the new polygon.
     *
     * @param v_match the existing vertex closing the hole
     * @param v_new   the new vertex closing the hole
     */
    private def holeAnglesWithDirection(v_match: Vertex, v_new: Vertex): (List[AngleDegree], String, String) =
      val matchEdge = tiling.halfEdges.find(_.origin.id == v_match.id).get
      val face = matchEdge.incidentFace.get
      val boundaryEdges = face.halfEdgesSafe

      // 1. Determine the shorter path (the "hole") on the boundary between the two vertices.
      val pathFwd = boundaryEdges.getPath(from = v_match, to = v_new)
      val pathBack = boundaryEdges.getPath(from = v_new, to = v_match)

      val (holePath, isForward) =
        if pathFwd.sizeCompare(pathBack) < 0 then (pathFwd, true)
        else (pathBack, false)

      // 2. Calculate the internal angles for a new polygon that would fill this hole.
      val holeAngles = holePath.map(_.angle.get)
      val polygonAngles =
        val sumOfOtherAngles = holeAngles.tail.sum2
        val closingAngle = SimplePolygon.alphaSum(holeAngles.length) - sumOfOtherAngles
        closingAngle :: holeAngles.tail

      // 3. Determine the starting vertex and adjust angle order based on the path direction.
      if isForward then
        (polygonAngles, v_match.id, holePath.head.destination.get.id)
      else
        // For a backward path, the angles must be rotated, and the start vertex is different.
        val lastEdge = holePath.last
        (polygonAngles.rotateRight(1), lastEdge.origin.id, lastEdge.destination.get.id)

    def addSimplePolygonToBoundary(onEdgeStartingWithVertexId: String, angles: List[AngleDegree]): Either[String, TilingDCEL] =
      for
        _ <- validateSides(angles.length, "simple")
        boundaryEdges <- tiling.getBoundaryEdges
        edgeToBuildOn <- boundaryEdges
          .find(_.origin.id == onEdgeStartingWithVertexId)
          .toRight(s"Edge starting with vertex $onEdgeStartingWithVertexId not found on the boundary.")
        (startVertex, endVertex) <- edgeToBuildOn.endpointsAsVertices
          .toRight("Edge has no destination vertex.")
        result <- addSimplePolygon(startVertex.id, endVertex.id, angles)
      yield
        result

    def addSimplePolygonToBoundary(onEdgeStartingWithVertexId: String, degrees: Int *): Either[String, TilingDCEL] =
      addSimplePolygonToBoundary(onEdgeStartingWithVertexId, degrees.map(AngleDegree(_)).toList)

    private def addSimplePolygonToBoundaryWithoutGuards(onEdgeStartingWithVertexId: String, angles: List[AngleDegree]): Option[TilingDCEL] =
      for
        boundaryEdges <- tiling.getBoundaryEdges.toOption
        edgeToBuildOn <- boundaryEdges.find(_.origin.id == onEdgeStartingWithVertexId)
        (startVertex, endVertex) <- edgeToBuildOn.endpointsAsVertices
        result <- addSimplePolygonWithoutGuards(startVertex.id, endVertex.id, angles)
      yield
        result
//        val (tempVertices, edgeResults, boundaryAngles) =
//          additionalVertices(startVertex, endVertex, edgeToBuildOn, angles, points, tiling.nextVertexIndex, tiling.outerFace)
//
//        val (newVertices, newHalfEdges, newFace) =
//          additionalElements(edgeToBuildOn, angles, tiling.nextFaceId, tempVertices, edgeResults, boundaryAngles)
//
//        // Return new DCEL with updated components
//        tiling.copy(
//          vertices = tiling.vertices ::: newVertices,
//          halfEdges = tiling.halfEdges ::: newHalfEdges,
//          innerFaces = tiling.innerFaces :+ newFace
//        )

//    @tailrec
    def addRegularPolygonToBoundary(onEdgeStartingWithVertexId: String, sides: Int): Either[String, TilingDCEL] =
      for
        _ <- validateSides(sides, "regular")
        boundaryEdges <- tiling.getBoundaryEdges
        edgeToBuildOn <- boundaryEdges
          .find(_.origin.id == onEdgeStartingWithVertexId)
          .toRight(s"Edge starting with vertex $onEdgeStartingWithVertexId not found on the boundary.")
        (startVertex, endVertex) <- edgeToBuildOn.endpointsAsVertices
          .toRight("Edge has no destination vertex.")
        result <- addRegularPolygon(startVertex.id, endVertex.id, sides)
      yield
        result

//        val either: Either[String, (TilingDCEL, TilingDCEL, Option[(Vertex, Vertex)])] =
//        for
//          _                        <- validateSides(sides, "regular")
//          boundaryEdges            <- tiling.getBoundaryEdges
//          edgeToBuildOn            <- boundaryEdges
//            .find(_.origin.id == onEdgeStartingWithVertexId)
//            .toRight(s"Edge starting with vertex $onEdgeStartingWithVertexId not found on the boundary.")
//          (startVertex, endVertex) <- edgeToBuildOn.endpointsAsVertices
//            .toRight("Edge has no destination vertex.")
//          polyAngle = polygonAngle(sides)
//          angles = List.fill(sides)(polyAngle)
//          points = calculateVertexPoints(angles, startVertex.coords, endVertex.coords)
//          result <- growthWithHoleCheck(startVertex, endVertex, edgeToBuildOn, angles, points, boundaryEdges)
//        yield
//          result
//
//      either match
//        case Left(value) => Left(value)
//        case Right((revisedTiling, clone, maybeHoleClosure)) =>
//          maybeHoleClosure match
//            case None => Right(revisedTiling)
//            case Some((v_match, v_new)) =>
//              val (holeAngles, startingVertexId, _) =
//                revisedTiling.holeAnglesWithDirection(v_match, v_new)
//              clone.addSimplePolygonToBoundaryWithoutGuards(startingVertexId, holeAngles).get
//                .addRegularPolygonToBoundary(onEdgeStartingWithVertexId, sides)

    @tailrec
    def addSimplePolygon(startVertexId: String, endVertexId: String, angles: List[AngleDegree]): Either[String, TilingDCEL] =
      val either: Either[String, (TilingDCEL, TilingDCEL, Option[(Vertex, Vertex)])] =
        for
          (startVertex, endVertex, edgeToBuildOn) <- tiling.findVerticesAndEdgeBetween(startVertexId, endVertexId)
          _      <- validateSides(angles.length, "simple")
          _      <- SimplePolygon.validatePolygonAngles(angles)
          points = calculateVertexPoints(angles, startVertex.coords, endVertex.coords)
          _      <- validatePoints(points)
          result <-
            val containerFace = edgeToBuildOn.incidentFace.get
            println(s"face: $containerFace")
            val containerBoundaryEdges = containerFace.halfEdgesSafe
            println(s"containerBoundaryEdges: $containerBoundaryEdges")
            growthWithHoleCheck(startVertex, endVertex, edgeToBuildOn, angles, points, containerBoundaryEdges)
        yield
          result

      either match
        case Left(value) => Left(value)
        case Right((revisedTiling, clone, maybeHoleClosure)) =>
          maybeHoleClosure match
            case None => Right(revisedTiling)
            case Some((v_match, v_new)) =>
              val (holeAngles, startingVertexId, _) =
                revisedTiling.holeAnglesWithDirection(v_match, v_new)
              clone.addSimplePolygonToBoundaryWithoutGuards(startingVertexId, holeAngles).get
                .addSimplePolygon(startVertexId, endVertexId, angles)

    def addSimplePolygon(startVertexId: String, endVertexId: String, degrees: Int *): Either[String, TilingDCEL] =
      addSimplePolygon(startVertexId, endVertexId, degrees.map(AngleDegree(_)).toList)

    private def addSimplePolygonWithoutGuards(startVertexId: String, endVertexId: String, angles: List[AngleDegree]): Option[TilingDCEL] =
      for
        (startVertex, endVertex, edgeToBuildOn) <- tiling.findVerticesAndEdgeBetween(startVertexId, endVertexId).toOption
        points = calculateVertexPoints(angles, startVertex.coords, endVertex.coords)
      yield
        val containerFace = edgeToBuildOn.incidentFace.get
//        val containerBoundaryEdges = containerFace.halfEdgesSafe

        val (tempVertices, edgeResults, boundaryAngles) =
          additionalVertices(startVertex, endVertex, edgeToBuildOn, angles, points, tiling.nextVertexIndex, containerFace).toOption.get

        val (newVertices, newHalfEdges, newFace) =
          additionalElements(edgeToBuildOn, angles, tiling.nextFaceId, tempVertices, edgeResults, boundaryAngles)

        // Return new DCEL with updated components
        tiling.copy(
          vertices = tiling.vertices ::: newVertices,
          halfEdges = tiling.halfEdges ::: newHalfEdges,
          innerFaces = tiling.innerFaces :+ newFace
        )

    @tailrec
    def addRegularPolygon(startVertexId: String, endVertexId: String, sides: Int): Either[String, TilingDCEL] =
      val either: Either[String, (TilingDCEL, TilingDCEL, Option[(Vertex, Vertex)])] =
        for
          (startVertex, endVertex, edgeToBuildOn) <- tiling.findVerticesAndEdgeBetween(startVertexId, endVertexId)
          _  <- validateSides(sides, "regular")
          polyAngle = polygonAngle(sides)
          angles = List.fill(sides)(polyAngle)
          points = calculateVertexPoints(angles, startVertex.coords, endVertex.coords)
          result <-
//            if tiling.isBoundaryEdge(edgeToBuildOn) then
//              addRegularPolygonToBoundary(startVertexId, sides).map((_, TilingDCEL.empty, None))
//            else
//              // @todo it could work also with a "bottleneck" edge, that is with both vertices on the boundary, but the inner angles at vertex should be split
//  //            val boundaryVertices = tiling.boundary
//  //            // if both vertices belong to the boundary, either the edge is the twin of a boundary edge or is a "bottleneck"
//  //            val hasBothVerticesOnBoundary = boundaryVertices.contains(v1) && boundaryVertices.contains(v2)
//              val hasEnclosingStart =
//                tiling.isBoundaryEdge(edgeToBuildOn.twin.get)
//                  && tiling.getInnerAnglesAtVertex(startVertexId).toOption.get.sum2.toRational < polyAngle.toRational
//              if hasEnclosingStart then
//                val hasEnclosingEnd =
//                  tiling.getInnerAnglesAtVertex(endVertexId).toOption.get.sum2.toRational < polyAngle.toRational
//                if !hasEnclosingEnd then
//                  Left("Polygon would be drawn inside the face")
//                else
//                  val boundaryAnglesFromVertex = tiling.getBoundaryEdgesPath(startVertex, startVertex).map(_.angle.get)
//                  val first = polyAngle - boundaryAnglesFromVertex.head.conjugate
//                  val last = polyAngle - boundaryAnglesFromVertex.last.conjugate
//                  val simplePolygonAngles = first :: boundaryAnglesFromVertex.tail.init ::: (last :: List.fill(sides - 2)(polyAngle))
//                  addSimplePolygonToBoundary(startVertexId, simplePolygonAngles) match
//                    case Left(message) if message.startsWith("The polygon is not simple") =>
//                      Left("The polygon is touching other boundary edges.")
//                    case either => either.map((_, TilingDCEL.empty, None))
//              else
                val innerFace = edgeToBuildOn.incidentFace.get
                println(s"face: $innerFace")
                val faceEdges = innerFace.halfEdgesSafe
                println(s"faceEdges: $faceEdges")
                growthWithHoleCheck(startVertex, endVertex, edgeToBuildOn, angles, points, faceEdges)

        yield
          result

      either match
        case Left (value) => Left(value)
        case Right ((revisedTiling, clone, maybeHoleClosure)) =>
          maybeHoleClosure match
            case None => Right(revisedTiling)
            case Some((v_match, v_new)) =>
              val (holeAngles, startingVertexId, endingVertexId) =
                revisedTiling.holeAnglesWithDirection(v_match, v_new)
              clone.addSimplePolygonWithoutGuards(startingVertexId, endingVertexId, holeAngles).get
                .addRegularPolygon(startVertexId, endVertexId, sides)

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
  ): Either[String, (List[Vertex], SharedEdgesResult, BoundaryAngles)] =
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
      vertexPoints = points.drop(2).reverse
      revisedVertexPoints = vertexPoints.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)
      newVertices = createVertices(revisedVertexPoints, vertexIndex)
    yield
      (newVertices, edgesResult, boundaryAngles)

  /** Finds a couple of vertices from the existing and the additional boundary sharing the same coords
   *  and thus marking a hole
   *
   * @param startVertex the boundary vertex from where the new vertices are added
   * @param boundaryEdges the edges forming the tiling's boundary
   * @param newVertices the added vertices
   */
  private def findHoleClosure(
    startVertex: Vertex,
    boundaryEdges: List[HalfEdge],
    newVertices: List[Vertex]
  ): Option[(Vertex, Vertex)] =
    // Find pairs of vertices (one from boundary, one new) that share coordinates
    val sharedVertices = newVertices.sameCoords(boundaryEdges.map(_.origin)).map(_.swap)

    sharedVertices match
      case Nil => None
      case many =>
        // Get the indices of the new vertices that are shared
        val indices = many.map(p => newVertices.indexOf(p._2))

        // Find the last vertex from the initial contiguous block of shared vertices
        val forwardContiguousCount = indices.zip(indices.tail)
          .takeWhile { case (a, b) => a + 1 == b }
          .length
        val endOfFirstBlock = many(forwardContiguousCount)

        // Find the first vertex from the final contiguous block of shared vertices
        val backwardContiguousCount = indices.reverse.zip(indices.reverse.tail)
          .takeWhile { case (a, b) => a - 1 == b }
          .length
        val startOfLastBlock = many(many.length - 1 - backwardContiguousCount)

        // Determine which closure point results in a smaller path on the boundary
        def shortestPathLength(to: Vertex): Int =
          val pathLength = boundaryEdges.getPath(from = startVertex, to = to).length
          math.min(pathLength, boundaryEdges.length - pathLength)

        val forwardPathLength = shortestPathLength(endOfFirstBlock._1)
        val backwardPathLength = shortestPathLength(startOfLastBlock._1)

        if forwardPathLength < backwardPathLength then
          Some(endOfFirstBlock)
        else
          Some(startOfLastBlock)

  private def additionalElements(
    edgeToBuildOn: HalfEdge,
    angles: List[AngleDegree],
    newFaceId: String,
    newVertices: List[Vertex],
    edgesResult: SharedEdgesResult,
    boundaryAngles: BoundaryAngles
  ): (List[Vertex], List[HalfEdge], Face) =
    val innerFace = edgeToBuildOn.incidentFace.get

    given outerFace: Face = innerFace

    val newFace = Face(newFaceId)

    // Different start and end vertex
    val revisedStartVertex = edgesResult.startEdge.destination.get
    val revisedEndVertex = edgesResult.endEdge.origin

    // Different boundary angles
    val revisedBoundaryAngles = BoundaryAngles(
      start = edgesResult.startCheck,
      end = edgesResult.endCheck,
      newVertices = boundaryAngles.newVertices.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)
    )

    // Different boundary
    val completeBoundary = BoundaryState(Some(edgesResult.startEdge), Some(edgesResult.endEdge))

    val allVertices = revisedStartVertex :: newVertices ::: revisedEndVertex :: Nil

    val revisedAngles = angles.reverse.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)

    val edgePairs = createEdgePairs(allVertices, outerFace, newFace, revisedBoundaryAngles.start, revisedAngles)
    val (newBoundaryEdges, newInnerEdges) = edgePairs.unzip

    val sharedAngles = angles.takeRight(edgesResult.startCounter) ++ angles.take(edgesResult.endCounter + 1)
    // Update existing structures
    updateExistingStructures(
      edgesResult.sharedEdges, newFace, sharedAngles,
      newBoundaryEdges, completeBoundary, revisedBoundaryAngles
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
    newBoundaryEdges: List[HalfEdge],
  ): Unit =
    verticesWithNewEdges.zip(newBoundaryEdges).foreach { (vertex, edge) =>
      vertex.leaving = Some(edge)
    }
