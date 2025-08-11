package io.github.scala_tessella
package dcel

import BigDecimalGeometry.*
import Polygon.{RegularPolygon, SimplePolygon}
import TilingBuilder.{calculateVertexPoints, validatePoints, validateSides}

import scala.annotation.tailrec

object TilingAddition:

  def calculateNewVertices(sides: Int, p1: BigPoint, p2: BigPoint): List[BigPoint] =
    val angle = RegularPolygon(sides).alpha.conjugate
    calculateVertexPoints(List.fill(sides)(angle), p1, p2).drop(2)

  private def createVertices(points: List[BigPoint], startingIndex: Int): List[Vertex] =
    points.zipWithIndex.map { (point, index) =>
      Vertex(s"V${startingIndex + index + 1}", point)
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
    val size = polygonAngles.length
    vertices.sliding(2).zipWithIndex.map {
      case (origin :: destination :: Nil, index) =>
        val shiftedIndex = (index - 1) % size
        val boundaryAngle = if index == 0 then startVertexAngle else polygonAngles(shiftedIndex).conjugate
        HalfEdge.createTwinHalfEdges(
          origin, destination, outerFace, newFace,
          boundaryAngle, polygonAngles(index)
        )
      case _ =>
        throw IllegalArgumentException("Invalid vertex sequence")
    }.toList

  // Simplify face generation using template interpolation
  private def generateFaceId(existingFaceCount: Int): String =
    s"F${existingFaceCount + 1}"

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
  )(using outerFace: Face): SharedEdgesResult =

    @tailrec
    def traverse(
      edge: HalfEdge,
      check: AngleDegree,
      angles: List[AngleDegree],
      acc: List[HalfEdge],
      getNext: HalfEdge => HalfEdge,
      getVertex: HalfEdge => Vertex
    ): (List[HalfEdge], AngleDegree, HalfEdge) =
      if !check.isFullCircle then (acc, check, edge)
      else
        val nextCheck = boundaryAngleForVertex(getVertex(edge), outerFace, angles.head)
        traverse(getNext(edge), nextCheck, angles.tail, edge :: acc, getNext, getVertex)

    val (prepended, startCheck, startEdge) =
      traverse(edgeToBuildOn.prev.get, boundaryAngles.start, boundaryAngles.newVertices.reverse, Nil, _.prev.get, _.origin)

    val (appended, endCheck, endEdge) =
      traverse(edgeToBuildOn.next.get, boundaryAngles.end, boundaryAngles.newVertices, Nil, _.next.get, _.destination.get)

    SharedEdgesResult(
      sharedEdges = prepended ::: edgeToBuildOn :: appended.reverse,
      startCheck = startCheck,
      startEdge = startEdge,
      startCounter = prepended.length,
      endCheck = endCheck,
      endEdge = endEdge,
      endCounter = appended.length
    )

  extension (tilingDCEL: TilingDCEL)

    private def holeAngles(v_match: Vertex, v_new: Vertex) =
      val boundaryEdgesAroundHole =
        tilingDCEL.getBoundaryEdgesPath(from = v_match, to = v_new)
      val boundaryAnglesAroundHole =
        boundaryEdgesAroundHole.map(_.angle.get)
      (
        SimplePolygon.alphaSum(boundaryAnglesAroundHole.length)
          - boundaryAnglesAroundHole.tail.fold(AngleDegree(0))(_ + _)
      ) :: boundaryAnglesAroundHole.tail

    @tailrec
    def addSimplePolygon(angles: List[AngleDegree], onEdgeStartingWithVertexId: String): Either[String, TilingDCEL] =
      val either: Either[String, (TilingDCEL, TilingDCEL, Option[(Vertex, Vertex)])] =
        for
          _      <- validateSides(angles.length, "simple")
          _      <- SimplePolygon.validatePolygonAngles(angles)
          boundaryEdges <- tilingDCEL.getBoundaryEdges
          edgeToBuildOn <- boundaryEdges
            .find (_.origin.id == onEdgeStartingWithVertexId)
            .toRight (s"Edge starting with vertex $onEdgeStartingWithVertexId not found on the boundary.")
          (startVertex, endVertex) <- edgeToBuildOn.endpointsAsVertices
            .toRight("Edge has no destination vertex.")
          points = calculateVertexPoints(angles, startVertex.coords, endVertex.coords)
          _      <- validatePoints(points)
        yield

          val (tempVertices, edgeResults, boundaryAngles) =
            additionalVertices(startVertex, endVertex, edgeToBuildOn, angles, points, tilingDCEL.vertices.size, tilingDCEL.outerFace)

          val maybeHoleClosure: Option[(Vertex, Vertex)] =
            findHoleClosure(boundaryEdges,tempVertices)

          val clone: TilingDCEL =
            if maybeHoleClosure.isDefined then tilingDCEL.deepCopy
            else TilingDCEL.empty

          val (newVertices, newHalfEdges, newFace) =
            additionalElements(edgeToBuildOn, angles, tilingDCEL.innerFaces.size, tilingDCEL.outerFace, tempVertices, edgeResults, boundaryAngles)

          // Return new DCEL with updated components
          val revisedTiling =
            tilingDCEL.copy(
              vertices = tilingDCEL.vertices ::: newVertices,
              halfEdges = tilingDCEL.halfEdges ::: newHalfEdges,
              innerFaces = tilingDCEL.innerFaces :+ newFace
            )

          (revisedTiling, clone, maybeHoleClosure)

      either match
        case Left(value) => Left(value)
        case Right((revisedTiling, clone, maybeHoleClosure)) =>
          maybeHoleClosure match
            case None => Right(revisedTiling)
            case Some((v_match, v_new)) =>
              val holeAngles =
                tilingDCEL.holeAngles(v_match, v_new)
              clone.addSimplePolygonWithoutGuards(holeAngles, v_match.id).get
                .addSimplePolygon(angles, onEdgeStartingWithVertexId)

    private def addSimplePolygonWithoutGuards(angles: List[AngleDegree], onEdgeStartingWithVertexId: String): Option[TilingDCEL] =
      for
        boundaryEdges <- tilingDCEL.getBoundaryEdges.toOption
        edgeToBuildOn <- boundaryEdges.find(_.origin.id == onEdgeStartingWithVertexId)
        (startVertex, endVertex) <- edgeToBuildOn.endpointsAsVertices
        points = calculateVertexPoints(angles, startVertex.coords, endVertex.coords)
      yield

        val (tempVertices, edgeResults, boundaryAngles) =
          additionalVertices(startVertex, endVertex, edgeToBuildOn, angles, points, tilingDCEL.vertices.size, tilingDCEL.outerFace)

        val (newVertices, newHalfEdges, newFace) =
          additionalElements(edgeToBuildOn, angles, tilingDCEL.innerFaces.size, tilingDCEL.outerFace, tempVertices, edgeResults, boundaryAngles)

        // Return new DCEL with updated components
        tilingDCEL.copy(
          vertices = tilingDCEL.vertices ::: newVertices,
          halfEdges = tilingDCEL.halfEdges ::: newHalfEdges,
          innerFaces = tilingDCEL.innerFaces :+ newFace
        )

    @tailrec
    def addRegularPolygon(sides: Int, onEdgeStartingWithVertexId: String): Either[String, TilingDCEL] =
      val either: Either[String, (TilingDCEL, TilingDCEL, Option[(Vertex, Vertex)])] =
        for
          _                        <- validateSides(sides, "regular")
          boundaryEdges            <- tilingDCEL.getBoundaryEdges
          edgeToBuildOn            <- boundaryEdges
            .find(_.origin.id == onEdgeStartingWithVertexId)
            .toRight(s"Edge starting with vertex $onEdgeStartingWithVertexId not found on the boundary.")
          (startVertex, endVertex) <- edgeToBuildOn.endpointsAsVertices
            .toRight("Edge has no destination vertex.")
        yield

          val polyAngle = polygonAngle(sides)
          val angles = List.fill(sides)(polyAngle)
          val points = calculateVertexPoints(angles, startVertex.coords, endVertex.coords)

          val (tempVertices, edgeResults, boundaryAngles) =
            additionalVertices(startVertex, endVertex, edgeToBuildOn, angles, points, tilingDCEL.vertices.size, tilingDCEL.outerFace)

          val maybeHoleClosure: Option[(Vertex, Vertex)] =
            findHoleClosure(boundaryEdges,tempVertices)

          val clone: TilingDCEL =
            if maybeHoleClosure.isDefined then tilingDCEL.deepCopy
            else TilingDCEL.empty

          val (newVertices, newHalfEdges, newFace) =
            additionalElements(edgeToBuildOn, angles, tilingDCEL.innerFaces.size, tilingDCEL.outerFace, tempVertices, edgeResults, boundaryAngles)

          // Return new DCEL with updated components
          val revisedTiling =
            tilingDCEL.copy(
              vertices = tilingDCEL.vertices ::: newVertices,
              halfEdges = tilingDCEL.halfEdges ::: newHalfEdges,
              innerFaces = tilingDCEL.innerFaces :+ newFace
            )
          (revisedTiling, clone, maybeHoleClosure)

      either match
        case Left(value) => Left(value)
        case Right((revisedTiling, clone, maybeHoleClosure)) =>
          maybeHoleClosure match
            case None => Right(revisedTiling)
            case Some((v_match, v_new)) =>
              val holeAngles =
                tilingDCEL.holeAngles(v_match, v_new)
              clone.addSimplePolygonWithoutGuards(holeAngles, v_match.id).get
                .addRegularPolygon(sides, onEdgeStartingWithVertexId)

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
    verticesSize: Int,
    outer: Face
  ): (List[Vertex], SharedEdgesResult, BoundaryAngles) =
    given outerFace: Face = outer

    // Calculate boundary angles
    val boundaryAngles = BoundaryAngles(
      start = boundaryAngleForVertex(startVertex, outerFace, angles.head),
      end = boundaryAngleForVertex(endVertex, outerFace, angles(1)),
      newVertices = angles.drop(2)
    )

    val edgesResult = findSharedEdges(edgeToBuildOn, boundaryAngles)

    // Create new components
    val vertexPoints = points.drop(2).reverse
    val revisedVertexPoints = vertexPoints.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)
    val newVertices = createVertices(revisedVertexPoints, verticesSize)
    (newVertices, edgesResult, boundaryAngles)

  /** Finds a couple of vertices from the existing and the additional boundary sharing the same coords
   *  and thus marking a hole
   *
   * @param boundaryEdges the edges forming the tiling's boundary
   * @param newVertices the added vertices
   */
  private def findHoleClosure(boundaryEdges: List[HalfEdge], newVertices: List[Vertex]): Option[(Vertex, Vertex)] =
    boundaryEdges.map(_.origin).sameCoords(newVertices) match
      case Nil => None
      case one :: Nil =>
        println(s"Warning: one shared vertex found, ${one._2} at place where ${one._1} already is.")
        Some(one)
      case _ :: two :: Nil =>
        println("Warning: two shared vertices found")
        Some(two)
      case _ =>
        throw new Error("Error: more than 2 shared vertices found")

  private def additionalElements(
    edgeToBuildOn: HalfEdge,
    angles: List[AngleDegree],
    innerFacesSize: Int,
    outer: Face,
    newVertices: List[Vertex],
    edgesResult: SharedEdgesResult,
    boundaryAngles: BoundaryAngles
  ): (List[Vertex], List[HalfEdge], Face) =
    given outerFace: Face = outer

    val newFace = Face(generateFaceId(innerFacesSize))

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

    // Update existing boundary edge from end vertex
    originalBoundary.next.foreach { nextEdge =>
      if nextEdge.origin.id == sharedEdges.last.destination.map(_.id).getOrElse("") then
        nextEdge.angle = Some(boundaryAngles.end)
    }

    // Update boundary in special shared edges case
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

    // Update outer face component if necessary
    if outerFace.outerComponent.exists(e => sharedEdges.contains(e)) then
      outerFace.outerComponent = newBoundaryEdges.headOption

  private def updateVertexLeavingEdges(
    verticesWithNewEdges: List[Vertex],
    newBoundaryEdges: List[HalfEdge],
  ): Unit =
    verticesWithNewEdges.zip(newBoundaryEdges).foreach { (vertex, edge) =>
      vertex.leaving = Some(edge)
    }
