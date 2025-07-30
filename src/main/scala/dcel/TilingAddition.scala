package io.github.scala_tessella
package dcel

import BigDecimalGeometry.*
import Polygon.RegularPolygon

object TilingAddition:

  // Sticking to AngleDegree as much as possible for precision
  def calculateNewVertices(sides: Int, p1: BigPoint, p2: BigPoint): List[BigPoint] =
    val angle = RegularPolygon(sides).alphaDegree
    val rotation = AngleDegree(180) + angle
    val startingDirectionRad = p1.angleTo(p2)
    val startingDirection = AngleDegree(startingDirectionRad)

    LazyList.unfold((p2, 3, startingDirection)) { case (curr, step, direction) =>
      if step > sides then None
      else
        val nextDirection = direction + rotation
        val next = curr.plus(BigPoint.fromPolar(1, nextDirection.toBigRadian))
        Some(next, (next, step + 1, nextDirection))
    }.toList

  private def createVertices(points: List[BigPoint], startingIndex: Int): List[Vertex] =
    points.zipWithIndex.map { (point, index) =>
      Vertex(s"V${startingIndex + index + 1}", point)
    }

  // Extract polygon angle calculation
  private def polygonAngle(sides: Int): AngleDegree =
    RegularPolygon(sides).alphaDegree

  // More descriptive boundary angle calculation
  private def boundaryAngleForVertex(
    vertex: Vertex,
    outerFace: Face,
    additionalInteriorAngle: AngleDegree
  ): AngleDegree =
    val currentInteriorSum = vertex.getCurrentInteriorAngleSum(outerFace)
    (currentInteriorSum + additionalInteriorAngle).conjugate

  // Use proper case class pattern matching instead of List extraction
  private def createEdgePairs(
    vertices: List[Vertex],
    outerFace: Face,
    newFace: Face,
    startVertexAngle: AngleDegree,
    polygonAngle: AngleDegree
  ): List[(HalfEdge, HalfEdge)] =
    vertices.sliding(2).zipWithIndex.map {
      case (origin :: destination :: Nil, index) =>
        val boundaryAngle = if index == 0 then startVertexAngle else polygonAngle.conjugate
        HalfEdge.createTwinHalfEdges(
          origin, destination, outerFace, newFace,
          boundaryAngle, polygonAngle
        )
      case _ =>
        throw IllegalArgumentException("Invalid vertex sequence")
    }.toList

  // Simplify face generation using template interpolation
  private def generateFaceId(existingFaceCount: Int): String =
    s"F${existingFaceCount + 1}"

  extension (tilingDCEL: TilingDCEL)

    def addRegularPolygon(sides: Int, onEdgeStartingWithVertexId: String): Either[String, TilingDCEL] =
      for
        _ <- TilingBuilder.validateSides(sides, "regular")
        boundaryEdges <- tilingDCEL.getBoundaryEdges
        edgeToBuildOn <- boundaryEdges
          .find(_.origin.id == onEdgeStartingWithVertexId)
          .toRight(s"Edge starting with vertex $onEdgeStartingWithVertexId not found on the boundary.")
        (startVertex, endVertex) <- edgeToBuildOn.endpointsAsVertices
          .toRight("Edge has no destination vertex.")
      yield
        given outerFace: Face = tilingDCEL.outerFace

        val polyAngle = polygonAngle(sides)

        // Calculate boundary angles
        val boundaryAngles = BoundaryAngles(
          start = boundaryAngleForVertex(startVertex, outerFace, polyAngle),
          end = boundaryAngleForVertex(endVertex, outerFace, polyAngle),
          newVertices = polyAngle.conjugate
        )

//        // Capture original boundary state
//        val originalBoundary = BoundaryState(edgeToBuildOn.prev, edgeToBuildOn.next)

        var sharedEdges = List(edgeToBuildOn)

        // Calculating possible shared edges
        var startCheck = boundaryAngles.start
        var startEdge = edgeToBuildOn.prev.get
        var startCounter = 0
        while startCheck.isFullCircle
        do
          startCheck = boundaryAngleForVertex(startEdge.origin, outerFace, polyAngle)
          sharedEdges = startEdge :: sharedEdges
          startEdge = startEdge.prev.get
          startCounter += 1

        var endCheck = boundaryAngles.end
        var endEdge = edgeToBuildOn.next.get
        var endCounter = 0
        while endCheck.isFullCircle
        do
          endCheck = boundaryAngleForVertex(endEdge.destination.get, outerFace, polyAngle)
          sharedEdges = sharedEdges :+ endEdge
          endEdge = endEdge.next.get
          endCounter += 1

        val hasMoreThanOneSharedEdge = sharedEdges.length > 1
//        println(s"hasMoreThanOneSharedEdge $hasMoreThanOneSharedEdge")

        // Different start and end vertex
        if hasMoreThanOneSharedEdge then
          println(s"sharedEdges $sharedEdges")
          println(s"startVertex $startVertex")
          println(s"endVertex $endVertex")
        val revisedStartVertex = startEdge.destination.get
        val revisedEndVertex = endEdge.origin
        if hasMoreThanOneSharedEdge then
          println(s"revisedStartVertex $revisedStartVertex")
          println(s"revisedEndVertex $revisedEndVertex")
          if startVertex != revisedStartVertex then println("WARN: different start vertex")
          if endVertex != revisedEndVertex then println("WARN: different end vertex")

        // Different boundary angles
        if hasMoreThanOneSharedEdge then
          println(s"boundaryAngles $boundaryAngles")
        val revisedBoundaryAngles = BoundaryAngles(
          start = startCheck,
          end = endCheck,
          newVertices = polyAngle.conjugate
        )
        if hasMoreThanOneSharedEdge then
          println(s"revisedBoundaryAngles $revisedBoundaryAngles")
          if boundaryAngles != revisedBoundaryAngles then println("WARN: different boundary angles")

        // Different boundary
//        println(s"originalBoundary $originalBoundary")
        val completeBoundary = BoundaryState(Some(startEdge), Some(endEdge))
        if hasMoreThanOneSharedEdge then
          println(s"completeBoundary $completeBoundary")
//        if originalBoundary != completeBoundary then println("WARN: different boundary")

        // Create new components
        val vertexPoints = calculateNewVertices(sides, endVertex.coords, startVertex.coords)
        if hasMoreThanOneSharedEdge then
          println(s"vertexPoints $vertexPoints")
        val revisedVertexPoints = vertexPoints.drop(startCounter).dropRight(endCounter)
        if hasMoreThanOneSharedEdge then
          println(s"revisedVertexPoints $revisedVertexPoints")
          if vertexPoints != revisedVertexPoints then println("WARN: different vertex points")

//        val newVertices = createVertices(vertexPoints, tilingDCEL.vertices.size)
//        println(s"newVertices $newVertices")
        val revisedNewVertices = createVertices(revisedVertexPoints, tilingDCEL.vertices.size)
        if hasMoreThanOneSharedEdge then
          println(s"revisedNewVertices $revisedNewVertices")
//        if newVertices != revisedNewVertices then println("WARN: different new vertices")

        val newFace = Face(generateFaceId(tilingDCEL.innerFaces.size))

//        val allVertices = startVertex :: newVertices ::: endVertex :: Nil
//        println(s"allVertices $allVertices")
        val revisedAllVertices = revisedStartVertex :: revisedNewVertices ::: revisedEndVertex :: Nil
        if hasMoreThanOneSharedEdge then
          println(s"revisedAllVertices $revisedAllVertices")
//        if allVertices != revisedAllVertices then println("WARN: different all vertices")

//        val edgePairs = createEdgePairs(allVertices, outerFace, newFace, boundaryAngles.start, polyAngle)
        val revisedEdgePairs = createEdgePairs(revisedAllVertices, outerFace, newFace, revisedBoundaryAngles.start, polyAngle)
//        val (newBoundaryEdges, newInnerEdges) = edgePairs.unzip
//        println(
//          s"""newBoundaryEdges: $newBoundaryEdges
//            |newInnerEdges: $newInnerEdges
//            |""".stripMargin
//        )
        val (revisedNewBoundaryEdges, revisedNewInnerEdges) = revisedEdgePairs.unzip
        if hasMoreThanOneSharedEdge then
          println(
            s"""revisedNewBoundaryEdges: $revisedNewBoundaryEdges
               |revisedNewInnerEdges: $revisedNewInnerEdges
               |""".stripMargin
          )

        // @todo probably wrong if hasMoreThanOneSharedEdge
        // Update existing structures
        if hasMoreThanOneSharedEdge then
          updateExistingStructuresRevised(
            edgeToBuildOn, sharedEdges, newFace, polyAngle,
            revisedNewBoundaryEdges, completeBoundary, revisedBoundaryAngles
          )
        else
          updateExistingStructures(
            edgeToBuildOn, newFace, polyAngle,
            revisedNewBoundaryEdges, completeBoundary, revisedBoundaryAngles
          )

        // @todo probably wrong if hasMoreThanOneSharedEdge
        // Link new face edges
        if hasMoreThanOneSharedEdge then
          linkNewFaceEdgesRevised(edgeToBuildOn, sharedEdges, revisedNewInnerEdges.reverse, newFace)
        else
          linkNewFaceEdges(edgeToBuildOn, revisedNewInnerEdges.reverse, newFace)

        // @todo probably wrong if hasMoreThanOneSharedEdge
        // Connect to boundary
        if !hasMoreThanOneSharedEdge then
          connectNewBoundaryEdges(revisedNewBoundaryEdges, completeBoundary, outerFace, edgeToBuildOn)
        else
          connectNewBoundaryEdgesRevised(revisedNewBoundaryEdges, completeBoundary, outerFace, edgeToBuildOn)

        // @todo probably wrong if hasMoreThanOneSharedEdge
        // Update vertex leaving edges
        if !hasMoreThanOneSharedEdge then
          updateVertexLeavingEdges(revisedStartVertex :: revisedNewVertices, revisedNewBoundaryEdges, revisedEndVertex, edgeToBuildOn)
        else
          updateVertexLeavingEdgesRevised(revisedStartVertex :: revisedNewVertices, revisedNewBoundaryEdges)

        // Return new DCEL with updated components
        tilingDCEL.copy(
          vertices = tilingDCEL.vertices ::: revisedNewVertices,
          halfEdges = tilingDCEL.halfEdges ::: revisedNewBoundaryEdges ::: revisedNewInnerEdges,
          innerFaces = tilingDCEL.innerFaces :+ newFace
        )

  // Helper case classes for better structure
  private case class BoundaryAngles(
    start: AngleDegree,
    end: AngleDegree,
    newVertices: AngleDegree
  )

  private case class BoundaryState(
    prev: Option[HalfEdge],
    next: Option[HalfEdge]
  )

  // More functional helper methods
  private def updateExistingStructures(
    edgeToBuildOn: HalfEdge,
    newFace: Face,
    polyAngle: AngleDegree,
    newBoundaryEdges: List[HalfEdge],
    originalBoundary: BoundaryState,
    boundaryAngles: BoundaryAngles
  ): Unit =
    // Update shared edge
    edgeToBuildOn.incidentFace = Some(newFace)
    edgeToBuildOn.angle = Some(polyAngle)

    // Update last boundary edge angle
    newBoundaryEdges.lastOption.foreach(_.angle = Some(boundaryAngles.newVertices))

    // Update existing boundary edge from end vertex
    originalBoundary.next.foreach { nextEdge =>
      if nextEdge.origin.id == edgeToBuildOn.destination.map(_.id).getOrElse("") then
        nextEdge.angle = Some(boundaryAngles.end)
    }

  private def updateExistingStructuresRevised(
    edgeToBuildOn: HalfEdge,
    sharedEdges: List[HalfEdge],
    newFace: Face,
    polyAngle: AngleDegree,
    newBoundaryEdges: List[HalfEdge],
    originalBoundary: BoundaryState,
    boundaryAngles: BoundaryAngles
  ): Unit =
    // Update shared edge
    sharedEdges.foreach(_.incidentFace = Some(newFace))
//    edgeToBuildOn.incidentFace = Some(newFace)
    sharedEdges.foreach(_.angle = Some(polyAngle))
//    edgeToBuildOn.angle = Some(polyAngle)

    // Update last boundary edge angle
    newBoundaryEdges.lastOption.foreach(_.angle = Some(boundaryAngles.newVertices))

    // Update existing boundary edge from end vertex
    originalBoundary.next.foreach { nextEdge =>
      if nextEdge.origin.id == sharedEdges.last.destination.map(_.id).getOrElse("") then
        nextEdge.angle = Some(boundaryAngles.end)
    }

    // Update boundary in special shared edges case
    if sharedEdges.length > 1 && newBoundaryEdges.length == 1 then
      newBoundaryEdges.head.angle = Some(newBoundaryEdges.head.angle.get - polyAngle)

  private def linkNewFaceEdges(
    edgeToBuildOn: HalfEdge,
    reversedInnerEdges: List[HalfEdge],
    newFace: Face
  ): Unit =
    val allInnerEdges = edgeToBuildOn :: reversedInnerEdges
    allInnerEdges.linkInCycle()
    newFace.outerComponent = Some(edgeToBuildOn)

  private def linkNewFaceEdgesRevised(
    edgeToBuildOn: HalfEdge,
    sharedEdges: List[HalfEdge],
    reversedInnerEdges: List[HalfEdge],
    newFace: Face
  ): Unit =
    println(
      s"""
         |sharedEdges: $sharedEdges
         |reversedInnerEdges: $reversedInnerEdges
         |""".stripMargin)
    val allInnerEdges = sharedEdges ::: reversedInnerEdges
    allInnerEdges.linkInCycle()
    newFace.outerComponent = Some(edgeToBuildOn)

  private def connectNewBoundaryEdges(
    newBoundaryEdges: List[HalfEdge],
    originalBoundary: BoundaryState,
    outerFace: Face,
    edgeToBuildOn: HalfEdge
  ): Unit =

    HalfEdge.insertBoundarySegment(
      originalBoundary.prev.get,
      originalBoundary.next.get,
      newBoundaryEdges
    )

    // Update outer face component if necessary
    if outerFace.outerComponent.contains(edgeToBuildOn) then
      outerFace.outerComponent = newBoundaryEdges.headOption

  private def connectNewBoundaryEdgesRevised(
     newBoundaryEdges: List[HalfEdge],
     originalBoundary: BoundaryState,
     outerFace: Face,
     edgeToBuildOn: HalfEdge
  ): Unit =

    println("START")
    println(
      s"""
         |newBoundaryEdges: $newBoundaryEdges
         |originalBoundary: $originalBoundary
         |outerFace: $outerFace
         |edgeToBuildOn: $edgeToBuildOn
         |""".stripMargin)
    HalfEdge.insertBoundarySegment(
      originalBoundary.prev.get,
      originalBoundary.next.get,
      newBoundaryEdges
    )

    // Update outer face component if necessary
    if outerFace.outerComponent.contains(edgeToBuildOn) then
      outerFace.outerComponent = newBoundaryEdges.headOption

    println("END")

  private def updateVertexLeavingEdges(
    verticesWithNewEdges: List[Vertex],
    newBoundaryEdges: List[HalfEdge],
    endVertex: Vertex,
    edgeToBuildOn: HalfEdge
  ): Unit =
    verticesWithNewEdges.zip(newBoundaryEdges).foreach { (vertex, edge) =>
      vertex.leaving = Some(edge)
    }
//    endVertex.leaving = edgeToBuildOn.twin

  // @todo dedicated method, check if it is needed
  private def updateVertexLeavingEdgesRevised(
    verticesWithNewEdges: List[Vertex],
    newBoundaryEdges: List[HalfEdge],
  ): Unit =
//    println("START")
    verticesWithNewEdges.zip(newBoundaryEdges).foreach { (vertex, edge) =>
      vertex.leaving = Some(edge)
    }
//    println(s"endVertex = $endVertex")
//    println(s"endVertex.leaving = ${endVertex.leaving}")
//    endVertex.leaving = edgeToBuildOn.twin
//    println("END")
