package io.github.scala_tessella
package dcel

import BigDecimalGeometry.*
import Polygon.RegularPolygon

object TilingAddition:

  private def calculateNewVertices(sides: Int, p1: BigPoint, p2: BigPoint): List[BigPoint] =
    val angle = RegularPolygon(sides).alphaDegree
    val rotation = AngleDegree(180) + angle

    LazyList.unfold((p1, p2, 3)) { case (prev, curr, step) =>
      if step > sides then None
      else
        val direction = prev.angleTo(curr)
        val next = curr.plus(BigPoint.fromPolar(1, direction + rotation.toBigRadian))
        Some(next, (curr, next, step + 1))
    }.toList

  private def createVertices(points: List[BigPoint], startingIndex: Int): List[Vertex] =
    points.zipWithIndex.map { (point, index) =>
      Vertex(s"V${startingIndex + index}", point)
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
    newVertexAngle: AngleDegree,
    polygonAngle: AngleDegree
  ): List[(HalfEdge, HalfEdge)] =
    vertices.sliding(2).zipWithIndex.map {
      case (origin :: destination :: Nil, index) =>
        val boundaryAngle = if index == 0 then startVertexAngle else newVertexAngle
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

        // Capture original boundary state
        val originalBoundary = BoundaryState(edgeToBuildOn.prev, edgeToBuildOn.next)

        // Create new components
        val vertexPoints = calculateNewVertices(sides, endVertex.coords, startVertex.coords)
        val newVertices = createVertices(vertexPoints, tilingDCEL.vertices.size)
        val newFace = Face(generateFaceId(tilingDCEL.innerFaces.size))
        val allVertices = startVertex :: newVertices ::: endVertex :: Nil
        val edgePairs = createEdgePairs(allVertices, outerFace, newFace, boundaryAngles.start, boundaryAngles.newVertices, polyAngle)
        val (newBoundaryEdges, newInnerEdges) = edgePairs.unzip

        // Update existing structures
        updateExistingStructures(
          edgeToBuildOn, newFace, polyAngle,
          newBoundaryEdges, originalBoundary, boundaryAngles
        )

        // Link new face edges
        linkNewFaceEdges(edgeToBuildOn, newInnerEdges.reverse, newFace)

        // Connect to boundary
        connectNewBoundaryEdges(newBoundaryEdges, originalBoundary, outerFace, edgeToBuildOn)

        // Update vertex leaving edges
        updateVertexLeavingEdges(startVertex :: newVertices, newBoundaryEdges, endVertex, edgeToBuildOn)

        // Return new DCEL with updated components
        tilingDCEL.copy(
          vertices = tilingDCEL.vertices ::: newVertices,
          halfEdges = tilingDCEL.halfEdges ::: newBoundaryEdges ::: newInnerEdges,
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

  private def linkNewFaceEdges(
    edgeToBuildOn: HalfEdge,
    reversedInnerEdges: List[HalfEdge],
    newFace: Face
  ): Unit =
    val allInnerEdges = edgeToBuildOn :: reversedInnerEdges
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

  private def updateVertexLeavingEdges(
    verticesWithNewEdges: List[Vertex],
    newBoundaryEdges: List[HalfEdge],
    endVertex: Vertex,
    edgeToBuildOn: HalfEdge
  ): Unit =
    verticesWithNewEdges.zip(newBoundaryEdges).foreach { (vertex, edge) =>
      vertex.leaving = Some(edge)
    }
    endVertex.leaving = edgeToBuildOn.twin
