package io.github.scala_tessella
package dcel

import BigDecimalGeometry.*
import Polygon.RegularPolygon

import scala.collection.mutable

object TilingAddition:

  def calculateNewVertices(sides: Int, p1: BigPoint, p2: BigPoint): List[BigPoint] =
    val angle = RegularPolygon(sides).alphaDegree
    val rotation = (AngleDegree(180) + angle).toBigRadian
    val startingDirection = p1.angleTo(p2)

    LazyList.unfold((p2, 3, startingDirection)) { case (curr, step, direction) =>
      if step > sides then None
      else
        val nextDirection = direction + rotation
        val next = curr.plus(BigPoint.fromPolar(1, nextDirection))
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
    boundaryAngles: BoundaryAngles,
    polyAngle: AngleDegree
  )(using outerFace: Face): SharedEdgesResult =

    @scala.annotation.tailrec
    def traverse(
      edge: HalfEdge,
      check: AngleDegree,
      acc: List[HalfEdge],
      getNext: HalfEdge => HalfEdge,
      getVertex: HalfEdge => Vertex
    ): (List[HalfEdge], AngleDegree, HalfEdge) =
      if !check.isFullCircle then (acc, check, edge)
      else
        val nextCheck = boundaryAngleForVertex(getVertex(edge), outerFace, polyAngle)
        traverse(getNext(edge), nextCheck, edge :: acc, getNext, getVertex)

    val (prepended, startCheck, startEdge) =
      traverse(edgeToBuildOn.prev.get, boundaryAngles.start, Nil, _.prev.get, _.origin)

    val (appended, endCheck, endEdge) =
      traverse(edgeToBuildOn.next.get, boundaryAngles.end, Nil, _.next.get, _.destination.get)

    SharedEdgesResult(
      sharedEdges = prepended.reverse ::: edgeToBuildOn :: appended.reverse,
      startCheck = startCheck,
      startEdge = startEdge,
      startCounter = prepended.length,
      endCheck = endCheck,
      endEdge = endEdge,
      endCounter = appended.length
    )

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

        val edgesResult = findSharedEdges(edgeToBuildOn, boundaryAngles, polyAngle)

        // Different start and end vertex
        val revisedStartVertex = edgesResult.startEdge.destination.get
        val revisedEndVertex = edgesResult.endEdge.origin

        // Different boundary angles
        val revisedBoundaryAngles = BoundaryAngles(
          start = edgesResult.startCheck,
          end = edgesResult.endCheck,
          newVertices = polyAngle.conjugate
        )

        // Different boundary
        val completeBoundary = BoundaryState(Some(edgesResult.startEdge), Some(edgesResult.endEdge))

        // Create new components
        val vertexPoints = calculateNewVertices(sides, endVertex.coords, startVertex.coords)
        val revisedVertexPoints = vertexPoints.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)
        val newVertices = createVertices(revisedVertexPoints, tilingDCEL.vertices.size)

        val newFace = Face(generateFaceId(tilingDCEL.innerFaces.size))

        val allVertices = revisedStartVertex :: newVertices ::: revisedEndVertex :: Nil

        val edgePairs = createEdgePairs(allVertices, outerFace, newFace, revisedBoundaryAngles.start, polyAngle)
        val (newBoundaryEdges, newInnerEdges) = edgePairs.unzip

        // Update existing structures
        updateExistingStructures(
          edgesResult.sharedEdges, newFace, polyAngle,
          newBoundaryEdges, completeBoundary, revisedBoundaryAngles
        )

        // Link new face edges
        linkNewFaceEdges(edgeToBuildOn, edgesResult.sharedEdges, newInnerEdges.reverse, newFace)

        // Connect to boundary
        connectNewBoundaryEdges(newBoundaryEdges, completeBoundary, outerFace, edgesResult.sharedEdges)

        // Update vertex leaving edges
        updateVertexLeavingEdges(revisedStartVertex :: newVertices, newBoundaryEdges)

        // Return new DCEL with updated components
        val revisedTiling =
          tilingDCEL.copy(
            vertices = tilingDCEL.vertices ::: newVertices,
            halfEdges = tilingDCEL.halfEdges ::: newBoundaryEdges ::: newInnerEdges,
            innerFaces = tilingDCEL.innerFaces :+ newFace
          )

        boundaryEdges.map(_.origin).sameCoords(newVertices) match
          case Nil => revisedTiling
          case (v_match, v_new) :: Nil =>
            println(s"Warning: one shared vertex found, $v_new place where $v_match already is.")
            val holeFace = Face(generateFaceId(revisedTiling.innerFaces.size + 1))

            // Find the new vertex that needs to be replaced
            val updatedVertices = revisedTiling.vertices.filterNot(_ == v_new)
            println(s"updatedVertices: $updatedVertices")

            // Find all half-edges that reference v_new and redirect them to v_match
            val oldToNewEdgeMap = mutable.Map[HalfEdge, HalfEdge]()

            // First pass: create new edges and build mapping
            val updatedHalfEdges = revisedTiling.halfEdges.map { edge =>
              if edge.origin == v_new then
                val newEdge = HalfEdge(v_match)
                // Copy all other properties except next/prev (we'll fix those later)
                newEdge.twin = edge.twin
                newEdge.incidentFace = edge.incidentFace
                newEdge.angle = edge.angle

                // Store the mapping for later reference updates
                oldToNewEdgeMap(edge) = newEdge

                // Update twin references immediately
                edge.twin.foreach(_.twin = Some(newEdge))

                // Update leaving edge for v_match if this was v_new's leaving edge
                if v_new.leaving.contains(edge) then
                  v_match.leaving = Some(newEdge)

                println(s"updatedEdge: $newEdge")
                newEdge
              else
                edge
            }
            println(s"updatedHalfEdges: $updatedHalfEdges")

            // Second pass: fix next/prev references using the mapping
            updatedHalfEdges.foreach { edge =>
              edge.next = edge.next.map { nextEdge =>
                oldToNewEdgeMap.getOrElse(nextEdge, nextEdge)
              }
              edge.prev = edge.prev.map { prevEdge =>
                oldToNewEdgeMap.getOrElse(prevEdge, prevEdge)
              }
            }
            println(s"done: $updatedHalfEdges")

            // Find the boundary edges that form the hole
            val boundaryEdgesAroundHole = v_match.incidentEdges.filter(_.incidentFace.contains(outerFace))
            println(s"boundaryEdgesAroundHole: $boundaryEdgesAroundHole")

            // Create the hole face and assign it to the appropriate edges
            val holeEdges = boundaryEdgesAroundHole.filter { edge =>
              // These are the edges that should belong to the hole face instead of outer face
              val twin = edge.twin.get
              twin.incidentFace.exists(face => revisedTiling.innerFaces.contains(face) || face == newFace)
            }
            println(s"holeEdges: $holeEdges")

            holeEdges.foreach { edge =>
              edge.incidentFace = Some(holeFace)
              edge.angle = Some(polygonAngle(sides))
            }

            // Set the hole face's outer component
            holeFace.outerComponent = holeEdges.headOption

            // Update the outer face component if needed
            if outerFace.outerComponent.exists(edge => holeEdges.contains(edge)) then
              val remainingBoundaryEdges = updatedHalfEdges.filter(_.incidentFace.contains(outerFace))
              outerFace.outerComponent = remainingBoundaryEdges.headOption

            // Return the updated tiling with the hole face
            revisedTiling.copy(
              vertices = updatedVertices,
              halfEdges = updatedHalfEdges,
              innerFaces = revisedTiling.innerFaces :+ holeFace
            )
          case one :: two :: Nil =>
            println("Warning: two shared vertices found")
            revisedTiling
          case _ =>
            println("Error: more than 2 shared vertices found")
            revisedTiling

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

  private def updateExistingStructures(
    sharedEdges: List[HalfEdge],
    newFace: Face,
    polyAngle: AngleDegree,
    newBoundaryEdges: List[HalfEdge],
    originalBoundary: BoundaryState,
    boundaryAngles: BoundaryAngles
  ): Unit =
    // Update shared edges
    sharedEdges.foreach(_.incidentFace = Some(newFace))
    sharedEdges.foreach(_.angle = Some(polyAngle))

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
