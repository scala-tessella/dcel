package io.github.scala_tessella
package dcel

import BigDecimalGeometry.*
import Polygon.{RegularPolygon, SimplePolygon}
import TilingBuilder.{calculateVertexPoints, validatePoints, validateSides}

import scala.annotation.tailrec
import scala.collection.mutable

object TilingAddition:

  def calculateNewVertices(sides: Int, p1: BigPoint, p2: BigPoint): List[BigPoint] =
    val angle = RegularPolygon(sides).alphaDegree.conjugate
    calculateVertexPoints(List.fill(sides)(angle), p1, p2).drop(2)

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
  private def createEdgePairs2(
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

  private def findSharedEdges3(
    edgeToBuildOn: HalfEdge,
    boundaryAngles: BoundaryAngles2
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
      traverse(edgeToBuildOn.prev.get, boundaryAngles.start, boundaryAngles.newVertices, Nil, _.prev.get, _.origin)

    val (appended, endCheck, endEdge) =
      traverse(edgeToBuildOn.next.get, boundaryAngles.end, boundaryAngles.newVertices.reverse, Nil, _.next.get, _.destination.get)

    SharedEdgesResult(
      sharedEdges = prepended.reverse ::: edgeToBuildOn :: appended.reverse,
      startCheck = startCheck,
      startEdge = startEdge,
      startCounter = prepended.length,
      endCheck = endCheck,
      endEdge = endEdge,
      endCounter = appended.length
    )

  private def findSharedEdges(
    edgeToBuildOn: HalfEdge,
    boundaryAngles: BoundaryAngles,
    polyAngle: AngleDegree
  )(using outerFace: Face): SharedEdgesResult =

    @tailrec
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

    def addSimplePolygon(angles: List[AngleDegree], onEdgeStartingWithVertexId: String): Either[String, TilingDCEL] =
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
        given outerFace: Face = tilingDCEL.outerFace

        // Calculate boundary angles
        val boundaryAngles = BoundaryAngles2(
          start = boundaryAngleForVertex(startVertex, outerFace, angles.head),
          end = boundaryAngleForVertex(endVertex, outerFace, angles(1)),
          newVertices = angles.drop(2)
        )

        println(
          s"""
             |startVertex: $startVertex
             |endVertex: $endVertex
             |boundaryAngles: $boundaryAngles""".stripMargin)

        val edgesResult = findSharedEdges3(edgeToBuildOn, boundaryAngles)

        println(s"edgesResult: $edgesResult")

        // Different start and end vertex
        val revisedStartVertex = edgesResult.startEdge.destination.get
        val revisedEndVertex = edgesResult.endEdge.origin
        println(s"revisedStartVertex: $revisedStartVertex")
        println(s"revisedEndVertex: $revisedEndVertex")

        // Different boundary angles
        val revisedBoundaryAngles = BoundaryAngles2(
          start = edgesResult.startCheck,
          end = edgesResult.endCheck,
          newVertices = boundaryAngles.newVertices.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)
        )
        println(s"revisedBoundaryAngles: $revisedBoundaryAngles")

        // Different boundary
        val completeBoundary = BoundaryState(Some(edgesResult.startEdge), Some(edgesResult.endEdge))
        println(s"completeBoundary: $completeBoundary")

        // Create new components
        val vertexPoints = points.drop(2).reverse
        println(s"vertexPoints: $vertexPoints")
        val revisedVertexPoints = vertexPoints.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)
        println(s"revisedVertexPoints: $revisedVertexPoints")
        val newVertices = createVertices(revisedVertexPoints, tilingDCEL.vertices.size)
        println(s"newVertices: $newVertices")

        val newFace = Face(generateFaceId(tilingDCEL.innerFaces.size))
        println(s"newFace: $newFace")

        val allVertices = revisedStartVertex :: newVertices ::: revisedEndVertex :: Nil
        println(s"allVertices: $allVertices")

        val revisedAngles = angles.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)
        println(s"revisedAngles: $revisedAngles")

        val edgePairs = createEdgePairs2(allVertices, outerFace, newFace, revisedBoundaryAngles.start, revisedAngles)
        val (newBoundaryEdges, newInnerEdges) = edgePairs.unzip
        println(s"newBoundaryEdges: $newBoundaryEdges")
        println(s"newBoundaryEdges angles: ${newBoundaryEdges.map(_.angle)}")
        println(s"newInnerEdges: $newInnerEdges")

        val sharedAngles = angles.takeRight(edgesResult.endCounter) ++ angles.take(edgesResult.startCounter + 1)
        println(s"sharedAngles: $sharedAngles")
        println(s"sharedEdges: ${edgesResult.sharedEdges}")
        // Update existing structures
        updateExistingStructures3(
          edgesResult.sharedEdges, newFace, sharedAngles,
          newBoundaryEdges, completeBoundary, revisedBoundaryAngles
        )

        println(s"newBoundaryEdges angles 2: ${newBoundaryEdges.map(_.angle)}")

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

        revisedTiling

    def addRegularPolygon(sides: Int, onEdgeStartingWithVertexId: String): Either[String, TilingDCEL] =
      for
        _                        <- validateSides(sides, "regular")
        boundaryEdges            <- tilingDCEL.getBoundaryEdges
        edgeToBuildOn            <- boundaryEdges
          .find(_.origin.id == onEdgeStartingWithVertexId)
          .toRight(s"Edge starting with vertex $onEdgeStartingWithVertexId not found on the boundary.")
        (startVertex, endVertex) <- edgeToBuildOn.endpointsAsVertices
          .toRight("Edge has no destination vertex.")
      yield
        given outerFace: Face = tilingDCEL.outerFace

        val polyAngle = polygonAngle(sides)
        val angles = List.fill(sides)(polyAngle)
        // Calculate boundary angles
//        val boundaryAngles = BoundaryAngles(
//          start = boundaryAngleForVertex(startVertex, outerFace, polyAngle),
//          end = boundaryAngleForVertex(endVertex, outerFace, polyAngle),
//          newVertices = polyAngle.conjugate
//        )

        val boundaryAngles2 = BoundaryAngles2(
          start = boundaryAngleForVertex(startVertex, outerFace, polyAngle),
          end = boundaryAngleForVertex(endVertex, outerFace, polyAngle),
          newVertices = angles.drop(2)
        )

//        val edgesResult = findSharedEdges(edgeToBuildOn, boundaryAngles, polyAngle)
        val edgesResult = findSharedEdges3(edgeToBuildOn, boundaryAngles2)

        // Different start and end vertex
        val revisedStartVertex = edgesResult.startEdge.destination.get
        val revisedEndVertex = edgesResult.endEdge.origin

        // Different boundary angles
        val revisedBoundaryAngles = BoundaryAngles(
          start = edgesResult.startCheck,
          end = edgesResult.endCheck,
          newVertices = polyAngle.conjugate
        )

        val revisedBoundaryAngles2 = BoundaryAngles2(
          start = edgesResult.startCheck,
          end = edgesResult.endCheck,
          newVertices = boundaryAngles2.newVertices.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)
        )

        // Different boundary
        val completeBoundary = BoundaryState(Some(edgesResult.startEdge), Some(edgesResult.endEdge))

        // Create new components
        val vertexPoints = calculateNewVertices(sides, endVertex.coords, startVertex.coords)
        val revisedVertexPoints = vertexPoints.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)
        val newVertices = createVertices(revisedVertexPoints, tilingDCEL.vertices.size)

        val newFace = Face(generateFaceId(tilingDCEL.innerFaces.size))

        val allVertices = revisedStartVertex :: newVertices ::: revisedEndVertex :: Nil

        val revisedAngles = angles.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)

//        val edgePairs = createEdgePairs(allVertices, outerFace, newFace, revisedBoundaryAngles.start, polyAngle)
        val edgePairs = createEdgePairs2(allVertices, outerFace, newFace, revisedBoundaryAngles2.start, revisedAngles)
        val (newBoundaryEdges, newInnerEdges) = edgePairs.unzip

        // Update existing structures
        updateExistingStructures2(
          edgesResult.sharedEdges, newFace, edgesResult.sharedEdges.map(_ => polyAngle),
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
            revisedTiling.stitchHole(v_match, v_new)
          case one :: two :: Nil =>
            println("Warning: two shared vertices found")
            revisedTiling
          case _ =>
            println("Error: more than 2 shared vertices found")
            revisedTiling

    /** Creates a valid TilingDCEL when two boundary vertices are touching forming an inner face
     *
     * @param v_match The already existing vertex touched
     * @param v_new   The new vertex touching
     * @return
     */
    private def stitchHole(v_match: Vertex, v_new: Vertex): TilingDCEL =
      println(s"Warning: one shared vertex found, $v_new at place where $v_match already is.")
      val holeFace = Face(generateFaceId(tilingDCEL.innerFaces.size + 1))

      // Find the boundary edges that will form the hole
      val boundaryEdgesAroundHole =
        tilingDCEL.getBoundaryEdgesPath(from = v_match, to = v_new)
      println(s"boundaryEdgesAroundHole: $boundaryEdgesAroundHole")

      // Remove the vertex that will be merged
      val updatedVertices = tilingDCEL.vertices.filterNot(_ == v_new)
      println(s"updatedVertices: $updatedVertices")

      // Create mapping for edges that need to be replaced
      val oldToNewEdgeMap = mutable.Map[HalfEdge, HalfEdge]()

      // First pass: create new edges for those originating from v_new
      val updatedHalfEdges = tilingDCEL.halfEdges.map { edge =>
        if edge.origin == v_new then
          val newEdge = HalfEdge(v_match)
          newEdge.twin = edge.twin
          newEdge.incidentFace = edge.incidentFace
          newEdge.angle = edge.angle
          newEdge.next = edge.next
          newEdge.prev = edge.prev

          oldToNewEdgeMap(edge) = newEdge
          edge.twin.foreach(_.twin = Some(newEdge))

          if v_new.leaving.contains(edge) then
            v_match.leaving = Some(newEdge)

          println(s"updatedEdge: $newEdge")
          newEdge
        else
          edge
      }
      println(s"updatedHalfEdges: $updatedHalfEdges")

      // Second pass: fix next/prev references
      updatedHalfEdges.foreach { edge =>
        // Update next reference if it points to a replaced edge
        edge.next = edge.next.map { nextEdge =>
          oldToNewEdgeMap.getOrElse(nextEdge, nextEdge)
        }
        // Update prev reference if it points to a replaced edge
        edge.prev = edge.prev.map { prevEdge =>
          oldToNewEdgeMap.getOrElse(prevEdge, prevEdge)
        }
      }
      println(s"done: $updatedHalfEdges")

      // Create the hole face and assign it to the appropriate edges
      val holeEdges = boundaryEdgesAroundHole.map(oldEdge =>
        oldToNewEdgeMap.getOrElse(oldEdge, oldEdge)
      )
      println(s"holeEdges: $holeEdges")

      // Create new inner edges for the hole face (twins of the boundary edges)
      val holeInnerEdges = holeEdges.map { boundaryEdge =>
        val innerEdge = HalfEdge(boundaryEdge.destination.get)
        innerEdge.twin = Some(boundaryEdge)
        boundaryEdge.twin = Some(innerEdge)
        innerEdge.incidentFace = Some(holeFace)

        // Calculate the interior angle for the hole face
        val vertex = innerEdge.origin
        val allIncidentEdges = vertex.incidentEdges
        val interiorEdges = allIncidentEdges.filterNot(_.incidentFace.contains(tilingDCEL.outerFace))
        val currentInteriorSum = interiorEdges.flatMap(_.angle).fold(AngleDegree(0))(_ + _)
        val holeInteriorAngle = currentInteriorSum.conjugate

        innerEdge.angle = Some(holeInteriorAngle)
        innerEdge
      }

      // Link the inner edges in a cycle
      holeInnerEdges.zip(holeInnerEdges.tail :+ holeInnerEdges.head).foreach { case (current, next) =>
        current.linkWith(next)
      }

      // Set the hole face's outer component
      holeFace.outerComponent = holeInnerEdges.headOption

      println(s"holeEdges with new incidentFace and angle: $holeEdges")

      // Update the outer face component if needed
      val remainingBoundaryEdges = updatedHalfEdges.filter(_.incidentFace.contains(tilingDCEL.outerFace))
      if remainingBoundaryEdges.nonEmpty then
        tilingDCEL.outerFace.outerComponent = remainingBoundaryEdges.headOption

      // Return the updated tiling with the hole face
      tilingDCEL.copy(
        vertices = updatedVertices,
        halfEdges = updatedHalfEdges ++ holeInnerEdges,
        innerFaces = tilingDCEL.innerFaces :+ holeFace
      )

  // Helper case classes for better structure
  private case class BoundaryAngles(
    start: AngleDegree,
    end: AngleDegree,
    newVertices: AngleDegree
  )

  private case class BoundaryAngles2(
    start: AngleDegree,
    end: AngleDegree,
    newVertices: List[AngleDegree]
  )

  private case class BoundaryState(
    prev: Option[HalfEdge],
    next: Option[HalfEdge]
  )

  private def updateExistingStructures3(
    sharedEdges: List[HalfEdge],
    newFace: Face,
    polyAngles: List[AngleDegree],
    newBoundaryEdges: List[HalfEdge],
    originalBoundary: BoundaryState,
    boundaryAngles: BoundaryAngles2
  ): Unit =
    // Update shared edges
    sharedEdges.foreach(_.incidentFace = Some(newFace))
    val sharedEdgesFirstAngle = newBoundaryEdges.head.angle
    sharedEdges.zipWithIndex.foreach((edge, index) => edge.angle = Some(polyAngles(index)))

    // Update last boundary edge angle
    newBoundaryEdges.lastOption.foreach(_.angle = Some(boundaryAngles.newVertices.head.conjugate))

    // Update existing boundary edge from end vertex
    originalBoundary.next.foreach { nextEdge =>
      if nextEdge.origin.id == sharedEdges.last.destination.map(_.id).getOrElse("") then
        nextEdge.angle = Some(boundaryAngles.end)
    }

    // Update boundary in special shared edges case
    if sharedEdges.length > 1 && newBoundaryEdges.length == 1 then
      newBoundaryEdges.head.angle = sharedEdgesFirstAngle

  private def updateExistingStructures2(
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
    newBoundaryEdges.lastOption.foreach(_.angle = Some(boundaryAngles.newVertices))

    // Update existing boundary edge from end vertex
    originalBoundary.next.foreach { nextEdge =>
      if nextEdge.origin.id == sharedEdges.last.destination.map(_.id).getOrElse("") then
        nextEdge.angle = Some(boundaryAngles.end)
    }

    // Update boundary in special shared edges case
    if sharedEdges.length > 1 && newBoundaryEdges.length == 1 then
      newBoundaryEdges.head.angle = sharedEdgesFirstAngle

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
    val sharedEdgesFirstAngle = newBoundaryEdges.head.angle
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
