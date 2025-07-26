package io.github.scala_tessella
package dcel

import BigDecimalGeometry.*
import Polygon.RegularPolygon

import spire.implicits.*

import scala.collection.mutable.ListBuffer

object TilingAddition:

  private def calculateNewVertices(sides: Int, p1: BigPoint, p2: BigPoint): List[BigPoint] =
    val angle = RegularPolygon(sides).alphaDegree
    val rotation = AngleDegree(180) + angle
    val newPoints = ListBuffer.empty[BigPoint]
    var prev = p1
    var curr = p2
    for (_ <- 3 to sides)
      val direction = prev.angleTo(curr)
      val next = curr.plus(BigPoint.fromPolar(1, direction + rotation.toBigRadian))
      newPoints.append(next)
      prev = curr
      curr = next
    newPoints.toList

  // Helper function to calculate boundary angle for a vertex given its interior angles
  private def calculateBoundaryAngle(interiorAngleSum: AngleDegree): AngleDegree =
    AngleDegree(360) - interiorAngleSum

  // Helper function to get current interior angle sum for a vertex
  private def getCurrentInteriorAngleSum(vertex: Vertex, outerFace: Face): AngleDegree =
    vertex.incidentEdges
      .filterNot(_.incidentFace.contains(outerFace))
      .flatMap(_.angle)
      .fold(AngleDegree(0))(_ + _)

  // Helper function to create a pair of twin half-edges
  private def createTwinHalfEdges(
    origin: Vertex,
    destination: Vertex,
    boundaryFace: Face,
    innerFace: Face,
    boundaryAngle: AngleDegree,
    innerAngle: AngleDegree
  ): (HalfEdge, HalfEdge) =
    val boundaryEdge = HalfEdge(origin = origin, incidentFace = Some(boundaryFace), angle = Some(boundaryAngle))
    val innerEdge = HalfEdge(origin = destination, twin = Some(boundaryEdge), incidentFace = Some(innerFace), angle = Some(innerAngle))
    boundaryEdge.twin = Some(innerEdge)
    (boundaryEdge, innerEdge)

  // Helper function to link edges in a cycle
  private def linkEdgesInCycle(edges: List[HalfEdge]): Unit =
    edges.zip(edges.tail :+ edges.head).foreach { case (curr, next) =>
      curr.next = Some(next)
      next.prev = Some(curr)
    }

  // Helper function to link edges in sequence
  private def linkEdgesInSequence(edges: List[HalfEdge]): Unit =
    edges.zip(edges.tail).foreach { case (prev, next) =>
      prev.next = Some(next)
      next.prev = Some(prev)
    }

  // Helper function to connect boundary edges to existing boundary
  private def connectToBoundary(
    newEdges: List[HalfEdge],
    originalPrev: Option[HalfEdge],
    originalNext: Option[HalfEdge]
  ): Unit =
    if newEdges.nonEmpty then
      originalPrev.foreach(_.next = Some(newEdges.head))
      newEdges.head.prev = originalPrev

      originalNext.foreach(_.prev = Some(newEdges.last))
      newEdges.last.next = originalNext

      linkEdgesInSequence(newEdges)

  extension (tilingDCEL: TilingDCEL)

    def addRegularPolygon(sides: Int, onEdgeStartingWithVertexId: String): Either[String, TilingDCEL] =
      for
        _ <- TilingBuilder.validateSides(sides, "regular")
        boundaryEdges <- tilingDCEL.getBoundaryEdges
        edgeToBuildOn <- boundaryEdges.find(_.origin.id == onEdgeStartingWithVertexId)
          .toRight(s"Edge starting with vertex $onEdgeStartingWithVertexId not found on the boundary.")
        v_start = edgeToBuildOn.origin
        v_end <- edgeToBuildOn.destination.toRight("Edge has no destination vertex.")
      yield {
        val polygonAngle = RegularPolygon(sides).alphaDegree

        // Calculate boundary angles for shared vertices
        def calculateBoundaryAngleForVertex(vertex: Vertex): AngleDegree =
          val currentSum = getCurrentInteriorAngleSum(vertex, tilingDCEL.outerFace)
          calculateBoundaryAngle(currentSum + polygonAngle)

        val boundaryAngleForStartVertex = calculateBoundaryAngleForVertex(v_start)
        val boundaryAngleForEndVertex = calculateBoundaryAngleForVertex(v_end)
        val boundaryAngleForNewVertices = calculateBoundaryAngle(polygonAngle)

        // Capture original boundary links before modification
        val originalPrev = edgeToBuildOn.prev
        val originalNext = edgeToBuildOn.next

        // 1. Calculate new vertex coordinates and create vertices
        val newVertexPoints = calculateNewVertices(sides, v_end.coords, v_start.coords)
        val newVertices = newVertexPoints.zipWithIndex.map {
          case (p, i) => Vertex(s"V${tilingDCEL.vertices.size + i + 1}", p)
        }.toList

        // 2. Create new Face
        val newFace = Face(s"F${tilingDCEL.innerFaces.size + 1}")

        // 3. Update the shared edge
        edgeToBuildOn.incidentFace = Some(newFace)
        edgeToBuildOn.angle = Some(polygonAngle)

        // 4. Create new half-edges for the polygon boundary
        val allVerticesForNewEdges = List(v_start) ++ newVertices ++ List(v_end)
        val newEdges = allVerticesForNewEdges.sliding(2).map {
          case List(origin, destination) =>
            val boundaryAngle = origin match
              case _ if origin == v_start => boundaryAngleForStartVertex
              case _ => boundaryAngleForNewVertices

            createTwinHalfEdges(
              origin, destination, tilingDCEL.outerFace, newFace,
              boundaryAngle, polygonAngle
            )
        }.toList

        val (newBoundaryEdges, newInnerEdges) = newEdges.unzip

        // Update angle for the last boundary edge
        newBoundaryEdges.lastOption.foreach(_.angle = Some(boundaryAngleForNewVertices))

        // 5. Update boundary angle for existing edge from v_end
        originalNext.foreach { nextEdge =>
          if nextEdge.origin == v_end then
            nextEdge.angle = Some(boundaryAngleForEndVertex)
        }

        // 6. Link all inner edges of the new face
        val allNewInnerEdges = edgeToBuildOn :: newInnerEdges.reverse
        linkEdgesInCycle(allNewInnerEdges)
        newFace.outerComponent = Some(edgeToBuildOn)

        // 7. Connect new boundary edges to existing boundary
        connectToBoundary(newBoundaryEdges, originalPrev, originalNext)

        // 8. Update outer face component if necessary
        if tilingDCEL.outerFace.outerComponent.contains(edgeToBuildOn) then
          tilingDCEL.outerFace.outerComponent = Some(newBoundaryEdges.head)

        // 9. Update leaving edges for vertices
        (v_start :: newVertices).zip(newBoundaryEdges).foreach { case (vertex, edge) =>
          vertex.leaving = Some(edge)
        }
        v_end.leaving = edgeToBuildOn.twin

        // 10. Return the new DCEL
        TilingDCEL(
          vertices = tilingDCEL.vertices ++ newVertices,
          halfEdges = tilingDCEL.halfEdges ++ newBoundaryEdges ++ newInnerEdges,
          innerFaces = tilingDCEL.innerFaces :+ newFace,
          outerFace = tilingDCEL.outerFace
        )
      }