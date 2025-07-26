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

        // Calculate the boundary angle for shared vertices (v_start and v_end)
        // These vertices now have an additional interior face incident to them
        def calculateBoundaryAngleForVertex(vertex: Vertex): AngleDegree =
          val currentInteriorAngleSum = vertex.incidentEdges
            .filterNot(_.incidentFace.contains(tilingDCEL.outerFace))
            .flatMap(_.angle)
            .fold(AngleDegree(0))(_ + _)
          val newInteriorAngleSum = currentInteriorAngleSum + polygonAngle
          AngleDegree(360) - newInteriorAngleSum

        val boundaryAngleForStartVertex = calculateBoundaryAngleForVertex(v_start)
        val boundaryAngleForEndVertex = calculateBoundaryAngleForVertex(v_end)

        // For new vertices, they only have one interior face (the new polygon)
        val boundaryAngleForNewVertices = AngleDegree(360) - polygonAngle

        // Capture original boundary links before modification
        val originalPrev = edgeToBuildOn.prev
        val originalNext = edgeToBuildOn.next

        // 1. Calculate new vertex coordinates
        val newVertexPoints = calculateNewVertices(sides, v_end.coords, v_start.coords)

        // 2. Create new Vertex instances
        val newVertices = newVertexPoints.zipWithIndex.map {
          case (p, i) => Vertex(s"V${tilingDCEL.vertices.size + i + 1}", p)
        }.toList

        // 3. Create new Face
        val newFace = Face(s"F${tilingDCEL.innerFaces.size + 1}")

        // 4. The shared edge's half-edge that was on the boundary now becomes an inner edge.
        edgeToBuildOn.incidentFace = Some(newFace)
        edgeToBuildOn.angle = Some(polygonAngle)

        val allVerticesForNewEdges = List(v_start) ++ newVertices ++ List(v_end)

        // 5. Create the new half-edges for the polygon boundary and their inner twins.
        // Use appropriate boundary angles for each vertex
        val newEdges = allVerticesForNewEdges.sliding(2).map {
          case List(o, d) =>
            val boundaryAngle = if o == v_start then
              boundaryAngleForStartVertex
            else if o == v_end then
              // This case won't occur in the sliding window since v_end is the last element
              boundaryAngleForNewVertices
            else
              // This is a new vertex
              boundaryAngleForNewVertices

            val boundaryEdge = HalfEdge(origin = o, incidentFace = Some(tilingDCEL.outerFace), angle = Some(boundaryAngle))
            val innerEdge = HalfEdge(origin = d, twin = Some(boundaryEdge), incidentFace = Some(newFace), angle = Some(polygonAngle))
            boundaryEdge.twin = Some(innerEdge)
            (boundaryEdge, innerEdge)
        }.toList

        val (newBoundaryEdges, newInnerEdges) = newEdges.unzip

        // Update the angle of the last boundary edge (from the last new vertex to v_end)
        // This edge originates from the last new vertex, but terminates at v_end which is shared
        newBoundaryEdges.lastOption.foreach(_.angle = Some(boundaryAngleForNewVertices))

        // 6. Find and update the boundary edge that originates from v_end
        originalNext.foreach { nextEdge =>
          if nextEdge.origin == v_end then
            nextEdge.angle = Some(boundaryAngleForEndVertex)
        }

        // 7. Link all inner edges of the new face
        val allNewInnerEdges = edgeToBuildOn :: newInnerEdges.reverse
        allNewInnerEdges.zip(allNewInnerEdges.tail :+ allNewInnerEdges.head).foreach { case (curr, next) =>
          curr.next = Some(next)
          next.prev = Some(curr)
        }
        newFace.outerComponent = Some(edgeToBuildOn)

        // 8. Link new boundary edges into the outer boundary
        originalPrev.foreach(_.next = Some(newBoundaryEdges.head))
        newBoundaryEdges.head.prev = originalPrev

        originalNext.foreach(_.prev = Some(newBoundaryEdges.last))
        newBoundaryEdges.last.next = originalNext

        newBoundaryEdges.zip(newBoundaryEdges.tail).foreach { case (p, n) =>
          p.next = Some(n)
          n.prev = Some(p)
        }

        if (tilingDCEL.outerFace.outerComponent.contains(edgeToBuildOn)) {
          tilingDCEL.outerFace.outerComponent = Some(newBoundaryEdges.head)
        }

        // 9. Update leaving edges for vertices
        (v_start :: newVertices).zip(newBoundaryEdges).foreach { case (v, edge) =>
          v.leaving = Some(edge)
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