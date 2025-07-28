package io.github.scala_tessella
package dcel

import BigDecimalGeometry.*
import Polygon.RegularPolygon

import scala.collection.mutable

/**
 * Object to manage boundary operations during tiling additions.
 */
object TilingAdditionBoundary:

  def calculateNewVertices(sides: Int, p1: BigPoint, p2: BigPoint): List[BigPoint] =
    val angle = RegularPolygon(sides).alphaDegree
    val rotation = AngleDegree(180) - angle

    LazyList.unfold((p1, p2, 3)) { case (prev, curr, step) =>
      if step > sides then None
      else
        val direction = prev.angleTo(curr)
        val next = curr.plus(BigPoint.fromPolar(1, direction + rotation.toBigRadian))
        Some(next, (curr, next, step + 1))
    }.toList

  /**
   * Identifies which part of the existing tiling boundary is shared
   * with a new polygon and which vertices of the new polygon are new.
   *
   * @param boundary The clockwise sequence of vertices of the tiling's boundary.
   * @param attachmentVertex The vertex on the boundary where the new polygon is attached.
   * @param sides The number of sides of the new regular polygon.
   * @return A tuple containing:
   *         - A list of vertices on the original boundary that will be shared.
   *         - A list of coordinates for the new vertices to be created.
   */
  def findBoundaryDivision(
     boundary: Vector[Vertex],
     attachmentVertex: Vertex,
     sides: Int
   ): (List[Vertex], List[BigPoint]) =
    val startIndex = boundary.indexOf(attachmentVertex)
    if (startIndex < 0) return (Nil, Nil)

    // A lazy, cyclic stream of boundary vertices starting from the attachment point, clockwise.
    val orderedBoundary = LazyList.from(0).map(i => boundary((startIndex + i) % boundary.size))

    val firstShared = orderedBoundary.head
    val secondShared = orderedBoundary(1)

    // Vertices of the new polygon are calculated CCW from the edge (secondShared, firstShared)
    val calculatedNewPoints = calculateNewVertices(sides, secondShared.coords, firstShared.coords)
    val allNewPolygonPoints = firstShared.coords :: secondShared.coords :: calculatedNewPoints

    val sharedBoundaryVertices = new mutable.ListBuffer[Vertex]()
    sharedBoundaryVertices += firstShared
    sharedBoundaryVertices += secondShared

    var boundaryIndex = 2
    var newPolygonPointIndex = 2
    var continueMatching = true

    while (newPolygonPointIndex < sides && continueMatching)
      val nextBoundaryVertex = orderedBoundary(boundaryIndex)
      val nextNewPolygonPoint = allNewPolygonPoints(newPolygonPointIndex)
      if nextBoundaryVertex.coords.almostEquals(nextNewPolygonPoint) then
        sharedBoundaryVertices += nextBoundaryVertex
        boundaryIndex += 1
        newPolygonPointIndex += 1
      else
        continueMatching = false

    val newPoints = allNewPolygonPoints.drop(sharedBoundaryVertices.size)

    (sharedBoundaryVertices.toList, newPoints)
