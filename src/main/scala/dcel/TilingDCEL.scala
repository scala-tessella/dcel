package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, BigBox, BigLineSegment, BigPoint}
import Polygon.RegularPolygon
import TilingSVG.*
import spire.implicits.*

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Represents the entire tiling structure as a container for its components.
 *
 * @param vertices   List of all vertices in the tiling.
 * @param halfEdges  List of all half-edges in the tiling.
 * @param innerFaces List of the tiling's interior faces.
 * @param outerFace  The single, unbounded outer face of the tiling.
 */
case class TilingDCEL(
  vertices: List[Vertex],
  halfEdges: List[HalfEdge],
  innerFaces: List[Face],
  outerFace: Face
):

  /** @return a list of all faces, both inner and outer */
  def faces: List[Face] =
    outerFace :: innerFaces

  /**
   * Finds the outer boundary of the tiling.
   *
   * The traversal follows the half-edges of the outer face, which are linked in
   * a clockwise order around the perimeter.
   *
   * @return A Vector of Vertices forming the perimeter, in clockwise order.
   *         Returns an empty Vector if the outer face has no boundary component.
   */
  def boundary: Vector[Vertex] =
    outerFace.outerComponent.map { startEdge =>
      val builder = Vector.newBuilder[Vertex]

      @tailrec
      def collectVertices(edge: HalfEdge): Unit =
        builder += edge.origin
        edge.next match
          case Some(nextEdge) if nextEdge ne startEdge => collectVertices(nextEdge)
          case _ => // Traversal complete or malformed loop

      collectVertices(startEdge)
      builder.result()
    }.getOrElse(Vector.empty)

  def boundarySafe: Vector[Vertex] =
    outerFace.outerComponent match
      case None => Vector.empty
      case Some(startEdge) =>
        val builder = Vector.newBuilder[Vertex]
        val visited = scala.collection.mutable.Set.empty[HalfEdge]

        @tailrec
        def collectVertices(edge: HalfEdge): Unit =
          if visited.contains(edge) then
            if edge eq startEdge then
              () // completed loop: that's fine
            else
              throw new Error("Malformed loop detected") // repeated non-start: broken!
          else
            builder += edge.origin
            visited += edge
            edge.next match
              case Some(nextEdge) => collectVertices(nextEdge)
              case None => throw new Error("Open chain")

        collectVertices(startEdge)
        builder.result()

  /**
   * Helper method to get all half-edges forming the outer boundary loop.
   */
  private def getBoundaryEdges: List[HalfEdge] =
    outerFace.outerComponent.map { start =>
      @tailrec
      def loop(current: HalfEdge, acc: List[HalfEdge], visited: Set[HalfEdge]): List[HalfEdge] =
        if visited(current) then
          throw new IllegalStateException("Malformed boundary: an edge was visited twice.")

        val next = current.next.getOrElse(throw new IllegalStateException("Boundary loop is not closed."))

        if next eq start then
          (current :: acc).reverse
        else
          loop(next, current :: acc, visited + current)

      loop(start, Nil, Set.empty)
    }.getOrElse(Nil)

  /**
   * Finds a boundary half-edge that originates at the vertex with the given ID.
   *
   * @param vertexId The ID of the origin vertex.
   * @return An Option containing the HalfEdge if found, otherwise None.
   */
  private def findBoundaryEdge(vertexId: String): Option[HalfEdge] =
    getBoundaryEdges.find(_.origin.id == vertexId)

  /**
   * Generates an SVG representation of the tiling.
   *
   * @param width       The desired width of the SVG canvas.
   * @param height      The desired height of the SVG canvas.
   * @param strokeWidth The width of the edge lines.
   * @param padding     The padding around the tiling within the SVG viewBox.
   * @param scale       The factor by which to scale the tiling coordinates.
   * @return A String containing the SVG markup.
   */
  def toSVG(
    width: Int = 800,
    height: Int = 600,
    strokeWidth: Double = 1.0,
    padding: Double = 20.0,
    scale: Double = 50.0
  ): String =
   this.toScalableVectorGraphics(width, height, strokeWidth, padding, scale)

object TilingDCEL:

  def empty: TilingDCEL =
    TilingBuilder.empty

  def createSimplePolygon(angles: List[AngleDegree]): Either[String, TilingDCEL] =
    TilingBuilder.createSimplePolygon(angles)

  def createRegularPolygon(sides: Int): Either[String, TilingDCEL] =
    TilingBuilder.createRegularPolygon(sides): Either[String, TilingDCEL]
