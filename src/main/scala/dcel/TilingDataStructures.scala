package io.github.scala_tessella
package dcel

import scala.annotation.tailrec

/**
 * Represents a vertex in the tiling.
 *
 * @param id A unique identifier for the vertex.
 * @param x  The x-coordinate of the vertex.
 * @param y  The y-coordinate of the vertex.
 * @param leaving An optional half-edge originating from this vertex.
 */
case class Vertex(id: String, x: Double, y: Double):
  var leaving: Option[HalfEdge] = None
  override def toString: String = s"Vertex($id)"

/**
 * Represents a directed edge segment.
 *
 * @param origin The vertex where this half-edge starts.
 */
case class HalfEdge(origin: Vertex):
  var twin: Option[HalfEdge] = None
  var incidentFace: Option[Face] = None
  var next: Option[HalfEdge] = None
  var prev: Option[HalfEdge] = None
  var angle: Double = 0.0

  override def toString: String =
    // Safely get the destination vertex's ID from the optional twin
    val destId = twin.map(_.origin.id).getOrElse("?")
    s"HalfEdge(${origin.id} -> $destId)"

/**
 * Represents a face (a polygon) in the tiling.
 *
 * @param id A unique identifier for the face.
 * @param outerComponent An optional half-edge on the boundary of the face.
 */
case class Face(id: String):
  var outerComponent: Option[HalfEdge] = None
  override def toString: String = s"Face($id)"

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
    outerFace.outerComponent match
      case None => Vector.empty
      case Some(startEdge) =>
        val builder = Vector.newBuilder[Vertex]

        @tailrec
        def collectVertices(edge: HalfEdge): Unit =
          builder += edge.origin
          edge.next match
            case Some(nextEdge) if nextEdge ne startEdge => collectVertices(nextEdge)
            case _ => // Traversal complete or malformed loop

        collectVertices(startEdge)
        builder.result()