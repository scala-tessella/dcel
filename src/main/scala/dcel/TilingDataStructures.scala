package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, BigPoint}

import scala.annotation.tailrec

/**
 * Represents a single vertex in the DCEL.
 *
 * @param id      A unique identifier for the vertex.
 * @param coords  The coordinates of the vertex.
 * @param leaving An optional reference to one of the half-edges originating from this vertex.
 */
case class Vertex(
  id: String,
  coords: BigPoint,
  var leaving: Option[HalfEdge] = None
):
  override def equals(obj: Any): Boolean =
    obj match
      case that: Vertex => this.id == that.id
      case _ => false

  override def hashCode(): Int = id.hashCode

/**
 * Represents a directed half-edge in the DCEL.
 *
 * @param origin       The vertex from which this half-edge originates.
 * @param twin         An optional reference to the half-edge that is its pair, running in the opposite direction.
 * @param incidentFace An optional reference to the face to the left of this edge.
 * @param next         An optional reference to the next half-edge in the boundary traversal of its incident face.
 * @param prev         An optional reference to the previous half-edge in the boundary traversal.
 * @param angle        The angle of the corner at the origin vertex, inside the incident face.
 */
case class HalfEdge(
  origin: Vertex,
  var twin: Option[HalfEdge] = None,
  var incidentFace: Option[Face] = None,
  var next: Option[HalfEdge] = None,
  var prev: Option[HalfEdge] = None,
  var angle: AngleDegree = AngleDegree(0.0)
):
  override def equals(obj: Any): Boolean = obj match
    case that: HalfEdge => this eq that
    case _ => false

  override def hashCode(): Int = System.identityHashCode(this)

/**
 * Represents a single face in the DCEL.
 *
 * @param id             A unique identifier for the face.
 * @param outerComponent An optional reference to one of the half-edges on the face's outer boundary.
 * @param innerComponents A list of optional references to half-edges, one for each inner boundary (hole).
 */
case class Face(
  id: String,
  var outerComponent: Option[HalfEdge] = None,
  var innerComponents: List[Option[HalfEdge]] = Nil
):
  override def equals(obj: Any): Boolean =
    obj match
      case that: Face => this.id == that.id
      case _ => false

  override def hashCode(): Int = id.hashCode

  /**
   * Get all half-edges forming a face loop.
   */
  def halfEdges: List[HalfEdge] =
    outerComponent.map { start =>
      @tailrec
      def loop(current: HalfEdge, acc: List[HalfEdge], visited: Set[HalfEdge]): List[HalfEdge] =
        if visited.contains(current) then return acc.reverse
        current.next match
          case Some(next) if next ne start => loop(next, current :: acc, visited + current)
          case _ => (current :: acc).reverse

      loop(start, Nil, Set.empty)
    }.getOrElse(Nil)
