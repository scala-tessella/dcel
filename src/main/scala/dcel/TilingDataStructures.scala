package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, BigPoint}

import scala.annotation.tailrec
import scala.collection.mutable

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

object HalfEdge:

  def linkEdges(prev: HalfEdge, next: HalfEdge): Unit =
    prev.next = Some(next)
    next.prev = Some(prev)

  def insertBoundarySegment(prevEdge: HalfEdge, nextEdge: HalfEdge, segment: List[HalfEdge]): Unit =
    HalfEdge.linkEdges(prevEdge, segment.head)
    HalfEdge.linkEdges(segment.last, nextEdge)
    segment.sliding(2).foreach {
      case List(current, next) => HalfEdge.linkEdges(current, next)
      case _ => // Single element, no linking needed
    }

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
    
object Face:

  def adjacencyMap(faces: List[Face]): Map[Face, List[Face]] =
    faces.map { face =>
      face -> face.halfEdges
        .map(_.twin.get.incidentFace.get)
        .filter(faces.contains)
    }.toMap

  def breadthFirstSearch(start: Face, adjacency: Map[Face, List[Face]]): Set[Face] =
    val visited = mutable.Set[Face](start)
    val queue = mutable.Queue[Face](start)

    while queue.nonEmpty do
      val current = queue.dequeue()
      adjacency.getOrElse(current, Nil).foreach { neighbor =>
        if !visited.contains(neighbor) then
          visited += neighbor
          queue.enqueue(neighbor)
      }

    visited.toSet
