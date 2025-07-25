package io.github.scala_tessella
package dcel

import BigDecimalGeometry.AngleDegree

import scala.annotation.tailrec
import scala.collection.mutable

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
  var angle: Option[AngleDegree] = None
):
  override def equals(obj: Any): Boolean = obj match
    case that: HalfEdge => this eq that
    case _ => false

  override def hashCode(): Int = System.identityHashCode(this)

  def destination: Option[Vertex] =
    twin.map(_.origin)

  def isComplete: Boolean =
    twin.isDefined && incidentFace.isDefined && next.isDefined && prev.isDefined && angle.isDefined

  def validate(): Either[String, Unit] =
    val errors = List(
      Option.when(twin.isEmpty)("Missing twin edge"),
      Option.when(incidentFace.isEmpty)("Missing incident face"),
      Option.when(next.isEmpty)("Missing next edge"),
      Option.when(prev.isEmpty)("Missing previous edge"),
      Option.when(angle.isEmpty)("Missing angle")
    ).flatten

    if errors.isEmpty then Right(())
    else Left(errors.mkString(", "))

  private def traverse[T](direction: HalfEdge => Option[HalfEdge])(f: HalfEdge => T = identity): List[T] =
    val startEdge = this

    @tailrec
    def collectEdges(current: HalfEdge, acc: List[T]): List[T] =
      val updatedAcc = f(current) :: acc
      direction(current) match
        case Some(next) if next ne startEdge => collectEdges(next, updatedAcc)
        case _ => updatedAcc.reverse

    collectEdges(startEdge, Nil)

  def vertexTraversal[T](f: HalfEdge => T = identity): List[T] =
    traverse[T](_.twin.flatMap(_.next))(f)

  def vertexTraversalWithGuards[T](f: HalfEdge => T = identity): Either[String, List[T]] =
    traverseWithGuards[T](_.twin.flatMap(_.next))(f)

  private def traverseWithGuards[T](direction: HalfEdge => Option[HalfEdge])(f: HalfEdge => T = identity): Either[String, List[T]] =
    val startEdge = this
    val visited = mutable.Set[HalfEdge]()

    @tailrec
    def collectEdges(current: HalfEdge, acc: List[T]): Either[String, List[T]] =
      if visited.contains(current) then
        Left(s"Cycle detected: edge from vertex ${current.origin.id} has already been visited")
      else
        visited += current
        val updatedAcc = f(current) :: acc

        direction(current) match
          case Some(next) if next ne startEdge =>
            collectEdges(next, updatedAcc)
          case Some(_) =>
            // next == startEdge, we've completed the traversal
            Right(updatedAcc.reverse)
          case None =>
            Left(s"Broken edge chain: edge from vertex ${current.origin.id} has no next")

    collectEdges(startEdge, Nil)

  def faceTraversal[T](f: HalfEdge => T = identity): List[T] =
    traverse[T](_.next)(f)

  def faceTraversalWithGuards[T](f: HalfEdge => T = identity): Either[String, List[T]] =
    traverseWithGuards[T](_.next)(f)

object HalfEdge:

  def createTwinPair(v1: Vertex, v2: Vertex): (HalfEdge, HalfEdge) =
    val edge1 = HalfEdge(v1)
    val edge2 = HalfEdge(v2)
    edge1.twin = Some(edge2)
    edge2.twin = Some(edge1)
    (edge1, edge2)

  def linkEdges(prev: HalfEdge, next: HalfEdge): Unit =
    prev.next = Some(next)
    next.prev = Some(prev)

  def linkChain(edges: List[HalfEdge]): Unit =
    edges.zip(edges.tail :+ edges.head).foreach { case (current, next) =>
      linkEdges(current, next)
    }
  
  def insertBoundarySegment(prevEdge: HalfEdge, nextEdge: HalfEdge, segment: List[HalfEdge]): Unit =
    HalfEdge.linkEdges(prevEdge, segment.head)
    HalfEdge.linkEdges(segment.last, nextEdge)
    segment.sliding(2).foreach {
      case List(current, next) => HalfEdge.linkEdges(current, next)
      case _ => // Single element, no linking needed
    }
