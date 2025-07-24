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

  def isComplete: Boolean =
    leaving.isDefined

  def validate(): Either[String, Unit] =
    if isComplete then Right(())
    else Left("Missing leaving edge")

  def incidentEdges: List[HalfEdge] =
    leaving match
      case None => List.empty
      case Some(startEdge) =>
        @tailrec
        def collectEdges(current: HalfEdge, acc: List[HalfEdge]): List[HalfEdge] = {
          val updatedAcc = current :: acc
          current.twin.flatMap(_.next) match {
            case Some(next) if next ne startEdge => collectEdges(next, updatedAcc)
            case _ => updatedAcc.reverse
          }
        }

        collectEdges(startEdge, Nil)

  def degree: Int = incidentEdges.length

  def adjacentVertices: List[Vertex] =
    incidentEdges.flatMap(_.destination)

  def incidentFaces: List[Face] =
    incidentEdges.flatMap(_.incidentFace)

object Vertex:

  /**
   * Builds an adjacency map for vertices that are connected through boundary edges.
   * Only includes vertices that are in the sharedVertices set.
   */
  def buildBoundaryVertexAdjacency(boundaryEdges: List[HalfEdge], sharedVertices: Set[Vertex]): Map[Vertex, List[Vertex]] =
    boundaryEdges
      .filter(edge => sharedVertices.contains(edge.origin))
      .groupBy(_.origin)
      .view
      .mapValues { edges =>
        edges.flatMap { edge =>
          val destination = edge.twin.get.origin
          Option.when(sharedVertices.contains(destination))(destination)
        }
      }
      .toMap

  /**
   * Performs a traversal to check if all target vertices are reachable from the start vertex
   * through the boundary path.
   */
  def checkConnectivity(start: Vertex, targetVertices: Set[Vertex], adjacency: Map[Vertex, List[Vertex]]): Option[Unit] =
    val visited = mutable.Set[Vertex](start)
    val queue = mutable.Queue[Vertex](start)

    while queue.nonEmpty do
      val current = queue.dequeue()
      adjacency.getOrElse(current, Nil).foreach { neighbor =>
        if !visited.contains(neighbor) then
          visited += neighbor
          queue.enqueue(neighbor)
      }

    Option.when(visited == targetVertices)(())

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

  // Area calculation
  def area: BigDecimal =
    val vertices = getVertices
    if vertices.length < 3 then BigDecimal(0)
    else
      // Shoelace formula
      val sum = vertices.zip(vertices.tail :+ vertices.head).map { case (v1, v2) =>
        v1.coords.x * v2.coords.y - v2.coords.x * v1.coords.y
      }.sum
      sum.abs / 2

  def isComplete: Boolean =
    outerComponent.isDefined && innerComponents.nonEmpty

  def validate(): Either[String, Unit] =
    val errors = List(
      Option.when(outerComponent.isEmpty)("Missing outer component edge"),
      Option.when(innerComponents.isEmpty)("Missing inner components edges"),
    ).flatten

    if errors.isEmpty then Right(())
    else Left(errors.mkString(", "))

  /**
   * Get all vertices that form the boundary of a face.
   * Returns vertices in the order they appear around the face boundary.
   */
  def getVertices: List[Vertex] =
    outerComponent match
      case None => List.empty
      case Some(startEdge) =>
        val vertices = mutable.ListBuffer[Vertex]()
        val visited = mutable.Set[HalfEdge]()
        @tailrec
        def traverseFace(edge: HalfEdge): Unit =
          if !visited.contains(edge) then
            visited += edge
            vertices += edge.origin
            edge.next match
              case Some(nextEdge) if nextEdge ne startEdge => traverseFace(nextEdge)
              case _ => // Done traversing or reached start

        traverseFace(startEdge)
        vertices.toList

  /**
   * Get all half-edges forming a face loop.
   */
  def halfEdges: Either[String, List[HalfEdge]] = {
    outerComponent match {
      case None => Right(List.empty)
      case Some(start) =>
        try {
          @tailrec
          def collectEdges(current: HalfEdge, acc: List[HalfEdge], visited: Set[HalfEdge]): Either[String, List[HalfEdge]] = {
            if (visited.contains(current)) {
              Left(s"Cycle detected in face $id at edge originating from ${current.origin.id}")
            } else {
              val newVisited = visited + current
              val newAcc = current :: acc

              current.next match {
                case Some(next) if next ne start =>
                  collectEdges(next, newAcc, newVisited)
                case Some(_) =>
                  // next == start, we've completed the loop
                  Right(newAcc.reverse)
                case None =>
                  Left(s"Broken edge chain in face $id at edge from ${current.origin.id}")
              }
            }
          }

          collectEdges(start, Nil, Set.empty)
        } catch {
          case e: Exception => Left(s"Error traversing face $id: ${e.getMessage}")
        }
    }
  }

  // Add safe version that returns empty list on error
  def halfEdgesSafe: List[HalfEdge] = halfEdges.getOrElse(List.empty)

object Face:

  def adjacencyMap(faces: List[Face]): Map[Face, List[Face]] =
    faces.map { face =>
      face -> face.halfEdgesSafe
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
