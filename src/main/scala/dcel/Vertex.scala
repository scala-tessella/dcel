package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, BigPoint, format}

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

  override def toString: String = s"Vertex $id at coords (${coords.x.format}, ${coords.y.format})"

  def isComplete: Boolean =
    leaving.isDefined

  def validate(): Either[String, Unit] =
    if isComplete then Right(())
    else Left("Missing leaving edge")

  def incidentEdges: List[HalfEdge] =
    leaving match
      case None => List.empty
      case Some(startEdge) => startEdge.vertexTraversal()

  def getCurrentInteriorAngleSum(outerFace: Face): AngleDegree =
    incidentEdges
      .filterNot(_.incidentFace.contains(outerFace))
      .flatMap(_.angle)
      .fold(AngleDegree(0))(_ + _)

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
