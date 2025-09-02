package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, BigPoint, format}
import Topology.breadthFirstSearch

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

  override def toString: String =
    s"Vertex $id at coords (${coords.x.format}, ${coords.y.format})${validate().swap.map(msg => s" [$msg]").getOrElse("")}"

  def isComplete: Boolean =
    leaving.isDefined

  def validate(): Either[String, Unit] =
    if isComplete then Right(())
    else Left("Missing leaving edge")

  def incidentEdgesUnsafe: List[HalfEdge] =
    leaving match
      case None => List.empty
      case Some(startEdge) => startEdge.vertexTraversalUnsafe()

  def incidentEdges: Either[String, List[HalfEdge]] =
    leaving match
      case None => Right(List.empty)
      case Some(startEdge) => startEdge.vertexTraversal()

  def currentInteriorAngleSumUnsafe(outerFace: Face): AngleDegree =
    incidentEdgesUnsafe.interiorAnglesSum(outerFace)

  def currentInteriorAngleSum(outerFace: Face): Either[String, AngleDegree] =
    incidentEdges.map(_.interiorAnglesSum(outerFace))

  def degree: Int = incidentEdgesUnsafe.length

  def isThread: Boolean = degree == 2

  def adjacentVerticesUnsafe: List[Vertex] =
    incidentEdgesUnsafe.flatMap(_.destination)

  def incidentFacesUnsafe: List[Face] =
    incidentEdgesUnsafe.flatMap(_.incidentFace)

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
    val visited = breadthFirstSearch(start, adjacency)
    Option.when(visited == targetVertices)(())

  extension (vertices: List[Vertex])

    def sameCoords(others: List[Vertex], accuracy: Double = BigDecimalGeometry.ACCURACY): List[(Vertex, Vertex)] =
      for
        v1 <- vertices
        v2 <- others
        if v1.coords.almostEquals(v2.coords, accuracy)
      yield (v1, v2)
