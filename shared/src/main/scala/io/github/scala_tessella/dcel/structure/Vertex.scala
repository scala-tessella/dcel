package io.github.scala_tessella.dcel.structure

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.format
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigDecimalGeometry, BigPoint}
import io.github.scala_tessella.dcel.{IncompleteError, TopologyError}

/** Represents a single vertex in the DCEL.
  *
  * @param id
  *   A unique identifier for the vertex.
  * @param coords
  *   The coordinates of the vertex.
  * @param leaving
  *   An optional reference to one of the half-edges originating from this vertex.
  */
final class Vertex(
    val id: VertexId,
    val coords: BigPoint,
    private[dcel] var leaving: Option[HalfEdge] = None
):
  override def equals(obj: Any): Boolean =
    obj match
      case that: Vertex => this.id.value == that.id.value
      case _            => false

  override def hashCode(): Int = id.value.hashCode

  override def toString: String =
    s"Vertex $id at coords (${coords.x.format}, ${coords.y.format})${validate().swap.map(error =>
        s" [${error.message}]"
      ).getOrElse("")}"

  def isComplete: Boolean =
    leaving.isDefined

  def validate(): Either[IncompleteError, Unit] =
    if isComplete then Right(())
    else Left(IncompleteError("Missing leaving edge"))

  private[dcel] def incidentEdgesUnsafe: List[HalfEdge] =
    leaving match
      case None            => List.empty
      case Some(startEdge) => startEdge.vertexTraversalUnsafe()

  def incidentEdges: Either[TopologyError, List[HalfEdge]] =
    leaving match
      case None            => Right(List.empty)
      case Some(startEdge) => startEdge.vertexTraversal()

  private[dcel] def currentInteriorAngleSumUnsafe(outerFace: Face): AngleDegree =
    incidentEdgesUnsafe.interiorAnglesSum(outerFace)

  def currentInteriorAngleSum(outerFace: Face): Either[TopologyError, AngleDegree] =
    incidentEdges.map: halfEdge =>
      halfEdge.interiorAnglesSum(outerFace)

  def degree: Int = incidentEdgesUnsafe.length

  def isThread: Boolean = degree == 2

  private[dcel] def adjacentVerticesUnsafe: List[Vertex] =
    incidentEdgesUnsafe.flatMap: halfEdge =>
      halfEdge.destination

  // Safe helper returning all distinct adjacent vertices
  def adjacentVertices: Either[TopologyError, List[Vertex]] =
    incidentEdges.map: halfEdges =>
      halfEdges
        .flatMap: halfEdge =>
          halfEdge.destination
        .distinct

  private[dcel] def incidentFacesUnsafe: List[Face] =
    incidentEdgesUnsafe.flatMap: halfEdge =>
      halfEdge.incidentFace

  private[dcel] def findEdgeBetweenUnsafe(other: Vertex): Option[HalfEdge] =
    incidentEdgesUnsafe.find: halfEdge =>
      halfEdge.destination.contains(other)

object Vertex:

  def apply(
      id: VertexId,
      coords: BigPoint,
      leaving: Option[HalfEdge] = None
  ): Vertex = new Vertex(id, coords, leaving)

  /** Builds an adjacency map for vertices that are connected through boundary edges. Only includes vertices
    * that are in the sharedVertices set.
    */
  def buildBoundaryVertexAdjacency(
      boundaryEdges: List[HalfEdge],
      sharedVertices: Set[Vertex]
  ): Map[Vertex, List[Vertex]] =
    boundaryEdges
      .filter: halfEdge =>
        sharedVertices.contains(halfEdge.origin)
      .groupBy: halfEdge =>
        halfEdge.origin
      .view
      .mapValues: halfEdges =>
        halfEdges.flatMap: halfEdge =>
          val destination = halfEdge.twin.get.origin
          Option.when(sharedVertices.contains(destination))(destination)
      .toMap

  extension (vertices: List[Vertex])

    def sameCoords(
        others: List[Vertex],
        accuracy: Double = BigDecimalGeometry.ACCURACY
    ): List[(Vertex, Vertex)] =
      for
        v1 <- vertices
        v2 <- others
        if v1.coords.almostEquals(v2.coords, accuracy)
      yield (v1, v2)
