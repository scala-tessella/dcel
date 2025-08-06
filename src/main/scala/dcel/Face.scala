package io.github.scala_tessella
package dcel

import scala.collection.mutable

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

  override def toString: String = s"Face $id"

  // Area calculation
  def area: BigDecimal =
    val vertices = getVertices.getOrElse(List.empty)
    if vertices.length < 3 then BigDecimal(0)
    else
      // Shoelace formula
      val sum = vertices.zip(vertices.tail :+ vertices.head).map { case (v1, v2) =>
        v1.coords.x * v2.coords.y - v2.coords.x * v1.coords.y
      }.sum
      sum.abs / 2

  def hasHoles: Boolean =
    innerComponents.nonEmpty

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
  def getVertices: Either[String, List[Vertex]] =
    outerComponent match
      case None => Right(List.empty)
      case Some(startEdge) => startEdge.faceTraversalWithGuards(_.origin)

  /**
   * Get all half-edges forming a face loop.
   */
  def halfEdges: Either[String, List[HalfEdge]] =
    outerComponent match
      case None => Right(List.empty)
      case Some(start) => start.faceTraversalWithGuards()

  // Add safe version that returns empty list on error
  def halfEdgesSafe: List[HalfEdge] =
    halfEdges.getOrElse(List.empty)

object Face:

  val outerId = "F0"

  val firstInnerId = "F1"

  def adjacencyMap(faces: List[Face]): Map[Face, List[Face]] =
    faces.map { face =>
      face -> face.halfEdgesSafe
        .flatMap(edge =>
          for
            twin <- edge.twin
            incidentFace <- twin.incidentFace
            if faces.contains(incidentFace)
          yield incidentFace
        )
    }.toMap
