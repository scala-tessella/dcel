package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, BigBox, BigLineSegment, BigPoint}
import Polygon.RegularPolygon
import TilingAddition.*
import TilingSVG.*

import spire.implicits.*

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

  def findVertex(id: String): Option[Vertex] =
    vertices.find(_.id == id)

  def findFace(id: String): Option[Face] =
    faces.find(_.id == id)

  def findEdgeBetween(v1: Vertex, v2: Vertex): Option[HalfEdge] =
    v1.incidentEdges.find(_.destination.contains(v2))

  def isConnected: Boolean =
    if innerFaces.isEmpty then true
    else
      val adjacency = Face.adjacencyMap(innerFaces)
      val reachable = Face.breadthFirstSearch(innerFaces.head, adjacency)
      reachable.size == innerFaces.size

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
      case Some(startEdge) => startEdge.faceTraversal(_.origin).toVector
      case None => Vector.empty

  def boundarySafe: Either[String, Vector[Vertex]] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversalWithGuards(_.origin).map(_.toVector)
      case None => Right(Vector.empty)

  /**
   * Helper method to get all half-edges forming the outer boundary loop.
   */
  def getBoundaryEdges: Either[String, List[HalfEdge]] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversalWithGuards()
      case None => Right(List.empty)

  /**
   * Finds a boundary half-edge that originates at the vertex with the given ID.
   *
   * @param vertexId The ID of the origin vertex.
   * @return An Option containing the HalfEdge if found, otherwise None.
   */
  private def findBoundaryEdge(vertexId: String): Option[HalfEdge] =
    getBoundaryEdges.toOption.flatMap(_.find(_.origin.id == vertexId))

  def maybeAddRegularPolygon(sides: Int, onEdgeStartingWithVertexId: String): Either[String, TilingDCEL] =
    this.addRegularPolygon(sides, onEdgeStartingWithVertexId)

  /**
   * Generates an SVG representation of the tiling.
   * The width, height, and viewBox are automatically calculated to fit the tiling at the given scale.
   *
   * @param strokeWidth The width of the edge lines.
   * @param padding     The padding around the tiling within the SVG viewBox.
   * @param scale       The factor by which to scale the tiling coordinates.
   * @return A String containing the SVG markup.
   */
  def toSVG(
    strokeWidth: Double = 1.0,
    padding: Double = 20.0,
    scale: Double = 50.0
  ): String =
    this.toScalableVectorGraphics(strokeWidth, padding, scale)

object TilingDCEL:

  def empty: TilingDCEL =
    TilingBuilder.empty

  def createSimplePolygon(angles: List[AngleDegree]): Either[String, TilingDCEL] =
    TilingBuilder.createSimplePolygon(angles)

  def createRegularPolygon(sides: Int): Either[String, TilingDCEL] =
    TilingBuilder.createRegularPolygon(sides): Either[String, TilingDCEL]

  def validate(tiling: TilingDCEL): Either[String, Unit] = {
    val errors = mutable.ListBuffer[String]()

    // Check vertex consistency
    tiling.vertices.foreach { vertex =>
      vertex.leaving match {
        case None => errors += s"Vertex ${vertex.id} has no leaving edge"
        case Some(edge) =>
          if (edge.origin ne vertex) {
            errors += s"Vertex ${vertex.id} leaving edge doesn't originate from it"
          }
      }
    }

    // Check half-edge consistency
    tiling.halfEdges.foreach { edge =>
      edge.twin match {
        case None => errors += s"Edge from ${edge.origin.id} has no twin"
        case Some(twin) =>
          if (twin.twin.contains(edge)) () // OK
          else errors += s"Edge from ${edge.origin.id} twin relationship is not symmetric"
      }

      edge.next match {
        case None => errors += s"Edge from ${edge.origin.id} has no next edge"
        case Some(next) =>
          if (next.prev.contains(edge)) () // OK
          else errors += s"Edge from ${edge.origin.id} next/prev relationship is broken"
      }
    }

    // Check face consistency
    (tiling.outerFace :: tiling.innerFaces).foreach { face =>
      face.halfEdges match {
        case Left(error) => errors += error
        case Right(edges) =>
          edges.foreach { edge =>
            if (!edge.incidentFace.contains(face)) {
              errors += s"Face ${face.id} contains edge that doesn't reference it back"
            }
          }
      }
    }

    // Check sum of angles for each inner face
    tiling.innerFaces.foreach { face =>
      face.halfEdges match {
        case Right(edges) =>
          val angles = edges.flatMap(_.angle)
          if (angles.length == edges.length) {
            if (angles.length >= 3) {
              Polygon.SimplePolygon.validatePolygonAngles(angles).left.foreach(error =>
                errors += s"Face ${face.id}: $error"
              )
            }
          } else {
            errors += s"Face ${face.id} has missing angles on some edges."
          }
        case Left(error) =>
          errors += s"Could not validate angles for face ${face.id} due to: $error"
      }
    }

    // Check sum of angles for the tiling boundary
    tiling.boundarySafe match {
      case Right(boundaryVertices) =>
        if (boundaryVertices.length >= 3) {
          val boundaryAngles = boundaryVertices.map { v =>
            v.incidentEdges
              .filterNot(_.incidentFace.contains(tiling.outerFace))
              .flatMap(_.angle)
              .fold(AngleDegree(0))(_ + _)
          }.toList
          Polygon.SimplePolygon.validatePolygonAngles(boundaryAngles).left.foreach(error =>
            errors += s"Boundary: $error"
          )
        }
      case Left(error) =>
        errors += s"Could not validate boundary angles due to: $error"
    }

    if (errors.isEmpty) Right(()) else Left(errors.mkString("; "))
  }