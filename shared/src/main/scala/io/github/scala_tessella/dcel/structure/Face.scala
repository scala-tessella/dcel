package io.github.scala_tessella.dcel.structure

import io.github.scala_tessella.dcel.Utils.{associate, sequence}
import io.github.scala_tessella.dcel.*
import io.github.scala_tessella.dcel.geometry.AngleDegree
import io.github.scala_tessella.dcel.structure.Utils.breadthFirstSearch

/** Represents a single face in the DCEL.
  *
  * @param id
  *   A unique identifier for the face.
  * @param outerComponent
  *   An optional reference to one of the half-edges on the face's outer boundary.
  * @param innerComponents
  *   A list of optional references to half-edges, one for each inner boundary (hole).
  */
final class Face(
    val id: FaceId,
    private[dcel] var outerComponent: Option[HalfEdge] = None,
    private[dcel] var innerComponents: List[Option[HalfEdge]] = Nil
):
  override def equals(obj: Any): Boolean =
    obj match
      case that: Face => this.id.value == that.id.value
      case _          => false

  override def hashCode(): Int = id.value.hashCode

  override def toString: String =
    s"Face $id${validate().swap.map(error => s" [${error.message}]").getOrElse("")}"

  def isOuter: Boolean =
    id == FaceId.outerId

  // Area calculation
  def area: BigDecimal =
    val vertices = getVertices.getOrElse(List.empty)
    vertices.map(_.coords).area

  def hasHoles: Boolean =
    innerComponents.nonEmpty

  def validate(): Either[IncompleteError, Unit] =
    val errors =
      List(
        Option.when(outerComponent.isEmpty)("Missing outer component edge")
      ).flatten ++
        (if innerComponents.exists(_.isEmpty) then
           List("One or more inner component edge references are missing")
         else Nil)

    if errors.isEmpty then Right(())
    else Left(IncompleteError(errors.mkString(", ")))

  private[dcel] def getVerticesUnsafe: List[Vertex] =
    outerComponent.get.faceTraversalUnsafe(_.origin)

  /** Get all vertices that form the boundary of a face. Returns vertices in the order they appear around the
    * face boundary.
    */
  def getVertices: Either[TopologyError, List[Vertex]] =
    outerComponent match
      case None            => Right(List.empty)
      case Some(startEdge) => startEdge.faceTraversal(_.origin)

  private[dcel] def halfEdgesUnsafe: List[HalfEdge] =
    outerComponent.get.faceTraversalUnsafe()

  /** Get all half-edges forming a face loop.
    */
  def halfEdges: Either[TopologyError, List[HalfEdge]] =
    outerComponent match
      case None        => Right(List.empty)
      case Some(start) => start.faceTraversal()

  private[dcel] def anglesUnsafe: List[AngleDegree] =
    halfEdgesUnsafe.map(_.angle.get)

  def angles: Either[TilingError, List[AngleDegree]] =
    halfEdges.flatMap(list => list.map(_.angle.toRight(GeometryError("Cannot find interior angle"))).sequence)

  private[dcel] def hasEqualAnglesUnsafe: Boolean =
    anglesUnsafe.toSet.size == 1

  /** Checks if the interior angles of the face are all equal. */
  def hasEqualAngles: Either[TilingError, Boolean] =
    angles.map(_.toSet.size == 1)

object Face:

  def apply(
      id: FaceId,
      outerComponent: Option[HalfEdge] = None,
      innerComponents: List[Option[HalfEdge]] = Nil
  ): Face = new Face(id, outerComponent, innerComponents)

  def outer: Face = Face(FaceId.outerId)

  def adjacencyMap(faces: List[Face]): Map[Face, List[Face]] =
    faces.associate { face =>

      face.halfEdges.getOrElse(List.empty)
        .flatMap(edge =>
          for
            twin         <- edge.twin
            incidentFace <- twin.incidentFace
            if faces.contains(incidentFace)
          yield incidentFace
        )
    }

  extension (faces: List[Face])

    def isConnected: Boolean =

      if faces.isEmpty then true
      else
        val adjacency = Face.adjacencyMap(faces)
        val reachable = breadthFirstSearch(faces.head, adjacency)
        reachable.size == faces.size
