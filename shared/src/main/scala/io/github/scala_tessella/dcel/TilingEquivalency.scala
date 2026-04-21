package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.Utils.associate
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.ring_seq.RingSeq.rotationsAndReflections

import scala.Ordering.Implicits.*

object TilingEquivalency:

  /** Groups elements from the input list into classes of equivalency. We are using a (more performant)
    * boundary equivalence of their associated `TilingDCEL` structures because the inner elements of the
    * structures are assumed equal.
    *
    * @param associatedTilings
    *   A list of tuples where each tuple contains an element of type `A` and a corresponding `TilingDCEL`
    *   structure.
    *
    * @return
    *   A list of grouped lists, where each inner list contains elements of type `A` belonging to the same
    *   equivalency class.
    */
  def groupByBoundaryEquivalency[A](associatedTilings: List[(A, TilingDCEL)]): List[List[A]] =
    associatedTilings
      .groupBy: (_, tiling) =>
        tiling.boundarySignature
      .values
      .map: groups =>
        groups.map: (elem, _) =>
          elem
      .toList

  private val defaultLeavingTransformer: (Vertex, HalfEdge, Map[HalfEdge, HalfEdge]) => Unit =
    (newVertex, oldLeavingEdge, halfEdgeMap) =>
      newVertex.leaving = Some(halfEdgeMap(oldLeavingEdge))

  extension (angles: List[AngleDegree])

    private def canonicalSequence: List[AngleDegree] =
      angles.rotationsAndReflections.min

  extension (vertex: Vertex)

    private def incidentAngles: List[AngleDegree] =
      vertex.incidentEdgesUnsafe.flatMap: halfEdge =>
        halfEdge.angle

    private def signature: List[AngleDegree] =
      vertex.incidentAngles.canonicalSequence

  extension (tiling: TilingDCEL)

    /** Computes a canonical key representing the boundary of the tiling. */
    private[dcel] def boundarySignature: List[List[AngleDegree]] =
      tiling.boundaryVerticesUnsafe.toList
        .map: vertex =>
          vertex.signature
        .rotationsAndReflections.min

    private def createMaps(
        coordsTransformer: BigPoint => BigPoint,
        vertexIdTransformer: VertexId => VertexId,
        faceIdTransformer: FaceId => FaceId
    ): (Map[Vertex, Vertex], Map[HalfEdge, HalfEdge], Map[Face, Face]) =
      val vertexMap   =
        tiling.vertices.associate: vertex =>
          Vertex(
            id = vertexIdTransformer(vertex.id),
            coords = coordsTransformer(vertex.coords)
          )
      val halfEdgeMap =
        tiling.halfEdges.associate: halfEdge =>
          HalfEdge(vertexMap(halfEdge.origin))
      val faceMap     =
        tiling.faces.associate: face =>
          Face(id = faceIdTransformer(face.id))
      (vertexMap, halfEdgeMap, faceMap)

    private def copyHalfEdgeRelationships(
        halfEdgeMap: Map[HalfEdge, HalfEdge],
        faceMap: Map[Face, Face]
    ): Unit =
      // Copy all relationships for half-edges
      tiling.halfEdges.foreach: oldEdge =>
        val newEdge = halfEdgeMap(oldEdge)
        // Copy twin relationship
        oldEdge.twin.foreach: oldTwin =>
          newEdge.twin = Some(halfEdgeMap(oldTwin))
        // Copy incident face
        oldEdge.incidentFace.foreach: oldFace =>
          newEdge.incidentFace = Some(faceMap(oldFace))
        // Copy next relationship
        oldEdge.next.foreach: oldNext =>
          newEdge.next = Some(halfEdgeMap(oldNext))
        // Copy prev relationship
        oldEdge.prev.foreach: oldPrev =>
          newEdge.prev = Some(halfEdgeMap(oldPrev))
        // Copy angle
        newEdge.angle = oldEdge.angle

    private def copyFaceRelationships(halfEdgeMap: Map[HalfEdge, HalfEdge], faceMap: Map[Face, Face]): Unit =
      tiling.faces.foreach: oldFace =>
        val newFace = faceMap(oldFace)
        oldFace.outerComponent.foreach: oldOuterComponent =>
          newFace.outerComponent = Some(halfEdgeMap(oldOuterComponent))

        // Copy inner components
        newFace.innerComponents = oldFace.innerComponents.map: optionalInnerComponent =>
          optionalInnerComponent.map:
            halfEdgeMap

    private def copyVertexRelationships(
        vertexLeavingTransformer: (Vertex, HalfEdge, Map[HalfEdge, HalfEdge]) => Unit,
        vertexMap: Map[Vertex, Vertex],
        halfEdgeMap: Map[HalfEdge, HalfEdge]
    ): Unit =
      // Copy vertex-leaving edge relationships
      tiling.vertices.foreach: oldVertex =>
        val newVertex = vertexMap(oldVertex)
        oldVertex.leaving.foreach: oldLeavingEdge =>
          vertexLeavingTransformer(newVertex, oldLeavingEdge, halfEdgeMap)

    private def rawCopy(
        coordsTransformer: BigPoint => BigPoint,
        vertexLeavingTransformer: (Vertex, HalfEdge, Map[HalfEdge, HalfEdge]) => Unit,
        vertexIdTransformer: VertexId => VertexId,
        faceIdTransformer: FaceId => FaceId
    ): TilingDCEL =
      // Create mapping from old to new components
      val (vertexMap, halfEdgeMap, faceMap) =
        createMaps(coordsTransformer, vertexIdTransformer, faceIdTransformer)

      // Copy all relationships for half-edges
      copyHalfEdgeRelationships(halfEdgeMap, faceMap)

      // Copy vertex-leaving edge relationships
      copyVertexRelationships(vertexLeavingTransformer, vertexMap, halfEdgeMap)

      // Copy face relationships
      copyFaceRelationships(halfEdgeMap, faceMap)

      // Create the new TilingDCEL with copied components
      TilingDCEL(
        vertices = tiling.vertices.map(vertexMap),
        halfEdges = tiling.halfEdges.map(halfEdgeMap),
        innerFaces = tiling.innerFaces.map(faceMap),
        outerFace = faceMap(tiling.outerFace)
      )

    /** Creates a deep copy of this TilingDCEL that is completely independent. Changes to the original will
      * not affect the copy and vice versa.
      *
      * @return
      *   A new TilingDCEL instance with all components copied and properly linked.
      */
    def deepCopy: TilingDCEL =
      rawCopy(
        coordsTransformer = identity,
        vertexLeavingTransformer = defaultLeavingTransformer,
        vertexIdTransformer = identity,
        faceIdTransformer = identity
      )

    def translatedDouble(
        coordsTransformer: BigPoint => BigPoint,
        vertexIdTransformer: VertexId => VertexId,
        faceIdTransformer: FaceId => FaceId
    ): TilingDCEL =
      rawCopy(
        coordsTransformer = coordsTransformer,
        vertexLeavingTransformer = defaultLeavingTransformer,
        vertexIdTransformer = vertexIdTransformer,
        faceIdTransformer = faceIdTransformer
      )

    /** Creates a vertically reflected copy of the TilingDCEL based on a reflection axis that lies at the
      * midpoint of the minimum and maximum y-coordinates of the vertices. All components (vertices,
      * half-edges, faces) are copied and updated to establish consistent relationships in the reflected
      * structure.
      *
      * @return
      *   A new TilingDCEL instance representing the vertically reflected copy of the original.
      */
    def verticallyReflectedCopy: TilingDCEL =
      // Get all x-coordinates to find the bounding box
      val yCoords = tiling.vertices.map(_.coords.y)
      // Return a deep copy if there are no vertices to reflect
      if yCoords.isEmpty then return tiling.deepCopy

      val minY            = yCoords.min
      val maxY            = yCoords.max
      val reflectionAxisY = (minY + maxY) / 2

      // Create mapping from old to new components
      val (vertexMap, halfEdgeMap, faceMap) =
        createMaps(
          coordsTransformer = point => BigPoint(point.x, y = reflectionAxisY * 2 - point.y),
          vertexIdTransformer = identity,
          faceIdTransformer = identity
        )

      // Wire HalfEdges with reversed orientation
      tiling.halfEdges.foreach: oldEdge =>
        val newEdge = halfEdgeMap(oldEdge)

        oldEdge.twin.foreach: t =>
          newEdge.twin = Some(halfEdgeMap(t))

          // Incident Face: face on the right of oldEdge (which is incident to twin)
          t.incidentFace.foreach: f =>
            newEdge.incidentFace = Some(faceMap(f))

          // Next: twin of prev of twin
          t.prev.flatMap(_.twin).foreach: target =>
            newEdge.next = Some(halfEdgeMap(target))

          // Prev: twin of next of twin
          t.next.flatMap(_.twin).foreach: target =>
            newEdge.prev = Some(halfEdgeMap(target))

          // Angle: angle of next of twin (which corresponds to the same corner in the new face)
          t.next.foreach: tn =>
            newEdge.angle = tn.angle

      // Wire Vertices
      tiling.vertices.foreach: oldVertex =>
        val newVertex = vertexMap(oldVertex)
        oldVertex.leaving.foreach: oldLeaving =>
          newVertex.leaving = Some(halfEdgeMap(oldLeaving))

      // Wire Faces
      tiling.faces.foreach: oldFace =>
        val newFace = faceMap(oldFace)

        // Outer component needs to be flipped (use twin) to maintain face-on-left invariant
        oldFace.outerComponent
          .flatMap: halfEdge =>
            halfEdge.twin
          .foreach: target =>
            newFace.outerComponent = Some(halfEdgeMap(target))

        // Inner components
        newFace.innerComponents =
          oldFace.innerComponents.map: maybeHalfEdge =>
            maybeHalfEdge
              .flatMap: halfEdge =>
                halfEdge.twin
              .map: halfEdge =>
                halfEdgeMap(halfEdge)

      TilingDCEL(
        vertices = tiling.vertices.map(vertexMap),
        halfEdges = tiling.halfEdges.map(halfEdgeMap),
        innerFaces = tiling.innerFaces.map(faceMap),
        outerFace = faceMap(tiling.outerFace)
      )

    private[dcel] def hasSameSizesOf(other: TilingDCEL): Boolean =
      tiling.vertices.size == other.vertices.size
        && tiling.innerFaces.size == other.innerFaces.size
        && tiling.halfEdges.size == other.halfEdges.size

    private def isEquivalentRawTo[T, U](
        other: TilingDCEL,
        faceSignaturesFrom: List[Face] => Iterable[T],
        vertexSignaturesFrom: (List[Vertex], Face) => Map[List[U], Int]
    ): Boolean =
      if !tiling.hasSameSizesOf(other) then
        return false

      val thisFaceSignatures  = faceSignaturesFrom(tiling.innerFaces)
      val otherFaceSignatures = faceSignaturesFrom(other.innerFaces)

      if thisFaceSignatures != otherFaceSignatures then
        return false

      val thisVertexSignatures  = vertexSignaturesFrom(tiling.vertices, tiling.outerFace)
      val otherVertexSignatures = vertexSignaturesFrom(other.vertices, other.outerFace)

      thisVertexSignatures == otherVertexSignatures

    /** Fast equivalence check for uniformity calculation that only compares boundary layers.
      *
      * This method is optimized for comparing tilings centered at different vertices with the same distance
      * parameter. It only compares the boundary (outermost layer) rather than the entire structure, since the
      * inner portions are identical.
      *
      * @param other
      *   The other TilingDCEL to compare with.
      * @return
      *   true if the boundary layers are equivalent, false otherwise.
      */
    def isBoundaryEquivalentTo(other: TilingDCEL): Boolean =
      // Quick size checks first
      tiling.boundaryVerticesUnsafe.sizeCompare(other.boundaryVerticesUnsafe) == 0
        && tiling.boundarySignature == other.boundarySignature
