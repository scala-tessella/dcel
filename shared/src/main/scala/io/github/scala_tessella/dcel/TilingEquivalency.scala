package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.Utils.associate
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.ring_seq.RingSeq.rotationsAndReflections

import scala.Ordering.Implicits.*
import scala.collection.mutable

object TilingEquivalency:

  private def toMultiset[T](list: List[T]): Map[T, Int] =
    list.groupMapReduce(identity)(_ => 1)(_ + _)

  /** Group the elements in classes of equivalent TilingDCEL. Uses boundary-only comparison for efficiency in
    * uniformity calculations.
    */
  def groupByBoundaryEquivalency[A](associatedTilings: List[(A, TilingDCEL)]): List[List[A]] =
    associatedTilings
      .foldLeft(List.empty[(TilingDCEL, List[A])]) { case (classes, (elem, tiling)) =>
        classes.indexWhere { case (representative, _) =>
          tiling.isBoundaryEquivalentTo(representative)
        } match
          case -1 =>
            // No equivalent class found, create a new one
            classes :+ (tiling, List(elem))
          case i  =>
            // Found an equivalent class at index i, add vertexId to it
            val (representative, elems) = classes(i)
            classes.updated(i, (representative, elem :: elems))
      }
      .map { case (_, elems) =>
        elems.reverse
      }

  private val defaultLeavingTransformer: (Vertex, HalfEdge, Map[HalfEdge, HalfEdge]) => Unit =
    (newVertex, oldLeavingEdge, halfEdgeMap) =>
      newVertex.leaving = Some(halfEdgeMap(oldLeavingEdge))

  // Compare only boundary vertex signatures (angles around each boundary vertex)
  given Ordering[AngleDegree] with
    def compare(x: AngleDegree, y: AngleDegree): Int =
      x.toRational.compare(y.toRational)

  extension (vertex: Vertex)

    private def incidentAngles: List[AngleDegree] =
      vertex.incidentEdgesUnsafe.flatMap: halfEdge =>
        halfEdge.angle

    private def getBoundaryVertexSignature: List[AngleDegree] =
      incidentAngles.rotationsAndReflections.min


  extension (tiling: TilingDCEL)

    private def createMaps(
        coordsTransformer: BigPoint => BigPoint,
        vertexIdTransformer: VertexId => VertexId,
        faceIdTransformer: FaceId => FaceId
    ): (Map[Vertex, Vertex], Map[HalfEdge, HalfEdge], Map[Face, Face]) =
      val vertexMap   =
        tiling.vertices.associate { v =>

          Vertex(
            id = vertexIdTransformer(v.id),
            coords = coordsTransformer(v.coords)
          )
        }
      val halfEdgeMap = tiling.halfEdges.associate(he => HalfEdge(vertexMap(he.origin)))
      val faceMap     =
        tiling.faces.associate { f =>

          Face(
            id = faceIdTransformer(f.id)
          )
        }
      (vertexMap, halfEdgeMap, faceMap)

    private def copyHalfEdgeRelationships(
        halfEdgeMap: Map[HalfEdge, HalfEdge],
        faceMap: Map[Face, Face]
    ): Unit =
      // Copy all relationships for half-edges
      tiling.halfEdges.foreach { oldEdge =>
        val newEdge = halfEdgeMap(oldEdge)

        // Copy twin relationship
        oldEdge.twin.foreach { oldTwin =>

          newEdge.twin = Some(halfEdgeMap(oldTwin))
        }

        // Copy incident face
        oldEdge.incidentFace.foreach { oldFace =>

          newEdge.incidentFace = Some(faceMap(oldFace))
        }

        // Copy next relationship
        oldEdge.next.foreach { oldNext =>

          newEdge.next = Some(halfEdgeMap(oldNext))
        }

        // Copy prev relationship
        oldEdge.prev.foreach { oldPrev =>

          newEdge.prev = Some(halfEdgeMap(oldPrev))
        }

        // Copy angle
        newEdge.angle = oldEdge.angle
      }

    private def copyFaceRelationships(halfEdgeMap: Map[HalfEdge, HalfEdge], faceMap: Map[Face, Face]): Unit =
      tiling.faces.foreach { oldFace =>
        val newFace = faceMap(oldFace)
        oldFace.outerComponent.foreach { oldOuterComponent =>

          newFace.outerComponent = Some(halfEdgeMap(oldOuterComponent))
        }

        // Copy inner components
        newFace.innerComponents = oldFace.innerComponents.map { optionalInnerComponent =>

          optionalInnerComponent.map(halfEdgeMap)
        }
      }

    private def copyVertexRelationships(
        vertexLeavingTransformer: (Vertex, HalfEdge, Map[HalfEdge, HalfEdge]) => Unit,
        vertexMap: Map[Vertex, Vertex],
        halfEdgeMap: Map[HalfEdge, HalfEdge]
    ): Unit =
      // Copy vertex-leaving edge relationships
      tiling.vertices.foreach { oldVertex =>
        val newVertex = vertexMap(oldVertex)
        oldVertex.leaving.foreach { oldLeavingEdge =>

          vertexLeavingTransformer(newVertex, oldLeavingEdge, halfEdgeMap)
        }
      }

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
      tiling.halfEdges.foreach { oldEdge =>
        val newEdge = halfEdgeMap(oldEdge)

        oldEdge.twin.foreach { t =>
          newEdge.twin = Some(halfEdgeMap(t))

          // Incident Face: face on the right of oldEdge (which is incident to twin)
          t.incidentFace.foreach { f =>

            newEdge.incidentFace = Some(faceMap(f))
          }

          // Next: twin of prev of twin
          t.prev.flatMap(_.twin).foreach { target =>

            newEdge.next = Some(halfEdgeMap(target))
          }

          // Prev: twin of next of twin
          t.next.flatMap(_.twin).foreach { target =>

            newEdge.prev = Some(halfEdgeMap(target))
          }

          // Angle: angle of next of twin (which corresponds to the same corner in the new face)
          t.next.foreach { tn =>

            newEdge.angle = tn.angle
          }
        }
      }

      // Wire Vertices
      tiling.vertices.foreach { oldVertex =>
        val newVertex = vertexMap(oldVertex)
        oldVertex.leaving.foreach { oldLeaving =>

          newVertex.leaving = Some(halfEdgeMap(oldLeaving))
        }
      }

      // Wire Faces
      tiling.faces.foreach { oldFace =>
        val newFace = faceMap(oldFace)

        // Outer component needs to be flipped (use twin) to maintain face-on-left invariant
        oldFace.outerComponent.flatMap(_.twin).foreach { target =>

          newFace.outerComponent = Some(halfEdgeMap(target))
        }

        // Inner components
        newFace.innerComponents = oldFace.innerComponents.map {
          _.flatMap(_.twin).map(halfEdgeMap)
        }
      }

      TilingDCEL(
        vertices = tiling.vertices.map(vertexMap),
        halfEdges = tiling.halfEdges.map(halfEdgeMap),
        innerFaces = tiling.innerFaces.map(faceMap),
        outerFace = faceMap(tiling.outerFace)
      )

    private def calculateBoundaryDistances: Map[Vertex, Int] =
      val distances = mutable.Map[Vertex, Int]()
      val queue     = mutable.Queue[(Vertex, Int)]()

      tiling.vertices.foreach { v =>

        if v.incidentEdgesUnsafe.exists(_.incidentFace.contains(tiling.outerFace)) then
          distances(v) = 0
          queue.enqueue((v, 0))
      }

      while queue.nonEmpty do
        val (current, dist) = queue.dequeue()
        current.incidentEdgesUnsafe.foreach { edge =>

          edge.destination.foreach { neighbor =>

            if !distances.contains(neighbor) then
              distances(neighbor) = dist + 1
              queue.enqueue((neighbor, dist + 1))
          }
        }
      distances.toMap

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

    /** Checks if this tiling is topologically equivalent to another. This comparison ignores spatial
      * coordinates and the specific IDs of vertices and faces, focusing instead on the combinatorial
      * structure of the tiling.
      *
      * Two tilings are considered topologically equivalent if they have:
      *   1. The same number of vertices, half-edges, and inner faces.
      *   2. The same multiset of face sizes (number of edges per face).
      *   3. The same multiset of vertex signatures, where a vertex signature is a canonical representation of
      *      the cycle of face sizes around that vertex.
      *
      * @param other
      *   The other TilingDCEL to compare with.
      * @return
      *   true if the two tilings are topologically equivalent, false otherwise.
      */
    def isTopologicallyEquivalentRobustTo(other: TilingDCEL): Boolean =
      if !tiling.hasSameSizesOf(other) then
        return false

      def computeCanonicalSignature(t: TilingDCEL): Map[String, Int] =
        // 1. Initial "colors" or labels for each vertex.
        var signatures: Map[Vertex, String] = t.vertices.map { v =>
          val faceCycle        = v.incidentEdgesUnsafe.flatMap(_.incidentFace)
          val faceSizes        = faceCycle.map(face =>
            if face == t.outerFace then 0
            else face.halfEdgesUnsafe.size
          )
          // The initial signature is a combination of degree and the canonical face size sequence.
          val initialSignature =
            s"${v.incidentEdgesUnsafe.size}:${faceSizes.rotationsAndReflections.min.mkString(",")}"
          v -> initialSignature
        }.toMap

        // 2. Iteratively refine signatures. The number of iterations is chosen to be the number of vertices,
        // which is a safe upper bound for information to propagate across the entire graph (related to graph diameter).
        val iterations = t.vertices.size
        (1 to iterations).foreach { _ =>
          val nextSignatures = t.vertices.associate { v =>
            // 3. For each vertex, collect the signatures of its neighbors.
            val neighborSignatures =
              v.incidentEdgesUnsafe.flatMap(_.destination).flatMap(signatures.get).sorted
            // 4. The new signature is a hash/combination of the current signature and the neighbors' signatures.
            s"${signatures(v)}|${neighborSignatures.mkString(";")}"
          }
          println(nextSignatures)
          signatures = nextSignatures
        }

        // 5. The final result is the multiset of the stable signatures.
        toMultiset(signatures.values.toList)

      computeCanonicalSignature(tiling) == computeCanonicalSignature(other)

    def isTopologicallyEquivalentTo(other: TilingDCEL): Boolean =
      def getVertexSignature(vertex: Vertex, outerFace: Face): List[Int] =
        val faceCycle = vertex.incidentEdgesUnsafe.flatMap(_.incidentFace)
        val faceSizes = faceCycle.map(face =>
          if face == outerFace then 0
          else face.halfEdgesUnsafe.size
        )
        faceSizes.rotationsAndReflections.min

      isEquivalentRawTo(
        other,
        _.map(_.halfEdgesUnsafe.size).sorted,
        (vertices, face) => toMultiset(vertices.map(getVertexSignature(_, face)))
      )

    def isEquivalentTo(other: TilingDCEL): Boolean =
      given Ordering[AngleDegree] with
        def compare(x: AngleDegree, y: AngleDegree): Int =
          x.toRational.compare(y.toRational)

      def getCanonicalAngleSequence(angles: List[AngleDegree]): List[AngleDegree] =
        angles.rotationsAndReflections.min

      def getFaceSignature(face: Face): List[AngleDegree] =
        getCanonicalAngleSequence(face.halfEdgesUnsafe.flatMap(_.angle))

      def getVertexSignature(vertex: Vertex): List[AngleDegree] =
        getCanonicalAngleSequence(vertex.incidentEdgesUnsafe.flatMap(_.angle))

      tiling.isEquivalentRawTo(
        other,
        faces => toMultiset(faces.map(getFaceSignature)),
        (vertices, _) => toMultiset(vertices.map(getVertexSignature))
      )

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
      if tiling.boundaryVertices.size != other.boundaryVertices.size then
        return false

      if tiling.boundaryEdges.size != other.boundaryEdges.size then
        return false

      // Compare only boundary vertex signatures (angles around each boundary vertex)
      given Ordering[AngleDegree] with
        def compare(x: AngleDegree, y: AngleDegree): Int =
          x.toRational.compare(y.toRational)

      def getBoundarySignatures(t: TilingDCEL) =
        t.boundaryVertices
          .map:
            _.getBoundaryVertexSignature
          .groupMapReduce(identity)(_ => 1):
            _ + _

      getBoundarySignatures(tiling) == getBoundarySignatures(other)

    def isReflectionOf(other: TilingDCEL): Boolean =
      ???

    def isRotationOf(other: TilingDCEL): Boolean =
      ???
