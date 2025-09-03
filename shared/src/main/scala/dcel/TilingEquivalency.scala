package dcel

import dcel.BigDecimalGeometry.{AngleDegree, BigPoint}

import io.github.scala_tessella.ring_seq.RingSeq.rotationsAndReflections

import scala.collection.mutable

object TilingEquivalency:

  private def toMultiset[T](list: List[T]): Map[T, Int] =
    list.groupMapReduce(identity)(_ => 1)(_ + _)

  extension (tiling: TilingDCEL)

    private def createMaps(coordsTransformer: BigPoint => BigPoint): (Map[Vertex, Vertex], Map[HalfEdge, HalfEdge], Map[Face, Face]) =
      val vertexMap = tiling.vertices.map(v => v -> Vertex(v.id, coordsTransformer(v.coords))).toMap
      val halfEdgeMap = tiling.halfEdges.map(he => he -> HalfEdge(vertexMap(he.origin))).toMap
      val faceMap = tiling.faces.map(f => f -> Face(f.id)).toMap
      (vertexMap, halfEdgeMap, faceMap)

    private def copyHalfEdgeRelationships(halfEdgeMap: Map[HalfEdge, HalfEdge], faceMap: Map[Face, Face]): Unit =
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
      vertexLeavingTransformer: (Vertex, HalfEdge, Map[HalfEdge, HalfEdge]) => Unit
    ): TilingDCEL =
      // Create mapping from old to new components
      val (vertexMap, halfEdgeMap, faceMap) = createMaps(coordsTransformer)

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

    /**
     * Creates a deep copy of this TilingDCEL that is completely independent.
     * Changes to the original will not affect the copy and vice versa.
     *
     * @return A new TilingDCEL instance with all components copied and properly linked.
     */
    def deepCopy: TilingDCEL =
      rawCopy(
        coordsTransformer = identity,
        vertexLeavingTransformer =
          (newVertex, oldLeavingEdge, halfEdgeMap) =>
            newVertex.leaving = Some(halfEdgeMap(oldLeavingEdge))
      )

    /** Creates a geometric reflection of the tiling across the vertical axis of its bounding box.
     * It achieves this by creating a deep copy of the tiling structure,
     * while re-calculating the x-coordinates of all vertices and reversing the orientation of the half-edge connections
     */
    def verticallyReflectedCopy: TilingDCEL =
      // Get all x-coordinates to find the bounding box
      val yCoords = tiling.vertices.map(_.coords.y)
      // Return a deep copy if there are no vertices to reflect
      if yCoords.isEmpty then return tiling.deepCopy

      val minY = yCoords.min
      val maxY = yCoords.max
      val reflectionAxisY = (minY + maxY) / 2

      rawCopy(
        // Reflecting vertex coordinates across the calculated horizontal axis
        coordsTransformer = point => point.copy(y = reflectionAxisY * 2 - point.y),
        // In a reflected copy, the edge cycle around a vertex is reversed.
        // The new `leaving` edge should be the one that PRECEDES the old `leaving` edge's twin.
        vertexLeavingTransformer =
          (newVertex, oldLeavingEdge, halfEdgeMap) =>
            oldLeavingEdge.prev.flatMap(_.twin).foreach(newLeaving =>
              newVertex.leaving = Some(halfEdgeMap(newLeaving))
            )
      )

    private def calculateBoundaryDistances: Map[Vertex, Int] =
      val distances = mutable.Map[Vertex, Int]()
      val queue = mutable.Queue[(Vertex, Int)]()

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

    private def hasSameSizesOf(other: TilingDCEL): Boolean =
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

      val thisFaceSignatures = faceSignaturesFrom(tiling.innerFaces)
      val otherFaceSignatures = faceSignaturesFrom(other.innerFaces)

      if thisFaceSignatures != otherFaceSignatures then
        return false

      val thisVertexSignatures = vertexSignaturesFrom(tiling.vertices, tiling.outerFace)
      val otherVertexSignatures = vertexSignaturesFrom(other.vertices, other.outerFace)

      thisVertexSignatures == otherVertexSignatures

    /**
     * Checks if this tiling is topologically equivalent to another.
     * This comparison ignores spatial coordinates and the specific IDs of vertices and faces,
     * focusing instead on the combinatorial structure of the tiling.
     *
     * Two tilings are considered topologically equivalent if they have:
     * 1. The same number of vertices, half-edges, and inner faces.
     * 2. The same multiset of face sizes (number of edges per face).
     * 3. The same multiset of vertex signatures, where a vertex signature is
     * a canonical representation of the cycle of face sizes around that vertex.
     *
     * @param other The other TilingDCEL to compare with.
     * @return true if the two tilings are topologically equivalent, false otherwise.
     */
    def isTopologicallyEquivalentRobustTo(other: TilingDCEL): Boolean =
      if !tiling.hasSameSizesOf(other) then
        return false

      def computeCanonicalSignature(t: TilingDCEL): Map[String, Int] =
        // 1. Initial "colors" or labels for each vertex.
        var signatures: Map[Vertex, String] = t.vertices.map { v =>
          val faceCycle = v.incidentEdgesUnsafe.flatMap(_.incidentFace)
          val faceSizes = faceCycle.map(face =>
            if face == t.outerFace then 0
            else face.halfEdgesUnsafe.size
          )
          // The initial signature is a combination of degree and the canonical face size sequence.
          val initialSignature = s"${v.incidentEdgesUnsafe.size}:${faceSizes.rotationsAndReflections.min.mkString(",")}"
          v -> initialSignature
        }.toMap

        // 2. Iteratively refine signatures. The number of iterations is chosen to be the number of vertices,
        // which is a safe upper bound for information to propagate across the entire graph (related to graph diameter).
        val iterations = t.vertices.size
        (1 to iterations).foreach { _ =>
          val nextSignatures = t.vertices.map { v =>
            // 3. For each vertex, collect the signatures of its neighbors.
            val neighborSignatures = v.incidentEdgesUnsafe.flatMap(_.destination).flatMap(signatures.get).sorted
            // 4. The new signature is a hash/combination of the current signature and the neighbors' signatures.
            val aggregatedSignature = s"${signatures(v)}|${neighborSignatures.mkString(";")}"
            v -> aggregatedSignature
          }.toMap
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

    def isReflectionOf(other: TilingDCEL): Boolean =
      ???

    def isRotationOf(other: TilingDCEL): Boolean =
      ???
