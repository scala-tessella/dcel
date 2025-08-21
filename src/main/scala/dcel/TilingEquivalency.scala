package io.github.scala_tessella
package dcel

import BigDecimalGeometry.AngleDegree

import ring_seq.RingSeq.rotationsAndReflections

object TilingEquivalency:

  private def toMultiset[T](list: List[T]): Map[T, Int] =
    list.groupMapReduce(identity)(_ => 1)(_ + _)

  extension (tiling: TilingDCEL)

    /**
     * Creates a deep copy of this TilingDCEL that is completely independent.
     * Changes to the original will not affect the copy and vice versa.
     *
     * @return A new TilingDCEL instance with all components copied and properly linked.
     */
    def deepCopy: TilingDCEL =
      // Create mapping from old to new components
      val vertexMap = tiling.vertices.map(v => v -> Vertex(v.id, v.coords)).toMap
      val faceMap = tiling.faces.map(f => f -> Face(f.id)).toMap
      val halfEdgeMap = tiling.halfEdges.map(he => he -> HalfEdge(vertexMap(he.origin))).toMap

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

      // Copy vertex-leaving edge relationships
      tiling.vertices.foreach { oldVertex =>
        val newVertex = vertexMap(oldVertex)
        oldVertex.leaving.foreach { oldLeavingEdge =>
          newVertex.leaving = Some(halfEdgeMap(oldLeavingEdge))
        }
      }

      // Copy face outer component relationships
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

      // Create the new TilingDCEL with copied components
      TilingDCEL(
        vertices = tiling.vertices.map(vertexMap),
        halfEdges = tiling.halfEdges.map(halfEdgeMap),
        innerFaces = tiling.innerFaces.map(faceMap),
        outerFace = faceMap(tiling.outerFace)
      )

    /** Creates a geometric reflection of the tiling across the Y-axis.
     *  It achieves this by creating a deep copy of the tiling structure,
     *  while negating the x-coordinates of all vertices and reversing the orientation of the half-edge connections
     * 
     * @return
     */
    def reflectedCopy: TilingDCEL =
      // Create mapping from old to new components, reflecting vertex coordinates across the Y-axis
      val vertexMap = tiling.vertices.map(v => v -> Vertex(v.id, v.coords.copy(x = -v.coords.x))).toMap
      val faceMap = tiling.faces.map(f => f -> Face(f.id)).toMap
      val halfEdgeMap = tiling.halfEdges.map(he => he -> HalfEdge(vertexMap(he.origin))).toMap

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

        // Swap next and prev relationships to reverse orientation
        oldEdge.next.foreach { oldNext =>
          newEdge.prev = Some(halfEdgeMap(oldNext))
        }
        oldEdge.prev.foreach { oldPrev =>
          newEdge.next = Some(halfEdgeMap(oldPrev))
        }

        // Copy angle
        newEdge.angle = oldEdge.angle
      }

      // Copy vertex-leaving edge relationships
      tiling.vertices.foreach { oldVertex =>
        val newVertex = vertexMap(oldVertex)
        oldVertex.leaving.foreach { oldLeavingEdge =>
          newVertex.leaving = Some(halfEdgeMap(oldLeavingEdge))
        }
      }

      // Copy face outer component relationships
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

      // Create the new TilingDCEL with copied components
      TilingDCEL(
        vertices = tiling.vertices.map(vertexMap),
        halfEdges = tiling.halfEdges.map(halfEdgeMap),
        innerFaces = tiling.innerFaces.map(faceMap),
        outerFace = faceMap(tiling.outerFace)
      )

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
    def isTopologicallyEquivalentTo(other: TilingDCEL): Boolean =
      def getVertexSignature(vertex: Vertex, outerFace: Face): List[Int] =
        val faceCycle = vertex.incidentEdges.flatMap(_.incidentFace)
        val faceSizes = faceCycle.map(face =>
          if face == outerFace then 0
          else face.halfEdgesSafe.size
        )
        faceSizes.rotationsAndReflections.min

      isEquivalentRawTo(
        other,
        _.map(_.halfEdgesSafe.size).sorted,
        (vertices, face) => toMultiset(vertices.map(getVertexSignature(_, face)))
      )

    def isEquivalentTo(other: TilingDCEL): Boolean =
      given Ordering[AngleDegree] with
        def compare(x: AngleDegree, y: AngleDegree): Int =
          x.toRational.compare(y.toRational)

      def getCanonicalAngleSequence(angles: List[AngleDegree]): List[AngleDegree] =
        angles.rotationsAndReflections.min

      def getFaceSignature(face: Face): List[AngleDegree] =
        getCanonicalAngleSequence(face.halfEdgesSafe.flatMap(_.angle))

      def getVertexSignature(vertex: Vertex): List[AngleDegree] =
        getCanonicalAngleSequence(vertex.incidentEdges.flatMap(_.angle))

      tiling.isEquivalentRawTo(
        other,
        faces => toMultiset(faces.map(getFaceSignature)),
        (vertices, _) => toMultiset(vertices.map(getVertexSignature))
      )

    def isRotationOf(other: TilingDCEL): Boolean =
      ???

    def isReflectionOf(other: TilingDCEL): Boolean =
      !tiling.isRotationOf(other) && tiling.isEquivalentTo(other)
