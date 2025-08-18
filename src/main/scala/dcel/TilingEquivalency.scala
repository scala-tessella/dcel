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
  
      // Copy vertex leaving edge relationships
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
  
    private def getVertexSignature(vertex: Vertex): List[Int] =
      val faceCycle = vertex.incidentEdges.flatMap(_.incidentFace)
      val faceSizes = faceCycle.map(face =>
        if face == tiling.outerFace then 0
        else face.halfEdgesSafe.size
      )
      faceSizes.rotationsAndReflections.min

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
  
      if tiling.vertices.size != other.vertices.size ||
        tiling.innerFaces.size != other.innerFaces.size ||
        tiling.halfEdges.size != other.halfEdges.size then
        return false
  
      val thisFaceSignatures = tiling.innerFaces.map(_.halfEdgesSafe.size).sorted
      val otherFaceSignatures = other.innerFaces.map(_.halfEdgesSafe.size).sorted
  
      if thisFaceSignatures != otherFaceSignatures then
        return false
  
      val thisVertexSignatures = toMultiset(tiling.vertices.map(tiling.getVertexSignature))
      val otherVertexSignatures = toMultiset(other.vertices.map(other.getVertexSignature))
  
      thisVertexSignatures == otherVertexSignatures
  
    def isEquivalentTo(other: TilingDCEL): Boolean =
      given Ordering[AngleDegree] with
        def compare(x: AngleDegree, y: AngleDegree): Int =
          x.toRational.compare(y.toRational)
  
      def getCanonicalAngleSequence(angles: List[AngleDegree]): List[AngleDegree] =
        if angles.isEmpty then List.empty
        else angles.rotationsAndReflections.min
  
      def getFaceSignature(face: Face): List[AngleDegree] =
        getCanonicalAngleSequence(face.halfEdgesSafe.flatMap(_.angle))
  
      def getVertexSignature(vertex: Vertex): List[AngleDegree] =
        getCanonicalAngleSequence(vertex.incidentEdges.flatMap(_.angle))
  
      if tiling.vertices.size != other.vertices.size ||
        tiling.innerFaces.size != other.innerFaces.size ||
        tiling.halfEdges.size != other.halfEdges.size then
        return false
  
      val thisFaceSignatures = toMultiset(tiling.innerFaces.map(getFaceSignature))
      val otherFaceSignatures = toMultiset(other.innerFaces.map(getFaceSignature))
  
      if thisFaceSignatures != otherFaceSignatures then
        return false
  
      val thisVertexSignatures = toMultiset(tiling.vertices.map(getVertexSignature))
      val otherVertexSignatures = toMultiset(other.vertices.map(getVertexSignature))
  
      thisVertexSignatures == otherVertexSignatures
