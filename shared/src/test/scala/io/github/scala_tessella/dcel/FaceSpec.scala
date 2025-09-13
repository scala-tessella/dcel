package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.BigDecimalGeometry.{AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.Topology.breadthFirstSearch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FaceSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  // Helper method to create a simple vertex
  private def createVertex(id: VertexId, x: Double, y: Double): Vertex =
    Vertex(id, BigPoint(x, y))

  // Helper method to create a simple triangular face with proper edge linking
  private def createTriangleFace(faceId: FaceId): (Face, List[HalfEdge], List[Vertex]) =
    val v1 = createVertex(V1, 0, 0)
    val v2 = createVertex(VertexId("V2"), 1, 0)
    val v3 = createVertex(VertexId("V3"), 0.5, 0.866)

    val face = Face(faceId)
    val he1  = HalfEdge(v1, incidentFace = Some(face))
    val he2  = HalfEdge(v2, incidentFace = Some(face))
    val he3  = HalfEdge(v3, incidentFace = Some(face))

    // Link edges in a triangle: he1 -> he2 -> he3 -> he1
    he1.next = Some(he2)
    he2.next = Some(he3)
    he3.next = Some(he1)

    he1.prev = Some(he3)
    he2.prev = Some(he1)
    he3.prev = Some(he2)

    face.outerComponent = Some(he1)

    (face, List(he1, he2, he3), List(v1, v2, v3))

  // Helper method to create a square face
  private def createSquareFace(faceId: FaceId): (Face, List[HalfEdge], List[Vertex]) =
    val v1 = createVertex(V1, 0, 0)
    val v2 = createVertex(VertexId("V2"), 1, 0)
    val v3 = createVertex(VertexId("V3"), 1, 1)
    val v4 = createVertex(V4, 0, 1)

    val face = Face(faceId)
    val he1  = HalfEdge(v1, incidentFace = Some(face))
    val he2  = HalfEdge(v2, incidentFace = Some(face))
    val he3  = HalfEdge(v3, incidentFace = Some(face))
    val he4  = HalfEdge(v4, incidentFace = Some(face))

    // Link edges in a square: he1 -> he2 -> he3 -> he4 -> he1
    he1.next = Some(he2)
    he2.next = Some(he3)
    he3.next = Some(he4)
    he4.next = Some(he1)

    he1.prev = Some(he4)
    he2.prev = Some(he1)
    he3.prev = Some(he2)
    he4.prev = Some(he3)

    face.outerComponent = Some(he1)

    (face, List(he1, he2, he3, he4), List(v1, v2, v3, v4))

  behavior of "Face construction"

  it should "create a face with just an ID" in {
    val face = Face(F1)
    allAssert(
      face.id.value shouldBe "F1",
      face.outerComponent shouldBe None,
      face.innerComponents shouldBe Nil
    )
  }

  it should "create a face with outer component" in {
    val vertex = createVertex(V1, 0, 0)
    val edge   = HalfEdge(vertex)
    val face   = Face(F1, Some(edge))
    allAssert(
      face.id.value shouldBe "F1",
      face.outerComponent shouldBe Some(edge),
      face.innerComponents shouldBe Nil
    )
  }

  it should "create a face with inner components" in {
    val vertex          = createVertex(V1, 0, 0)
    val edge1           = HalfEdge(vertex)
    val edge2           = HalfEdge(vertex)
    val innerComponents = List(Some(edge1), Some(edge2))
    val face            = Face(F1, None, innerComponents)
    allAssert(
      face.id.value shouldBe "F1",
      face.outerComponent shouldBe None,
      face.innerComponents shouldBe innerComponents
    )
  }

  behavior of "Face equality"

  it should "be equal to another face with the same ID" in {
    val vertex = createVertex(V1, 0, 0)
    val edge   = HalfEdge(vertex)
    val face1  = Face(F1)
    val face2  = Face(F1, Some(edge)) // Different components

    face1 shouldEqual face2
  }

  it should "not be equal to another face with a different ID" in {
    val face1 = Face(F1)
    val face2 = Face(F2)

    face1 shouldNot equal(face2)
  }

  it should "not be equal to objects of other types" in {
    val face = Face(F1)
    allAssert(
      face shouldNot equal("F1"),
      face shouldNot equal(1),
      face shouldNot equal(None)
    )
  }

  it should "have the same hashCode as another face with the same ID" in {
    val face1 = Face(F1)
    val face2 = Face(F1)

    face1.hashCode() shouldEqual face2.hashCode()
  }

  it should "have different hashCodes for faces with different IDs" in {
    val face1 = Face(F1)
    val face2 = Face(F2)

    face1.hashCode() shouldNot equal(face2.hashCode())
  }

  behavior of "Face.validate"

  it should "return Right(()) when face is complete" in {
    val vertex = createVertex(V1, 0, 0)
    val edge1  = HalfEdge(vertex)
    val edge2  = HalfEdge(vertex)
    val face   = Face(F1, Some(edge1), List(Some(edge2)))

    face.validate() shouldBe Right(())
  }

  it should "return Left with missing outer component error" in {
    val vertex = createVertex(V1, 0, 0)
    val edge   = HalfEdge(vertex)
    val face   = Face(F1, None, List(Some(edge)))

    val result = face.validate()
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Missing outer component edge")
    )
  }

  it should "return Right(()) with missing inner components error" in {
    val vertex = createVertex(V1, 0, 0)
    val edge   = HalfEdge(vertex)
    val face   = Face(F1, Some(edge))

    val result = face.validate()
    result.isRight shouldBe true
  }

  it should "return Left with both errors when both are missing" in {
    val face         = Face(F1)
    val result       = face.validate()
    val errorMessage = result.left.value.message
    allAssert(
      result.isLeft shouldBe true,
      errorMessage should include("Missing outer component edge")
    )
  }

  behavior of "Face.getVertices"

  it should "return empty list when outer component is None" in {
    val face = Face(F1)
    face.getVertices.toOption.get shouldBe List.empty[Vertex]
  }

  it should "return single vertex for self-loop edge" in {
    val vertex = createVertex(V1, 0, 0)
    val face   = Face(F1)
    val edge   = HalfEdge(vertex, incidentFace = Some(face))
    edge.next = Some(edge) // Self-loop
    face.outerComponent = Some(edge)

    face.getVertices.getOrElse(List.empty) shouldBe List(vertex)
  }

  it should "return vertices in order for triangle face" in {
    val (face, _, vertices) = createTriangleFace(FaceId("F_triangle"))

    val result = face.getVertices.getOrElse(List.empty)
    allAssert(
      result should have length 3,
      result shouldBe vertices
    )
  }

  it should "return vertices in order for square face" in {
    val (face, _, vertices) = createSquareFace(FaceId("F_square"))

    val result = face.getVertices.getOrElse(List.empty)
    allAssert(
      result should have length 4,
      result shouldBe vertices
    )
  }

  it should "handle broken edge chain NOT gracefully" in {
    val v1   = createVertex(V1, 0, 0)
    val v2   = createVertex(VertexId("V2"), 1, 0)
    val face = Face(F1)
    val he1  = HalfEdge(v1, incidentFace = Some(face))
    val he2  = HalfEdge(v2, incidentFace = Some(face))

    he1.next = Some(he2)
    // he2.next is None - broken chain
    face.outerComponent = Some(he1)

    val result = face.getVertices
    result.isLeft shouldBe true
  }

  behavior of "Face.halfEdges"

  it should "return Right(List.empty) when outer component is None" in {
    val face = Face(F1)
    face.halfEdges shouldBe Right(List.empty)
  }

  it should "return Right with single edge for self-loop" in {
    val vertex = createVertex(V1, 0, 0)
    val face   = Face(F1)
    val edge   = HalfEdge(vertex, incidentFace = Some(face))
    edge.next = Some(edge)
    face.outerComponent = Some(edge)

    face.halfEdges shouldBe Right(List(edge))
  }

  it should "return Right with edges in order for triangle face" in {
    val (face, edges, _) = createTriangleFace(FaceId("F_triangle"))

    val result = face.halfEdges
    allAssert(
      result.isRight shouldBe true,
      result.value should contain theSameElementsInOrderAs edges
    )
  }

  it should "return Right with edges in order for square face" in {
    val (face, edges, _) = createSquareFace(FaceId("F_square"))

    val result = face.halfEdges
    allAssert(
      result.isRight shouldBe true,
      result.value should contain theSameElementsInOrderAs edges
    )
  }

  it should "return Left when edge chain is broken" in {
    val v1   = createVertex(V1, 0, 0)
    val v2   = createVertex(VertexId("V2"), 1, 0)
    val face = Face(FaceId("F_broken"))
    val he1  = HalfEdge(v1, incidentFace = Some(face))
    val he2  = HalfEdge(v2, incidentFace = Some(face))

    he1.next = Some(he2)
    // he2.next is None - broken chain
    face.outerComponent = Some(he1)

    val result = face.halfEdges
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should startWith("Broken edge chain")
    )
  }

  it should "return Left when cycle is detected" in {
    val v1   = createVertex(V1, 0, 0)
    val v2   = createVertex(VertexId("V2"), 1, 0)
    val v3   = createVertex(VertexId("V3"), 0.5, 0.866)
    val face = Face(FaceId("F_cycle"))
    val he1  = HalfEdge(v1, incidentFace = Some(face))
    val he2  = HalfEdge(v2, incidentFace = Some(face))
    val he3  = HalfEdge(v3, incidentFace = Some(face))

    // Create a malformed cycle: he1 -> he2 -> he2 (he2 points to itself)
    he1.next = Some(he2)
    he2.next = Some(he2)
    face.outerComponent = Some(he1)

    val result = face.halfEdges
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should startWith("Cycle detected")
    )
  }

  behavior of "Face.halfEdgesSafe"

  it should "return empty list when halfEdges returns error" in {
    val v1   = createVertex(V1, 0, 0)
    val face = Face(FaceId("F_broken"))
    val he1  = HalfEdge(v1, incidentFace = Some(face))
    // he1.next is None - broken chain
    face.outerComponent = Some(he1)

    face.halfEdges.isLeft shouldBe true
  }

  it should "return edges when halfEdges succeeds" in {
    val (face, edges, _) = createTriangleFace(FaceId("F_triangle"))

    face.halfEdgesUnsafe should contain theSameElementsInOrderAs edges
  }

  behavior of "Face.area"

  it should "return 0 for face with no vertices" in {
    val face = Face(FaceId("F_empty"))
    face.area shouldBe BigDecimal(0)
  }

  it should "return 0 for face with less than 3 vertices" in {
    val v1   = createVertex(V1, 0, 0)
    val v2   = createVertex(VertexId("V2"), 1, 0)
    val face = Face(FaceId("F_line"))
    val he1  = HalfEdge(v1, incidentFace = Some(face))
    val he2  = HalfEdge(v2, incidentFace = Some(face))

    he1.next = Some(he2)
    he2.next = Some(he1)
    face.outerComponent = Some(he1)

    face.area shouldBe BigDecimal(0)
  }

  it should "calculate correct area for triangle" in {
    val (face, _, _) = createTriangleFace(FaceId("F_triangle"))

    // Triangle with vertices at (0,0), (1,0), (0.5, 0.866) should have area ≈ 0.433
    val area = face.area
    allAssert(
      area should be > BigDecimal(0.4),
      area should be < BigDecimal(0.5)
    )
  }

  it should "calculate correct area for unit square" in {
    val (face, _, _) = createSquareFace(FaceId("F_square"))

    // Unit square should have area = 1
    face.area shouldBe BigDecimal(1)
  }

  it should "calculate area using shoelace formula correctly" in {
    // Create a rectangular face with known area
    val v1 = createVertex(V1, 0, 0)
    val v2 = createVertex(VertexId("V2"), 2, 0)
    val v3 = createVertex(VertexId("V3"), 2, 3)
    val v4 = createVertex(V4, 0, 3)

    val face = Face(FaceId("F_rectangle"))
    val he1  = HalfEdge(v1, incidentFace = Some(face))
    val he2  = HalfEdge(v2, incidentFace = Some(face))
    val he3  = HalfEdge(v3, incidentFace = Some(face))
    val he4  = HalfEdge(v4, incidentFace = Some(face))

    he1.next = Some(he2)
    he2.next = Some(he3)
    he3.next = Some(he4)
    he4.next = Some(he1)

    face.outerComponent = Some(he1)

    // Rectangle with width=2, height=3 should have area=6
    face.area shouldBe BigDecimal(6)
  }

  behavior of "Face.hasEqualAngles"

  it should "return true for a face with all equal angles" in {
    val (face, edges, _) = createSquareFace(F1)
    edges.foreach(_.angle = Some(AngleDegree(90)))
    face.hasEqualAngles shouldBe true
  }

  it should "return false for a face with unequal angles" in {
    val (face, edges, _) = createSquareFace(F1)
    edges.head.angle = Some(AngleDegree(90))
    edges(1).angle = Some(AngleDegree(90))
    edges(2).angle = Some(AngleDegree(80))
    edges(3).angle = Some(AngleDegree(100))
    face.hasEqualAngles shouldBe false
  }

  it should "return false if any edge angle is missing" in {
    val (face, edges, _) = createSquareFace(F1)
    edges.head.angle = Some(AngleDegree(90))
    edges(1).angle = Some(AngleDegree(90))
    edges(2).angle = Some(AngleDegree(90))
    // edges(3).angle is None
    face.hasEqualAngles shouldBe false
  }

  it should "return false for a face with fewer than 3 edges" in {
    val v1 = createVertex(V1, 0, 0)
    val v2 = createVertex(V2, 1, 0)
    val face = Face(F1)
    val he1 = HalfEdge(v1, incidentFace = Some(face), angle = Some(AngleDegree(180)))
    val he2 = HalfEdge(v2, incidentFace = Some(face), angle = Some(AngleDegree(180)))
    he1.next = Some(he2)
    he2.next = Some(he1)
    face.outerComponent = Some(he1)
    face.hasEqualAngles shouldBe false
  }

  it should "return false when halfEdges traversal fails" in {
    val v1 = createVertex(V1, 0, 0)
    val v2 = createVertex(V2, 1, 0)
    val face = Face(F1)
    val he1 = HalfEdge(v1, incidentFace = Some(face), angle = Some(AngleDegree(90)))
    val he2 = HalfEdge(v2, incidentFace = Some(face), angle = Some(AngleDegree(90)))
    he1.next = Some(he2)
    // he2.next is None, so halfEdges will fail
    face.outerComponent = Some(he1)
    face.hasEqualAngles shouldBe false
  }

  behavior of "Face mutable state"

  it should "allow modification of outer component" in {
    val face   = Face(F1)
    val vertex = createVertex(V1, 0, 0)
    val edge1  = HalfEdge(vertex)
    val edge2  = HalfEdge(vertex)

    allAssert(
      face.outerComponent shouldBe None, {
        face.outerComponent = Some(edge1)
        face.outerComponent shouldBe Some(edge1)
      }, {
        face.outerComponent = Some(edge2)
        face.outerComponent shouldBe Some(edge2)
      }, {
        face.outerComponent = None
        face.outerComponent shouldBe None
      }
    )
  }

  it should "allow modification of inner components" in {
    val face   = Face(F1)
    val vertex = createVertex(V1, 0, 0)
    val edge1  = HalfEdge(vertex)
    val edge2  = HalfEdge(vertex)

    allAssert(
      face.innerComponents shouldBe Nil, {
        face.innerComponents = List(Some(edge1))
        face.innerComponents shouldBe List(Some(edge1))
      }, {
        face.innerComponents = List(Some(edge1), Some(edge2))
        face.innerComponents shouldBe List(Some(edge1), Some(edge2))
      }, {
        face.innerComponents = List(None, Some(edge1))
        face.innerComponents shouldBe List(None, Some(edge1))
      }, {
        face.innerComponents = Nil
        face.innerComponents shouldBe Nil
      }
    )
  }

  behavior of "Face companion object - adjacencyMap"

  it should "return empty adjacencies for faces with no edges" in {
    val face1 = Face(F1)
    val face2 = Face(F2)
    val faces = List(face1, face2)

    val adjacencyMap = Face.adjacencyMap(faces)
    allAssert(
      adjacencyMap(face1) shouldBe List.empty[Face],
      adjacencyMap(face2) shouldBe List.empty[Face]
    )
  }

  it should "return correct adjacency for connected faces" in {
    // Create two adjacent triangular faces
    val v1 = createVertex(V1, 0, 0)
    val v2 = createVertex(V2, 1, 0)
    val v3 = createVertex(V3, 0.5, 0.866)
    val v4 = createVertex(V4, 0.5, -0.866)

    val face1 = Face(F1)
    val face2 = Face(F2)

    // Face 1: triangle v1-v2-v3
    val he1_12 = HalfEdge(v1, incidentFace = Some(face1))
    val he1_23 = HalfEdge(v2, incidentFace = Some(face1))
    val he1_31 = HalfEdge(v3, incidentFace = Some(face1))

    // Face 2: triangle v1-v4-v2 (shares edge v1-v2 with face1)
    val he2_14 = HalfEdge(v1, incidentFace = Some(face2))
    val he2_42 = HalfEdge(v4, incidentFace = Some(face2))
    val he2_21 = HalfEdge(v2, incidentFace = Some(face2))

    // Link face 1 edges
    he1_12.next = Some(he1_23)
    he1_23.next = Some(he1_31)
    he1_31.next = Some(he1_12)

    // Link face 2 edges
    he2_14.next = Some(he2_42)
    he2_42.next = Some(he2_21)
    he2_21.next = Some(he2_14)

    // Make he1_12 and he2_21 twins (they share the edge v1-v2)
    he1_12.twin = Some(he2_21)
    he2_21.twin = Some(he1_12)

    // Set twins for all other edges to dummy edges to avoid None.get errors
    // In a real DCEL, all edges would have twins
    val dummyTwin1_23 = HalfEdge(v3, incidentFace = Some(Face(FaceId("dummy1"))))
    val dummyTwin1_31 = HalfEdge(v1, incidentFace = Some(Face(FaceId("dummy2"))))
    val dummyTwin2_14 = HalfEdge(v4, incidentFace = Some(Face(FaceId("dummy3"))))
    val dummyTwin2_42 = HalfEdge(v2, incidentFace = Some(Face(FaceId("dummy4"))))

    he1_23.twin = Some(dummyTwin1_23)
    dummyTwin1_23.twin = Some(he1_23)
    he1_31.twin = Some(dummyTwin1_31)
    dummyTwin1_31.twin = Some(he1_31)
    he2_14.twin = Some(dummyTwin2_14)
    dummyTwin2_14.twin = Some(he2_14)
    he2_42.twin = Some(dummyTwin2_42)
    dummyTwin2_42.twin = Some(he2_42)

    face1.outerComponent = Some(he1_12)
    face2.outerComponent = Some(he2_14)

    val faces        = List(face1, face2)
    val adjacencyMap = Face.adjacencyMap(faces)

    allAssert(
      adjacencyMap(face1) should contain(face2),
      adjacencyMap(face2) should contain(face1)
    )
  }

  it should "handle faces with missing twin information gracefully" in {
    val (face1, _, _) = createTriangleFace(F1)
    val faces         = List(face1)

    // This should not crash even though edges don't have twins properly set
    val adjacencyMap = Face.adjacencyMap(faces)
    adjacencyMap shouldBe a[Map[Face, List[Face]]]
  }

  behavior of "Face companion object - breadthFirstSearch"

  it should "return single face when no adjacencies exist" in {
    val face1     = Face(F1)
    val adjacency = Map(face1 -> List.empty[Face])

    val result = breadthFirstSearch(face1, adjacency)
    result shouldBe Set(face1)
  }

  it should "return all connected faces" in {
    val face1 = Face(F1)
    val face2 = Face(F2)
    val face3 = Face(F3)

    val adjacency = Map(
      face1 -> List(face2),
      face2 -> List(face1, face3),
      face3 -> List(face2)
    )

    val result = breadthFirstSearch(face1, adjacency)
    result shouldBe Set(face1, face2, face3)
  }

  it should "return only reachable faces in disconnected graph" in {
    val face1 = Face(F1)
    val face2 = Face(F2)
    val face3 = Face(F3)
    val face4 = Face(FaceId("F4"))

    val adjacency = Map(
      face1 -> List(face2),
      face2 -> List(face1),
      face3 -> List(face4),
      face4 -> List(face3)
    )

    val result = breadthFirstSearch(face1, adjacency)
    result shouldBe Set(face1, face2)
  }

  it should "handle cycles in the adjacency graph" in {
    val face1 = Face(F1)
    val face2 = Face(F2)

    val adjacency = Map(
      face1 -> List(face2, face1), // Self-loop and connection to face2
      face2 -> List(face1)
    )

    val result = breadthFirstSearch(face1, adjacency)
    result shouldBe Set(face1, face2)
  }

  it should "handle missing entries in adjacency map" in {
    val face1 = Face(F1)
    val face2 = Face(F2)

    val adjacency = Map(
      face1 -> List(face2)
      // face2 is not in the map
    )

    val result = breadthFirstSearch(face1, adjacency)
    result shouldBe Set(face1, face2)
  }
