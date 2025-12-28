package io.github.scala_tessella.dcel.structure

import io.github.scala_tessella.dcel.{TilingBuilder, TilingTestHelpers}
import io.github.scala_tessella.dcel.geometry.BigPoint
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VertexSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "Vertex construction"

  it should "create a vertex with ID and coordinates" in:
    val vertex = Vertex(V1, BigPoint(1.0, 2.0))
    allAssert(
      vertex.id shouldBe V1,
      vertex.coords shouldBe BigPoint(1.0, 2.0),
      vertex.leaving shouldBe None,
      vertex.toString shouldBe "Vertex V1 at coords (1, 2) [Missing leaving edge]"
    )

  it should "create a vertex with an optional leaving edge" in:
    val vertex         = Vertex(V1, BigPoint(1.0, 2.0))
    val edge           = HalfEdge(vertex)
    val vertexWithEdge = Vertex(V2, BigPoint(3.0, 4.0), Some(edge))
    allAssert(
      vertexWithEdge.id shouldBe V2,
      vertexWithEdge.coords shouldBe BigPoint(3.0, 4.0),
      vertexWithEdge.leaving shouldBe Some(edge),
      vertexWithEdge.toString shouldBe "Vertex V2 at coords (3, 4)"
    )

  behavior of "Vertex equality"

  it should "be equal to another vertex with the same ID" in:
    val v1 = Vertex(V1, BigPoint(0, 0))
    val v2 = Vertex(V1, BigPoint(100, 200)) // Different coordinates
    v1 shouldEqual v2

  it should "not be equal to another vertex with a different ID" in:
    val v1 = Vertex(V1, BigPoint(0, 0))
    val v2 = Vertex(V2, BigPoint(0, 0)) // Same coordinates, different ID
    v1 shouldNot equal(v2)

  it should "not be equal to objects of other types" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    allAssert(
      vertex shouldNot equal(V1),
      vertex shouldNot equal(BigPoint(0, 0)),
      vertex shouldNot equal(None)
    )

  it should "have the same hashCode as another vertex with the same ID" in:
    val v1 = Vertex(V1, BigPoint(0, 0))
    val v2 = Vertex(V1, BigPoint(100, 200))
    v1.hashCode() shouldEqual v2.hashCode()

  it should "have different hashCodes for vertices with different IDs" in:
    val v1 = Vertex(V1, BigPoint(0, 0))
    val v2 = Vertex(V2, BigPoint(0, 0))
    v1.hashCode() shouldNot equal(v2.hashCode())

  behavior of "Vertex.isComplete"

  it should "return false when leaving edge is None" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    vertex.isComplete shouldBe false

  it should "return true when leaving edge is defined" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)
    vertex.leaving = Some(edge)
    vertex.isComplete shouldBe true

  behavior of "Vertex.validate"

  it should "return Right(()) when vertex is complete" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)
    vertex.leaving = Some(edge)
    vertex.validate() shouldBe Right(())

  it should "return Left with error message when vertex is incomplete" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val result = vertex.validate()
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message shouldBe "Missing leaving edge"
    )

  behavior of "Vertex.incidentEdges"

  it should "return empty list when vertex has no leaving edge" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    vertex.incidentEdgesUnsafe shouldBe List.empty[HalfEdge]

  it should "return single edge when vertex has one self-loop edge" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)
    val twin   = HalfEdge(vertex)

    edge.twin = Some(twin)
    twin.twin = Some(edge)
    twin.next = Some(edge)

    vertex.leaving = Some(edge)

    vertex.incidentEdgesUnsafe shouldBe List(edge)

  it should "return all incident edges in a triangle configuration" in:
    // Create triangle vertices
    val v1 = Vertex(V1, BigPoint(0, 0))
    val v2 = Vertex(V2, BigPoint(1, 0))
    val v3 = Vertex(V3, BigPoint(0.5, 0.866))

    // Create half-edges for triangle
    val e12 = HalfEdge(v1); val e21 = HalfEdge(v2)
    val e23 = HalfEdge(v2); val e32 = HalfEdge(v3)
    val e31 = HalfEdge(v3); val e13 = HalfEdge(v1)

    // Set twins
    e12.twin = Some(e21); e21.twin = Some(e12)
    e23.twin = Some(e32); e32.twin = Some(e23)
    e31.twin = Some(e13); e13.twin = Some(e31)

    // Set next pointers for the cycle around v1
    e21.next = Some(e13)
    e13.next = Some(e12)

    // Set leaving edges
    v1.leaving = Some(e12)

    val incidentEdges = v1.incidentEdgesUnsafe
    incidentEdges should contain theSameElementsAs List(e12, e13)

  it should "handle complex vertex with multiple incident edges" in:
    // Create a vertex with 4 incident edges (like a cross)
    val center = Vertex(VertexId("Center"), BigPoint(0, 0))
    val v1     = Vertex(V1, BigPoint(1, 0))
    val v2     = Vertex(V2, BigPoint(0, 1))
    val v3     = Vertex(V3, BigPoint(-1, 0))
    val v4     = Vertex(V4, BigPoint(0, -1))

    // Create edges from center to each vertex
    val ec1 = HalfEdge(center); val e1c = HalfEdge(v1)
    val ec2 = HalfEdge(center); val e2c = HalfEdge(v2)
    val ec3 = HalfEdge(center); val e3c = HalfEdge(v3)
    val ec4 = HalfEdge(center); val e4c = HalfEdge(v4)

    // Set twins
    ec1.twin = Some(e1c); e1c.twin = Some(ec1)
    ec2.twin = Some(e2c); e2c.twin = Some(ec2)
    ec3.twin = Some(e3c); e3c.twin = Some(ec3)
    ec4.twin = Some(e4c); e4c.twin = Some(ec4)

    // Create cycle around center vertex
    e1c.next = Some(ec2)
    e2c.next = Some(ec3)
    e3c.next = Some(ec4)
    e4c.next = Some(ec1)

    center.leaving = Some(ec1)

    val incidentEdges = center.incidentEdgesUnsafe
    incidentEdges should contain theSameElementsAs List(ec1, ec2, ec3, ec4)

  it should "handle broken edge chain gracefully" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge1  = HalfEdge(vertex)
    val edge2  = HalfEdge(vertex)
    val twin1  = HalfEdge(vertex)

    edge1.twin = Some(twin1)
    twin1.twin = Some(edge1)
    // Deliberately don't set twin1.next, creating a broken chain

    vertex.leaving = Some(edge1)

    // Should return just the starting edge when chain is broken
    vertex.incidentEdgesUnsafe shouldBe List(edge1)

  behavior of "Vertex.degree"

  it should "return 0 for isolated vertex" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    vertex.degree shouldBe 0

  it should "return 1 for vertex with self-loop" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)
    val twin   = HalfEdge(vertex)

    edge.twin = Some(twin)
    twin.twin = Some(edge)
    twin.next = Some(edge)

    vertex.leaving = Some(edge)

    vertex.degree shouldBe 1

  it should "return correct degree for vertex in triangle" in:
    // Set up triangle as before
    val v1 = Vertex(V1, BigPoint(0, 0))
    val v2 = Vertex(V2, BigPoint(1, 0))
    val v3 = Vertex(V3, BigPoint(0.5, 0.866))

    val e12 = HalfEdge(v1); val e21 = HalfEdge(v2)
    val e31 = HalfEdge(v3); val e13 = HalfEdge(v1)

    e12.twin = Some(e21); e21.twin = Some(e12)
    e31.twin = Some(e13); e13.twin = Some(e31)

    e21.next = Some(e13)
    e13.next = Some(e12)

    v1.leaving = Some(e12)

    v1.degree shouldBe 2

  it should "return correct degree for vertex with multiple edges" in:
    // Use the 4-edge vertex setup from before
    val center = Vertex(VertexId("Center"), BigPoint(0, 0))
    val v1     = Vertex(V1, BigPoint(1, 0))
    val v2     = Vertex(V2, BigPoint(0, 1))
    val v3     = Vertex(V3, BigPoint(-1, 0))
    val v4     = Vertex(V4, BigPoint(0, -1))

    val ec1 = HalfEdge(center); val e1c = HalfEdge(v1)
    val ec2 = HalfEdge(center); val e2c = HalfEdge(v2)
    val ec3 = HalfEdge(center); val e3c = HalfEdge(v3)
    val ec4 = HalfEdge(center); val e4c = HalfEdge(v4)

    ec1.twin = Some(e1c); e1c.twin = Some(ec1)
    ec2.twin = Some(e2c); e2c.twin = Some(ec2)
    ec3.twin = Some(e3c); e3c.twin = Some(ec3)
    ec4.twin = Some(e4c); e4c.twin = Some(ec4)

    e1c.next = Some(ec2)
    e2c.next = Some(ec3)
    e3c.next = Some(ec4)
    e4c.next = Some(ec1)

    center.leaving = Some(ec1)

    center.degree shouldBe 4

  behavior of "Vertex.adjacentVertices"

  it should "return empty list for isolated vertex" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    vertex.adjacentVerticesUnsafe shouldBe List.empty[Vertex]

  it should "return adjacent vertices in triangle" in:
    val v1 = Vertex(V1, BigPoint(0, 0))
    val v2 = Vertex(V2, BigPoint(1, 0))
    val v3 = Vertex(V3, BigPoint(0.5, 0.866))

    val e12 = HalfEdge(v1); val e21 = HalfEdge(v2)
    val e31 = HalfEdge(v3); val e13 = HalfEdge(v1)

    e12.twin = Some(e21); e21.twin = Some(e12)
    e31.twin = Some(e13); e13.twin = Some(e31)

    e21.next = Some(e13)
    e13.next = Some(e12)

    v1.leaving = Some(e12)

    val adjacent = v1.adjacentVerticesUnsafe
    adjacent should contain theSameElementsAs List(v2, v3)

  it should "handle self-loop correctly" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)
    val twin   = HalfEdge(vertex)

    edge.twin = Some(twin)
    twin.twin = Some(edge)
    twin.next = Some(edge)

    vertex.leaving = Some(edge)

    // Self-loop should list the vertex as adjacent to itself
    vertex.adjacentVerticesUnsafe shouldBe List(vertex)

  it should "return all adjacent vertices for complex vertex" in:
    val center = Vertex(VertexId("Center"), BigPoint(0, 0))
    val v1     = Vertex(V1, BigPoint(1, 0))
    val v2     = Vertex(V2, BigPoint(0, 1))
    val v3     = Vertex(V3, BigPoint(-1, 0))
    val v4     = Vertex(V4, BigPoint(0, -1))

    val ec1 = HalfEdge(center); val e1c = HalfEdge(v1)
    val ec2 = HalfEdge(center); val e2c = HalfEdge(v2)
    val ec3 = HalfEdge(center); val e3c = HalfEdge(v3)
    val ec4 = HalfEdge(center); val e4c = HalfEdge(v4)

    ec1.twin = Some(e1c); e1c.twin = Some(ec1)
    ec2.twin = Some(e2c); e2c.twin = Some(ec2)
    ec3.twin = Some(e3c); e3c.twin = Some(ec3)
    ec4.twin = Some(e4c); e4c.twin = Some(ec4)

    e1c.next = Some(ec2)
    e2c.next = Some(ec3)
    e3c.next = Some(ec4)
    e4c.next = Some(ec1)

    center.leaving = Some(ec1)

    val adjacent = center.adjacentVerticesUnsafe
    adjacent should contain theSameElementsAs List(v1, v2, v3, v4)

  it should "compute all adjacent vertices" in:
    val tiling = TilingBuilder.createRhombusNet(2, 2)
    tiling.vertices
      .map: vertex =>
        vertex.id -> vertex.adjacentVerticesUnsafe.map: adjacentVertex =>
          adjacentVertex.id
      .toMap shouldEqual
      Map(
        V1   -> List(V2, V4),
        V2   -> List(V3, V1, V5),
        V3   -> List(V6, V2),
        V4   -> List(V5, V1, "V7"),
        V5   -> List(V6, V2, V4, "V8"),
        V6   -> List("V9", V3, V5),
        "V7" -> List("V8", V4),
        "V8" -> List("V9", V5, "V7"),
        "V9" -> List("V8", V6)
      )

  behavior of "Vertex.incidentFaces"

  it should "return empty list for isolated vertex" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    vertex.incidentFacesUnsafe shouldBe List.empty[Face]

  it should "return incident faces for vertex in triangle" in:
    val v1 = Vertex(V1, BigPoint(0, 0))
    val v2 = Vertex(V2, BigPoint(1, 0))
    val v3 = Vertex(V3, BigPoint(0.5, 0.866))

    val innerFace = Face(FaceId("Inner"))
    val outerFace = Face(FaceId("Outer"))

    val e12 = HalfEdge(v1, incidentFace = Some(innerFace))
    val e21 = HalfEdge(v2, incidentFace = Some(outerFace))
    val e31 = HalfEdge(v3, incidentFace = Some(outerFace))
    val e13 = HalfEdge(v1, incidentFace = Some(innerFace))

    e12.twin = Some(e21); e21.twin = Some(e12)
    e31.twin = Some(e13); e13.twin = Some(e31)

    e21.next = Some(e13)
    e13.next = Some(e12)

    v1.leaving = Some(e12)

    val faces = v1.incidentFacesUnsafe
    faces should contain theSameElementsAs List(innerFace, innerFace)

  it should "handle edges without incident faces" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex) // No incident face set
    val twin   = HalfEdge(vertex)

    edge.twin = Some(twin)
    twin.twin = Some(edge)
    twin.next = Some(edge)

    vertex.leaving = Some(edge)

    vertex.incidentFacesUnsafe shouldBe List.empty[Face]

  behavior of "Vertex companion object methods"

  behavior of "Vertex.buildBoundaryVertexAdjacency"

  it should "build adjacency map for simple boundary" in:
    val v1 = Vertex(V1, BigPoint(0, 0))
    val v2 = Vertex(V2, BigPoint(1, 0))
    val v3 = Vertex(V3, BigPoint(1, 1))

    val e12 = HalfEdge(v1); val e21 = HalfEdge(v2)
    val e23 = HalfEdge(v2); val e32 = HalfEdge(v3)

    e12.twin = Some(e21); e21.twin = Some(e12)
    e23.twin = Some(e32); e32.twin = Some(e23)

    val boundaryEdges  = List(e12, e23)
    val sharedVertices = Set(v1, v2, v3)

    val adjacency = Vertex.buildBoundaryVertexAdjacency(boundaryEdges, sharedVertices)

    allAssert(
      adjacency should contain key v1,
      adjacency should contain key v2,
      adjacency(v1) should contain(v2),
      adjacency(v2) should contain(v3)
    )

  it should "filter out vertices not in shared set" in:
    val v1 = Vertex(V1, BigPoint(0, 0))
    val v2 = Vertex(V2, BigPoint(1, 0))
    val v3 = Vertex(V3, BigPoint(1, 1))

    val e12 = HalfEdge(v1); val e21 = HalfEdge(v2)

    e12.twin = Some(e21); e21.twin = Some(e12)

    val boundaryEdges  = List(e12)
    val sharedVertices = Set(v1) // v2 not in shared set

    val adjacency = Vertex.buildBoundaryVertexAdjacency(boundaryEdges, sharedVertices)

    allAssert(
      adjacency should contain key v1,
      adjacency(v1) shouldBe List.empty[Vertex] // v2 filtered out
    )

  it should "return empty map for empty boundary edges" in:
    val adjacency = Vertex.buildBoundaryVertexAdjacency(List.empty, Set.empty)
    adjacency shouldBe Map.empty[Vertex, List[Vertex]]

  behavior of "Vertex mutable state"

  it should "allow modification of leaving edge" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge1  = HalfEdge(vertex)
    val edge2  = HalfEdge(vertex)

    allAssert(
      vertex.leaving shouldBe None, {
        vertex.leaving = Some(edge1)
        vertex.leaving shouldBe Some(edge1)
      }, {
        vertex.leaving = Some(edge2)
        vertex.leaving shouldBe Some(edge2)
      }, {
        vertex.leaving = None
        vertex.leaving shouldBe None
      }
    )

  it should "maintain consistency after leaving edge changes" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)

    allAssert(
      vertex.isComplete shouldBe false,
      vertex.validate().isLeft shouldBe true, {
        vertex.leaving = Some(edge)
        vertex.isComplete shouldBe true
      },
      vertex.validate().isRight shouldBe true
    )

  behavior of "Vertex edge cases"

  it should "handle vertices with same coordinates but different IDs" in:
    val coords = BigPoint(1.5, 2.5)
    val v1     = Vertex(V1, coords)
    val v2     = Vertex(V2, coords)

    allAssert(
      v1 shouldNot equal(v2),
      v1.coords shouldEqual v2.coords,
      v1.id shouldNot equal(v2.id)
    )

  it should "handle empty string ID" in:
    val vertex = Vertex(VertexId(""), BigPoint(0, 0))
    allAssert(
      vertex.id.value shouldBe "",
      vertex.hashCode() shouldBe "".hashCode()
    )

  it should "handle very large coordinates" in:
    val largeCoords = BigPoint(
      BigDecimal("999999999999999999.999999999999999999"),
      BigDecimal("-999999999999999999.999999999999999999")
    )
    val vertex      = Vertex(VertexId("VLarge"), largeCoords)
    allAssert(
      vertex.coords shouldBe largeCoords,
      vertex.id.value shouldBe "VLarge"
    )

  it should "handle special characters in ID" in:
    val specialId = "V_1-2.3@test#"
    val vertex    = Vertex(VertexId(specialId), BigPoint(0, 0))
    allAssert(
      vertex.id.value shouldBe specialId,
      vertex.hashCode() shouldBe specialId.hashCode()
    )

  behavior of "TilingDCEL.findEdgeBetween"

  it should "find an edge between two connected vertices" in:
    val v0 = triangle.findVertexUnsafe(V1).get
    val v1 = triangle.findVertexUnsafe(V2).get
    val v2 = triangle.findVertexUnsafe(V3).get

    allAssert(
      v0.findEdgeBetweenUnsafe(v1) shouldBe defined,
      v1.findEdgeBetweenUnsafe(v2) shouldBe defined,
      v2.findEdgeBetweenUnsafe(v0) shouldBe defined
    )

  it should "return None for vertices that are not connected" in:
    val v0 = square.findVertexUnsafe(V1).get
    val v2 = square.findVertexUnsafe(V3).get

    // V0 and V2 are diagonal vertices in a square, not directly connected
    v0.findEdgeBetweenUnsafe(v2) shouldBe None

  it should "return None when either vertex has no incident edges" in:
    val isolatedVertex = Vertex(VertexId("Isolated"), BigPoint(10, 10))
    val v0             = triangle.findVertexUnsafe(V1).get

    isolatedVertex.findEdgeBetweenUnsafe(v0) shouldBe None
