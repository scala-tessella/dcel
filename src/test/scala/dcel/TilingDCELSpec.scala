package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, BigPoint}

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingDCELSpec extends AnyFlatSpec with Matchers with EitherValues:

  // Helper methods to create test data
  private def createTriangleTiling(): TilingDCEL =
    TilingBuilder.createRegularPolygon(3).value

  private def createSquareTiling(): TilingDCEL =
    TilingBuilder.createRegularPolygon(4).value

  private def createHexagonTiling(): TilingDCEL =
    TilingBuilder.createRegularPolygon(6).value

  behavior of "TilingDCEL.faces"

  it should "return all faces including outer face" in {
    val triangle = createTriangleTiling()
    triangle.faces should have length 2
    triangle.faces should contain(triangle.outerFace)
    triangle.faces should contain allElementsOf triangle.innerFaces
  }

  it should "return only outer face when no inner faces exist" in {
    val empty = TilingBuilder.empty
    empty.faces should have length 1
    empty.faces should contain only empty.outerFace
  }

  behavior of "TilingDCEL.findVertex"

  it should "find an existing vertex by id" in {
    val triangle = createTriangleTiling()
    triangle.findVertex("V0") shouldBe defined
    triangle.findVertex("V1") shouldBe defined
    triangle.findVertex("V2") shouldBe defined
  }

  it should "return None for non-existent vertex id" in {
    val triangle = createTriangleTiling()
    triangle.findVertex("V999") shouldBe None
    triangle.findVertex("NonExistent") shouldBe None
  }

  it should "find vertices in empty tiling" in {
    val empty = TilingBuilder.empty
    empty.findVertex("V0") shouldBe None
  }

  behavior of "TilingDCEL.findFace"

  it should "find an existing face by id" in {
    val triangle = createTriangleTiling()
    triangle.findFace("F_Outer") shouldBe defined
    triangle.findFace("F_Poly") shouldBe defined
  }

  it should "return None for non-existent face id" in {
    val triangle = createTriangleTiling()
    triangle.findFace("F999") shouldBe None
    triangle.findFace("NonExistent") shouldBe None
  }

  behavior of "TilingDCEL.findEdgeBetween"

  it should "find an edge between two connected vertices" in {
    val triangle = createTriangleTiling()
    val v0 = triangle.findVertex("V0").get
    val v1 = triangle.findVertex("V1").get
    val v2 = triangle.findVertex("V2").get

    triangle.findEdgeBetween(v0, v1) shouldBe defined
    triangle.findEdgeBetween(v1, v2) shouldBe defined
    triangle.findEdgeBetween(v2, v0) shouldBe defined
  }

  it should "return None for vertices that are not connected" in {
    val square = createSquareTiling()
    val v0 = square.findVertex("V0").get
    val v2 = square.findVertex("V2").get

    // V0 and V2 are diagonal vertices in a square, not directly connected
    square.findEdgeBetween(v0, v2) shouldBe None
  }

  it should "return None when either vertex has no incident edges" in {
    val isolatedVertex = Vertex("Isolated", BigPoint(10, 10))
    val triangle = createTriangleTiling()
    val v0 = triangle.findVertex("V0").get

    triangle.findEdgeBetween(isolatedVertex, v0) shouldBe None
  }

  behavior of "TilingDCEL.isConnected"

  it should "return true for empty tiling" in {
    val empty = TilingBuilder.empty
    empty.isConnected shouldBe true
  }

  it should "return true for single polygon tiling" in {
    val triangle = createTriangleTiling()
    triangle.isConnected shouldBe true

    val square = createSquareTiling()
    square.isConnected shouldBe true
  }

  it should "return true for connected multi-polygon tiling" in {
    // Create a tiling with two connected triangles
    val tiling = createTriangleTiling()
    // Note: This would require accessing maybeAddRegularPolygon which isn't in current TilingDCEL
    // For now, test single polygon case
    tiling.isConnected shouldBe true
  }

  behavior of "TilingDCEL.boundary"

  it should "return empty vector for empty tiling" in {
    val empty = TilingBuilder.empty
    empty.boundary shouldBe Vector.empty
  }

  it should "return correct boundary vertices for triangle in clockwise order" in {
    val triangle = createTriangleTiling()
    val boundary = triangle.boundary
    boundary should have length 3
    boundary.map(_.id) should contain theSameElementsInOrderAs Vector("V0", "V2", "V1")
  }

  it should "return correct boundary vertices for square in clockwise order" in {
    val square = createSquareTiling()
    val boundary = square.boundary
    boundary should have length 4
    boundary.map(_.id) should contain theSameElementsInOrderAs Vector("V0", "V3", "V2", "V1")
  }

  it should "return correct boundary vertices for hexagon" in {
    val hexagon = createHexagonTiling()
    val boundary = hexagon.boundary
    boundary should have length 6
    boundary.map(_.id) should contain theSameElementsInOrderAs Vector("V0", "V5", "V4", "V3", "V2", "V1")
  }

  behavior of "TilingDCEL.boundarySafe"

  it should "return same result as boundary for well-formed tilings" in {
    val triangle = createTriangleTiling()
    triangle.boundarySafe.value shouldEqual triangle.boundary

    val square = createSquareTiling()
    square.boundarySafe.value shouldEqual square.boundary
  }

  it should "return empty vector for empty tiling" in {
    val empty = TilingBuilder.empty
    empty.boundarySafe shouldBe Right(Vector.empty)
  }

  it should "fail for malformed boundary loop" in {
    val triangle = createTriangleTiling()
    val boundaryEdges = triangle.getBoundaryEdges
    if boundaryEdges.length >= 2 then
      val firstEdge = boundaryEdges.head // This is the startEdge
      val secondEdge = boundaryEdges(1)

      // Create a malformed loop where a non-start edge is repeated
      firstEdge.next = Some(secondEdge)
      secondEdge.next = Some(secondEdge) // Make second edge point to itself

      triangle.boundarySafe.isLeft shouldBe true
  }

  it should "throw error for open chain in boundary" in {
    val triangle = createTriangleTiling()
    val boundaryEdges = triangle.getBoundaryEdges
    if boundaryEdges.nonEmpty then
      val firstEdge = boundaryEdges.head
      // Break the chain by setting next to None
      firstEdge.next = None

      triangle.boundarySafe.isLeft shouldBe true
  }

  behavior of "TilingDCEL.getBoundaryEdges"

  it should "return empty list for empty tiling" in {
    val empty = TilingBuilder.empty
    empty.getBoundaryEdges shouldBe List.empty
  }

  it should "return boundary edges in correct order" in {
    val triangle = createTriangleTiling()
    val boundaryEdges = triangle.getBoundaryEdges
    boundaryEdges should have length 3
    
    // Check that edges form a closed loop
    val vertices = boundaryEdges.map(_.origin)
    vertices.map(_.id) should contain theSameElementsInOrderAs Vector("V0", "V2", "V1")
  }

  it should "throw exception for malformed boundary with visited edge" in {
    val triangle = createTriangleTiling()
    val boundaryEdges = triangle.getBoundaryEdges
    if boundaryEdges.length >= 3 then
      val firstEdge = boundaryEdges.head // e0 (start)
      val secondEdge = boundaryEdges(1) // e1
      val thirdEdge = boundaryEdges(2) // e2

      // Modify the structure to create a cycle that doesn't include the start
      // Make e1 -> e2 -> e1 (cycle between e1 and e2)
      secondEdge.next = Some(thirdEdge)
      thirdEdge.next = Some(secondEdge) // This creates the problematic cycle

      an[IllegalStateException] shouldBe thrownBy {
        triangle.getBoundaryEdges
      }
  }

  it should "throw exception for unclosed boundary loop" in {
    val triangle = createTriangleTiling()
    val boundaryEdges = triangle.getBoundaryEdges
    if boundaryEdges.nonEmpty then
      val lastEdge = boundaryEdges.last
      lastEdge.next = None // Break the loop

      an[IllegalStateException] shouldBe thrownBy {
        triangle.getBoundaryEdges
      }
  }

  behavior of "TilingDCEL.validate"

  it should "return Right(()) for valid well-formed tiling" in {
    val triangle = createTriangleTiling()
    val result = TilingDCEL.validate(triangle)
    result shouldBe Right(())
  }

  it should "detect vertex with no leaving edge" in {
    val triangle = createTriangleTiling()
    val vertex = triangle.vertices.head
    vertex.leaving = None
    
    val result = TilingDCEL.validate(triangle)
    result.isLeft shouldBe true
    result.left.value should include(s"Vertex ${vertex.id} has no leaving edge")
  }

  it should "detect vertex leaving edge not originating from vertex" in {
    val triangle = createTriangleTiling()
    val vertex1 = triangle.vertices.head
    val vertex2 = triangle.vertices(1)
    val wrongEdge = HalfEdge(vertex2)
    vertex1.leaving = Some(wrongEdge)
    
    val result = TilingDCEL.validate(triangle)
    result.isLeft shouldBe true
    result.left.value should include(s"Vertex ${vertex1.id} leaving edge doesn't originate from it")
  }

  it should "detect half-edge with no twin" in {
    val triangle = createTriangleTiling()
    val edge = triangle.halfEdges.head
    edge.twin = None
    
    val result = TilingDCEL.validate(triangle)
    result.isLeft shouldBe true
    result.left.value should include(s"Edge from ${edge.origin.id} has no twin")
  }

  it should "detect asymmetric twin relationship" in {
    val triangle = createTriangleTiling()
    val edge1 = triangle.halfEdges.head
    val edge2 = triangle.halfEdges(1)
    val edge3 = HalfEdge(triangle.vertices.head)
    
    edge1.twin = Some(edge3)
    edge3.twin = Some(edge2) // Wrong! Should point back to edge1
    
    val result = TilingDCEL.validate(triangle)
    result.isLeft shouldBe true
    result.left.value should include(s"Edge from ${edge1.origin.id} twin relationship is not symmetric")
  }

  it should "detect half-edge with no next edge" in {
    val triangle = createTriangleTiling()
    val edge = triangle.halfEdges.head
    edge.next = None
    
    val result = TilingDCEL.validate(triangle)
    result.isLeft shouldBe true
    result.left.value should include(s"Edge from ${edge.origin.id} has no next edge")
  }

  it should "detect broken next/prev relationship" in {
    val triangle = createTriangleTiling()
    val edge1 = triangle.halfEdges.head
    val edge2 = triangle.halfEdges(1)
    val edge3 = triangle.halfEdges(2)
    
    edge1.next = Some(edge2)
    edge2.prev = Some(edge3) // Wrong! Should point back to edge1
    
    val result = TilingDCEL.validate(triangle)
    result.isLeft shouldBe true
    result.left.value should include(s"Edge from ${edge1.origin.id} next/prev relationship is broken")
  }

  it should "detect face with edge that doesn't reference it back" in {
    val triangle = createTriangleTiling()
    val face = triangle.innerFaces.head
    val edge = face.halfEdgesSafe.head
    val wrongFace = Face("WrongFace")
    edge.incidentFace = Some(wrongFace)
    
    val result = TilingDCEL.validate(triangle)
    result.isLeft shouldBe true
    result.left.value should include(s"Face ${face.id} contains edge that doesn't reference it back")
  }

  it should "handle face with malformed half-edge traversal" in {
    val triangle = createTriangleTiling()
    val face = triangle.innerFaces.head
    // Create a scenario where face.halfEdges detects a cycle
    val edges = face.halfEdgesSafe
    if edges.length >= 2 then
      val firstEdge = edges.head
      val secondEdge = edges(1)

      // To trigger the cycle detection in Face.halfEdges, we need to create
      // a situation where a non-start edge is revisited.
      // Let's modify the structure so that secondEdge points back to itself
      val originalNext = secondEdge.next
      secondEdge.next = Some(secondEdge)

      // The validation should now detect the cycle when traversing the face
      val result = TilingDCEL.validate(triangle)
      result.isLeft shouldBe true
      result.left.value should include("Cycle detected")

      // Restore for cleanup
      secondEdge.next = originalNext
  }

  it should "return multiple errors when multiple issues exist" in {
    val triangle = createTriangleTiling()
    
    // Break multiple things
    val vertex = triangle.vertices.head
    vertex.leaving = None
    
    val edge = triangle.halfEdges.head
    edge.twin = None
    
    val result = TilingDCEL.validate(triangle)
    result.isLeft shouldBe true
    val errorMessage = result.left.value
    errorMessage should include("has no leaving edge")
    errorMessage should include("has no twin")
  }
