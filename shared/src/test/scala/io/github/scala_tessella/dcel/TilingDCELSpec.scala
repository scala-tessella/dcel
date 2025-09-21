package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.BigDecimalGeometry.{AngleDegree, BigPoint}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingDCELSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingDCEL.faces"

  it should "return all faces including outer face" in
    allAssert(
      triangle.faces should have length 2,
      triangle.faces should contain(triangle.outerFace),
      triangle.faces should contain allElementsOf triangle.innerFaces
    )

  it should "return only outer face when no inner faces exist" in {
    val empty = emptyTiling
    allAssert(
      empty.faces should have length 1,
      empty.faces should contain only empty.outerFace
    )
  }

  behavior of "TilingDCEL.findVertex"

  it should "find an existing vertex by id" in
    allAssert(
      triangle.findVertexUnsafe(V1) shouldBe defined,
      triangle.findVertexUnsafe(V2) shouldBe defined,
      triangle.findVertexUnsafe(V3) shouldBe defined
    )

  it should "return None for non-existent vertex id" in
    allAssert(
      triangle.findVertexUnsafe(VertexId("V999")) shouldBe None,
      triangle.findVertexUnsafe(VertexId("NonExistent")) shouldBe None
    )

  it should "find vertices in empty tiling" in {
    emptyTiling.findVertexUnsafe(VertexId("V0")) shouldBe None
  }

  behavior of "TilingDCEL.findFace"

  it should "find an existing face by id" in
    allAssert(
      triangle.findFace(FaceId.outerId).toOption shouldBe defined,
      triangle.findFace(FaceId.firstInnerId).toOption shouldBe defined
    )

  it should "return None for non-existent face id" in
    allAssert(
      triangle.findFace(FaceId("F999")).toOption shouldBe None,
      triangle.findFace(FaceId("NonExistent")).toOption shouldBe None
    )

  behavior of "TilingDCEL.hasConnectedFaces"

  it should "return true for empty tiling" in {
    emptyTiling.hasConnectedFaces shouldBe true
  }

  it should "return true for single polygon tiling" in
    allAssert(
      triangle.hasConnectedFaces shouldBe true,
      square.hasConnectedFaces shouldBe true
    )

  it should "return true for connected multi-polygon tiling" in {
    val twoTriangles = triangle.maybeAddRegularPolygonToBoundary(V1, 3).value
    twoTriangles.hasConnectedFaces shouldBe true
  }

  behavior of "TilingDCEL.boundary"

  it should "return empty vector for empty tiling" in {
    emptyTiling.boundaryVerticesUnsafe shouldBe Vector.empty[Vertex]
  }

  it should "return correct boundary vertices for triangle in clockwise order" in {
    val boundary = triangle.boundaryVerticesUnsafe
    allAssert(
      boundary should have length 3,
      boundary.map(_.id) should contain theSameElementsInOrderAs Vector(V1, V3, V2)
    )
  }

  it should "return correct boundary vertices for square in clockwise order" in {
    val boundary = square.boundaryVerticesUnsafe
    allAssert(
      boundary should have length 4,
      boundary.map(_.id) should contain theSameElementsInOrderAs Vector(V1, V4, V3, V2)
    )
  }

  it should "return correct boundary vertices for hexagon" in {
    val boundary = hexagon.boundaryVerticesUnsafe
    allAssert(
      boundary should have length 6,
      boundary.map(_.id) should contain theSameElementsInOrderAs Vector(V1, "V6", "V5", V4, V3, V2)
    )
  }

  behavior of "TilingDCEL.boundarySafe"

  it should "return same result as boundary for well-formed tilings" in
    allAssert(
      triangle.boundaryVertices.value shouldEqual triangle.boundaryVerticesUnsafe,
      square.boundaryVertices.value shouldEqual square.boundaryVerticesUnsafe
    )

  it should "return empty vector for empty tiling" in {
    emptyTiling.boundaryVertices shouldBe Right(Vector.empty)
  }

  it should "fail for malformed boundary loop" in {
    val tiling        = triangle
    val boundaryEdges = tiling.boundaryEdges.value
    val firstEdge     = boundaryEdges.head // This is the startEdge
    val secondEdge    = boundaryEdges(1)

    // Create a malformed loop where a non-start edge is repeated
    firstEdge.next = Some(secondEdge)
    secondEdge.next = Some(secondEdge) // Make second edge point to itself

    tiling.boundaryVertices.isLeft shouldBe true
  }

  it should "fail for open chain in boundary" in {
    val tiling        = triangle
    val boundaryEdges = tiling.boundaryEdges.value
    val firstEdge     = boundaryEdges.head
    // Break the chain by setting next to None
    firstEdge.next = None

    tiling.boundaryVertices.isLeft shouldBe true
  }

  behavior of "TilingDCEL.getBoundaryEdges"

  it should "return empty list for empty tiling" in {
    emptyTiling.boundaryEdges shouldBe Right(List.empty)
  }

  it should "return boundary edges in correct order" in {
    val boundaryEdges = triangle.boundaryEdges.value
    allAssert(
      boundaryEdges should have length 3, {
        // Check that edges form a closed loop
        val vertices = boundaryEdges.map(_.origin)
        vertices.map(_.id) should contain theSameElementsInOrderAs Vector(V1, V3, V2)
      }
    )
  }

  it should "fail for malformed boundary with visited edge" in {
    val tiling        = triangle
    val boundaryEdges = tiling.boundaryEdges.value
    val firstEdge     = boundaryEdges.head // e0 (start)
    val secondEdge    = boundaryEdges(1)   // e1
    val thirdEdge     = boundaryEdges(2)   // e2

    // Modify the structure to create a cycle that doesn't include the start
    // Make e1 -> e2 -> e1 (cycle between e1 and e2)
    secondEdge.next = Some(thirdEdge)
    thirdEdge.next = Some(secondEdge) // This creates the problematic cycle

    tiling.boundaryEdges.isLeft shouldBe true
  }

  it should "fail for unclosed boundary loop" in {
    val tiling        = triangle
    val boundaryEdges = tiling.boundaryEdges.value
    // Break the loop by making the last edge not point back to the first
    boundaryEdges.last.next = None
    tiling.boundaryEdges.isLeft shouldBe true
  }

  behavior of "TilingDCEL.getAnglesAtVertex"

  it should "return the angles for a vertex where all incident edges have an angle" in {
    val v1     = triangle.findVertexUnsafe(V1).get
    v1.incidentEdgesUnsafe.filter(_.hasIncidentFace(triangle.outerFace)).foreach(_.angle =
      Some(AngleDegree(300))
    )
    val result = triangle.getAnglesAtVertex(V1)
    result.value should contain theSameElementsAs List(AngleDegree(60), AngleDegree(300))
  }

  it should "return an error for a non-existent vertex" in {
    val result = triangle.getAnglesAtVertex(VertexId("V999"))
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message shouldEqual "Vertex with ID 'V999' not found."
    )
  }

  behavior of "TilingDCEL.empty"

  it should "create empty tiling" in {
    val emptyTiling = TilingDCEL.empty
    allAssert(
      emptyTiling.vertices shouldBe empty,
      emptyTiling.halfEdges shouldBe empty,
      emptyTiling.innerFaces shouldBe empty,
      emptyTiling.outerFace.id shouldBe FaceId.outerId
    )
  }

  behavior of "TilingDCEL.validate"

  it should "succeed for a valid single polygon tiling" in {
    TilingDCEL.validate(square) shouldBe Right(())
  }

  it should "succeed for a valid multi-polygon tiling" in {
    val twoSquares = square.maybeAddRegularPolygonToBoundary(V1, 4).value
    TilingDCEL.validate(twoSquares) shouldBe Right(())
  }

  it should "fail if a vertex has no leaving edge" in {
    val tiling = square
    tiling.vertices.head.leaving = None
    val result = TilingDCEL.validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("has no leaving edge")
    )
  }

  it should "fail if an edge has no twin" in {
    val tiling = square
    tiling.halfEdges.head.twin = None
    val result = TilingDCEL.validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("has no twin")
    )
  }

  it should "fail if an edge's next/prev relationship is broken" in {
    val tiling = square
    val edge   = tiling.halfEdges.head
    edge.next.get.prev = None // Break the link
    val result = TilingDCEL.validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Next/prev relationship broken")
    )
  }

  it should "fail if an inner face has an incorrect sum of angles" in {
    val tiling = square
    // Tamper with an angle
    tiling.innerFaces.head.outerComponent.get.angle = Some(AngleDegree(89))
    val result = TilingDCEL.validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("The sum of interior angles is incorrect")
    )
  }

  it should "fail if an inner face has a full circle angle" in {
    val tiling = square
    // Tamper with angles to make one a full circle while keeping the sum correct
    val edges  = tiling.innerFaces.head.halfEdges.value
    edges(0).angle = Some(AngleDegree(360))
    edges(1).angle = Some(AngleDegree(0))
    edges(2).angle = Some(AngleDegree(90))
    edges(3).angle = Some(AngleDegree(-90))
    val result = TilingDCEL.validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("cannot have full circles as interior angles")
    )
  }

  it should "fail if a face edge does not point back to the face" in {
    val tiling = square
    // Make an inner edge "forget" its face
    tiling.innerFaces.head.outerComponent.get.incidentFace = None
    val result = TilingDCEL.validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("references back")
    )
  }

  it should "fail if the boundary angles do not sum correctly" in {
    val twoSquares      = square.maybeAddRegularPolygonToBoundary(V1, 4).value
    // V2 is on the boundary. The inner edge from V2 belongs to the first square.
    val v2              = twoSquares.findVertexUnsafe(V2).get
    val innerEdgeFromV2 = v2.incidentEdgesUnsafe.find(_.incidentFace.exists(_.id == FaceId.firstInnerId)).get

    // Distort the angle, which affects both the face and boundary angle sums
    innerEdgeFromV2.angle = Some(AngleDegree(80))

    val result = TilingDCEL.validate(twoSquares)
    allAssert(
      result.isLeft shouldBe true, {
        val error         = result.left.value.message
        // Check that at least one of the expected errors is present, as iteration order is not guaranteed
        val faceError     = "Face F1: GeometryError(The sum of interior angles is incorrect"
        val boundaryError = "Boundary: The sum of interior angles is incorrect"
        (error.contains(faceError) || error.contains(boundaryError)) shouldBe true
      }
    )
  }

  it should "fail if a boundary angle is undefined" in {
    val tiling = square
    tiling.boundaryEdges.value.head.angle = None
    val result = TilingDCEL.validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Tiling has at least one half-edge with no angle defined")
    )
  }

  it should "fail if a boundary angle is a full circle (360 degrees)" in {
    val tiling = square
    tiling.boundaryEdges.value.head.angle = Some(AngleDegree(360))
    val result = TilingDCEL.validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Full circle boundary angles")
    )
  }

  it should "fail if a boundary angle is a full circle (0 degrees)" in {
    val tiling = square
    tiling.boundaryEdges.value.head.angle = Some(AngleDegree(0))
    val result = TilingDCEL.validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Full circle boundary angles")
    )
  }
