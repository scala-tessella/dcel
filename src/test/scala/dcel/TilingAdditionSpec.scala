package io.github.scala_tessella
package dcel

import TilingAddition.*
import BigDecimalGeometry.AngleDegree

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.compat.numeric

class TilingAdditionSpec extends AnyFlatSpec with Matchers with EitherValues:

  behavior of "TilingDCEL.addRegularPolygon"

  // Helper method to verify DCEL validity
  private def verifyValidTiling(tiling: TilingDCEL): Unit =
    val structuralCheck = TilingDCEL.validate(tiling)
    structuralCheck.isRight shouldBe true

    val spatialCheck = TilingDCEL.spatiallyValidate(tiling)
    spatialCheck.isRight shouldBe true

  // Basic addition tests
  it should "add a triangle to a triangle, producing a valid DCEL" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle.addRegularPolygon(3, "V1")

    result.isRight shouldBe true
    val tiling = result.value

    verifyValidTiling(tiling)

    // Check structure
    tiling.vertices should have size 4
    tiling.innerFaces should have size 2
    tiling.halfEdges should have size 10 // 4 boundary + 6 inner edges

    // Check boundary angles
    tiling.outerFace.halfEdgesSafe.map(_.angle.get.toString).mkString(", ") shouldBe "240, 300, 240, 300"
    tiling.outerFace.halfEdgesSafe.map(_.incidentFace.get.id).mkString(", ") shouldBe "F0, F0, F0, F0"

    // Check inner face angles
    tiling.innerFaces.map(_.halfEdgesSafe.map(_.angle.get.toString).mkString(", ")) shouldBe List("60, 60, 60", "60, 60, 60")
    tiling.innerFaces.map(_.halfEdgesSafe.map(_.incidentFace.get.id).mkString(", ")) shouldBe List("F1, F1, F1", "F2, F2, F2")

    // Check boundary
    val boundary = tiling.boundary
    boundary should have length 4
    boundary.map(_.id) should contain theSameElementsInOrderAs Vector("V1", "V4", "V3", "V2")
  }

  it should "add a square to a triangle, producing a valid DCEL" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle.addRegularPolygon(4, "V1")

    result.isRight shouldBe true
    val tiling = result.value

    verifyValidTiling(tiling)

    tiling.vertices should have size 5
    tiling.innerFaces should have size 2

    // Check that the sum of angles around shared vertices is 360°
    val v0 = tiling.findVertex("V1").get
    val anglesAroundV0 = v0.incidentEdges.flatMap(_.angle).map(_.toRational).sum
    AngleDegree(anglesAroundV0).isFullCircle shouldBe true
  }

  it should "add a triangle to a square, producing a valid DCEL" in {
    val square = TilingBuilder.createRegularPolygon(4).value
    val result = square.addRegularPolygon(3, "V1")

    result.isRight shouldBe true
    val tiling = result.value

    verifyValidTiling(tiling)

    tiling.vertices should have size 5
    tiling.innerFaces should have size 2
  }

  it should "add a hexagon to a triangle, producing a valid DCEL" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle.addRegularPolygon(6, "V1")

    result.isRight shouldBe true
    val tiling = result.value

    verifyValidTiling(tiling)

    tiling.vertices should have size 7
    tiling.innerFaces should have size 2
  }

  // Sequential addition tests
  it should "successfully add multiple polygons in sequence" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value

    val step1 = triangle.addRegularPolygon(3, "V1")
    step1.isRight shouldBe true
    verifyValidTiling(step1.value)

    val step2 = step1.value.addRegularPolygon(3, "V2")
    step2.isRight shouldBe true
    verifyValidTiling(step2.value)

    val finalTiling = step2.value
    finalTiling.vertices should have size 5
    finalTiling.innerFaces should have size 3
  }

  it should "maintain correct boundary traversal after multiple additions" in {
    val square = TilingBuilder.createRegularPolygon(4).value
    val withTriangle = square.addRegularPolygon(3, "V1").value
    val withPentagon = withTriangle.addRegularPolygon(5, "V2").value

    verifyValidTiling(withPentagon)

    // Check that boundary is still traversable
    val boundary = withPentagon.boundary
    boundary should not be empty

    // Verify boundary forms a closed loop
    val boundaryEdges = withPentagon.getBoundaryEdges.value
    boundaryEdges.foreach { edge =>
      edge.next should be(defined)
      edge.prev should be(defined)
    }
  }

  // Edge case tests for different vertex positions
  it should "add polygons on different boundary vertices" in {
    val square = TilingBuilder.createRegularPolygon(4).value

    // Test adding on each vertex
    for (vertexId <- List("V1", "V2", "V3", "V4")) {
      val result = square.addRegularPolygon(3, vertexId)
      result.isRight shouldBe true
      verifyValidTiling(result.value)
    }
  }

  // Large polygon tests
  it should "handle large polygons correctly" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle.addRegularPolygon(12, "V1") // Dodecagon

    result.isRight shouldBe true
    val tiling = result.value

    verifyValidTiling(tiling)

    tiling.vertices should have size 13
    tiling.innerFaces should have size 2
  }

  // Angle calculation tests
  it should "correctly calculate boundary angles for shared vertices" in {
    val square = TilingBuilder.createRegularPolygon(4).value
    val result = square.addRegularPolygon(4, "V1")

    result.isRight shouldBe true
    val tiling = result.value

    // V0 is shared between two squares, so boundary angle should be 360 - 90 - 90 = 180
    val v0BoundaryEdge = tiling.getBoundaryEdges.value.find(_.origin.id == "V1").get
    v0BoundaryEdge.angle.get shouldBe AngleDegree(180)
  }

  it should "correctly handle vertices with multiple incident faces" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val withTriangle2 = triangle.addRegularPolygon(3, "V1").value
    val withTriangle3 = withTriangle2.addRegularPolygon(3, "V1").value

    verifyValidTiling(withTriangle3)

    // V0 now has 3 triangles, so boundary angle should be 360 - 3*60 = 180
    val v0BoundaryEdge = withTriangle3.getBoundaryEdges.value.find(_.origin.id == "V1").get
    v0BoundaryEdge.angle.get shouldBe AngleDegree(180)
  }

  // Error condition tests
  it should "fail to add a polygon with less than 3 sides" in {
    val square = TilingBuilder.createRegularPolygon(4).value
    val result = square.addRegularPolygon(2, "V2")
    result.isLeft shouldBe true
    result.left.value should include("must have at least 3 sides")
  }

  it should "fail to add a polygon with 0 sides" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle.addRegularPolygon(0, "V1")
    result.isLeft shouldBe true
    result.left.value should include("must have at least 3 sides")
  }

  it should "fail to add a polygon with negative sides" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle.addRegularPolygon(-1, "V1")
    result.isLeft shouldBe true
    result.left.value should include("must have at least 3 sides")
  }

  it should "fail to add a polygon on a non-existent vertex" in {
    val square = TilingBuilder.createRegularPolygon(4).value
    val result = square.addRegularPolygon(3, "V99")
    result.isLeft shouldBe true
    result.left.value should include("not found on the boundary")
  }

  it should "fail to add a polygon on an empty vertex ID" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle.addRegularPolygon(3, "")
    result.isLeft shouldBe true
    result.left.value should include("not found on the boundary")
  }

  // Boundary integrity tests
  it should "maintain boundary connectivity after addition" in {
    val pentagon = TilingBuilder.createRegularPolygon(5).value
    val result = pentagon.addRegularPolygon(3, "V2")

    result.isRight shouldBe true
    val tiling = result.value

    // Check that boundary is still a single connected component
    val boundaryEdges = tiling.getBoundaryEdges.value
    boundaryEdges should not be empty

    // Verify each edge has proper next/prev links
    boundaryEdges.foreach { edge =>
      edge.next should be(defined)
      edge.prev should be(defined)
      edge.next.get.prev should contain(edge)
      edge.prev.get.next should contain(edge)
    }
  }

  it should "preserve vertex IDs and not create duplicates" in {
    val hexagon = TilingBuilder.createRegularPolygon(6).value
    val originalVertexIds = hexagon.vertices.map(_.id).toSet

    val result = hexagon.addRegularPolygon(4, "V3")
    result.isRight shouldBe true
    val tiling = result.value

    val newVertexIds = tiling.vertices.map(_.id).toSet

    // Original vertices should still exist
    originalVertexIds.subsetOf(newVertexIds) shouldBe true

    // No duplicate IDs
    tiling.vertices.map(_.id).distinct should have size tiling.vertices.size
  }

  // Face integrity tests
  it should "correctly assign face IDs to new faces" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle.addRegularPolygon(4, "V1")

    result.isRight shouldBe true
    val tiling = result.value

    val faceIds = tiling.innerFaces.map(_.id).toSet
    faceIds should contain(Face.firstInnerId) // Original face
    faceIds should contain("F2") // New face
    faceIds should have size 2
  }

  it should "ensure all half-edges have incident faces assigned" in {
    val square = TilingBuilder.createRegularPolygon(4).value
    val result = square.addRegularPolygon(6, "V1")

    result.isRight shouldBe true
    val tiling = result.value

    tiling.halfEdges.foreach { edge =>
      edge.incidentFace should be(defined)
    }
  }

  // Stress tests
  it should "handle adding many small polygons" in {
    var currentTiling = TilingBuilder.createRegularPolygon(6).value

    // Add triangles to create a flower pattern
    for (i <- 1 to 6)
      val result = currentTiling.addRegularPolygon(3, s"V$i")
      result.isRight shouldBe true
      currentTiling = result.value
      verifyValidTiling(currentTiling)

    currentTiling.innerFaces should have size 7 // 1 hexagon + 6 triangles
  }

  it should "successfully add an hexagon with coincident vertices" in {
    // Start with a single hexagon (V0-V1-V2-V3-V4-V5)
    val initialTiling = TilingBuilder.createRegularPolygon(6).value
    initialTiling.vertices.length shouldBe 6
    initialTiling.innerFaces.length shouldBe 1
    initialTiling.boundary.map(_.id) shouldBe Vector("V1", "V6", "V5", "V4", "V3", "V2")

    val result = initialTiling
      .maybeAddRegularPolygon(6, "V1").value
      .maybeAddRegularPolygon(6, "V3").value
      .maybeAddRegularPolygon(6, "V2")
    result.isRight shouldBe true

    val newTiling = result.value
    //println(newTiling.toSVG())
    verifyValidTiling(newTiling)
  }
