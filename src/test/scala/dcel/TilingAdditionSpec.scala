package io.github.scala_tessella
package dcel

import TilingAddition.*
import BigDecimalGeometry.{AngleDegree, BigPoint}

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

  // Tests for calculateNewVertices method
  behavior of "TilingAddition.calculateNewVertices"

  it should "calculate vertices for a triangle correctly" in {
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    val result = calculateNewVertices(3, p1, p2)

    result should have length 1
    // For a triangle, the third vertex should form an equilateral triangle
    val expectedThirdVertex = BigPoint(BigDecimal("0.5"), BigDecimal("-0.866025403784439")) // approximately sqrt(3)/2
    result.head.almostEquals(expectedThirdVertex) shouldBe true
  }

  it should "calculate vertices for a square correctly" in {
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    val result = calculateNewVertices(4, p1, p2)

    result should have length 2
    // For a square starting from (0,0) to (1,0), the next vertices should be at (1,1) and (0,1)
    val expectedVertex1 = BigPoint(BigDecimal(1), BigDecimal(-1))
    val expectedVertex2 = BigPoint(BigDecimal(0), BigDecimal(-1))

    result(0).almostEquals(expectedVertex1) shouldBe true
    result(1).almostEquals(expectedVertex2) shouldBe true
  }

  it should "calculate vertices for a pentagon correctly" in {
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    val result = calculateNewVertices(5, p1, p2)

    result should have length 3
    // Pentagon has interior angle of 108 degrees
    // Each vertex should be positioned using polar coordinates with 72-degree external angles
    result.foreach { vertex =>
      vertex.x should not be BigDecimal(0) // Vertices should not be at origin
      vertex.y should not be BigDecimal(0) // Vertices should have non-zero y coordinates (except possibly the last)
    }
  }

  it should "calculate vertices for a hexagon correctly" in {
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    val result = calculateNewVertices(6, p1, p2)

    result should have length 4
    // For a regular hexagon, we expect specific geometric relationships
    result.foreach { vertex =>
      // All vertices should be at unit distance from the center of the hexagon
      val distanceFromCenter = math.sqrt((vertex.x - BigDecimal("0.5")).pow(2).toDouble + (vertex.y - BigDecimal("-0.866025403784439")).pow(2).toDouble)
      // The distance should be approximately 1 (unit hexagon)
      math.abs(distanceFromCenter - 1.0) should be < 0.1
    }
  }

  it should "return correct number of vertices for different polygon sizes" in {
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    // Test various polygon sizes
    for (sides <- 3 to 12) {
      val result = calculateNewVertices(sides, p1, p2)
      result should have length (sides - 2) // We already have 2 vertices (p1, p2), need (sides - 2) more
    }
  }

  it should "handle different starting positions correctly" in {
    val testCases = List(
      (BigPoint(BigDecimal(0), BigDecimal(0)), BigPoint(BigDecimal(1), BigDecimal(0))),
      (BigPoint(BigDecimal(1), BigDecimal(1)), BigPoint(BigDecimal(2), BigDecimal(1))),
      (BigPoint(BigDecimal(-1), BigDecimal(-1)), BigPoint(BigDecimal(0), BigDecimal(-1))),
      (BigPoint(BigDecimal(0), BigDecimal(1)), BigPoint(BigDecimal(0), BigDecimal(2)))
    )

    testCases.foreach { case (p1, p2) =>
      val result = calculateNewVertices(4, p1, p2) // Test with squares
      result should have length 2
      result.foreach { vertex =>
        vertex.x should not be null
        vertex.y should not be null
      }
    }
  }

  it should "maintain proper geometric relationships for triangles" in {
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    val result = calculateNewVertices(3, p1, p2)
    val p3 = result.head

    // Check that all sides have equal length (equilateral triangle)
    val side1Length = math.sqrt((p2.x - p1.x).pow(2).toDouble + (p2.y - p1.y).pow(2).toDouble)
    val side2Length = math.sqrt((p3.x - p2.x).pow(2).toDouble + (p3.y - p2.y).pow(2).toDouble)
    val side3Length = math.sqrt((p1.x - p3.x).pow(2).toDouble + (p1.y - p3.y).pow(2).toDouble)

    math.abs(side1Length - side2Length) should be < 1e-6
    math.abs(side2Length - side3Length) should be < 1e-6
    math.abs(side3Length - side1Length) should be < 1e-6
  }

  it should "create vertices that form a closed polygon when combined with input points" in {
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    for (sides <- 3 to 8) {
      val newVertices = calculateNewVertices(sides, p1, p2)
      val allVertices = p1 :: p2 :: newVertices

      // Check that we have the correct total number of vertices
      allVertices should have length sides

      // The last calculated vertex should be positioned to close the polygon back to p1
      if (newVertices.nonEmpty) {
        val lastVertex = newVertices.last
        // The distance from the last vertex back to p1 should be approximately equal to the side length
        val closingDistance = math.sqrt((p1.x - lastVertex.x).pow(2).toDouble + (p1.y - lastVertex.y).pow(2).toDouble)
        val sideLength = math.sqrt((p2.x - p1.x).pow(2).toDouble + (p2.y - p1.y).pow(2).toDouble)

        math.abs(closingDistance - sideLength) should be < 1e-6
      }
    }
  }

  it should "handle edge case with minimum polygon size" in {
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    val result = calculateNewVertices(3, p1, p2) // Minimum valid polygon
    result should have length 1
    result.head should not be null
  }

  it should "produce vertices with consistent orientation" in {
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    // Test with a square to check orientation
    val result = calculateNewVertices(4, p1, p2)
    val allVertices = List(p1, p2) ++ result

    // Calculate the signed area to determine orientation (should be negative for clockwise)
    val signedArea = allVertices.zip(allVertices.tail :+ allVertices.head).map { case (curr, next) =>
      curr.x * next.y - next.x * curr.y
    }.sum / 2

    // For a properly oriented polygon, the signed area should be negative
    signedArea.toDouble should be < 0.0
  }

  // Basic addition tests
  it should "add a triangle to a triangle, producing a valid DCEL" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle.addRegularPolygon(3, "V1")

    result.isRight shouldBe true
    val tiling = result.value
//    println(tiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))

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
//    println(tiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))

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
//    println(tiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))

    verifyValidTiling(tiling)

    tiling.vertices should have size 5
    tiling.innerFaces should have size 2
  }

  it should "add a hexagon to a triangle, producing a valid DCEL" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle.addRegularPolygon(6, "V1")

    result.isRight shouldBe true
    val tiling = result.value
//    println(tiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))

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
//    println(finalTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))

    finalTiling.vertices should have size 5
    finalTiling.innerFaces should have size 3
  }

  it should "maintain correct boundary traversal after multiple additions" in {
    val square = TilingBuilder.createRegularPolygon(4).value
    val withTriangle = square.addRegularPolygon(3, "V1").value
    val withPentagon = withTriangle.addRegularPolygon(5, "V2").value

    verifyValidTiling(withPentagon)
//    println(withPentagon.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))

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

//    println(currentTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
    currentTiling.innerFaces should have size 7 // 1 hexagon + 6 triangles
  }

  it should "successfully add a triangle with more than one edge shared" in {
    val initialTiling = TilingBuilder.createRegularPolygon(3).value

    val result = initialTiling
      .maybeAddRegularPolygon(3, "V1").value
      .maybeAddRegularPolygon(3, "V1").value
      .maybeAddRegularPolygon(3, "V1").value
      .maybeAddRegularPolygon(3, "V1").value
      .maybeAddRegularPolygon(3, "V1")
    result.isRight shouldBe true

    val newTiling = result.value
//    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
//    println(TilingDCEL.validate(newTiling))
    verifyValidTiling(newTiling)
  }

  it should "successfully add an hexagon with more than one edge shared on both sides of the edge to build on" in {
    val initialTiling = TilingBuilder.createRegularPolygon(6).value

    val result = initialTiling
      .maybeAddRegularPolygon(6, "V1").value
      .maybeAddRegularPolygon(6, "V7").value
      .maybeAddRegularPolygon(6, "V1")
    result.isRight shouldBe true

    val newTiling = result.value
//    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
//    println(TilingDCEL.validate(newTiling))
    verifyValidTiling(newTiling)
  }

  it should "successfully add an hexagon with more than one edge shared on one side of the edge to build on" in {
    val initialTiling = TilingBuilder.createRegularPolygon(6).value

    val result = initialTiling
      .maybeAddRegularPolygon(6, "V1").value
      .maybeAddRegularPolygon(6, "V7").value
      .maybeAddRegularPolygon(6, "V2")
    result.isRight shouldBe true

    val newTiling = result.value
//    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
//    println(TilingDCEL.validate(newTiling))
    verifyValidTiling(newTiling)
  }

  it should "successfully add an hexagon with more than one edge shared on the other side of the edge to build on" in {
    val initialTiling = TilingBuilder.createRegularPolygon(6).value

    val result = initialTiling
      .maybeAddRegularPolygon(6, "V1").value
      .maybeAddRegularPolygon(6, "V7").value
      .maybeAddRegularPolygon(6, "V7")
    result.isRight shouldBe true

    val newTiling = result.value
//    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
//    println(TilingDCEL.validate(newTiling))
    verifyValidTiling(newTiling)
  }

  it should "successfully add a square with more than one edge shared on one side of the edge to build on" in {
    val initialTiling = TilingBuilder.createRegularPolygon(4).value

    val result = initialTiling
      .maybeAddRegularPolygon(4, "V1").value
      .maybeAddRegularPolygon(4, "V1").value
      .maybeAddRegularPolygon(4, "V1")
    result.isRight shouldBe true

    val newTiling = result.value
//    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
//    println(TilingDCEL.validate(newTiling))
    verifyValidTiling(newTiling)
  }

  it should "successfully add a square with more than one edge shared on the other side of the edge to build on" in {
    val initialTiling = TilingBuilder.createRegularPolygon(4).value

    val result = initialTiling
      .maybeAddRegularPolygon(4, "V1").value
      .maybeAddRegularPolygon(4, "V1").value
      .maybeAddRegularPolygon(4, "V2")
    result.isRight shouldBe true

    val newTiling = result.value
//    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
//    println(TilingDCEL.validate(newTiling))
    verifyValidTiling(newTiling)
  }

  val commonTiling: TilingDCEL =
    TilingBuilder.createRegularPolygon(4).value
      .maybeAddRegularPolygon(3, "V1").value
      .maybeAddRegularPolygon(3, "V3").value
      .maybeAddRegularPolygon(3, "V5").value
      .maybeAddRegularPolygon(3, "V3").value
      .maybeAddRegularPolygon(4, "V7").value

  it should "successfully create a complex tessellation " in {
    val result = commonTiling
      .maybeAddRegularPolygon(3, "V4").value
      .maybeAddRegularPolygon(4, "V3")
    result.isRight shouldBe true

    val newTiling = result.value
//    println(TilingDCEL.validate(newTiling))
//    println(newTiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
    verifyValidTiling(newTiling)
  }

  it should "successfully fill a hole created by a share vertex" in {
    val result = commonTiling
      .maybeAddRegularPolygon(4, "V3")
    result.isRight shouldBe true

    val newTiling = result.value
//    println(TilingDCEL.validate(newTiling))
//    println(newTiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
    verifyValidTiling(newTiling)
  }

//  it should "successfully fill a hole created by a shared edge" in {
//    val initialTiling = TilingBuilder.createRegularPolygon(3).value
//
//    val result = initialTiling
//      .maybeAddRegularPolygon(4, "V1").value
//      .maybeAddRegularPolygon(4, "V3").value
//      .maybeAddRegularPolygon(3, "V5").value
//      .maybeAddRegularPolygon(3, "V6").value
//      .maybeAddRegularPolygon(4, "V5").value
//      .maybeAddRegularPolygon(3, "V6").value
//      .maybeAddRegularPolygon(3, "V11").value
//      .maybeAddRegularPolygon(4, "V6")
//    result.isRight shouldBe true
//
//    val newTiling = result.value
////    println(newTiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
////    println(TilingDCEL.validate(newTiling))
//    verifyValidTiling(newTiling)
//  }
