package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingAddition.*
import io.github.scala_tessella.dcel.TilingBuilder.vertexIdV
import io.github.scala_tessella.dcel.TilingDeletion.*
import io.github.scala_tessella.dcel.TilingEquivalency.*
import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint, RegularPolygon, SimplePolygon}
import io.github.scala_tessella.dcel.structure.{FaceId, VertexId}
import io.github.scala_tessella.ring_seq.RingSeq.*
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingAdditionSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingDCEL.addRegularPolygonToBoundary"

  // Helper method to verify DCEL validity
  private def verifyValidTiling(tiling: TilingDCEL): Assertion =
    val comprehensiveCheck = validate(tiling)
    comprehensiveCheck.isRight shouldBe true

  // Tests for calculateNewVertices method
  behavior of "TilingAddition.calculateNewVertices"

  it should "calculate vertices for a triangle correctly" in:
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    val result = calculateNewVertices(3, p1, p2)

    allAssert(
      result should have length 1, {
        // For a triangle, the third vertex should form an equilateral triangle
        val expectedThirdVertex =
          BigPoint(BigDecimal("0.5"), BigDecimal("-0.866025403784439")) // approximately sqrt(3)/2
        result.head.almostEquals(expectedThirdVertex) shouldBe true
      }
    )

  it should "calculate vertices for a square correctly" in:
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    val result = calculateNewVertices(4, p1, p2)

    allAssert(
      result should have length 2, {
        // For a square starting from (0,0) to (1,0), the next vertices should be at (1,1) and (0,1)
        val expectedVertex1 = BigPoint(BigDecimal(1), BigDecimal(-1))
        val expectedVertex2 = BigPoint(BigDecimal(0), BigDecimal(-1))
        allAssert(
          result(0).almostEquals(expectedVertex1) shouldBe true,
          result(1).almostEquals(expectedVertex2) shouldBe true
        )
      }
    )

  it should "calculate vertices for a pentagon correctly" in:
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    val result = calculateNewVertices(5, p1, p2)

    allAssert(
      result should have length 3, {
        // Pentagon has each interior angle of 108 degrees
        // Each vertex should be positioned using polar coordinates with 72-degree external angles
        val assertions =
          result.map: vertex =>
            allAssert(
              vertex.x should not be BigDecimal(0), // Vertices should not be at origin
              vertex.y should not be BigDecimal(
                0
              )                                     // Vertices should have non-zero y coordinates (except possibly the last)
            )
        allAssert(assertions*)
      }
    )

  it should "calculate vertices for a hexagon correctly" in:
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    val result = calculateNewVertices(6, p1, p2)

    allAssert(
      result should have length 4,
      // For a regular hexagon, we expect specific geometric relationships
      allAssert(
        result.map { vertex =>
          // All vertices should be at unit distance from the center of the hexagon
          val distanceFromCenter = math.sqrt((vertex.x - BigDecimal("0.5")).pow(
            2
          ).toDouble + (vertex.y - BigDecimal("-0.866025403784439")).pow(2).toDouble)
          // The distance should be approximately 1 (unit hexagon)
          math.abs(distanceFromCenter - 1.0) should be < 0.1
        }*
      )
    )

  it should "return correct number of vertices for different polygon sizes" in:
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    // Test various polygon sizes
    for (sides <- 3 to 12) {
      val result = calculateNewVertices(sides, p1, p2)
      result should have length (sides - 2) // We already have 2 vertices (p1, p2), need (sides - 2) more
    }

  it should "handle different starting positions correctly" in:
    val testCases = List(
      (BigPoint(BigDecimal(0), BigDecimal(0)), BigPoint(BigDecimal(1), BigDecimal(0))),
      (BigPoint(BigDecimal(1), BigDecimal(1)), BigPoint(BigDecimal(2), BigDecimal(1))),
      (BigPoint(BigDecimal(-1), BigDecimal(-1)), BigPoint(BigDecimal(0), BigDecimal(-1))),
      (BigPoint(BigDecimal(0), BigDecimal(1)), BigPoint(BigDecimal(0), BigDecimal(2)))
    )

    allAssert(
      testCases.map { case (p1, p2) =>
        val result = calculateNewVertices(4, p1, p2) // Test with squares
        result should have length 2
      }*
    )

  it should "maintain proper geometric relationships for triangles" in:
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    val result = calculateNewVertices(3, p1, p2)
    val p3     = result.head

    // Check that all sides have equal length (equilateral triangle)
    val side1Length = math.sqrt((p2.x - p1.x).pow(2).toDouble + (p2.y - p1.y).pow(2).toDouble)
    val side2Length = math.sqrt((p3.x - p2.x).pow(2).toDouble + (p3.y - p2.y).pow(2).toDouble)
    val side3Length = math.sqrt((p1.x - p3.x).pow(2).toDouble + (p1.y - p3.y).pow(2).toDouble)

    allAssert(
      math.abs(side1Length - side2Length) should be < 1e-6,
      math.abs(side2Length - side3Length) should be < 1e-6,
      math.abs(side3Length - side1Length) should be < 1e-6
    )

  it should "create vertices that form a closed polygon when combined with input points" in:
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    for (sides <- 3 to 8) {
      val newVertices = calculateNewVertices(sides, p1, p2)
      val allVertices = p1 :: p2 :: newVertices

      allAssert(
        // Check that we have the correct total number of vertices
        allVertices should have length sides,
        // The last calculated vertex should be positioned to close the polygon back to p1
        if newVertices.nonEmpty then
          val lastVertex      = newVertices.last
          // The distance from the last vertex back to p1 should be approximately equal to the side length
          val closingDistance =
            math.sqrt((p1.x - lastVertex.x).pow(2).toDouble + (p1.y - lastVertex.y).pow(2).toDouble)
          val sideLength      = math.sqrt((p2.x - p1.x).pow(2).toDouble + (p2.y - p1.y).pow(2).toDouble)

          math.abs(closingDistance - sideLength) should be < 1e-6
        else
          succeed
      )
    }

  it should "handle edge case with minimum polygon size" in:
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    val result = calculateNewVertices(3, p1, p2) // Minimum valid polygon
    result should have length 1

  it should "produce vertices with consistent orientation" in:
    val p1 = BigPoint(BigDecimal(0), BigDecimal(0))
    val p2 = BigPoint(BigDecimal(1), BigDecimal(0))

    // Test with a square to check orientation
    val result      = calculateNewVertices(4, p1, p2)
    val allVertices = List(p1, p2) ++ result

    // Calculate the signed area to determine orientation (should be negative for clockwise)
    val signedArea = allVertices.zip(allVertices.tail :+ allVertices.head).map { case (curr, next) =>
      curr.x * next.y - next.x * curr.y
    }.sum / 2

    // For a properly oriented polygon, the signed area should be negative
    signedArea.toDouble should be < 0.0

  // Basic addition tests
  it should "add a square to a triangle, producing a valid DCEL" in:
    val result = triangle.addRegularPolygonToBoundary(V1, RegularPolygon(4))

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true)),
        allAssert(
          verifyValidTiling(tiling),
          tiling.vertices should have size 5,
          tiling.innerFaces should have size 2, {
            // Check that the sum of angles around shared vertices is 360°
            val v0             = tiling.findVertexUnsafe(V1).get
            val anglesAroundV0 = v0.incidentEdgesUnsafe.flatMap(_.angle).sumExact
            anglesAroundV0.isFullCircle shouldBe true
          }
        )
      }
    )

  val irregularPentagonAngles: Vector[AngleDegree] =
    Vector(90, 150, 60, 150, 90).map(AngleDegree(_))

  it should "add an irregular pentagon to a triangle, producing a valid DCEL" in:
    val result = triangle.addSimplePolygonToBoundary(V1, SimplePolygon(irregularPentagonAngles))

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(TilingDCEL.validate(tiling))
        //    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        allAssert(
          verifyValidTiling(tiling),
          tiling.vertices should have size 6,
          tiling.innerFaces should have size 2
        )
      }
    )

  it should "add the same irregular pentagon with a different orientation to a triangle, producing a valid DCEL" in:
    val result =
      triangle.addSimplePolygonToBoundary(V1, SimplePolygon(irregularPentagonAngles.rotateRight(1)))

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(TilingDCEL.validate(tiling))
        //    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        allAssert(
          verifyValidTiling(tiling),
          tiling.vertices should have size 6,
          tiling.innerFaces should have size 2
        )
      }
    )

  /** Common bench  <img src="file:../../../../../resources/commonBench.svg"/> */
  def commonBench: TilingDCEL =
    square
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V3, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V5, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V3, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V7"), RegularPolygon(4)).value

  it should "add an irregular pentagon with shared edges" in:
    val result = commonBench
      .addSimplePolygonToBoundary(V4, SimplePolygon(irregularPentagonAngles.rotateLeft(2)))

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(TilingDCEL.validate(tiling))
        //    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        allAssert(
          verifyValidTiling(tiling),
          tiling.vertices should have size 11,
          tiling.innerFaces should have size 7
        )
      }
    )

  it should "add an irregular pentagon with shared edges to a different edge" in:
    val result = commonBench
      .addSimplePolygonToBoundary(VertexId("V10"), SimplePolygon(irregularPentagonAngles.rotateLeft(1)))
    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(TilingDCEL.validate(tiling))
        //    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        allAssert(
          verifyValidTiling(tiling),
          tiling.vertices should have size 11,
          tiling.innerFaces should have size 7
        )
      }
    )

  it should "add an irregular pentagon with shared edges to a third different edge" in:
    val result = commonBench
      .addSimplePolygonToBoundary(V3, SimplePolygon(irregularPentagonAngles.rotateLeft(3)))

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(TilingDCEL.validate(tiling))
        //    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        allAssert(
          verifyValidTiling(tiling),
          tiling.vertices should have size 11,
          tiling.innerFaces should have size 7
        )
      }
    )

  it should "add a triangle to a square, producing a valid DCEL" in:
    val result = square.addRegularPolygonToBoundary(V1, RegularPolygon(3))

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(tiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
        allAssert(
          verifyValidTiling(tiling),
          tiling.vertices should have size 5,
          tiling.innerFaces should have size 2
        )
      }
    )

  it should "add a hexagon to a triangle, producing a valid DCEL" in:
    val result = triangle.addRegularPolygonToBoundary(V1, RegularPolygon(6))

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(tiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
        allAssert(
          verifyValidTiling(tiling),
          tiling.vertices should have size 7,
          tiling.innerFaces should have size 2
        )
      }
    )

  // Sequential addition tests
  it should "successfully add multiple polygons in sequence" in:
    val step1 = triangle.addRegularPolygonToBoundary(V1, RegularPolygon(3))
    allAssert(
      step1.isRight shouldBe true,
      verifyValidTiling(step1.value), {

        val step2 = step1.value.addRegularPolygonToBoundary(V2, RegularPolygon(3))
        allAssert(
          step2.isRight shouldBe true,
          verifyValidTiling(step2.value), {
            val finalTiling = step2.value
            //    println(finalTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
            allAssert(
              finalTiling.vertices should have size 5,
              finalTiling.innerFaces should have size 3
            )
          }
        )
      }
    )

  it should "maintain correct boundary traversal after multiple additions" in:
    val withTriangle = square.addRegularPolygonToBoundary(V1, RegularPolygon(3)).value
    val withPentagon = withTriangle.addRegularPolygonToBoundary(V2, RegularPolygon(5)).value

    allAssert(
      verifyValidTiling(withPentagon),
      //    println(withPentagon.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
      {
        // Check that the boundary is still traversable
        val boundary = withPentagon.boundaryVertices
        boundary should not be empty
      }, {
        // Verify boundary forms a closed loop
        val boundaryEdges = withPentagon.boundaryEdgesSafer.value
        val assertions    =
          boundaryEdges.map: edge =>
            allAssert(
              edge.next should be(defined),
              edge.prev should be(defined)
            )
        allAssert(assertions*)
      }
    )

  // Large polygon tests
  it should "handle large polygons correctly" in:
    val result = triangle.addRegularPolygonToBoundary(V1, RegularPolygon(12)) // Dodecagon

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        allAssert(
          verifyValidTiling(tiling),
          tiling.vertices should have size 13,
          tiling.innerFaces should have size 2
        )
      }
    )

  // Angle calculation tests
  it should "correctly calculate boundary angles for shared vertices" in:
    val result = square.addRegularPolygonToBoundary(V1, RegularPolygon(4))

    allAssert(
      result.isRight shouldBe true, {
        val tiling         = result.value
        // V0 is shared between two squares, so the boundary angle should be 360 - 90 - 90 = 180
        val v0BoundaryEdge = tiling.boundaryEdgesSafer.value.find(_.origin.id == V1).get
        v0BoundaryEdge.angle.get shouldBe AngleDegree(180)
      }
    )

  it should "correctly handle vertices with multiple incident faces" in:
    val withTriangle2 = triangle.addRegularPolygonToBoundary(V1, RegularPolygon(3)).value
    val withTriangle3 = withTriangle2.addRegularPolygonToBoundary(V1, RegularPolygon(3)).value

    allAssert(
      verifyValidTiling(withTriangle3), {
        // V0 now has 3 triangles, so the boundary angle should be 360 - 3*60 = 180
        val v0BoundaryEdge = withTriangle3.boundaryEdgesSafer.value.find(_.origin.id == V1).get
        v0BoundaryEdge.angle.get shouldBe AngleDegree(180)
      }
    )

  it should "fail to add a polygon on a non-existent vertex" in:
    val result = square.addRegularPolygonToBoundary(VertexId("V99"), RegularPolygon(3))
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("not found on the boundary")
    )

  it should "fail to add a polygon on an empty vertex ID" in:
    val result = triangle.addRegularPolygonToBoundary(VertexId(""), RegularPolygon(3))
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("not found on the boundary")
    )

  // Boundary integrity tests
  it should "maintain boundary connectivity after addition" in:
    val pentagon = TilingBuilder.createRegularPolygon(RegularPolygon(5))
    val result   = pentagon.addRegularPolygonToBoundary(V2, RegularPolygon(3))

    allAssert(
      result.isRight shouldBe true, {
        val tiling        = result.value
        // Check that boundary is still a single connected component
        val boundaryEdges = tiling.boundaryEdgesSafer.value
        allAssert(
          boundaryEdges should not be empty, {
            val assertions =
              boundaryEdges.map: edge =>
                allAssert(
                  edge.next should be(defined),
                  edge.prev should be(defined),
                  edge.next.get.prev should contain(edge),
                  edge.prev.get.next should contain(edge)
                )
            // Verify each edge has proper next/prev links
            allAssert(assertions*)
          }
        )
      }
    )

  it should "preserve vertex IDs and not create duplicates" in:
    val originalVertexIds = hexagon.vertices.map(_.id).toSet

    val result = hexagon.addRegularPolygonToBoundary(V3, RegularPolygon(4))
    allAssert(
      result.isRight shouldBe true, {
        val tiling       = result.value
        val newVertexIds = tiling.vertices.map(_.id).toSet
        allAssert(
          // Original vertices should still exist
          originalVertexIds.subsetOf(newVertexIds) shouldBe true,
          // No duplicate IDs
          tiling.vertices.map(_.id).distinct should have size tiling.vertices.size
        )
      }
    )

  // Face integrity tests
  it should "correctly assign face IDs to new faces" in:
    val result = triangle.addRegularPolygonToBoundary(V1, RegularPolygon(4))

    allAssert(
      result.isRight shouldBe true, {
        val tiling  = result.value
        val faceIds = tiling.innerFaces.map(_.id).toSet
        allAssert(
          faceIds should contain(F1), // Original face
          faceIds should contain(F2), // New face
          faceIds should have size 2
        )
      }
    )

  it should "ensure all half-edges have incident faces assigned" in:
    val result = square.addRegularPolygonToBoundary(V1, RegularPolygon(6))

    allAssert(
      result.isRight shouldBe true, {
        val tiling     = result.value
        val assertions =
          tiling.halfEdges.map: edge =>
            edge.incidentFace should be(defined)
        allAssert(assertions*)
      }
    )

  // Stress tests
  it should "handle adding many small polygons" in:
    var currentTiling = hexagon

    // Add triangles to create a flower pattern
    for (i <- 1 to 6)
      val result = currentTiling.addRegularPolygonToBoundary(vertexIdV(i), RegularPolygon(3))
      allAssert(
        result.isRight shouldBe true, {
          currentTiling = result.value
          verifyValidTiling(currentTiling)
        }
      )

//    println(currentTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
    currentTiling.innerFaces should have size 7 // 1 hexagon + 6 triangles

  /** Five triangles <img src="file:../../../../../resources/fiveTrianglesInHex.svg"/> */
  def fiveTrianglesInHex: TilingDCEL =
    triangle
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value

  it should "successfully add a triangle with more than one edge shared" in:
    val result = fiveTrianglesInHex
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3))
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        //    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
        //    println(TilingDCEL.validate(newTiling))
        verifyValidTiling(newTiling)
      }
    )

  /** Three hexagons <img src="file:../../../../../resources/threeHexagons.svg"/> */
  def threeHexagons: TilingDCEL =
    hexagon
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(6)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V7"), RegularPolygon(6)).value

  it should "successfully add an hexagon with more than one edge shared on both sides of the edge to build on" in:
    val result = threeHexagons
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(6))
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        //    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
        //    println(TilingDCEL.validate(newTiling))
        verifyValidTiling(newTiling)
      }
    )

  it should "successfully add an hexagon with more than one edge shared on one side of the edge to build on" in:
    val result = threeHexagons
      .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(6))
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        //    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
        //    println(TilingDCEL.validate(newTiling))
        verifyValidTiling(newTiling)
      }
    )

  it should "successfully add an hexagon with more than one edge shared on the other side of the edge to build on" in:
    val result = threeHexagons
      .maybeAddRegularPolygonToBoundary(VertexId("V7"), RegularPolygon(6))
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        //    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
        //    println(TilingDCEL.validate(newTiling))
        verifyValidTiling(newTiling)
      }
    )

  /** Three squares <img src="file:../../../../../resources/threeSquares.svg"/> */
  def threeSquares: TilingDCEL =
    square
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(4)).value
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(4)).value

  it should "successfully add a square with more than one edge shared on one side of the edge to build on" in:
    val result = threeSquares
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(4))
    allAssert(
      result.isRight shouldBe true, {

        val newTiling = result.value
        //    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
        //    println(TilingDCEL.validate(newTiling))
        verifyValidTiling(newTiling)
      }
    )

  it should "successfully add a square with more than one edge shared on the other side of the edge to build on" in:
    val result = threeSquares
      .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(4))
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        //    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
        //    println(TilingDCEL.validate(newTiling))
        verifyValidTiling(newTiling)
      }
    )

  it should "successfully fill a hole created by a shared vertex" in:
    val result = commonBench
      .maybeAddRegularPolygonToBoundary(V3, RegularPolygon(4))
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        //    println(TilingDCEL.validate(newTiling))
        //    println(newTiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        verifyValidTiling(newTiling)
      }
    )

  /** Irregular hole almost joined by side <img
    * src="file:../../../../../resources/irregularHoleAlmostJoinedBySide.svg"/>
    */
  def irregularHoleAlmostJoinedBySide: TilingDCEL =
    hexagon
      .maybeAddRegularPolygonToBoundary(V6, RegularPolygon(6)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V7"), RegularPolygon(6)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V11"), RegularPolygon(6)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V16"), RegularPolygon(6)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V19"), RegularPolygon(6)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V23"), RegularPolygon(6)).value

  /** Irregular hole almost joined by vertex <img
    * src="file:../../../../../resources/irregularHoleAlmostJoinedByVertex.svg"/>
    */
  def irregularHoleAlmostJoinedByVertex: TilingDCEL =
    irregularHoleAlmostJoinedBySide
      .maybeAddRegularPolygonToBoundary(VertexId("V27"), RegularPolygon(3)).value

  it should "successfully fill another hole created by a shared vertex" in:
    val result = irregularHoleAlmostJoinedByVertex
      .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(3))
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        //    println(newTiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        //    println(TilingDCEL.validate(newTiling))
        verifyValidTiling(newTiling)
      }
    )

  /** Regular hole almost joined <img src="file:../../../../../resources/regularHoleAlmostJoinedBySide.svg"/>
    */
  def regularHoleAlmostJoinedBySide: TilingDCEL =
    commonBench
      .maybeAddRegularPolygonToBoundary(VertexId("V9"), RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V11"), RegularPolygon(3)).value

  it should "successfully fill a hole created by a shared edge" in:
    val result = regularHoleAlmostJoinedBySide
      .maybeAddRegularPolygonToBoundary(V3, RegularPolygon(4))
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        //    println(newTiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        //    println(TilingDCEL.validate(newTiling))
        verifyValidTiling(newTiling)
      }
    )

  it should "successfully fill another hole created by a shared edge" in:
    val result = irregularHoleAlmostJoinedBySide
      .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(6))

    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        //    println(newTiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        //    println(TilingDCEL.validate(newTiling))
        verifyValidTiling(newTiling)
      }
    )

  /** Regular holes almost joined by side <img
    * src="file:../../../../../resources/regularHolesAlmostJoinedBySide.svg"/>
    */
  def regularHolesAlmostJoinedBySide: TilingDCEL =
    hexagon
      .maybeAddRegularPolygonToBoundary(V6, RegularPolygon(6)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V7"), RegularPolygon(6)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V11"), RegularPolygon(6)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V15"), RegularPolygon(6)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V21"), RegularPolygon(6)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V23"), RegularPolygon(6)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V27"), RegularPolygon(6)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V31"), RegularPolygon(6)).value

  it should "successfully fill two holes created by shared edges" in:
    val result = regularHolesAlmostJoinedBySide
      .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(6))

    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        //    println(newTiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        //    println(TilingDCEL.validate(newTiling))
        verifyValidTiling(newTiling)
      }
    )

  it should "fail if boundary crossing" in:
    val result = regularHoleAlmostJoinedBySide
      .maybeAddRegularPolygonToBoundary(V3, RegularPolygon(5))
    result.isLeft shouldBe true

  /** Irregular shape <img src="file:../../../../../resources/irregularShape.svg"/> */
  def irregularShape: TilingDCEL =
    TilingBuilder.createSimplePolygon(90, 180, 180, 90, 150, 60, 240, 330, 90, 90, 150, 150).value

  it should "fail if just one vertex is added but crosses the boundary" in:
    val result = irregularShape
      .addRegularPolygonToBoundary(VertexId("V7"), RegularPolygon(3))
    result.isLeft shouldBe true

  behavior of "TilingBuilder.addRegularPolygon"

  it should "act the same of addRegularPolygonToBoundary when applied to the boundary directed edge" in:
    val result = triangle.addRegularPolygon(V1, V3, RegularPolygon(4))
    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        allAssert(
          verifyValidTiling(tiling), {
            val tiling2 = triangle.addRegularPolygonToBoundary(V1, RegularPolygon(4)).value
            tiling.isEquivalentTo(tiling2) shouldBe true
          }
        )
      }
    )

  it should "fail for widening" in:
    val result = dodecagon.addRegularPolygon(V1, V2, RegularPolygon(13))

    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("wider than container")
    )

  it should "fail for being the same as the container" in:
    val result = dodecagon.addRegularPolygon(V1, V2, RegularPolygon(12))

    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Same as container")
    )

  /** Double square <img src="file:../../../../../resources/doubleSquare.svg"/> */
  def doubleSquare: TilingDCEL =
    TilingBuilder.createSimplePolygon(90, 180, 90, 180, 90, 180, 90, 180).value

  it should "add an inner regular polygon sharing a second edge" in:
    val result = doubleSquare
      .addRegularPolygon(V1, V2, RegularPolygon(4))

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(TilingDCEL.validate(tiling))
        //    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        verifyValidTiling(tiling)
      }
    )

  /** Concentric squares <img src="file:../../../../../resources/concentricSquares.svg"/> */
  def concentricSquares: TilingDCEL =
    doubleSquare
      .addRegularPolygon(V1, V2, RegularPolygon(4)).value

  it should "add an inner regular polygon sharing more edges" in:
    val result = concentricSquares
      .addRegularPolygon(V2, V3, RegularPolygon(4))

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(TilingDCEL.validate(tiling))
        //    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        verifyValidTiling(tiling)
      }
    )

  /** <img src="file:../../../../../resources/parallelogram.svg"/> */
  def parallelogram: TilingDCEL =
    TilingBuilder.createSimplePolygon(30, 180, 150, 30, 180, 150).value

  it should "fail to add an inner regular polygon crossing the boundary" in:
    val result = parallelogram
      .addRegularPolygon(V2, V3, RegularPolygon(3))
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Boundary intersection")
    )

  /** Parallelogram plus triangle <img src="file:../../../../../resources/parallelogramPlusTriangle.svg"/> */
  def parallelogramPlusTriangle: TilingDCEL =
    parallelogram.addRegularPolygonToBoundary(V6, RegularPolygon(3)).value

  it should "fail to add an inner regular polygon crossing the polygon boundary" in:
    val result = parallelogramPlusTriangle
      .addRegularPolygon(V2, V3, RegularPolygon(3))
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Boundary intersection")
    )

  def base: TilingDCEL =
    val angles =
      List(120, 180, 180, 180, 120, 120, 180, 120, 60, 180, 300, 180, 180, 300, 180, 60, 120, 180, 120)
    TilingBuilder.createSimplePolygon(angles*).value

  /** Another almost joined by vertex <img
    * src="file:../../../../../resources/anotherAlmostJoinedByVertex.svg"/>
    */
  def anotherAlmostJoinedByVertex: TilingDCEL =
    base.addRegularPolygonToBoundary(VertexId("V16"), RegularPolygon(3)).value

  it should "add a special boundary regular polygon creating a hole" in:
    val result = anotherAlmostJoinedByVertex
      .addRegularPolygonToBoundary(VertexId("V16"), RegularPolygon(3))

    result.isRight shouldBe true

  /** Specular version <img src="file:../../../../../resources/anotherAlmostJoinedByVertexSpecular.svg"/> */
  def anotherAlmostJoinedByVertexSpecular: TilingDCEL =
    base.addRegularPolygonToBoundary(VertexId("V10"), RegularPolygon(3)).value

  it should "add another specular boundary regular polygon creating a hole" in:
    val result = anotherAlmostJoinedByVertexSpecular
      .addRegularPolygonToBoundary(VertexId("V20"), RegularPolygon(3))

    result.isRight shouldBe true

  /** Dodecagon with inner square <img src="file:../../../../../resources/dodecagonWithInnerSquare.svg"/> */
  def dodecagonWithInnerSquare: TilingDCEL =
    dodecagon.addRegularPolygon(V1, V2, RegularPolygon(4)).value

  it should "add an inner regular polygon creating a hole" in:
    val result = dodecagonWithInnerSquare
      .addRegularPolygon(V3, V4, RegularPolygon(4))

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(TilingDCEL.validate(tiling))
        //    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        verifyValidTiling(tiling)
      }
    )

  it should "add an inner regular polygon creating a specular hole" in:
    val result = dodecagonWithInnerSquare
      .addRegularPolygon(VertexId("V11"), VertexId("V12"), RegularPolygon(4))

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(TilingDCEL.validate(tiling))
        //    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
        verifyValidTiling(tiling)
      }
    )

  /** Vertex xing <img src="file:../../../../../resources/vertexCrossing.svg"/> */
  def vertexCrossing: TilingDCEL =
    TilingBuilder.createTriangleNet(4, 4)
      .deleteVertex(VertexId("V13")).value
      .deleteFace(FaceId("F16")).value
      .deleteFace(FaceId("F15")).value
      .deleteFace(FaceId("F19")).value

  it should "fail to add a polygon that crosses the boundary at vertices" in:
    val result = vertexCrossing
      .addRegularPolygonToBoundary(VertexId("V9"), RegularPolygon(6))
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Angle wider than container")
    )

  /** Vertex xing simplified <img src="file:../../../../../resources/vertexCrossingSimplified.svg"/> */
  def vertexCrossingSimplified: TilingDCEL =
    vertexCrossing
      .deleteVertex(VertexId("V19")).value

  it should "fail to add a polygon that crosses the boundary simplified" in:
    val result = vertexCrossingSimplified
      .addRegularPolygonToBoundary(VertexId("V9"), RegularPolygon(6))
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Angle wider than container")
    )

  /** Three decagons <img src="file:../../../../../resources/threeDecagons.svg"/> */
  def threeDecagons: TilingDCEL =
    TilingBuilder.createRegularPolygon(RegularPolygon(10))
      .addRegularPolygonToBoundary(V1, RegularPolygon(10)).value
      .addRegularPolygonToBoundary(VertexId("V12"), RegularPolygon(10)).value

  val attachingSimplePolygon: SimplePolygon =
    SimplePolygon(Vector(
      144, 144, 144, 144, 144, 144, 144, 144, 24, 240, 240, 240, 240, 24
    ).map(AngleDegree(_)).rotateLeft(7))

  /** Attaching shape <img src="file:../../../../../resources/attaching.svg"/> */
  def attaching: TilingDCEL =
    TilingBuilder.createSimplePolygon(attachingSimplePolygon).value

  it should "add an irregular polygon forming another irregular polygon" in:

    /** Attached shape <img src="file:../../../../../resources/attached.svg"/> */
    val attached = threeDecagons
      .maybeAddSimplePolygonToBoundary(VertexId("V3"), attachingSimplePolygon)
    attached.isLeft shouldBe false

  behavior of "TilingBuilder.maybeAddRegularPolygonToBoundary"

  /** Tiling with three dodecagons <img src="file:../../../../../resources/threeDodecagons.svg"/>
    */
  def threeDodecagons: TilingDCEL =
    TilingBuilder.createRegularPolygon(RegularPolygon(12))
      .maybeAddRegularPolygonToBoundary(V3, RegularPolygon(12)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V11"), RegularPolygon(12)).value

  it should "laterally add a fourth dodecagon creating two holes" in:
    val result = threeDodecagons.maybeAddRegularPolygonToBoundary(VertexId("V21"), RegularPolygon(12))
    result.value.innerFaces.size shouldBe 6

  it should "add from the other side a fourth dodecagon creating two holes" in:
    val result = threeDodecagons.maybeAddRegularPolygonToBoundary(VertexId("V23"), RegularPolygon(12))
    result.value.innerFaces.size shouldBe 6

  it should "add a fourth dodecagon creating two holes" in:
    val result = threeDodecagons.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(12))
    println(result)
    result.isRight shouldBe true

  it should "add a fourth dodecagon creating one hole having previously filled one" in:
    val result =
      threeDodecagons
        .maybeAddRegularPolygonToBoundary(VertexId("V11"), RegularPolygon(3)).value
        .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(12))
    result.value.innerFaces.size shouldBe 6

  it should "add a fourth dodecagon creating one hole having previously filled the other" in:
    val result =
      threeDodecagons
        .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(3)).value
        .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(12))
    result.value.innerFaces.size shouldBe 6

  /** Tiling with two pots <img src="file:../../../../../resources/twoPots.svg"/>
    */
  def twoPots: TilingDCEL =
    TilingBuilder
      .createSimplePolygon(
        90, 180, 180, 180, 180, 90, 180, 90, 90, 270, 270, 90, 90, 270, 270, 90, 90, 180
      ).value

  def rectangularTwoLid: SimplePolygon =
    SimplePolygon(180, 180, 180, 90, 90, 180, 180, 180, 180, 90, 90, 180)

  it should "have the lid covering two square holes when attached to the middle" in:
    val result = twoPots.maybeAddSimplePolygonToBoundary(VertexId("V13"), rectangularTwoLid)
    println(result)
    result.isRight shouldBe true

  it should "have the lid covering two square holes when attached on the left" in:
    val result =
      twoPots.maybeAddSimplePolygonToBoundary(
        VertexId("V17"),
        SimplePolygon(rectangularTwoLid.toAngles.rotateRight(2))
      )
    result.value.innerFaces.size shouldBe 4

  it should "have the lid covering two square holes when attached on the right" in:
    val result =
      twoPots.maybeAddSimplePolygonToBoundary(
        VertexId("V9"),
        SimplePolygon(rectangularTwoLid.toAngles.rotateLeft(2))
      )
    result.value.innerFaces.size shouldBe 4

  it should "have the lid covering one remaining square hole" in:
    val result = twoPots
      .maybeAddRegularPolygonToBoundary(VertexId("V16"), RegularPolygon(4)).value
      .maybeAddSimplePolygonToBoundary(VertexId("V13"), rectangularTwoLid)
    result.value.innerFaces.size shouldBe 4

  it should "have the lid covering the other remaining square hole" in:
    val result = twoPots
      .maybeAddRegularPolygonToBoundary(VertexId("V12"), RegularPolygon(4)).value
      .maybeAddSimplePolygonToBoundary(VertexId("V13"), rectangularTwoLid)
    result.value.innerFaces.size shouldBe 4

  /** Tiling with three pots <img src="file:../../../../../resources/threePots.svg"/>
    */
  def threePots: TilingDCEL =
    TilingBuilder
      .createSimplePolygon(
        90, 180, 180, 180, 180, 180, 180, 90, 180, 90, 90, 270, 270, 90, 90, 270, 270, 90, 90, 270, 270, 90,
        90, 180
      ).value

  def rectangularThreeLid: SimplePolygon =
    SimplePolygon(90, 180, 180, 180, 180, 180, 180, 90, 90, 180, 180, 180, 180, 180, 180, 90)

  it should "have the lid covering three square holes when attached on the far left" in:
    val result =
      threePots.maybeAddSimplePolygonToBoundary(
        VertexId("V23"),
        rectangularThreeLid
      )
    result.value.innerFaces.size shouldBe 5

  it should "have the lid covering three square holes when attached on the second from left" in:
    val result =
      threePots.maybeAddSimplePolygonToBoundary(
        VertexId("V19"),
        SimplePolygon(rectangularThreeLid.toAngles.rotateLeft(2))
      )
    println(result)
    result.value.innerFaces.size shouldBe 5
