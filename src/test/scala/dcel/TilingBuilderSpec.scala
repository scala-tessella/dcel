package io.github.scala_tessella
package dcel

import BigDecimalGeometry.AngleDegree

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingBuilderSpec extends AnyFlatSpec with Matchers with EitherValues:

  behavior of "TilingBuilder.empty"

  it should "create empty tiling" in {
    val empty = TilingBuilder.empty
    empty.vertices shouldBe List.empty
    empty.halfEdges shouldBe List.empty
    empty.innerFaces shouldBe List.empty
    empty.outerFace.id shouldBe Face.outerId
  }

  behavior of "TilingBuilder.createSimplePolygon"

  // --- Success cases ---

  it should "create a valid TilingDCEL for a regular triangle" in {
    val triangleAngles = List.fill(3)(AngleDegree(60))
    val result = TilingBuilder.createSimplePolygon(triangleAngles)

    result.isRight shouldBe true
    val tiling = result.value

    tiling.vertices.length shouldBe 3
    tiling.faces.length shouldBe 2 // F1 and F0
    tiling.halfEdges.length shouldBe 6 // 3 inner, 3 outer
  }

  it should "create a valid TilingDCEL for a square" in {
    val squareAngles = List.fill(4)(AngleDegree(90))
    val result = TilingBuilder.createSimplePolygon(squareAngles)

    result.isRight shouldBe true
    val tiling = result.value

    tiling.vertices.length shouldBe 4
    tiling.faces.length shouldBe 2
    tiling.halfEdges.length shouldBe 8
  }

  it should "create a valid TilingDCEL for a regular hexagon" in {
    val hexagonAngles = List.fill(6)(AngleDegree(120))
    val result = TilingBuilder.createSimplePolygon(hexagonAngles)

    result.isRight shouldBe true
    val tiling = result.value

    tiling.vertices.length shouldBe 6
    tiling.faces.length shouldBe 2
    tiling.halfEdges.length shouldBe 12
  }

  // --- Failure cases ---

  it should "fail to create a polygon with fewer than 3 sides" in {
    val twoAngles = List.fill(2)(AngleDegree(90))
    val result = TilingBuilder.createSimplePolygon(twoAngles)

    result.isLeft shouldBe true
    result.left.value should include ("at least 3 sides")
  }

  it should "fail if an interior angle is a full circle" in {
    // The sum is correct, but the check for full circle angles comes first.
    val anglesWithFullCircle = List(AngleDegree(180), AngleDegree(0), AngleDegree(0), AngleDegree(180))
    val result = TilingBuilder.createSimplePolygon(anglesWithFullCircle)

    result.isLeft shouldBe true
    result.left.value shouldBe "The polygon cannot have full circles as interior angles."
  }

  it should "fail if the sum of interior angles is incorrect" in {
    val wrongSumAngles = List(AngleDegree(60), AngleDegree(60), AngleDegree(70)) // Sum is 190, should be 180 for a triangle
    val result = TilingBuilder.createSimplePolygon(wrongSumAngles)

    result.isLeft shouldBe true
    result.left.value should include ("The sum of interior angles is incorrect")
  }

  it should "fail if the polygon is self-intersecting" in {
    // A crossed hexagon ("bow-tie" style)
    // The sum of angles is (6-2)*180=720, but the geometry crosses itself.
    // Note: This polygon fails the final angle validation, but the self-intersection
    // check should catch it first.
    val intersectingAngles = List(AngleDegree(150), AngleDegree(150), AngleDegree(30), AngleDegree(150), AngleDegree(150), AngleDegree(90))
    val result = TilingBuilder.createSimplePolygon(intersectingAngles)

    result.isLeft shouldBe true
    result.left.value should include ("The polygon is not simple (it has vertices that are equal, which is not allowed)")
  }

  it should "fail if the polygon does not close geometrically, even with correct angle sum" in {
    // These pentagon angles sum to 540 degrees, which is correct for a pentagon ((5-2)*180),
    // but the sequence of angles does not form a closed polygon with unit-length sides.
    val nonClosingAngles = List(AngleDegree(90), AngleDegree(90), AngleDegree(135), AngleDegree(135), AngleDegree(90))
    val result = TilingBuilder.createSimplePolygon(nonClosingAngles)

    result.isLeft shouldBe true
    result.left.value should include ("The polygon does not close")
  }

  it should "fail if the angles are geometrically inconsistent" in {
    // These angles sum to 350, but a 4-sided polygon's angles must sum to 360.
    val inconsistentAngles = List(AngleDegree(90), AngleDegree(90), AngleDegree(90), AngleDegree(80))
    val result = TilingBuilder.createSimplePolygon(inconsistentAngles)

    result.isLeft shouldBe true
    result.left.value should include ("The sum of interior angles is incorrect")
  }

  it should "fail for a four-sided polygon with 60-degree angles" in {
    // These angles sum to 240, but a 4-sided polygon's angles must sum to 360.
    val invalidAngles = List.fill(4)(AngleDegree(60))
    val result = TilingBuilder.createSimplePolygon(invalidAngles)

    result.isLeft shouldBe true
    result.left.value should include ("The sum of interior angles is incorrect")
  }

  behavior of "TilingBuilder.createRegularPolygon"

  it should "create a valid TilingDCEL for a regular triangle" in {
    val result = TilingBuilder.createRegularPolygon(3)
    result.isRight shouldBe true
    val tiling = result.value
    tiling.vertices.length shouldBe 3
    tiling.faces.length shouldBe 2
    tiling.halfEdges.length shouldBe 6
  }

  it should "create a valid TilingDCEL for a square" in {
    val result = TilingBuilder.createRegularPolygon(4)
    result.isRight shouldBe true
    val tiling = result.value
    tiling.vertices.length shouldBe 4
    tiling.vertices.map(_.id).mkString(", ") shouldBe "V0, V1, V2, V3"
    tiling.faces.length shouldBe 2
    tiling.faces.map(_.id).mkString(", ") shouldBe "F0, F1"
    tiling.halfEdges.length shouldBe 8
    tiling.outerFace.halfEdgesSafe.map(_.angle.get).mkString(", ") shouldBe "270, 270, 270, 270"
    tiling.outerFace.halfEdgesSafe.map(_.incidentFace.get.id).mkString(", ") shouldBe "F0, F0, F0, F0"
    tiling.innerFaces.map(_.halfEdgesSafe.map(_.angle.get).mkString(", ")) shouldBe List("90, 90, 90, 90")
    tiling.innerFaces.map(_.halfEdgesSafe.map(_.incidentFace.get.id).mkString(", ")) shouldBe List("F1, F1, F1, F1")
  }

  it should "create a valid TilingDCEL for a regular pentagon" in {
    val result = TilingBuilder.createRegularPolygon(5)
    result.isRight shouldBe true
    val tiling = result.value
    tiling.vertices.length shouldBe 5
    tiling.faces.length shouldBe 2
    tiling.halfEdges.length shouldBe 10
    tiling.outerFace.halfEdgesSafe.map(_.angle.get).mkString(", ") shouldBe "252, 252, 252, 252, 252"
    tiling.outerFace.halfEdgesSafe.map(_.incidentFace.get.id).mkString(", ") shouldBe "F0, F0, F0, F0, F0"
    tiling.innerFaces.map(_.halfEdgesSafe.map(_.angle.get).mkString(", ")) shouldBe List("108, 108, 108, 108, 108")
    tiling.innerFaces.map(_.halfEdgesSafe.map(_.incidentFace.get.id).mkString(", ")) shouldBe List("F1, F1, F1, F1, F1")
  }

  it should "fail to create a polygon with fewer than 3 sides" in {
    val result = TilingBuilder.createRegularPolygon(2)
    result.isLeft shouldBe true
    result.left.value should include ("at least 3 sides")
  }