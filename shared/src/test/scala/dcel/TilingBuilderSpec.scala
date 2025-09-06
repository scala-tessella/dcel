package dcel

import dcel.BigDecimalGeometry.AngleDegree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingBuilderSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingBuilder.createSimplePolygon"

  // --- Success cases ---

  it should "create a valid TilingDCEL for a regular triangle" in {
    val triangleAngles = List.fill(3)(AngleDegree(60))
    val result         = TilingBuilder.createSimplePolygon(triangleAngles)

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        allAssert(
          tiling.vertices.length shouldBe 3,
          tiling.faces.length shouldBe 2,    // F1 and F0
          tiling.halfEdges.length shouldBe 6 // 3 inner, 3 outer
        )
      }
    )
  }

  it should "create a valid TilingDCEL for a square" in {
    val squareAngles = List.fill(4)(AngleDegree(90))
    val result       = TilingBuilder.createSimplePolygon(squareAngles)

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        allAssert(
          tiling.vertices.length shouldBe 4,
          tiling.faces.length shouldBe 2,
          tiling.halfEdges.length shouldBe 8
        )
      }
    )
  }

  it should "create a valid TilingDCEL for a regular hexagon" in {
    val hexagonAngles = List.fill(6)(AngleDegree(120))
    val result        = TilingBuilder.createSimplePolygon(hexagonAngles)

    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        allAssert(
          tiling.vertices.length shouldBe 6,
          tiling.faces.length shouldBe 2,
          tiling.halfEdges.length shouldBe 12
        )
      }
    )
  }

  // --- Failure cases ---

  it should "fail to create a polygon with fewer than 3 sides" in {
    val twoAngles = List.fill(2)(AngleDegree(90))
    val result    = TilingBuilder.createSimplePolygon(twoAngles)

    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("at least 3 sides")
    )
  }

  it should "fail if an interior angle is a full circle" in {
    // The sum is correct, but the check for full circle angles comes first.
    val anglesWithFullCircle = List(AngleDegree(180), AngleDegree(0), AngleDegree(0), AngleDegree(180))
    val result               = TilingBuilder.createSimplePolygon(anglesWithFullCircle)

    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message shouldBe "The polygon cannot have full circles as interior angles."
    )
  }

  it should "fail if the sum of interior angles is incorrect" in {
    val wrongSumAngles =
      List(AngleDegree(60), AngleDegree(60), AngleDegree(70)) // Sum is 190, should be 180 for a triangle
    val result         = TilingBuilder.createSimplePolygon(wrongSumAngles)

    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("The sum of interior angles is incorrect")
    )
  }

  it should "fail if the polygon is self-intersecting" in {
    // A crossed hexagon ("bow-tie" style)
    // The sum of angles is (6-2)*180=720, but the geometry crosses itself.
    // Note: This polygon fails the final angle validation, but the self-intersection
    // check should catch it first.
    val intersectingAngles = List(
      AngleDegree(150),
      AngleDegree(150),
      AngleDegree(30),
      AngleDegree(150),
      AngleDegree(150),
      AngleDegree(90)
    )
    val result             = TilingBuilder.createSimplePolygon(intersectingAngles)

    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include(
        "The polygon is not simple (it has vertices that are equal, which is not allowed)"
      )
    )
  }

  it should "fail if the polygon does not close geometrically, even with correct angle sum" in {
    // These pentagon angles sum to 540 degrees, which is correct for a pentagon ((5-2)*180),
    // but the sequence of angles does not form a closed polygon with unit-length sides.
    val nonClosingAngles =
      List(AngleDegree(90), AngleDegree(90), AngleDegree(135), AngleDegree(135), AngleDegree(90))
    val result           = TilingBuilder.createSimplePolygon(nonClosingAngles)

    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("The polygon does not close")
    )
  }

  it should "fail if the angles are geometrically inconsistent" in {
    // These angles sum to 350, but a 4-sided polygon's angles must sum to 360.
    val inconsistentAngles = List(AngleDegree(90), AngleDegree(90), AngleDegree(90), AngleDegree(80))
    val result             = TilingBuilder.createSimplePolygon(inconsistentAngles)

    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("The sum of interior angles is incorrect")
    )
  }

  it should "fail for a four-sided polygon with 60-degree angles" in {
    // These angles sum to 240, but a 4-sided polygon's angles must sum to 360.
    val invalidAngles = List.fill(4)(AngleDegree(60))
    val result        = TilingBuilder.createSimplePolygon(invalidAngles)

    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("The sum of interior angles is incorrect")
    )
  }

  behavior of "TilingBuilder.createRegularPolygon"

  it should "create a valid TilingDCEL for a regular triangle" in {
    val result = TilingBuilder.createRegularPolygon(3)
    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        allAssert(
          tiling.vertices.length shouldBe 3,
          tiling.faces.length shouldBe 2,
          tiling.halfEdges.length shouldBe 6
        )
      }
    )
  }

  it should "create a valid TilingDCEL for a square" in {
    val result = TilingBuilder.createRegularPolygon(4)
    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        allAssert(
          tiling.vertices.length shouldBe 4,
          tiling.vertices.map(_.id).mkString(", ") shouldBe "V1, V2, V3, V4",
          tiling.faces.length shouldBe 2,
          tiling.faces.map(_.id).mkString(", ") shouldBe "F0, F1",
          tiling.halfEdges.length shouldBe 8,
          tiling.outerFace.halfEdgesUnsafe.map(_.angle.get).mkString(", ") shouldBe "270, 270, 270, 270",
          tiling.outerFace.halfEdgesUnsafe.map(
            _.incidentFace.get.id
          ).mkString(", ") shouldBe "F0, F0, F0, F0",
          tiling.innerFaces.map(_.halfEdgesUnsafe.map(_.angle.get).mkString(", ")) shouldBe List(
            "90, 90, 90, 90"
          ),
          tiling.innerFaces.map(_.halfEdgesUnsafe.map(_.incidentFace.get.id).mkString(", ")) shouldBe List(
            "F1, F1, F1, F1"
          )
        )
      }
    )
  }

  it should "create a valid TilingDCEL for a regular pentagon" in {
    val result = TilingBuilder.createRegularPolygon(5)
    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        allAssert(
          tiling.vertices.length shouldBe 5,
          tiling.faces.length shouldBe 2,
          tiling.halfEdges.length shouldBe 10,
          tiling.outerFace.halfEdgesUnsafe.map(_.angle.get).mkString(", ") shouldBe "252, 252, 252, 252, 252",
          tiling.outerFace.halfEdgesUnsafe.map(
            _.incidentFace.get.id
          ).mkString(", ") shouldBe "F0, F0, F0, F0, F0",
          tiling.innerFaces.map(_.halfEdgesUnsafe.map(_.angle.get).mkString(", ")) shouldBe List(
            "108, 108, 108, 108, 108"
          ),
          tiling.innerFaces.map(_.halfEdgesUnsafe.map(_.incidentFace.get.id).mkString(", ")) shouldBe List(
            "F1, F1, F1, F1, F1"
          )
        )
      }
    )
  }

  it should "fail to create a polygon with fewer than 3 sides" in {
    val result = TilingBuilder.createRegularPolygon(2)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("at least 3 sides")
    )
  }

  behavior of "TilingBuilder.createRhombusNet"

  /** <img src="file:../../resources/rhombusNet.svg"/> */
  def rhombusNet: TilingDCEL =
    TilingBuilder.createRhombusNet(3, 3, AngleDegree(60))

  it should "create a valid TilingDCEL with a net of rhombi" in {
    TilingDCEL.validate(rhombusNet).isRight shouldBe true
  }

  behavior of "TilingBuilder.createTriangleNet"

  /** <img src="file:../../resources/triangleNet.svg"/> */
  def triangleNet: TilingDCEL =
    TilingBuilder.createTriangleNet(3, 3)

  it should "create a valid TilingDCEL with a net of regular triangles" in {
    TilingDCEL.validate(triangleNet).isRight shouldBe true
  }

  behavior of "TilingBuilder.createHexagonNet"

  /** <img src="file:../../resources/hexagonNet.svg"/> */
  def hexagonNet: TilingDCEL =
    TilingBuilder.createHexagonNet(3, 3, AngleDegree(90))

  it should "create a valid TilingDCEL with a net of regular hexagons" in {
    TilingDCEL.validate(hexagonNet).isRight shouldBe true
  }
