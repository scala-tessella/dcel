package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon, SimplePolygon}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingBuilderSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingBuilder.createSimplePolygon"

  // --- Success cases ---

  it should "create a valid TilingDCEL for a regular triangle" in {
    val triangleAngles = Vector.fill(3)(AngleDegree(60))
    val result         = TilingBuilder.createSimplePolygon(SimplePolygon(triangleAngles))

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
    val squareAngles = Vector.fill(4)(AngleDegree(90))
    val result       = TilingBuilder.createSimplePolygon(SimplePolygon(squareAngles))

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
    val hexagonAngles = Vector.fill(6)(AngleDegree(120))
    val result        = TilingBuilder.createSimplePolygon(SimplePolygon(hexagonAngles))

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

  it should "fail if the polygon is self-intersecting" in {
    // A crossed hexagon ("bow-tie" style)
    // The sum of angles is (6-2)*180=720, but the geometry crosses itself.
    // Note: This polygon fails the final angle validation, but the self-intersection
    // check should catch it first.
    val intersectingAngles = Vector(
      AngleDegree(150),
      AngleDegree(150),
      AngleDegree(30),
      AngleDegree(150),
      AngleDegree(150),
      AngleDegree(90)
    )
    val result             = TilingBuilder.createSimplePolygon(SimplePolygon(intersectingAngles))

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
      Vector(AngleDegree(90), AngleDegree(90), AngleDegree(135), AngleDegree(135), AngleDegree(90))
    val result           = TilingBuilder.createSimplePolygon(SimplePolygon(nonClosingAngles))

    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("The polygon does not close")
    )
  }

  behavior of "TilingBuilder.createRegularPolygon"

  it should "create a valid TilingDCEL for a regular triangle" in {
    val tiling = triangle
    allAssert(
      tiling.vertices.length shouldBe 3,
      tiling.faces.length shouldBe 2,
      tiling.halfEdges.length shouldBe 6
    )
  }

  it should "create a valid TilingDCEL for a square" in {
    val tiling = square
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

  it should "create a valid TilingDCEL for a regular pentagon" in {
    val tiling = TilingBuilder.createRegularPolygon(RegularPolygon(5))
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

  behavior of "TilingBuilder.createRhombusNet"

  /** <img src="file:../../../../../resources/rhombusNet.svg"/> */
  def rhombusNet: TilingDCEL =
    TilingBuilder.createRhombusNet(3, 3, AngleDegree(60))

  it should "create a valid TilingDCEL with a net of rhombi" in {
    validate(rhombusNet).isRight shouldBe true
  }

  behavior of "TilingBuilder.createTriangleNet"

  /** <img src="file:../../../../../resources/triangleNet.svg"/> */
  def triangleNet: TilingDCEL =
    TilingBuilder.createTriangleNet(3, 3)

  it should "create a valid TilingDCEL with a net of regular triangles" in {
    validate(triangleNet).isRight shouldBe true
  }

  behavior of "TilingBuilder.createHexagonNet"

  /** <img src="file:../../../../../resources/hexagonNet.svg"/> */
  def hexagonNet: TilingDCEL =
    TilingBuilder.createHexagonNet(3, 3, AngleDegree(90))

  it should "create a valid TilingDCEL with a net of regular hexagons" in {
    validate(hexagonNet).isRight shouldBe true
  }

  behavior of "TilingBuilder.createRing"

  it should "create a valid TilingDCEL with a ring of regular triangles" in {
    /** <img src="file:../../../../../resources/ring3.svg"/> */
    val triangleRing: TilingDCEL =
      TilingBuilder.createRing(RegularPolygon(3))
    triangleRing.innerFaces.length shouldBe 6
  }

  it should "create a valid TilingDCEL with a ring of regular pentagons" in {
    /** <img src="file:../../../../../resources/ring5.svg"/> */
    val pentagonRing: TilingDCEL =
      TilingBuilder.createRing(RegularPolygon(5))
    pentagonRing.innerFaces.length shouldBe 11
  }

  it should "create a valid TilingDCEL with a ring of regular eptagons" in {
    /** <img src="file:../../../../../resources/ring7.svg"/> */
    val eptagonRing: TilingDCEL =
      TilingBuilder.createRing(RegularPolygon(7))
    eptagonRing.innerFaces.length shouldBe 15
  }
