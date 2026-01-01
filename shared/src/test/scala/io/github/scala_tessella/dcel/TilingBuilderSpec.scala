package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon, SimplePolygon}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingBuilderSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingBuilder.createSimplePolygon"

  // --- Success cases ---

  it should "create a valid TilingDCEL for a regular triangle" in:
    val triangleDegrees = Vector.fill(3)(60)
    val result          = TilingBuilder.createSimplePolygon(triangleDegrees*)

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

  it should "create a valid TilingDCEL for a square" in:
    val squareDegrees = Vector.fill(4)(90)
    val result        = TilingBuilder.createSimplePolygon(squareDegrees*)

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

  it should "create a valid TilingDCEL for a regular hexagon" in:
    val hexagonDegrees = Vector.fill(6)(120)
    val result         = TilingBuilder.createSimplePolygon(hexagonDegrees*)

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

  it should "fail if the polygon is self-intersecting" in:
    // A crossed hexagon ("bow-tie" style)
    // The sum of angles is (6-2)*180=720, but the geometry crosses itself.
    // Note: This polygon fails the final angle validation, but the self-intersection
    // check should catch it first.
    val intersectingDegrees = Vector(150, 150, 30, 150, 150, 90)
    val result              = TilingBuilder.createSimplePolygon(intersectingDegrees*)

    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include(
        "The polygon is self-intersecting"
      )
    )

  it should "fail if the polygon does not close geometrically, even with correct angle sum" in:
    // These pentagon angles sum to 540 degrees, which is correct for a pentagon ((5-2)*180),
    // but the sequence of angles does not form a closed polygon with unit-length sides.
    val nonClosingDegrees =
      Vector(90, 90, 135, 135, 90)
    val result            = TilingBuilder.createSimplePolygon(nonClosingDegrees*)

    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("The polygon does not close")
    )

  behavior of "TilingBuilder.createRegularPolygon"

  it should "create a valid TilingDCEL for a regular triangle" in:
    val tiling = triangle
    allAssert(
      tiling.vertices.length shouldBe 3,
      tiling.faces.length shouldBe 2,
      tiling.halfEdges.length shouldBe 6
    )

  it should "create a valid TilingDCEL for a square" in:
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

  it should "create a valid TilingDCEL for a regular pentagon" in:
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

  behavior of "TilingBuilder.createRhombusNet"

  it should "create a valid TilingDCEL with a net of rhombi" in:

    /** <img src="file:../../../../../resources/rhombusNet.svg"/> */
    val rhombusNet: TilingDCEL =
      TilingBuilder.createRhombusNet(3, 3, AngleDegree(60))
    validate(rhombusNet).isRight shouldBe true

  behavior of "TilingBuilder.createTriangleNet"

  it should "create a valid TilingDCEL with a net of regular triangles" in:

    /** <img src="file:../../../../../resources/triangleNet.svg"/> */
    val triangleNet: TilingDCEL =
      TilingBuilder.createTriangleNet(3, 3)
    validate(triangleNet).isRight shouldBe true

  behavior of "TilingBuilder.createHexagonNet"

  it should "create a valid TilingDCEL with a net of regular hexagons" in:

    /** <img src="file:../../../../../resources/hexagonNet.svg"/> */
    val hexagonNet: TilingDCEL =
      TilingBuilder.createHexagonNet(3, 3, AngleDegree(90))
    validate(hexagonNet).isRight shouldBe true

  behavior of "TilingBuilder.createRing"

  it should "create a valid TilingDCEL with a ring of regular triangles" in:

    /** <img src="file:../../../../../resources/ring3.svg"/> */
    val triangleRing: TilingDCEL =
      TilingBuilder.createRing(RegularPolygon(3))
    triangleRing.innerFaces.length shouldBe 6

  it should "create a valid TilingDCEL with a ring of squares" in:

    /** <img src="file:../../../../../resources/ring4.svg"/> */
    val squareRing: TilingDCEL =
      TilingBuilder.createRing(RegularPolygon(4))
    squareRing.innerFaces.length shouldBe 4

  it should "create a valid TilingDCEL with a ring of regular pentagons" in:

    /** <img src="file:../../../../../resources/ring5.svg"/> */
    val pentagonRing: TilingDCEL =
      TilingBuilder.createRing(RegularPolygon(5))
    pentagonRing.innerFaces.length shouldBe 11

  it should "create a valid TilingDCEL with a ring of regular hexagons" in:

    /** <img src="file:../../../../../resources/ring6.svg"/> */
    val hexagonRing: TilingDCEL =
      TilingBuilder.createRing(RegularPolygon(6))
    hexagonRing.innerFaces.length shouldBe 7

  it should "create a valid TilingDCEL with a ring of regular eptagons" in:

    /** <img src="file:../../../../../resources/ring7.svg"/> */
    val eptagonRing: TilingDCEL =
      TilingBuilder.createRing(RegularPolygon(7))
    eptagonRing.innerFaces.length shouldBe 15

  it should "create a valid TilingDCEL with a ring of regular octagons" in:

    /** <img src="file:../../../../../resources/ring8.svg"/> */
    val octagonRing: TilingDCEL =
      TilingBuilder.createRing(RegularPolygon(8))
    octagonRing.innerFaces.length shouldBe 9

  it should "create a valid TilingDCEL with a ring of regular ennagons" in:

    /** <img src="file:../../../../../resources/ring9.svg"/> */
    val ennagonRing: TilingDCEL =
      TilingBuilder.createRing(RegularPolygon(9))
    ennagonRing.innerFaces.length shouldBe 19

  it should "create a valid TilingDCEL with a ring of regular decagons" in:

    /** <img src="file:../../../../../resources/ring10.svg"/> */
    val decagonRing: TilingDCEL =
      TilingBuilder.createRing(RegularPolygon(10))
    decagonRing.innerFaces.length shouldBe 11

//  /** @note
//    *   from 46 sides onwards this is failing, if ACCURACY at 1.0e-12; from 92 sides onwards, if at 1.0e-11
//    */
//  it should "create a valid TilingDCEL with a ring of regular centagons" in:
//
//    val centagonRing: TilingDCEL =
//      TilingBuilder.createRing(RegularPolygon(100))
//    allAssert(
//      centagonRing.innerFaces.length shouldBe 101,
//      centagonRing.vertices.length shouldBe 9800
//    )

  behavior of "TilingBuilder.createHoledTriangleNet"

  it should "create an hexagon net" in:

    /** <img src="file:../../../../../resources/uniform1_all_hex.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => (i - j) % 3 == 0)
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 1,
      result.innerFaces.size shouldBe 22
    )

  behavior of "TilingBuilder.idFromFaceId"

  it should "return the id" in:
    val faceId = TilingBuilder.faceIdF(100)
    TilingBuilder.idFromFaceId(faceId) shouldBe 100
