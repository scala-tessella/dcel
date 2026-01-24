package io.github.scala_tessella.dcel.geometry

//import io.github.scala_tessella.dcel.conversion.TilingSVG.toParallelogonTiling
import io.github.scala_tessella.dcel.geometry.{AngleDegree, SimplePolygon}
import io.github.scala_tessella.dcel.{TilingBuilder, TilingTestHelpers}
import io.github.scala_tessella.ring_seq.SymmetryOps.{Edge, Vertex}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SimplePolygonSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "SimplePolygon.parallelogonIndices"

  val simpleTriangle: SimplePolygon = SimplePolygon(60, 60, 60)

  they should "NOT be found for a triangle" in:
    simpleTriangle.parallelogonIndices shouldBe Nil

  val simpleSquare: SimplePolygon = SimplePolygon(90, 90, 90, 90)

  they should "be found for a square" in:
    simpleSquare.parallelogonIndices shouldBe List(0, 1, 2, 3)

  they should "NOT be found for a pentagon" in:
    SimplePolygon(108, 108, 108, 108, 108).parallelogonIndices shouldBe Nil

  they should "NOT be found for a triangle of side 2" in:
    val doubledTriangle = simpleTriangle.multiplySidesBy(2)
    doubledTriangle.parallelogonIndices shouldBe Nil

  they should "NOT be found for a triangle of side 4" in:
    val quadrupledTriangle = simpleTriangle.multiplySidesBy(4)
    quadrupledTriangle.parallelogonIndices shouldBe Nil

  they should "be found for a 2x2 square" in:
    val square2x2 = simpleSquare.multiplySidesBy(2)
    square2x2.parallelogonIndices shouldBe List(0, 2, 4, 6)

  they should "be found for a 2x2 square with shifted angles" in:
    val angles =
      Vector.fill(4)(Vector(AngleDegree(180), AngleDegree(90))).flatten
    val simple = SimplePolygon(angles)
    simple.parallelogonIndices shouldBe List(1, 3, 5, 7)

  they should "be found for a 3x3 square" in:
    val square3x3 = simpleSquare.multiplySidesBy(3)
    allAssert(
      square3x3.parallelogonIndices shouldBe List(0, 3, 6, 9),
      square3x3.parallelogonIndexClasses shouldBe
        List(List(0, 3, 6, 9), List(1, 8), List(2, 7), List(4, 11), List(5, 10))
    )

  they should "be found for a rhombus" in:
    val rhombus = SimplePolygon(120, 60, 120, 60)
    rhombus.parallelogonIndices shouldBe List(0, 1, 2, 3)

  they should "be found for a 1x2 rectangle" in:
    val rectangle1x2 = SimplePolygon(90, 90, 180, 90, 90, 180)
    rectangle1x2.parallelogonIndices shouldBe List(0, 1, 3, 4)

  they should "be found for a 2x1 parallelogram" in:

    /** <img src="file:../../../../../../resources/simple/parallelogram2x1.svg"/> */
    val parallelogram2x1 = SimplePolygon(60, 120, 180, 60, 120, 180)
//    println(parallelogram2x1.toParallelogonTiling())
    parallelogram2x1.parallelogonIndices shouldBe List(0, 1, 3, 4)

  they should "be found for a regular pentagon" in:
    SimplePolygon(RegularPolygon(5).angles).parallelogonIndices shouldBe Nil

  they should "be found for an hexagon" in:
    val unit = SimplePolygon(90, 120, 150, 90, 120, 150)
    unit.parallelogonIndices shouldBe List(0, 1, 2, 3, 4, 5)

  /** <img src="file:../../../../../../resources/simple/twoJoinedHexs.svg"/> */
  val twoJoinedHexs: SimplePolygon =
    SimplePolygon(120, 120, 240, 120, 120, 120, 120, 240, 120, 120)

  they should "be found for a 2 joined regular hexagons boundary" in:
//    println(twoJoinedHexs.toParallelogonTiling())
    allAssert(
      twoJoinedHexs.parallelogonIndices shouldBe List(0, 1, 4, 5, 6, 9),
      twoJoinedHexs.parallelogonIndexClasses shouldBe
        List(List(0, 4, 6), List(1, 5, 9), List(2, 8), List(3, 7))
    )

  they should "be found for a 2 joined regular hexagons boundary multiplied by 2" in:

    /** <img src="file:../../../../../../resources/simple/doubledJoinedHexs.svg"/> */
    val doubledJoinedHexs = twoJoinedHexs.multiplySidesBy(2)
//    println(doubledJoinedHexs.toParallelogonTiling())
    doubledJoinedHexs.parallelogonIndices shouldBe List(0, 2, 8, 10, 12, 18)

  they should "be true for a 2x2 joined regular hexagons boundary" in:

    /** <img src="file:../../../../../../resources/simple/fourJoinedHexs.svg"/> */
    val fourJoinedHexs: SimplePolygon =
      SimplePolygon(120, 120, 240, 120, 120, 240, 120, 120, 120, 240, 120, 120, 240, 120)
//    println(fourJoinedHexs.toParallelogonTiling())
    fourJoinedHexs.parallelogonIndices shouldBe List(0, 1, 4, 7, 8, 11)

  they should "be true for a 8x8 joined regular hexagons boundary" in:
    val sixtyFourJoinedHexs: SimplePolygon =
      TilingBuilder.createHexagonNet(8, 8).value.boundarySimplePolygon
//    println(sixtyFourJoinedHexs.toParallelogonTiling())
    allAssert(
      sixtyFourJoinedHexs.parallelogonIndices shouldBe List(0, 3, 18, 31, 34, 49),
      sixtyFourJoinedHexs.parallelogonIndexClasses shouldBe
        List(
          List(0, 18, 34),
          List(3, 31, 49),
          List(1, 33),
          List(2, 32),
          List(4, 48),
          List(5, 47),
          List(6, 46),
          List(7, 45),
          List(8, 44),
          List(9, 43),
          List(10, 42),
          List(11, 41),
          List(12, 40),
          List(13, 39),
          List(14, 38),
          List(15, 37),
          List(16, 36),
          List(17, 35),
          List(19, 61),
          List(20, 60),
          List(21, 59),
          List(22, 58),
          List(23, 57),
          List(24, 56),
          List(25, 55),
          List(26, 54),
          List(27, 53),
          List(28, 52),
          List(29, 51),
          List(30, 50)
        )
    )

  they should "be true for a carved boundary" in:

    /** <img src="file:../../../../../../resources/simple/carved.svg"/> */
    val carved: SimplePolygon =
      SimplePolygon(120, 180, 120, 180, 120, 240, 120, 60, 240, 180, 120, 120, 240, 120)
//    println(carved.toParallelogonTiling())
    carved.parallelogonIndices shouldBe List(0, 3, 4, 7, 10, 11)

  they should "be found for a scale" in:

    /** <img src="file:../../../../../../resources/simple/scale-3.3.4.3.4.svg"/> */
    val scale = SimplePolygon(90, 150, 120, 150, 90, 210, 60, 210)
//    println(scale.toParallelogonTiling())
    scale.parallelogonIndices shouldBe List(0, 2, 4, 6)

  they should "be found for a comma" in:

    /** <img src="file:../../../../../../resources/simple/comma-3.3.3.4.4.svg"/> */
    val comma = SimplePolygon(90, 90, 150, 120, 60, 210)
//    println(comma.toParallelogonTiling())
    comma.parallelogonIndices shouldBe List(0, 1, 3, 4)

  they should "be found for a devil" in:

    /** <img src="file:../../../../../../resources/simple/devil-3.12.12.svg"/> */
    val devil = SimplePolygon(150, 150, 150, 150, 150, 150, 150, 150, 210, 60, 210, 210, 60, 210)
//    println(devil.toParallelogonTiling())
    allAssert(
      devil.parallelogonIndices shouldBe List(0, 2, 5, 7, 9, 12),
      devil.parallelogonIndexClasses shouldBe
        List(List(0, 5, 9), List(2, 7, 12), List(1, 8), List(3, 11), List(4, 10), List(6, 13))
    )

  they should "be found for a 3.6.3.6 tessellation unit" in:

    /** <img src="file:../../../../../../resources/simple/unit-3.6.3.6.svg"/> */
    val unit = SimplePolygon(60, 180, 120, 120, 120, 300, 120, 120, 180, 60, 240, 60, 240, 240)
//    println(unit.toParallelogonTiling())
    unit.parallelogonIndices shouldBe List(0, 2, 7, 9)

  they should "be found for a 3.4.6.4 tessellation unit" in:

    /** <img src="file:../../../../../../resources/simple/fan-3.4.6.4.svg"/> */
    val unit = SimplePolygon(90, 210, 120, 120, 210, 90, 150, 150, 150, 150)
//    println(unit.toParallelogonTiling())
    unit.parallelogonIndices shouldBe List(0, 2, 3, 5, 7, 8)

  they should "be found for a 4.6.12 tessellation unit" in:

    /** <img src="file:../../../../../../resources/simple/badge-4.6.12.svg"/> */
    val badge =
      SimplePolygon(150, 150, 150, 150, 150, 150, 240, 90, 210, 120, 120, 210, 210, 120, 120, 210, 90, 240)
//    println(badge.toParallelogonTiling())
    allAssert(
      badge.parallelogonIndices shouldBe List(1, 4, 7, 10, 13, 16),
      badge.parallelogonIndexClasses shouldBe
        List(
          List(1, 7, 13),
          List(4, 10, 16),
          List(2, 12),
          List(3, 11),
          List(5, 15),
          List(6, 14),
          List(8, 0),
          List(9, 17)
        )
    )

  /** <img src="file:../../../../../../resources/simple/bulb-4.8.8.svg"/> */
  val bulb: SimplePolygon =
    SimplePolygon(90, 90, 225, 135, 135, 135, 135, 135, 135, 225)

  they should "be true for a 4.8.8 tessellation unit" in:
    //    println(bulb.toParallelogonTiling())
    bulb.parallelogonIndices shouldBe List(0, 1, 3, 5, 6, 8)

  they should "be true for a doubled 4.8.8 tessellation unit" in:

    /** <img src="file:../../../../../../resources/simple/bulbDoubled.svg"/> */
    val doubledBulb: SimplePolygon =
      bulb.multiplySidesBy(2)
//    println(doubledBulb.toParallelogonTiling())
    doubledBulb.parallelogonIndices shouldBe List(0, 2, 6, 10, 12, 16)

  they should "be found for a 3.3.3.3.6 tessellation half unit" in:

    /** <img src="file:../../../../../../resources/simple/vase-3.3.3.3.6.svg"/> */
    val unit = SimplePolygon(60, 180, 120, 180, 120, 120, 180, 120, 120, 240)
//    println(unit.toParallelogonTiling())
    unit.parallelogonIndices shouldBe List(0, 2, 3, 5, 7, 8)

  behavior of "symmetry"

  it must "be found in two joined hexs" in:

    /** <img src="file:../../../../../../resources/simple/symmetry/twoJoinedHexs.svg"/> */
    val simplePolygon = twoJoinedHexs
//    println(twoJoinedHexs.toScalableVectorG(showReflection = true, showRotation = true))
    allAssert(
      simplePolygon.rotationalSymmetryOrder shouldBe 2,
      simplePolygon.rotationalIndices shouldBe List(2, 7),
      simplePolygon.reflectionalSymmetryOrder shouldBe 2,
      simplePolygon.reflectionalIndexPairs shouldBe List((Edge(4, 5), Edge(9, 0)), (Vertex(2), Vertex(7)))
    )

  it must "be found in a bulb" in:

    /** <img src="file:../../../../../../resources/simple/symmetry/bulb.svg"/> */
    val simplePolygon = bulb
    //    println(bulb.toScalableVectorG(showReflection = true, showRotation = true))
    allAssert(
      simplePolygon.rotationalSymmetryOrder shouldBe 1,
      simplePolygon.rotationalIndices shouldBe List(2),
      simplePolygon.reflectionalSymmetryOrder shouldBe 1,
      simplePolygon.reflectionalIndexPairs shouldBe List((Edge(0, 1), Edge(5, 6)))
    )
