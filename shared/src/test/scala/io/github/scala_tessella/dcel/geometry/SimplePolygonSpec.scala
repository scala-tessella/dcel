package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.conversion.TilingSVG.{toParallelogonTiling, toScalableVectorG}
import io.github.scala_tessella.dcel.geometry.{AngleDegree, SimplePolygon}
import io.github.scala_tessella.dcel.{TilingBuilder, TilingTestHelpers}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SimplePolygonSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "SimplePolygon.areOppositeShifted"

  it should "return None if the two opposite sequences of turns are of different" in {
    val xs = Vector(-45, 45, -45, 45).map(AngleDegree(_))
    val ys = Vector(0, 45, -45, 45).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(xs, ys) shouldBe None
  }

  it should "return None if each sequence has length <= 1" in {
    val s = Vector(45).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(s, s) shouldBe None
  }

  it should "detect a shift of 1 if turns are all 0°" in {
    val s  = Vector(0, 0).map(AngleDegree(_))
    val s1 = Vector(0, 0, 0).map(AngleDegree(_))
    allAssert(
      SimplePolygon.areOppositeShifted(s, s) shouldBe Some(1),
      SimplePolygon.areOppositeShifted(s1, s1) shouldBe Some(1)
    )
  }

  it should "return None for other opposite sequences containing the same turns (not fitting)" in {
    val s  = Vector(90, 90, 90, 90).map(AngleDegree(_))
    val s1 = Vector(45, 45, 45, 45).map(AngleDegree(_))
    allAssert(
      SimplePolygon.areOppositeShifted(s, s) shouldBe None,
      SimplePolygon.areOppositeShifted(s1, s1) shouldBe None
    )
  }

  it should "detect a shift of 1" in {
    val s = Vector(45, -45, 45).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(s, s) shouldBe Some(1)
  }

  it can "detect a shift of 2 in sequences of length 3" in {
    val s = Vector(45, 45, -45).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(s, s) shouldBe Some(2)
  }

  it can "NOT detect a shift of 2 in sequences of length 4" in {
    val s = Vector(45, 45, 45, -45).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(s, s) shouldBe None
  }

  it can "detect another shift of 2 in sequences of length 4" in {
    val s = Vector(45, 45, -45, 45).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(s, s) shouldBe None
  }

  it should "detect a shift of 2" in {
    val s = Vector(45, 0, -45, 0, 45, 0).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(s, s) shouldBe Some(2)
  }

  behavior of "SimplePolygon.areOpposite"

  it should "return None if the two opposite sequences of turns are of different size" in {
    val xs = Vector(-45, 45, -45, 45).map(AngleDegree(_))
    val ys = Vector(45, -45, 45).map(AngleDegree(_))
    SimplePolygon.areOpposite(xs, ys) shouldBe None
  }

  it should "detect a shift of 0 in fitting sequences" in {
    val xs = Vector(-45, 45, -45).map(AngleDegree(_))
    val ys = Vector(45, -45, 45).map(AngleDegree(_))
    SimplePolygon.areOpposite(xs, ys) shouldBe Some(0)
  }

  it should "detect a shift of 0 if turns are all 0°" in {
    val s  = Vector(0, 0).map(AngleDegree(_))
    val s1 = Vector(0, 0, 0).map(AngleDegree(_))
    allAssert(
      SimplePolygon.areOpposite(s, s) shouldBe Some(0),
      SimplePolygon.areOpposite(s1, s1) shouldBe Some(0)
    )
  }

  behavior of "SimplePolygon.parallelogonIndices"

  val simpleSquare: SimplePolygon = SimplePolygon(90, 90, 90, 90)

  they should "be found for a square" in
    allAssert(
      simpleSquare.parallelogonIndices shouldBe Some((0, 1, 2, 3)),
      simpleSquare.parallelogonIndicesNew shouldBe Some((0, 1, 2, 3)),
      simpleSquare.parallelogonHexIndices shouldBe List(0, 1, 2, 3)
    )

  they should "be found for a 2x2 square" in {
    val square2x2 = simpleSquare.multiplySidesBy(2)
    allAssert(
      square2x2.parallelogonIndices shouldBe Some((0, 2, 4, 6)),
      square2x2.parallelogonIndicesNew shouldBe Some((0, 2, 4, 6)),
      square2x2.parallelogonHexIndices shouldBe List(0, 2, 4, 6)
    )
  }

  they should "be found for a 2x2 square with shifted angles" in {
    val angles =
      Vector.fill(4)(Vector(AngleDegree(180), AngleDegree(90))).flatten
    val simple = SimplePolygon(angles)
    allAssert(
      simple.parallelogonIndices shouldBe Some((1, 3, 5, 7)),
      simple.parallelogonIndicesNew shouldBe Some((1, 3, 5, 7)),
      simple.parallelogonHexIndices shouldBe List(0, 1, 3, 4, 5, 7)
    )
  }

  they should "be found for a 3x3 square" in {
    val square3x3 = simpleSquare.multiplySidesBy(3)
    allAssert(
      square3x3.parallelogonIndices shouldBe Some((0, 3, 6, 9)),
      square3x3.parallelogonIndicesNew shouldBe Some((0, 3, 6, 9)),
      square3x3.parallelogonHexIndices shouldBe List(0, 3, 6, 9)
    )
  }

  they should "be found for a rhombus" in {
    val rhombus = SimplePolygon(120, 60, 120, 60)
    allAssert(
      rhombus.parallelogonIndices shouldBe Some((0, 1, 2, 3)),
      rhombus.parallelogonHexIndices shouldBe List(0, 1, 2, 3)
    )
  }

  they should "be found for a 1x2 rectangle" in {
    val rectangle1x2 = SimplePolygon(90, 90, 180, 90, 90, 180)
    allAssert(
      rectangle1x2.parallelogonIndices shouldBe Some((0, 1, 3, 4)),
      rectangle1x2.parallelogonIndicesNew shouldBe Some((0, 1, 3, 4)),
      rectangle1x2.parallelogonHexIndices shouldBe List(0, 1, 3, 4)
    )
  }

  they should "be found for a 2x1 parallelogram" in {

    /** <img src="file:../../../../../../resources/simple/parallelogram2x1.svg"/> */
    val parallelogram2x1 = SimplePolygon(60, 120, 180, 60, 120, 180)
//    println(parallelogram2x1.toParallelogonTiling())
    allAssert(
      parallelogram2x1.parallelogonIndices shouldBe Some((0, 1, 3, 4)),
      parallelogram2x1.parallelogonIndicesNew shouldBe Some((0, 1, 3, 4)),
      parallelogram2x1.parallelogonHexIndices shouldBe List(0, 1, 3, 4)
    )
  }

  they should "be found for a regular pentagon" in {
    SimplePolygon(RegularPolygon(5).angles).parallelogonHexIndices shouldBe Nil
  }

  they should "be found for an hexagon" in {
    val unit = SimplePolygon(90, 120, 150, 90, 120, 150)
    allAssert(
      unit.parallelogonIndices shouldBe None,
      unit.parallelogonIndicesNew shouldBe None,
      unit.parallelogonHexIndices shouldBe List(0, 1, 2, 3, 4, 5)
    )
  }

  /** <img src="file:../../../../../../resources/simple/twoJoinedHexs.svg"/> */
  val twoJoinedHexs: SimplePolygon =
    SimplePolygon(120, 120, 240, 120, 120, 120, 120, 240, 120, 120)

  they should "be found for a 2 joined regular hexagons boundary" in
//    println(twoJoinedHexs.toParallelogonTiling())
    allAssert(
      twoJoinedHexs.parallelogonIndices shouldBe Some((0, 1, 5, 6)),
      twoJoinedHexs.parallelogonIndicesNew shouldBe Some((0, 4, 5, 9)),
      twoJoinedHexs.parallelogonHexIndices shouldBe List(0, 1, 4, 5, 6, 9),
      twoJoinedHexs.parallelogonEquivalences shouldBe
        List(List(0, 3, 6), List(1, 5, 8), List(2, 7), List(4, 9)),
      twoJoinedHexs.parallelogonEquivalencesNew shouldBe
        List(List(0, 4, 6), List(1, 5, 9), List(2, 8), List(3, 7))
    )

  they should "be found for a 2 joined regular hexagons boundary multiplied by 2" in {

    /** <img src="file:../../../../../../resources/simple/doubledJoinedHexs.svg"/> */
    val doubledJoinedHexs = twoJoinedHexs.multiplySidesBy(2)
//    println(doubledJoinedHexs.toParallelogonTiling())
    allAssert(
      doubledJoinedHexs.parallelogonIndices shouldBe Some((0, 2, 10, 12)),
      doubledJoinedHexs.parallelogonIndicesNew shouldBe Some((0, 6, 10, 16)),
      doubledJoinedHexs.parallelogonHexIndices shouldBe List(0, 2, 8, 10, 12, 18)
    )
  }

  they should "be true for a 2x2 joined regular hexagons boundary" in {

    /** <img src="file:../../../../../../resources/simple/fourJoinedHexs.svg"/> */
    val fourJoinedHexs: SimplePolygon =
      SimplePolygon(120, 120, 240, 120, 120, 240, 120, 120, 120, 240, 120, 120, 240, 120)
//    println(fourJoinedHexs.toParallelogonTiling())
    allAssert(
      fourJoinedHexs.parallelogonIndices shouldBe Some((0, 3, 7, 10)),
      fourJoinedHexs.parallelogonIndicesNew shouldBe Some((0, 3, 7, 10)),
      fourJoinedHexs.parallelogonHexIndices shouldBe List(0, 1, 4, 7, 8, 11)
    )
  }

  they should "be true for a 8x8 joined regular hexagons boundary" in {
    val sixtyFourJoinedHexs: SimplePolygon =
      TilingBuilder.createHexagonNet(8, 8).boundarySimplePolygon
//    println(sixtyFourJoinedHexs.toParallelogonTiling())
    allAssert(
      sixtyFourJoinedHexs.parallelogonIndices shouldBe Some((0, 13, 31, 44)),
      sixtyFourJoinedHexs.parallelogonIndicesNew shouldBe Some((6, 21, 37, 52)),
      sixtyFourJoinedHexs.parallelogonHexIndices shouldBe List(0, 7, 22, 31, 38, 53),

      //      sixtyFourJoinedHexs.parallelogonEquivalences shouldBe
//        List(
//          List(0, 13, 22, 31, 37, 53), List(1, 36), List(2, 35), List(3, 34), List(4, 33), List(5, 32), List(6, 44),
//          List(7, 43), List(8, 42), List(9, 41), List(10, 40), List(11, 39), List(12, 38), List(14, 52), List(15, 51),
//          List(16, 50), List(17, 49), List(18, 48), List(19, 47), List(20, 46), List(21, 45), List(23, 61),
//          List(24, 60), List(25, 59), List(26, 58), List(27, 57), List(28, 56), List(29, 55), List(30, 54)
//        ),
      sixtyFourJoinedHexs.parallelogonEquivalencesNew shouldBe
        List(
          List(0, 28),
          List(1, 27),
          List(2, 26),
          List(3, 25),
          List(4, 24),
          List(5, 23),
          List(6, 22, 52),
          List(7, 51),
          List(8, 50),
          List(9, 49),
          List(10, 48),
          List(11, 47),
          List(12, 46),
          List(13, 45),
          List(14, 44),
          List(15, 43),
          List(16, 42),
          List(17, 41),
          List(18, 40),
          List(19, 39),
          List(20, 38),
          List(21, 37, 53),
          List(29, 61),
          List(30, 60),
          List(31, 59),
          List(32, 58),
          List(33, 57),
          List(34, 56),
          List(35, 55),
          List(36, 54)
        )
    )
  }

  they should "be true for a carved boundary" in {

    /** <img src="file:../../../../../../resources/simple/carved.svg"/> */
    val carved: SimplePolygon =
      SimplePolygon(120, 180, 120, 180, 120, 240, 120, 60, 240, 180, 120, 120, 240, 120)
//    println(carved.toParallelogonTiling())
    allAssert(
      carved.parallelogonIndices shouldBe Some((0, 3, 7, 10)),
      carved.parallelogonIndicesNew shouldBe Some((0, 3, 7, 10)),
      carved.parallelogonHexIndices shouldBe List(0, 3, 4, 7, 10, 11)
    )
  }

  they should "be found for a scale" in {

    /** <img src="file:../../../../../../resources/simple/scale-3.3.4.3.4.svg"/> */
    val scale = SimplePolygon(90, 150, 120, 150, 90, 210, 60, 210)
//    println(scale.toParallelogonTiling())
    allAssert(
      scale.parallelogonIndices shouldBe Some((0, 2, 4, 6)),
      scale.parallelogonIndicesNew shouldBe Some((0, 2, 4, 6)),
      scale.parallelogonHexIndices shouldBe List(0, 2, 4, 6)
    )
  }

  they should "be found for a comma" in {

    /** <img src="file:../../../../../../resources/simple/comma-3.3.3.4.4.svg"/> */
    val comma = SimplePolygon(90, 90, 150, 120, 60, 210)
//    println(comma.toParallelogonTiling())
    allAssert(
      comma.parallelogonIndices shouldBe Some((0, 1, 3, 4)),
      comma.parallelogonIndicesNew shouldBe Some((0, 1, 3, 4)),
      comma.parallelogonHexIndices shouldBe List(0, 1, 3, 4)
    )
  }

  they should "be found for a devil" in {

    /** <img src="file:../../../../../../resources/simple/devil-3.12.12.svg"/> */
    val devil = SimplePolygon(150, 150, 150, 150, 150, 150, 150, 150, 210, 60, 210, 210, 60, 210)
//    println(devil.toParallelogonTiling())
    allAssert(
      devil.parallelogonIndices shouldBe Some((2, 5, 9, 12)),
      devil.parallelogonIndicesNew shouldBe Some((2, 5, 9, 12)),
      devil.parallelogonHexIndices shouldBe List(0, 2, 5, 7, 9, 12),
      devil.parallelogonEquivalences shouldBe
        List(List(0, 5, 9), List(1, 8), List(2, 7, 12), List(3, 11), List(4, 10), List(6, 13)),
      devil.parallelogonEquivalencesNew shouldBe
        List(List(0, 8), List(1, 7), List(2, 6, 12), List(3, 11), List(4, 10), List(5, 9, 13))
    )
  }

  they should "be found for a 3.6.3.6 tessellation unit" in {

    /** <img src="file:../../../../../../resources/simple/unit-3.6.3.6.svg"/> */
    val unit = SimplePolygon(60, 180, 120, 120, 120, 300, 120, 120, 180, 60, 240, 60, 240, 240)
//    println(unit.toParallelogonTiling())
    allAssert(
      unit.parallelogonIndices shouldBe Some((0, 2, 7, 9)),
      unit.parallelogonIndicesNew shouldBe Some((0, 2, 7, 9)),
      unit.parallelogonHexIndices shouldBe List(0, 2, 7, 9)
    )
  }

  they should "be found for a 3.4.6.4 tessellation unit" in {

    /** <img src="file:../../../../../../resources/simple/unit-3.4.6.4.svg"/> */
    val unit = SimplePolygon(90, 210, 120, 120, 210, 210, 120, 210, 90, 150, 150, 150, 150, 240, 150, 150)
//    println(unit.toParallelogonTiling())
    allAssert(
      unit.parallelogonIndices shouldBe Some((2, 6, 10, 14)),
      unit.parallelogonIndicesNew shouldBe Some((0, 2, 8, 10)),
      unit.parallelogonHexIndices shouldBe List(0, 2, 3, 8, 10, 11)
    )
  }

  they should "be found for a 4.6.12 tessellation unit" in {

    /** <img src="file:../../../../../../resources/simple/badge-4.6.12.svg"/> */
    val badge =
      SimplePolygon(150, 150, 150, 150, 150, 150, 240, 90, 210, 120, 120, 210, 210, 120, 120, 210, 90, 240)
//    println(badge.toParallelogonTiling())
    allAssert(
      badge.parallelogonIndices shouldBe Some((1, 4, 10, 13)),
      badge.parallelogonIndicesNew shouldBe Some((1, 7, 10, 16)),
      badge.parallelogonHexIndices shouldBe List(1, 4, 7, 10, 13, 16),
      badge.parallelogonEquivalences shouldBe
        List(
          List(0, 8),
          List(1, 7, 13),
          List(2, 12),
          List(3, 11),
          List(4, 10, 16),
          List(5, 15),
          List(6, 14),
          List(9, 17)
        ),
      badge.parallelogonEquivalencesNew shouldBe
        List(
          List(0, 8),
          List(1, 7, 12),
          List(2, 11),
          List(3, 10, 16),
          List(4, 15),
          List(5, 14),
          List(6, 13),
          List(9, 17)
        )
    )
  }

  /** <img src="file:../../../../../../resources/simple/bulb-4.8.8.svg"/> */
  val bulb: SimplePolygon =
    SimplePolygon(90, 90, 225, 135, 135, 135, 135, 135, 135, 225)

  they should "be true for a 4.8.8 tessellation unit" in
//    println(bulb.toParallelogonTiling())
    allAssert(
      bulb.parallelogonIndices shouldBe Some((0, 1, 5, 6)),
      bulb.parallelogonIndicesNew shouldBe Some((0, 3, 5, 8)),
      bulb.parallelogonHexIndices shouldBe List(0, 1, 3, 5, 6, 8)
    )

  they should "be true for a doubled 4.8.8 tessellation unit" in {

    /** <img src="file:../../../../../../resources/simple/bulbDoubled.svg"/> */
    val doubledBulb: SimplePolygon =
      bulb.multiplySidesBy(2)
//    println(doubledBulb.toParallelogonTiling())
    allAssert(
      doubledBulb.parallelogonIndices shouldBe Some((0, 2, 10, 12)),
      doubledBulb.parallelogonIndicesNew shouldBe Some((0, 6, 10, 16)),
      doubledBulb.parallelogonHexIndices shouldBe List(0, 2, 6, 10, 12, 16)
    )
  }

  they should "be found for a 3.3.3.3.6 tessellation half unit" in {

    /** <img src="file:../../../../../../resources/simple/unit-3.3.3.3.6half.svg"/> */
    val unit = SimplePolygon(60, 180, 120, 180, 120, 120, 180, 120, 120, 240)
//    println(unit.toParallelogonTiling())
    allAssert(
      unit.parallelogonIndices shouldBe None,
      unit.parallelogonIndicesNew shouldBe Option((0, 2, 5, 7)),
      unit.parallelogonHexIndices shouldBe List(0, 2, 3, 5, 7, 8)
    )
  }

  they should "be found for a 3.3.3.3.6 tessellation unit" in {

    /** <img src="file:../../../../../../resources/simple/unit-3.3.3.3.6.svg"/> */
    val unit = SimplePolygon(120, 180, 120, 120, 240, 180, 120, 240, 60, 180, 120, 180, 120, 240, 180, 120)
//    println(unit.toParallelogonTiling())
    allAssert(
      unit.parallelogonIndices shouldBe None,
      unit.parallelogonIndicesNew shouldBe Option(0, 2, 8, 10),
      unit.parallelogonHexIndices shouldBe List(0, 2, 3, 8, 10, 11)
    )
  }
