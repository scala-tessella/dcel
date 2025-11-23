package io.github.scala_tessella.dcel.geometry

//import io.github.scala_tessella.dcel.conversion.TilingSVG.toScalableVectorG
import io.github.scala_tessella.dcel.geometry.SimplePolygon.ParallelogramTranslation.*
import io.github.scala_tessella.dcel.{TilingBuilder, TilingTestHelpers}
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon, SimplePolygon}
import io.github.scala_tessella.ring_seq.RingSeq.rotationsAndReflections
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.*

class PolygonSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "SimplePolygon.apply"

  val squareAngles = Vector(AngleDegree(90), AngleDegree(90), AngleDegree(90), AngleDegree(90))

  it should "validate the angles of a correct simple polygon" in {
    SimplePolygon(squareAngles).toAngles.nonEmpty shouldBe true
  }

  val triangleAngles = Vector(AngleDegree(60), AngleDegree(60), AngleDegree(60))

  it should "validate the angles of another correct simple polygon" in {
    SimplePolygon(triangleAngles).toAngles.nonEmpty shouldBe true
  }

  // New: minimal length constraints
  it should "reject polygons with fewer than 3 angles" in
    allAssert(
      the[IllegalArgumentException] thrownBy SimplePolygon(
        Vector.empty
      ) should have message "A simple polygon must have at least 3 sides.",
      the[IllegalArgumentException] thrownBy SimplePolygon(
        Vector(AngleDegree(180))
      ) should have message "A simple polygon must have at least 3 sides.",
      the[IllegalArgumentException] thrownBy SimplePolygon(
        Vector(AngleDegree(100), AngleDegree(80))
      ) should have message "A simple polygon must have at least 3 sides."
    )

  // New: normalization handling (negative and >180 accepted only via normalisation if sum matches)
  it should "accept angles that normalise to a valid simple polygon" in {
    val weird = Vector(
      AngleDegree(-300),
      AngleDegree(450),
      AngleDegree(30)
    ) // normalised -> 60,90,30 (sum 180) -> triangle
    SimplePolygon(weird).toAngles.size shouldBe 3
  }

  it should "invalidate angles if their sum is incorrect" in {
    // Sum is too small
    val wrongAngles = Vector(AngleDegree(90), AngleDegree(90), AngleDegree(90), AngleDegree(89))
    an[IllegalArgumentException] should be thrownBy SimplePolygon(wrongAngles)
  }

  it should "invalidate angles if any angle is a full circle" in {
    val anglesWithFullCircle = Vector(AngleDegree(360), AngleDegree(0), AngleDegree(90), AngleDegree(-90))
    an[IllegalArgumentException] should be thrownBy SimplePolygon(anglesWithFullCircle)
  }

  // New: toAngles preserves order and content
  it should "preserve the provided angles order and size" in {
    val angles = Vector(AngleDegree(60), AngleDegree(120), AngleDegree(60), AngleDegree(120))
    val simple = SimplePolygon(angles)
    simple.toAngles shouldBe angles
  }

  // New: alphaSum helper
  it should "compute alphaSum correctly" in
    allAssert(
      SimplePolygon.alphaSum(3).toRational shouldBe Rational(180),
      SimplePolygon.alphaSum(4).toRational shouldBe Rational(360),
      SimplePolygon.alphaSum(6).toRational shouldBe Rational(720)
    )

  behavior of "SimplePolygon.multiplySidesBy"

  it should "triplicate a triangle" in {
    SimplePolygon(triangleAngles).multiplySidesBy(3) shouldBe
      Vector(60, 180, 180, 60, 180, 180, 60, 180, 180).map(AngleDegree(_))
  }

  it should "remain the same when multiplied by one" in {
    SimplePolygon(triangleAngles).multiplySidesBy(1) shouldBe
      triangleAngles
  }

  it should "fail if factor is < 1" in {
    the[IllegalArgumentException] thrownBy
      SimplePolygon(triangleAngles).multiplySidesBy(0) should have message
      "A simple polygon must have sides of at least unit length."
  }

  behavior of "SimplePolygon.parallelogonIndices"

  val simpleSquare: SimplePolygon = SimplePolygon(squareAngles)

  it should "be found for a square" in
    allAssert(
      simpleSquare.parallelogonIndices shouldBe Some((0, 1, 2, 3)),
      simpleSquare.parallelogonEquivalences shouldBe
        List(
          List(0, 1, 2, 3)
        ),
      simpleSquare.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC   -> 1,
            SideBD   -> 3
          )
        )
    )

  it should "be found for a regular pentagon" in {
    SimplePolygon(RegularPolygon(5).angles).parallelogonIndices shouldBe None
  }

  it should "be found for a 2x2 square" in {
    val square2x2 = simpleSquare.multiplySidesBy(2)
    allAssert(
      square2x2.parallelogonIndices shouldBe Some((0, 2, 4, 6)),
      square2x2.parallelogonEquivalences shouldBe
        List(
          List(0, 2, 4, 6),
          List(1, 5),
          List(3, 7)
        ),
      square2x2.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC   -> 2,
            SideBD   -> 6
          )
        )
    )
  }

  it should "be found for a 2x2 square with shifted angles" in {
    val angles =
      Vector.fill(4)(Vector(AngleDegree(180), AngleDegree(90))).flatten
    val simple = SimplePolygon(angles)
    allAssert(
      simple.parallelogonIndices shouldBe Some((1, 3, 5, 7)),
      simple.parallelogonEquivalences shouldBe
        List(
          List(0, 4),
          List(1, 3, 5, 7),
          List(2, 6)
        ),
      simple.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 1,
            SideAC   -> 3,
            SideBD   -> 7
          )
        )
    )
  }

  def checkIndicesForAllRotationsAndReflections(simple: SimplePolygon): Assertion =
    simple.toAngles.rotationsAndReflections.distinct
      .forall(SimplePolygon(_).parallelogonIndices.isDefined) shouldBe true

  def checkEquivalencesForAllRotationsAndReflections(
      simple: SimplePolygon,
      expectedGroupsCount: Int,
      isShifted: Boolean = false
  ): Assertion =
    simple.toAngles.rotationsAndReflections.distinct
      .forall(angles =>
        val g = SimplePolygon(angles).parallelogonEquivalences
        g.size == expectedGroupsCount
        && g.count(_.size == 2) == expectedGroupsCount - (if isShifted then 2 else 1)
        && g.filter(_.size > 2).forall(_.size == (if isShifted then 3 else 4))
      ) shouldBe true

  it should "be found for a 3x3 square" in {
    val square3x3 = SimplePolygon(squareAngles).multiplySidesBy(3)
    allAssert(
      square3x3.parallelogonIndices shouldBe Some((0, 3, 6, 9)),
      square3x3.parallelogonEquivalences shouldBe
        List(
          List(0, 3, 6, 9),
          List(1, 8),
          List(2, 7),
          List(4, 11),
          List(5, 10)
        ),
      square3x3.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC   -> 3,
            SideBD   -> 9
          )
        ),
      checkIndicesForAllRotationsAndReflections(square3x3),
      checkEquivalencesForAllRotationsAndReflections(square3x3, 5)
    )
  }

  it should "be found for a regular hexagon" in {
    SimplePolygon(RegularPolygon(6).angles).parallelogonIndices shouldBe None
  }

  it should "be found for a scale" in {

    /** <img src="file:../../../../../../resources/simple/scale-3.3.4.3.4.svg"/> */
    val scale = SimplePolygon(90, 150, 120, 150, 90, 210, 60, 210)
    allAssert(
      scale.parallelogonIndices shouldBe Some((0, 2, 4, 6)),
      scale.parallelogonEquivalences shouldBe
        List(
          List(0, 2, 4, 6),
          List(1, 5),
          List(3, 7)
        ),
      scale.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC   -> 2,
            SideBD   -> 6
          )
        ),
      checkIndicesForAllRotationsAndReflections(scale),
      checkEquivalencesForAllRotationsAndReflections(scale, 3)
    )
  }

  it should "be found for a comma" in {

    /** <img src="file:../../../../../../resources/simple/comma-3.3.3.4.4.svg"/> */
    val comma = SimplePolygon(90, 90, 150, 120, 60, 210)
    allAssert(
      comma.parallelogonIndices shouldBe Some((0, 1, 3, 4)),
      comma.parallelogonEquivalences shouldBe
        List(
          List(0, 1, 3, 4),
          List(2, 5)
        ),
      comma.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC -> 1,
            SideBD -> 4
          )
        ),
      checkIndicesForAllRotationsAndReflections(comma),
      checkEquivalencesForAllRotationsAndReflections(comma, 2)
    )
  }

  it should "be found for a devil" in {

    /** <img src="file:../../../../../../resources/simple/devil-3.12.12.svg"/> */
    val devil = SimplePolygon(150, 150, 150, 150, 150, 150, 150, 150, 210, 60, 210, 210, 60, 210)
    allAssert(
      devil.parallelogonIndices shouldBe Some((2, 5, 9, 12)),
      devil.parallelogonEquivalences shouldBe
        List(
          List(0, 5, 9),
          List(1, 8),
          List(2, 7, 12),
          List(3, 11),
          List(4, 10),
          List(6, 13)
        ),
      devil.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC -> 5,
            SideBD -> 9
          )
        ),
      checkIndicesForAllRotationsAndReflections(devil),
      checkEquivalencesForAllRotationsAndReflections(devil, 6, isShifted = true)
    )
  }

  it should "be found for a 3.6.3.6 tessellation unit" in {

    /** <img src="file:../../../../../../resources/simple/unit-3.6.3.6.svg"/> */
    val unit = SimplePolygon(60, 180, 120, 120, 120, 300, 120, 120, 180, 60, 240, 60, 240, 240)
    allAssert(
      unit.parallelogonIndices shouldBe Some((0, 2, 7, 9)),
      unit.parallelogonEquivalences shouldBe
        List(
          List(0, 2, 7, 9),
          List(1, 8),
          List(3, 13),
          List(4, 12),
          List(5, 11),
          List(6, 10)
        ),
      unit.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC -> 2,
            SideBD -> 9
          )
        ),
      checkIndicesForAllRotationsAndReflections(unit),
      checkEquivalencesForAllRotationsAndReflections(unit, 6)
    )
  }


  it should "be found for a 1x2 rectangle" in {
    val rectangle1x2 = SimplePolygon(90, 90, 180, 90, 90, 180)
    allAssert(
      rectangle1x2.parallelogonIndices shouldBe Some((0, 1, 3, 4)),
      rectangle1x2.parallelogonEquivalences shouldBe
        List(
          List(0, 1, 3, 4),
          List(2, 5)
        ),
      rectangle1x2.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC   -> 1,
            SideBD   -> 4
          )
        ),
      checkIndicesForAllRotationsAndReflections(rectangle1x2),
      checkEquivalencesForAllRotationsAndReflections(rectangle1x2, 2)
    )
  }

  it should "be found for a 2x1 parallelogram" in {

    /** <img src="file:../../../../../../resources/simple/parallelogram2x1.svg"/> */
    val parallelogram2x1 = SimplePolygon(60, 120, 180, 60, 120, 180)
    allAssert(
      parallelogram2x1.parallelogonIndices shouldBe Some((0, 1, 3, 4)),
      parallelogram2x1.parallelogonEquivalences shouldBe
        List(
          List(0, 1, 3, 4),
          List(2, 5)
        ),
      parallelogram2x1.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC   -> 1,
            SideBD   -> 4
          )
        ),
      checkIndicesForAllRotationsAndReflections(parallelogram2x1),
      checkEquivalencesForAllRotationsAndReflections(parallelogram2x1, 2)
    )
  }

  /** <img src="file:../../../../../../resources/simple/twoJoinedHexs.svg"/> */
  val twoJoinedHexs: SimplePolygon =
    SimplePolygon(120, 120, 240, 120, 120, 120, 120, 240, 120, 120)

  it should "be found for a 2 joined regular hexagons boundary" in
    allAssert(
      twoJoinedHexs.parallelogonIndices shouldBe Some((0, 1, 5, 6)),
      twoJoinedHexs.parallelogonEquivalences shouldBe
        List(
          List(0, 4, 6),
          List(1, 5, 9),
          List(2, 8),
          List(3, 7)
        ),
      twoJoinedHexs.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC   -> 4,
            SideBD   -> 6
          )
        ),
      checkIndicesForAllRotationsAndReflections(twoJoinedHexs),
      checkEquivalencesForAllRotationsAndReflections(twoJoinedHexs, 4, isShifted = true)
    )

  it should "be found for a 2 joined regular hexagons boundary multiplied by 2" in {

    /** <img src="file:../../../../../../resources/simple/doubledJoinedHexs.svg"/> */
    val doubledJoinedHexs = twoJoinedHexs.multiplySidesBy(2)
    allAssert(
      doubledJoinedHexs.parallelogonIndices shouldBe Some((0, 2, 10, 12)),
      doubledJoinedHexs.parallelogonEquivalences shouldBe
        List(
          List(0, 8, 12),
          List(1, 11),
          List(2, 10, 18),
          List(3, 17),
          List(4, 16),
          List(5, 15),
          List(6, 14),
          List(7, 13),
          List(9, 19)
        ),
      doubledJoinedHexs.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC   -> 8,
            SideBD   -> 12
          )
        ),
      checkIndicesForAllRotationsAndReflections(doubledJoinedHexs),
      checkEquivalencesForAllRotationsAndReflections(doubledJoinedHexs, 9, isShifted = true)
    )
  }

  it should "be true for a 2x2 joined regular hexagons boundary" in {

    /** <img src="file:../../../../../../resources/simple/fourJoinedHexs.svg"/> */
    val fourJoinedHexs: SimplePolygon =
      SimplePolygon(120, 120, 240, 120, 120, 240, 120, 120, 120, 240, 120, 120, 240, 120)
    allAssert(
      fourJoinedHexs.parallelogonIndices shouldBe Some((0, 3, 7, 10)),
      fourJoinedHexs.parallelogonEquivalences shouldBe
        List(
          List(0, 4, 10),
          List(1, 9),
          List(2, 8),
          List(3, 7, 11),
          List(5, 13),
          List(6, 12)
        ),
      fourJoinedHexs.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC   -> 4,
            SideBD   -> 10
          )
        ),
      checkIndicesForAllRotationsAndReflections(fourJoinedHexs),
      checkEquivalencesForAllRotationsAndReflections(fourJoinedHexs, 6, isShifted = true)
    )
  }

  it should "be true for a carved boundary" in {

    /** <img src="file:../../../../../../resources/simple/carved.svg"/> */
    val carved: SimplePolygon =
      SimplePolygon(120, 180, 120, 180, 120, 240, 120, 60, 240, 180, 120, 120, 240, 120)
    allAssert(
      carved.parallelogonIndices shouldBe Some((0, 3, 7, 10)),
      carved.parallelogonEquivalences shouldBe
        List(
          List(0, 4, 10),
          List(1, 9),
          List(2, 8),
          List(3, 7, 11),
          List(5, 13),
          List(6, 12)
        ),
      carved.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC   -> 4,
            SideBD   -> 10
          )
        ),
      checkIndicesForAllRotationsAndReflections(carved),
      checkEquivalencesForAllRotationsAndReflections(carved, 6, isShifted = true)
    )
  }

  it should "be true for a 8x8 joined regular hexagons boundary" in {
    val sixtyFourJoinedHexs: SimplePolygon =
      TilingBuilder.createHexagonNet(8, 8).boundarySimplePolygon
    allAssert(
      sixtyFourJoinedHexs.parallelogonIndices shouldBe Some((0, 13, 31, 41)),
      sixtyFourJoinedHexs.parallelogonEquivalences should contain(List(6, 22, 38)),
      sixtyFourJoinedHexs.parallelogonEquivalences should contain(List(7, 37, 53)),
      sixtyFourJoinedHexs.parallelogonEquivalences shouldBe
        List(
          List(0, 28),
          List(1, 27),
          List(2, 26),
          List(3, 25),
          List(4, 24),
          List(5, 23),
          List(6, 22, 38),
          List(7, 37, 53),
          List(8, 52),
          List(9, 51),
          List(10, 50),
          List(11, 49),
          List(12, 48),
          List(13, 47),
          List(14, 46),
          List(15, 45),
          List(16, 44),
          List(17, 43),
          List(18, 42),
          List(19, 41),
          List(20, 40),
          List(21, 39),
          List(29, 61),
          List(30, 60),
          List(31, 59),
          List(32, 58),
          List(33, 57),
          List(34, 56),
          List(35, 55),
          List(36, 54)
        ),
      sixtyFourJoinedHexs.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 6,
            SideAC   -> 22,
            SideBD   -> 38
          )
        ),
      checkIndicesForAllRotationsAndReflections(sixtyFourJoinedHexs),
      checkEquivalencesForAllRotationsAndReflections(sixtyFourJoinedHexs, 30, isShifted = true)
    )
  }

  /** <img src="file:../../../../../../resources/simple/bulb-4.8.8.svg"/> */
  val bulb: SimplePolygon =
    SimplePolygon(90, 90, 225, 135, 135, 135, 135, 135, 135, 225)

  it should "be true for a 4.8.8 tessellation unit" in {
    allAssert(
      bulb.parallelogonIndices shouldBe Some((0, 1, 5, 6)),
      bulb.parallelogonEquivalences shouldBe
        List(
          List(0, 3, 6),
          List(1, 5, 8),
          List(2, 7),
          List(4, 9)
        ),
      bulb.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC -> 3,
            SideBD -> 6
          )
        ),
      checkIndicesForAllRotationsAndReflections(bulb),
      checkEquivalencesForAllRotationsAndReflections(bulb, 4, isShifted = true)
    )
  }

  it should "be true for a doubled 4.8.8 tessellation unit" in {

    /** <img src="file:../../../../../../resources/simple/doubledOctagonRoot.svg"/> */
    val doubledBulb: SimplePolygon =
      bulb.multiplySidesBy(2)
    allAssert(
      doubledBulb.parallelogonIndices shouldBe Some((0, 2, 10, 12)),
      doubledBulb.parallelogonEquivalences shouldBe
        List(
          List(0, 6, 12),
          List(1, 11),
          List(2, 10, 16),
          List(3, 15),
          List(4, 14),
          List(5, 13),
          List(7, 19),
          List(8, 18),
          List(9, 17)
        ),
      doubledBulb.parallelogonTranslationIndices shouldBe
        Option(
          Map(
            Identity -> 0,
            SideAC -> 6,
            SideBD -> 12
          )
        ),
      checkIndicesForAllRotationsAndReflections(doubledBulb),
      checkEquivalencesForAllRotationsAndReflections(doubledBulb, 9, isShifted = true)
    )
  }

  behavior of "RegularPolygon"

  it should "be created with a valid number of sides" in
    allAssert(
      RegularPolygon(3).toSides shouldBe 3,
      RegularPolygon(4).toSides shouldBe 4,
      RegularPolygon(10).toSides shouldBe 10
    )

  it should "fail to be created with fewer than 3 sides" in
    allAssert(
      an[IllegalArgumentException] should be thrownBy RegularPolygon(2),
      an[IllegalArgumentException] should be thrownBy RegularPolygon(0),
      an[IllegalArgumentException] should be thrownBy RegularPolygon(-5)
    )

  it should "return the correct number of sides via toSides" in {
    val hexagon = RegularPolygon(6)
    hexagon.toSides shouldBe 6
  }

  it should "calculate the correct interior angle in degrees" in
    allAssert(
      RegularPolygon(3).alpha.toRational shouldBe Rational(60), // Triangle
      RegularPolygon(4).alpha.toRational shouldBe Rational(90), // Square
      RegularPolygon(6).alpha.toRational shouldBe Rational(120) // Hexagon
    )

  // New: angles vector size and uniformity
  it should "produce a vector of sides angles each equal to alpha" in {
    val n    = 7
    val poly = RegularPolygon(n)
    val as   = poly.angles
    allAssert(
      as.size shouldBe n,
      all(as.map(_.toRational)) shouldBe poly.alpha.toRational
    )
  }

  // New: sum of angles equals alphaSum(n)
  it should "have total interior angle sum equal to alphaSum(n)" in {
    val n   = 9
    val sum = RegularPolygon(n).angles.map(_.toRational).reduce(_ + _)
    sum shouldBe SimplePolygon.alphaSum(n).toRational
  }

  // New: sanity for larger n
  it should "produce sensible alpha for large n" in {
    val n     = 1000
    val alpha = RegularPolygon(n).alpha.toRational
    // alpha approaches 180 as n grows; it must be less than 180
    allAssert(
      assert(alpha < Rational(180)),
      assert(alpha > Rational(0))
    )
  }
