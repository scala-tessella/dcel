package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.TilingTestHelpers
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon, SimplePolygon}
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

  it should "be found for a square" in {
    allAssert(
      SimplePolygon(squareAngles).parallelogonIndices shouldBe Some((0, 1, 2, 3)),
      SimplePolygon(squareAngles).parallelogonEquivalences shouldBe
      List(
        List(0, 1, 2, 3)
      )
    )
  }

  it should "be found for a regular pentagon" in {
    SimplePolygon(RegularPolygon(5).angles).parallelogonIndices shouldBe None
  }

  it should "be found for a 2x2 square" in {
    allAssert(
      SimplePolygon(squareAngles).multiplySidesBy(2).parallelogonIndices shouldBe Some((0, 2, 4, 6)),
      SimplePolygon(squareAngles).multiplySidesBy(2).parallelogonEquivalences shouldBe
        List(
          List(0, 2, 4, 6),
          List(1, 5),
          List(3, 7)
        )
    )
  }

  it should "be found for a 2x2 square with shifted angles" in {
    val angles =
      Vector.fill(4)(Vector(AngleDegree(180), AngleDegree(90))).flatten
    allAssert(
      SimplePolygon(angles).parallelogonIndices shouldBe Some((1, 3, 5, 7)),
      SimplePolygon(angles).parallelogonEquivalences shouldBe
        List(
          List(0, 4),
          List(1, 3, 5, 7),
          List(2, 6)
        )
    )
  }

  it should "be found for a 3x3 square" in {
    allAssert(
      SimplePolygon(squareAngles).multiplySidesBy(3).parallelogonIndices shouldBe Some((0, 3, 6, 9)),
      SimplePolygon(squareAngles).multiplySidesBy(3).parallelogonEquivalences shouldBe
        List(
          List(0, 3, 6, 9),
          List(1, 8),
          List(2, 7),
          List(4, 11),
          List(5, 10)
        )
    )
  }

  it should "be found for a regular hexagon" in {
    SimplePolygon(RegularPolygon(6).angles).parallelogonIndices shouldBe None
  }

  it should "be found for a scale" in {

    /** <img src="file:../../../../../../resources/simple/scale.svg"/> */
    val scale = SimplePolygon(90, 150, 120, 150, 90, 210, 60, 210)
    allAssert(
      scale.parallelogonIndices shouldBe Some((0, 2, 4, 6)),
      scale.parallelogonEquivalences shouldBe
        List(
          List(0, 2, 4, 6),
          List(1, 5),
          List(3, 7)
        )
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
        )
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
        )
    )
  }

  /** <img src="file:../../../../../../resources/simple/twoJoinedHexs.svg"/> */
  val twoJoinedHexs: SimplePolygon =
    SimplePolygon(120, 120, 240, 120, 120, 120, 120, 240, 120, 120)

  it should "be found for a 2 joined regular hexagons boundary" in {
    allAssert(
      twoJoinedHexs.parallelogonIndices shouldBe Some((0, 4, 5, 9)),
      twoJoinedHexs.parallelogonEquivalences shouldBe
        List(
          List(0, 4, 6),
          List(1, 5, 9),
          List(2, 8),
          List(3, 7)
        )
    )
  }

  it should "be found for a 2 joined regular hexagons boundary multiplied by 2" in {

    /** <img src="file:../../../../../../resources/simple/doubledJoinedHexs.svg"/> */
    val doubledJoinedHexs = twoJoinedHexs.multiplySidesBy(2)
    doubledJoinedHexs.parallelogonIndices shouldBe Some((0, 8, 10, 18))
    doubledJoinedHexs.parallelogonEquivalences shouldBe
      List()
  }

  it should "be true for a 2x2 joined regular hexagons boundary" in {

    /** <img src="file:../../../../../../resources/simple/fourJoinedHexs.svg"/> */
    val fourJoinedHexs: SimplePolygon =
      SimplePolygon(120, 120, 240, 120, 120, 240, 120, 120, 120, 240, 120, 120, 240, 120)
    allAssert(
      fourJoinedHexs.parallelogonIndices shouldBe Some((0, 3, 7, 10)),
      fourJoinedHexs.parallelogonEquivalences shouldBe
        List(
//          List(0, 3, 6, 8, 11),
//          List(1, 7, 10),
//          List(2, 9),
//          List(4, 13),
//          List(5, 12)
        )
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
