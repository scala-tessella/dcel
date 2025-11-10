package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.TilingTestHelpers
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon, SimplePolygon}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.*

class PolygonSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "SimplePolygon.apply"

  it should "validate the angles of a correct simple polygon" in {
    val squareAngles = Vector(AngleDegree(90), AngleDegree(90), AngleDegree(90), AngleDegree(90))
    SimplePolygon(squareAngles).toAngles.nonEmpty shouldBe true
  }

  it should "validate the angles of another correct simple polygon" in {
    val triangleAngles = Vector(AngleDegree(60), AngleDegree(60), AngleDegree(60))
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

  behavior of "SimplePolygon.parallelogonIndices"

  it should "be found for a square" in {
    val squareAngles = Vector.fill(4)(AngleDegree(90))
    SimplePolygon(squareAngles).parallelogonIndices shouldBe Some((0, 1, 2, 3))
  }

  it should "be found for a regular pentagon" in {
    val pentagonAngles =
      Vector.fill(5)(AngleDegree(108))
    SimplePolygon(pentagonAngles).parallelogonIndices shouldBe None
  }

  it should "be found for a 2x2 square" in {
    val angles =
      Vector.fill(4)(Vector(AngleDegree(90), AngleDegree(180))).flatten
    SimplePolygon(angles).parallelogonIndices shouldBe Some((0, 2, 4, 6))
  }

  it should "be found for a 2x2 square with shifted angles" in {
    val angles =
      Vector.fill(4)(Vector(AngleDegree(180), AngleDegree(90))).flatten
    SimplePolygon(angles).parallelogonIndices shouldBe Some((1, 3, 5, 7))
  }

  it should "be found for a 3x3 square" in {
    val angles =
      Vector.fill(4)(Vector(AngleDegree(90), AngleDegree(180), AngleDegree(180))).flatten
    SimplePolygon(angles).parallelogonIndices shouldBe Some((0, 3, 6, 9))
  }

  it should "be found for a regular hexagon" in {
    val hexagonAngles =
      Vector.fill(6)(AngleDegree(120))
    SimplePolygon(hexagonAngles).parallelogonIndices shouldBe None
  }

  it should "be found for a scale" in {
    val angles =
      Vector(90, 150, 120, 150, 90, 210, 60, 210).map(AngleDegree(_))
    SimplePolygon(angles).parallelogonIndices shouldBe Some((0, 2, 4, 6))
  }

  it should "be found for a 1x2 rectangle" in {
    val angles =
      Vector.fill(2)(Vector(AngleDegree(90), AngleDegree(90), AngleDegree(180))).flatten
    SimplePolygon(angles).parallelogonIndices shouldBe Some((0, 1, 3, 4))
  }

  it should "be found for a 2x1 parallelogram" in {
    val angles =
      Vector.fill(2)(Vector(AngleDegree(60), AngleDegree(120), AngleDegree(180))).flatten
    SimplePolygon(angles).parallelogonIndices shouldBe Some((0, 1, 3, 4))
  }

  it should "be found for a 2 joined regular hexagons boundary" in {
    val angles =
      Vector.fill(2)(Vector(
        AngleDegree(120),
        AngleDegree(120),
        AngleDegree(120),
        AngleDegree(120),
        AngleDegree(240)
      )).flatten
    SimplePolygon(angles).parallelogonIndices shouldBe Some((0, 1, 5, 6))
  }

  behavior of "SimplePolygon.canTileTorus"

  it should "be true for a square" in {
    val squareAngles = Vector.fill(4)(AngleDegree(90))
    SimplePolygon(squareAngles).canTileTorus shouldBe true
  }

  it should "be false for a regular pentagon" in {
    val pentagonAngles =
      Vector.fill(5)(AngleDegree(108))
    SimplePolygon(pentagonAngles).canTileTorus shouldBe false
  }

  it should "be true for a 2x2 square" in {
    val angles =
      Vector.fill(4)(Vector(AngleDegree(90), AngleDegree(180))).flatten
    SimplePolygon(angles).canTileTorus shouldBe true
  }

  it should "be false for a regular hexagon" in {
    val hexagonAngles =
      Vector.fill(6)(AngleDegree(120))
    SimplePolygon(hexagonAngles).canTileTorus shouldBe false
  }

  it should "be true for a scale" in {
    val angles =
      Vector(90, 150, 120, 150, 90, 210, 60, 210).map(AngleDegree(_))
    SimplePolygon(angles).canTileTorus shouldBe true
  }

  it should "be true for a 1x2 rectangle" in {
    val angles =
      Vector.fill(2)(Vector(AngleDegree(90), AngleDegree(90), AngleDegree(180))).flatten
    SimplePolygon(angles).canTileTorus shouldBe true
  }

  it should "be true for a 2x1 parallelogram" in {
    val angles =
      Vector.fill(2)(Vector(AngleDegree(60), AngleDegree(120), AngleDegree(180))).flatten
    SimplePolygon(angles).canTileTorus shouldBe true
  }

  it should "be true for a 2 joined regular hexagons boundary" in {
    val angles =
      Vector.fill(2)(Vector(
        AngleDegree(120),
        AngleDegree(120),
        AngleDegree(120),
        AngleDegree(120),
        AngleDegree(240)
      )).flatten
    SimplePolygon(angles).canTileTorus shouldBe true
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
