package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.{GeometryError, SpatialError, TilingTestHelpers}
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon, SimplePolygon}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.*

class PolygonSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "SimplePolygon.apply"

  val squareDegrees: Vector[Int] = Vector(90, 90, 90, 90)

  it should "validate the angles of a correct simple polygon" in:
    SimplePolygon(squareDegrees*).toAngles.nonEmpty shouldBe true

  val triangleDegrees: Vector[Int] = Vector(60, 60, 60)

  it should "validate the angles of another correct simple polygon" in:
    SimplePolygon(triangleDegrees*).toAngles.nonEmpty shouldBe true

  // New: minimal length constraints
  it should "reject polygons with fewer than 3 angles" in:
    val error = GeometryError("A simple polygon must have at least 3 sides.")
    allAssert(
      SimplePolygon.fromUntrusted(Vector.empty).left.value shouldBe error,
      SimplePolygon.fromUntrusted(Vector(AngleDegree(180))).left.value shouldBe error,
      SimplePolygon.fromUntrusted(Vector(AngleDegree(100), AngleDegree(80))).left.value shouldBe error
    )

  // New: normalization handling (negative and >180 accepted only via normalisation if sum matches)
  it should "accept angles that normalise to a valid simple polygon" in:
    val weird = Vector(
      AngleDegree(-300),
      AngleDegree(420),
      AngleDegree(60)
    ) // normalised -> 60,60,60 (sum 180) -> triangle
    SimplePolygon(weird).toAngles.size shouldBe 3

  it should "invalidate angles if their sum is incorrect" in:
    // Sum is too small
    val wrongDegrees = Vector(90, 90, 90, 89)
    SimplePolygon.fromUntrusted(wrongDegrees*).left.value shouldBe
      GeometryError(
        "The sum of interior angles is incorrect for a polygon with 4 unit sides. Expected 360,00, but got 359,00."
      )

  it should "invalidate angles if any angle is a full circle" in:
    val withFullCircleDegrees = Vector(360, 0, 90, -90)
    SimplePolygon.fromUntrusted(withFullCircleDegrees*).left.value shouldBe
      GeometryError("The polygon cannot have full circles as interior angles.")

  // New: toAngles preserves order and content
  it should "preserve the provided angles order and size" in:
    val angles = Vector(AngleDegree(60), AngleDegree(120), AngleDegree(60), AngleDegree(120))
    val simple = SimplePolygon(angles)
    simple.toAngles shouldBe angles

  // New: alphaSum helper
  it should "compute alphaSum correctly" in
    allAssert(
      SimplePolygon.alphaSum(3).toRational shouldBe Rational(180),
      SimplePolygon.alphaSum(4).toRational shouldBe Rational(360),
      SimplePolygon.alphaSum(6).toRational shouldBe Rational(720)
    )

  it should "reject a polygon with self-intersecting edges" in:
    val selfIntersectingRingDegrees =
      Vector(90, 180, 180, 90, 180, 180, 90, 150, 60, 240, 270, 270, 240, 60, 150, 90, 180, 180)
    SimplePolygon.fromUntrusted(selfIntersectingRingDegrees*).left.value shouldBe
      SpatialError("The polygon is self-intersecting.")

  it should "reject a polygon self-intersecting at vertex" in:
    val selfIntersectingHexagonDegrees =
      Vector(60, 60, 240, 60, 60, 240)
    SimplePolygon.fromUntrusted(selfIntersectingHexagonDegrees*).left.value shouldBe
      SpatialError("The polygon is self-intersecting.")

  it should "reject a polygon which does not close" in:
    // These pentagon angles sum to 540 degrees, which is correct for a pentagon ((5-2)*180),
    // but the sequence of angles does not form a closed polygon with unit-length sides.
    val nonClosingDegrees =
      Vector(90, 90, 135, 135, 90)
    SimplePolygon.fromUntrusted(nonClosingDegrees*).left.value shouldBe
      SpatialError("The polygon does not close. The final edge has length 1,8478 instead of 1.0.")

  behavior of "SimplePolygon.multiplySidesBy"

  it should "triplicate a triangle" in:
    SimplePolygon(triangleDegrees*).multiplySidesBy(3) shouldBe
      Vector(60, 180, 180, 60, 180, 180, 60, 180, 180).map: degree =>
        AngleDegree(degree)

  it should "remain the same when multiplied by one" in:
    SimplePolygon(triangleDegrees*).multiplySidesBy(1) shouldBe
      triangleDegrees.map: degree =>
        AngleDegree(degree)

  it should "fail if factor is < 1" in:
    the[IllegalArgumentException] thrownBy
      SimplePolygon(triangleDegrees*).multiplySidesBy(0) should have message
      "A simple polygon must have sides of at least unit length."

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

  it should "return the correct number of sides via toSides" in:
    val hexagon = RegularPolygon(6)
    hexagon.toSides shouldBe 6

  it should "calculate the correct interior angle in degrees" in
    allAssert(
      RegularPolygon(3).alpha.toRational shouldBe Rational(60), // Triangle
      RegularPolygon(4).alpha.toRational shouldBe Rational(90), // Square
      RegularPolygon(6).alpha.toRational shouldBe Rational(120) // Hexagon
    )

  // New: angles vector size and uniformity
  it should "produce a vector of sides angles each equal to alpha" in:
    val n    = 7
    val poly = RegularPolygon(n)
    val as   = poly.angles
    allAssert(
      as.size shouldBe n,
      all(as.map(_.toRational)) shouldBe poly.alpha.toRational
    )

  // New: sum of angles equals alphaSum(n)
  it should "have total interior angle sum equal to alphaSum(n)" in:
    val n   = 9
    val sum = RegularPolygon(n).angles.map(_.toRational).reduce(_ + _)
    sum shouldBe SimplePolygon.alphaSum(n).toRational

  // New: sanity for larger n
  it should "produce sensible alpha for large n" in:
    val n     = 1000
    val alpha = RegularPolygon(n).alpha.toRational
    // alpha approaches 180 as n grows; it must be less than 180
    allAssert(
      assert(alpha < Rational(180)),
      assert(alpha > Rational(0))
    )
