package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.BigDecimalGeometry.{ACCURACY, AngleDegree}
import io.github.scala_tessella.dcel.Polygon.{RegularPolygon, SimplePolygon}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math._

class PolygonSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private val bigDecimalAccuracy = BigDecimal(ACCURACY)

  private def approx(a: BigDecimal, b: BigDecimal): Boolean =
    (a - b).abs < bigDecimalAccuracy

  behavior of "SimplePolygon.apply"

  it should "validate the angles of a correct simple polygon" in {
    val squareAngles = Vector(AngleDegree(90), AngleDegree(90), AngleDegree(90), AngleDegree(90))
    SimplePolygon(squareAngles).toAngles.nonEmpty shouldBe true
  }

  it should "validate the angles of another correct simple polygon" in {
    val triangleAngles = Vector(AngleDegree(60), AngleDegree(60), AngleDegree(60))
    SimplePolygon(triangleAngles).toAngles.nonEmpty shouldBe true
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
