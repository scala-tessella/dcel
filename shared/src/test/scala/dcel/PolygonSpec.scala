package dcel

import dcel.BigDecimalGeometry.{ACCURACY, AngleDegree, BigRadian}
import dcel.Polygon.{RegularPolygon, SimplePolygon}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.*

class PolygonSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private val bigDecimalAccuracy = BigDecimal(ACCURACY)

  private def approx(a: BigDecimal, b: BigDecimal): Boolean =
    (a - b).abs < bigDecimalAccuracy

  behavior of "SimplePolygon.validatePolygonAngles"

  it should "validate the angles of a correct simple polygon" in {
    val squareAngles = List(AngleDegree(90), AngleDegree(90), AngleDegree(90), AngleDegree(90))
    SimplePolygon.validatePolygonAngles(squareAngles) should be(Right(()))
  }

  it should "validate the angles of another correct simple polygon" in {
    val triangleAngles = List(AngleDegree(60), AngleDegree(60), AngleDegree(60))
    SimplePolygon.validatePolygonAngles(triangleAngles) should be(Right(()))
  }

  it should "invalidate angles if their sum is incorrect" in {
    // Sum is too small
    val wrongAngles = List(AngleDegree(90), AngleDegree(90), AngleDegree(90), AngleDegree(89))
    val result      = SimplePolygon.validatePolygonAngles(wrongAngles)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("The sum of interior angles is incorrect")
    )
  }

  it should "invalidate angles if any angle is a full circle" in {
    val anglesWithFullCircle = List(AngleDegree(360), AngleDegree(0), AngleDegree(90), AngleDegree(-90))
    val result               = SimplePolygon.validatePolygonAngles(anglesWithFullCircle)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("cannot have full circles as interior angles")
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
