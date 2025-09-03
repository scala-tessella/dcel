package dcel

import dcel.BigDecimalGeometry.*
import dcel.BigDecimalGeometry.BigRadian.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.Rational
import spire.implicits.*

class BigDecimalGeometrySpec extends AnyFlatSpec with Matchers:

  private val accuracy = 1.0E-12
  private val bigDecimalAccuracy = BigDecimal(accuracy)

  // Helper to compare BigDecimals with tolerance
  private def approx(a: BigDecimal, b: BigDecimal): Boolean = {
    (a - b).abs < bigDecimalAccuracy
  }

  behavior of "AngleDegree"

  it should "be created from Int and Rational" in {
    AngleDegree(90).toRational shouldBe Rational(90)
    AngleDegree(Rational(45, 2)).toRational shouldBe Rational(45, 2)
  }

  it should "support arithmetic operations" in {
    val a90 = AngleDegree(90)
    val a45 = AngleDegree(45)
    (a90 + a45).toRational shouldBe Rational(135)
    (a90 - a45).toRational shouldBe Rational(45)
    a90.inverted.toRational shouldBe Rational(-90)
    (a45 * 2).toRational shouldBe Rational(90)
    (a90 / 2).toRational shouldBe Rational(45)
  }

  it should "convert to BigRadian correctly" in {
    AngleDegree(180).toBigRadian.almostEquals(TAU_2, accuracy) shouldBe true
    AngleDegree(90).toBigRadian.almostEquals(TAU_4, accuracy) shouldBe true
    AngleDegree(0).toBigRadian.almostEquals(BigRadian(0.0), accuracy) shouldBe true
  }

  it should "convert back losing precision" in {
    AngleDegree(AngleDegree(180).toBigRadian) should not be AngleDegree(180)
    AngleDegree(AngleDegree(90).toBigRadian) should not be AngleDegree(90)
    AngleDegree(AngleDegree(0).toBigRadian) shouldBe AngleDegree(0)
  }

  it should "check if an angle is a full circle" in {
    AngleDegree(360).isFullCircle shouldBe true
    AngleDegree(720).isFullCircle shouldBe true
    AngleDegree(0).isFullCircle shouldBe true
    AngleDegree(-360).isFullCircle shouldBe true
    AngleDegree(180).isFullCircle shouldBe false
    AngleDegree(361).isFullCircle shouldBe false
    AngleDegree(Rational(360)).isFullCircle shouldBe true
    AngleDegree(Rational(1, 2)).isFullCircle shouldBe false
  }

  it should "normalise an angle to be >= 0 and < 360" in {
    AngleDegree(450).normalised.toRational shouldBe Rational(90)
    AngleDegree(360).normalised.toRational shouldBe Rational(0)
    AngleDegree(90).normalised.toRational shouldBe Rational(90)
    AngleDegree(0).normalised.toRational shouldBe Rational(0)
    AngleDegree(-90).normalised.toRational shouldBe Rational(270)
    AngleDegree(-360).normalised.toRational shouldBe Rational(0)
    AngleDegree(-450).normalised.toRational shouldBe Rational(270)
  }

  behavior of "BigRadian"

  it should "support arithmetic and comparison" in {
    val pi = BigRadian.TAU_2
    val half_pi = BigRadian.TAU_4
    (half_pi + half_pi).almostEquals(pi, accuracy) shouldBe true
    (pi - half_pi).almostEquals(half_pi, accuracy) shouldBe true
    (half_pi * 2).almostEquals(pi, accuracy) shouldBe true
    (pi / 2).almostEquals(half_pi, accuracy) shouldBe true
  }

  behavior of "BigPoint"

  private val p0 = BigPoint(0, 0)
  private val p1 = BigPoint(1, 0)
  private val p2 = BigPoint(1, 1)
  private val p3 = BigPoint(0, 1)

  it should "be created and manipulated" in {
    p0.x shouldBe 0
    p1.plus(p3) shouldBe p2
  }

  it should "check for approximate equality" in {
    val p1_approx = BigPoint(1 + accuracy / 2, 0)
    p1.almostEquals(p1_approx, accuracy) shouldBe true
    p1.almostEquals(p2, accuracy) shouldBe false
  }

  it should "be created from polar coordinates" in {
    val p_polar = BigPoint.fromPolar(BigDecimal(1.0), BigRadian.TAU_4)
    p_polar.almostEquals(p3, accuracy) shouldBe true
  }

  it should "be moved by polar coordinates" in {
    val p_moved = p1.plusPolar(BigDecimal(1.0))(BigRadian.TAU_4)
    p_moved.almostEquals(p2, accuracy) shouldBe true
  }

  it should "calculate distance to another point" in {
    approx(p0.distanceTo(p1), BigDecimal(1.0)) shouldBe true
    approx(p0.distanceTo(p2), spire.math.sqrt(BigDecimal(2.0))) shouldBe true
  }

  it should "calculate angle to another point" in {
    p0.angleTo(p1).almostEquals(BigRadian(0), accuracy) shouldBe true
    p0.angleTo(p3).almostEquals(BigRadian.TAU_4, accuracy) shouldBe true
    p0.angleTo(BigPoint(-1, 0)).almostEquals(BigRadian.TAU_2, accuracy) shouldBe true
  }

  it should "determine orientation of three points" in {
    BigPoint.orientation(p0, p1, p3) shouldBe Orientation.Counterclockwise
    BigPoint.orientation(p0, p3, p1) shouldBe Orientation.Clockwise
    BigPoint.orientation(p0, p1, BigPoint(2, 0)) shouldBe Orientation.Collinear
  }

  it should "check if a point is on a segment" in {
    BigPoint.onSegment(p0, p1, BigPoint(2, 0)) shouldBe true
    BigPoint.onSegment(p0, p3, BigPoint(2, 0)) shouldBe false
  }

  it should "check if a polygon is simple" in {
    val square = List(p0, p1, p2, p3)
    square.isSimplePolygon shouldBe true
    val hourglass = List(p0, p2, p1, p3)
    hourglass.isSimplePolygon shouldBe false
  }

  it should "check for almost equal points in a list" in {
    val acc = 0.1
    val p_A = BigPoint(1.0, 1.0)
    val p_B = BigPoint(2.0, 2.0)
    val p_C = BigPoint(3.0, 3.0)
    val p_A_almost = BigPoint(1.0 + acc / 2, 1.0)

    // No duplicates
    List(p_A, p_B, p_C).hasNoAlmostEqualPoints(acc) shouldBe true

    // Empty and single-element lists
    List.empty.hasNoAlmostEqualPoints(acc) shouldBe true
    List(p_A).hasNoAlmostEqualPoints(acc) shouldBe true

    // With identical points
    List(p_A, p_B, p_A).hasNoAlmostEqualPoints(acc) shouldBe false

    // With almost-equal points
    List(p_A, p_B, p_A_almost).hasNoAlmostEqualPoints(acc) shouldBe false

    // Points that are close but outside the accuracy
    val p_A_close = BigPoint(1.0 + acc * 1.5, 1.0)
    List(p_A, p_B, p_A_close).hasNoAlmostEqualPoints(acc) shouldBe true

    // Test case with points in adjacent cells that are almost equal
    val p_D = BigPoint(1.0, 1.0)
    val p_E = BigPoint(1.0 + acc * 0.9, 1.0 + acc * 0.9) // adjacent cell, but almost equal
    List(p_D, p_E).hasNoAlmostEqualPoints(acc) shouldBe false

    // Test case with points in adjacent cells that are not almost equal
    val large_acc = 1.0
    val p_F = BigPoint(0.1, 0.1)
    val p_G = BigPoint(0.1, 1.2) // p_G is in an adjacent cell to p_F
    List(p_F, p_G).hasNoAlmostEqualPoints(large_acc) shouldBe true
  }

  behavior of "BigLineSegment"

  private val s_diag = BigLineSegment(p0, p2)
  private val s_horiz = BigLineSegment(p0, p1)
  private val s_vert = BigLineSegment(p1, p2)

  it should "calculate its length" in {
    approx(s_diag.length, spire.math.sqrt(BigDecimal(2.0))) shouldBe true
    approx(s_horiz.length, BigDecimal(1.0)) shouldBe true
  }

  it should "calculate its midpoint" in {
    s_diag.midPoint.almostEquals(BigPoint(0.5, 0.5), accuracy) shouldBe true
  }

  it should "calculate its horizontal angle" in {
    s_diag.horizontalAngle.almostEquals(BigRadian.TAU_4 / 2, accuracy) shouldBe true
    s_horiz.horizontalAngle.almostEquals(BigRadian(0), accuracy) shouldBe true
  }

  it should "check for intersection with another segment" in {
    // General case intersection
    s_diag.intersects(BigLineSegment(p1, p3)) shouldBe true
    // No intersection
    s_horiz.intersects(BigLineSegment(p3, p2)) shouldBe false
    // Collinear and overlapping
    BigLineSegment(p0, p2).intersects(BigLineSegment(BigPoint(0.5, 0.5), BigPoint(2, 2))) shouldBe true
    // Collinear and not overlapping
    s_horiz.intersects(BigLineSegment(BigPoint(2, 0), BigPoint(3, 0))) shouldBe false
    // Touching at endpoint
    s_horiz.intersects(s_vert) shouldBe true
  }

  behavior of "BigBox"

  private val box = BigBox(0, 0, 1, 1)

  it should "check if it contains a point" in {
    box.contains(BigPoint(0.5, 0.5)) shouldBe true
    box.contains(p0) shouldBe true // on boundary
    box.contains(p2) shouldBe true // on boundary
    box.contains(BigPoint(1.1, 0.5)) shouldBe false
  }

  it should "be enlarged" in {
    val enlargedBox = box.expand(BigDecimal(0.5))
    enlargedBox.minX shouldBe BigDecimal(-0.5)
    enlargedBox.maxX shouldBe BigDecimal(1.5)
    enlargedBox.minY shouldBe BigDecimal(-0.5)
    enlargedBox.maxY shouldBe BigDecimal(1.5)
    enlargedBox.contains(BigPoint(1.5, 1.5)) shouldBe true
  }
