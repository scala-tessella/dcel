package io.github.scala_tessella.dcel

import BigDecimalGeometry.*
import BigRadian.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.Rational
import spire.implicits.*

class BigDecimalGeometrySpec extends AnyFlatSpec with Matchers {

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
    val p_polar = BigPoint.createPolar(BigDecimal(1.0), BigRadian.TAU_4)
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
    BigPoint.orientation(p0, p1, p3) shouldBe 2 // counterclockwise
    BigPoint.orientation(p0, p3, p1) shouldBe 1 // clockwise
    BigPoint.orientation(p0, p1, BigPoint(2, 0)) shouldBe 0 // collinear
  }

  it should "check if a point is on a segment" in {
    BigPoint.onSegment(p0, p1, BigPoint(2, 0)) shouldBe true
    BigPoint.onSegment(p0, p3, BigPoint(2, 0)) shouldBe false
  }

  it should "check if a polygon is simple" in {
    val square = List(p0, p1, p2, p3)
    BigPoint.isSimple(square) shouldBe true
    val hourglass = List(p0, p2, p1, p3)
    BigPoint.isSimple(hourglass) shouldBe false
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
    BigLineSegment.doIntersect(s_diag, BigLineSegment(p1, p3)) shouldBe true
    // No intersection
    BigLineSegment.doIntersect(s_horiz, BigLineSegment(p3, p2)) shouldBe false
    // Collinear and overlapping
    BigLineSegment.doIntersect(BigLineSegment(p0, p2), BigLineSegment(BigPoint(0.5, 0.5), BigPoint(2, 2))) shouldBe true
    // Collinear and not overlapping
    BigLineSegment.doIntersect(s_horiz, BigLineSegment(BigPoint(2, 0), BigPoint(3, 0))) shouldBe false
    // Touching at endpoint
    BigLineSegment.doIntersect(s_horiz, s_vert) shouldBe true
  }

  behavior of "BigBox"

  private val box = BigBox(0, 1, 0, 1)

  it should "check if it contains a point" in {
    box.contains(BigPoint(0.5, 0.5)) shouldBe true
    box.contains(p0) shouldBe true // on boundary
    box.contains(p2) shouldBe true // on boundary
    box.contains(BigPoint(1.1, 0.5)) shouldBe false
  }

  it should "be enlarged" in {
    val enlargedBox = box.enlarge(BigDecimal(0.5))
    enlargedBox.x0 shouldBe BigDecimal(-0.5)
    enlargedBox.x1 shouldBe BigDecimal(1.5)
    enlargedBox.y0 shouldBe BigDecimal(-0.5)
    enlargedBox.y1 shouldBe BigDecimal(1.5)
    enlargedBox.contains(BigPoint(1.5, 1.5)) shouldBe true
  }
}
