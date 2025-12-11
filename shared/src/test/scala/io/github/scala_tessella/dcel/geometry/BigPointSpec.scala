package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.TilingTestHelpers
import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.{ACCURACY, Orientation}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BigPointSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private val acc = BigDecimal(1e-12)

  behavior of "BigPoint construction and accessors"

  it should "construct from BigDecimal and expose x/y" in:
    val p = BigPoint(BigDecimal(1), BigDecimal(2))
    allAssert(
      p.x shouldBe BigDecimal(1),
      p.y shouldBe BigDecimal(2)
    )

  it should "provide origin constant (0,0)" in
    allAssert(
      BigPoint.origin.x shouldBe BigDecimal(0),
      BigPoint.origin.y shouldBe BigDecimal(0)
    )

  behavior of "BigPoint basic operations"

  it should "add and subtract points" in:
    val a = BigPoint(1, 2)
    val b = BigPoint(3, 4)
    val s = a + b
    val d = b - a
    allAssert(
      s.x shouldBe BigDecimal(4),
      s.y shouldBe BigDecimal(6),
      d.x shouldBe BigDecimal(2),
      d.y shouldBe BigDecimal(2)
    )

  it should "compute dot and cross products" in:
    val a = BigPoint(1, 2)
    val b = BigPoint(3, 4)
    allAssert(
      a.dot(b) shouldBe BigDecimal(11),
      a.cross(b) shouldBe BigDecimal(-2)
    )

  it should "scale by Double and keep coordinates proportional" in:
    val p = BigPoint(2, -3).scaled(2.5)
    allAssert(
      p.x shouldBe BigDecimal(5.0),
      p.y shouldBe BigDecimal(-7.5)
    )

  it should "flip Y coordinate" in:
    BigPoint(5, 7).flippedY shouldEqual BigPoint(5, -7)

  behavior of "BigPoint approximate equality"

  it should "detect almost equal within default accuracy" in:
    val a = BigPoint(1.0, 2.0)
    val b = BigPoint(1.0 + ACCURACY / 2, 2.0 - ACCURACY / 2)
    val c = BigPoint(1.0 + ACCURACY * 2, 2.0)
    allAssert(
      a.almostEquals(b) shouldBe true,
      a.almostEquals(c) shouldBe false
    )

  it should "respect custom accuracy" in:
    val a = BigPoint(0.0, 0.0)
    val b = BigPoint(1e-4, -1e-4)
    allAssert(
      a.almostEquals(b, accuracy = 1e-3) shouldBe true,
      a.almostEquals(b, accuracy = 1e-5) shouldBe false
    )

  behavior of "BigPoint polar utilities"

  it should "create point from polar coordinates" in:
    val p = BigPoint.fromPolar(BigDecimal(2.0), BigRadian.TAU_4) // (cos pi/2, sin pi/2) = (0, 1)
    allAssert(
      p.x.abs shouldBe <=(acc),
      (p.y - BigDecimal(2.0)).abs shouldBe <=(acc)
    )

  it should "move by polar vector with plusPolar and plusPolarUnit" in:
    val p      = BigPoint(1.0, 1.0)
    val by1    = p.plusPolar(BigDecimal(2.0))(BigRadian(0.0)) // +x
    val byUnit = p.plusPolarUnit(BigRadian.TAU_4)             // +y
    allAssert(
      (by1.x - BigDecimal(3.0)).abs <= acc shouldBe true,
      (by1.y - BigDecimal(1.0)).abs <= acc shouldBe true,
      (byUnit.x - BigDecimal(1.0)).abs <= acc shouldBe true,
      (byUnit.y - BigDecimal(2.0)).abs <= acc shouldBe true
    )

  behavior of "BigPoint orientation and onSegment"

  it should "classify orientation of triplets" in:
    val p = BigPoint(0, 0)
    val q = BigPoint(1, 1)
    val r = BigPoint(2, 2) // collinear
    val s = BigPoint(2, 1) // clockwise relative to p->q
    val t = BigPoint(1, 2) // counterclockwise
    allAssert(
      BigPoint.orientation(p, q, r) shouldBe Orientation.Collinear,
      BigPoint.orientation(p, q, s) shouldBe Orientation.Clockwise,
      BigPoint.orientation(p, q, t) shouldBe Orientation.Counterclockwise
    )

  it should "detect point on segment for collinear points" in:
    val p = BigPoint(0, 0)
    val r = BigPoint(4, 4)
    val q = BigPoint(2, 2)
    val o = BigPoint(5, 5)
    allAssert(
      BigPoint.onSegment(p, q, r) shouldBe true,
      BigPoint.onSegment(p, o, r) shouldBe false
    )

  behavior of "BigPoint geometric helpers"

  it should "compute angleTo and distanceTo consistently with a segment" in:
    val a = BigPoint(0, 0)
    val b = BigPoint(0, 3)
    allAssert(
      a.distanceTo(b) shouldBe BigDecimal(3),
      a.angleTo(b).almostEquals(BigRadian.TAU_4, 1e-12) shouldBe true
    )

  behavior of "List[BigPoint] extensions"

  it should "compute centroid for non-empty and return origin for empty" in:
    val pts = List(BigPoint(0, 0), BigPoint(2, 0), BigPoint(2, 2), BigPoint(0, 2))
    val c   = pts.centroid
    val e   = List.empty[BigPoint].centroid
    allAssert(
      c.x shouldBe BigDecimal(1),
      c.y shouldBe BigDecimal(1),
      e shouldBe BigPoint.origin
    )

  it should "check for absence of almost equal points (fast negative) and detect duplicates" in:
    val unique   = List(BigPoint(0, 0), BigPoint(1, 1), BigPoint(2, 2))
    val nearDup  = List(BigPoint(0, 0), BigPoint(1e-7, 1e-7))
    val exactDup = List(BigPoint(1, 1), BigPoint(1, 1))
    allAssert(
      unique.hasNoAlmostEqualPoints() shouldBe true,
      nearDup.hasNoAlmostEqualPoints(accuracy = 1e-6) shouldBe false,
      exactDup.hasNoAlmostEqualPoints(accuracy = 0.0) shouldBe false
    )

  it should "validate simple polygon detection" in:
    val square = List(BigPoint(0, 0), BigPoint(2, 0), BigPoint(2, 2), BigPoint(0, 2))
    val bow    = List(BigPoint(0, 0), BigPoint(2, 2), BigPoint(0, 2), BigPoint(2, 0)) // self-intersecting bow
    allAssert(
      square.isSimplePolygon shouldBe true,
      bow.isSimplePolygon shouldBe false
    )

  it should "compute polygon area with shoelace formula and treat degenerate inputs as zero" in:
    val triCW   = List(BigPoint(0, 0), BigPoint(4, 0), BigPoint(0, 3))                 // area 6, clockwise
    val triCCW  = List(BigPoint(0, 0), BigPoint(0, 3), BigPoint(4, 0))                 // area 6, counterclockwise
    val square  = List(BigPoint(0, 0), BigPoint(2, 0), BigPoint(2, 2), BigPoint(0, 2)) // area 4
    val colline = List(BigPoint(0, 0), BigPoint(1, 1), BigPoint(2, 2))                 // degenerate -> 0
    val twoPts  = List(BigPoint(0, 0), BigPoint(1, 1))                                 // < 3 -> 0
    val empty   = List.empty[BigPoint]                                                 // < 3 -> 0
    allAssert(
      triCW.area shouldBe BigDecimal(6),
      triCCW.area shouldBe BigDecimal(6),
      square.area shouldBe BigDecimal(4),
      colline.area shouldBe BigDecimal(0),
      twoPts.area shouldBe BigDecimal(0),
      empty.area shouldBe BigDecimal(0)
    )
