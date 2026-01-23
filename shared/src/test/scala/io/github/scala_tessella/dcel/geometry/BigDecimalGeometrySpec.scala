package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.TilingTestHelpers
import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BigDecimalGeometrySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "BigDecimalGeometry.almostEqual"

  it should "be symmetric and use absolute difference" in
    allAssert(
      BigDecimal(1.0).almostEqual(BigDecimal(1.0 + ACCURACY / 2)) shouldBe true,
      BigDecimal(1.0 + ACCURACY / 2).almostEqual(BigDecimal(1.0)) shouldBe true,
      BigDecimal(-1000).almostEqual(BigDecimal(0)) shouldBe false,
      BigDecimal(0).almostEqual(BigDecimal(-1000)) shouldBe false
    )

  behavior of "BigDecimalGeometry.format"

  it should "format with up to 6 decimals and strip trailing zeros" in
    allAssert(
      BigDecimal("0").format shouldBe "0",
      BigDecimal("1").format shouldBe "1",
      BigDecimal("1.2").format shouldBe "1.2",
      BigDecimal("1.200000").format shouldBe "1.2",
      BigDecimal("3.1415926535").format shouldBe "3.141593",
      BigDecimal("2.9999999").format shouldBe "3",
      BigDecimal("1000.000000").format shouldBe "1000"
    )

  behavior of "BigDecimalGeometry constants"

  it should "expose ACCURACY and BigAcc consistently" in
    allAssert(
//      ACCURACY shouldBe 1.0e-12 +- 0.0,
      (BigAcc - BigDecimal(ACCURACY)).abs <= BigDecimal(ACCURACY) shouldBe true
    )

  behavior of "SpatialGrid and IntersectionDetection"

  it should "return false for empty segment collections" in:
    BigDecimalGeometry.IntersectionDetection.hasProperIntersection(Nil, Nil) shouldBe false

  it should "detect no intersections for clearly disjoint sets" in:
    val a1 = BigLineSegment(BigPoint(0, 0), BigPoint(1, 0))
    val a2 = BigLineSegment(BigPoint(0, 1), BigPoint(1, 1))
    val b1 = BigLineSegment(BigPoint(10, 10), BigPoint(11, 10))
    val b2 = BigLineSegment(BigPoint(10, 11), BigPoint(11, 11))
    BigDecimalGeometry.IntersectionDetection.hasProperIntersection(
      List(a1, a2),
      List(b1, b2),
      cellSize = Some(BigDecimal(0.5))
    ) shouldBe false

  it should "detect a proper intersection between two sets" in:
    val s1 = BigLineSegment(BigPoint(0, 0), BigPoint(2, 2))
    val s2 = BigLineSegment(BigPoint(0, 2), BigPoint(2, 0))
    BigDecimalGeometry.IntersectionDetection.hasProperIntersection(
      List(s1),
      List(s2),
      cellSize = Some(BigDecimal(0.5))
    ) shouldBe true

  it should "not treat touching endpoints as proper intersections" in:
    val s1 = BigLineSegment(BigPoint(0, 0), BigPoint(1, 0))
    val s2 = BigLineSegment(BigPoint(1, 0), BigPoint(1, 1))
    BigDecimalGeometry.IntersectionDetection.hasProperIntersection(
      List(s1),
      List(s2),
      cellSize = Some(BigDecimal(0.5))
    ) shouldBe false

  it should "work with auto cell size estimation" in:
    val A = List(
      BigLineSegment(BigPoint(0, 0), BigPoint(5, 0)),
      BigLineSegment(BigPoint(0, 1), BigPoint(5, 1))
    )
    val B = List(
      BigLineSegment(BigPoint(2, -1), BigPoint(2, 2))
    )
    // A horizontal set crossed by a vertical segment at x=2
    BigDecimalGeometry.IntersectionDetection.hasProperIntersection(A, B) shouldBe true

  behavior of "IntersectionDetection.hasSelfIntersection"

  it should "return false for a collection of non-intersecting segments" in:
    val segments = List(
      BigLineSegment(BigPoint(0, 0), BigPoint(1, 0)),
      BigLineSegment(BigPoint(0, 1), BigPoint(1, 1)),
      BigLineSegment(BigPoint(0, 2), BigPoint(1, 2))
    )
    IntersectionDetection.hasSelfIntersection(segments) shouldBe false

  it should "return true if any two segments in the collection intersect" in:
    val segments = List(
      BigLineSegment(BigPoint(0, 0), BigPoint(2, 2)),
      BigLineSegment(BigPoint(0, 2), BigPoint(2, 0)),
      BigLineSegment(BigPoint(10, 10), BigPoint(11, 11))
    )
    IntersectionDetection.hasSelfIntersection(segments) shouldBe true

  it should "return false for segments sharing endpoints (like a polygon boundary)" in:
    val segments = List(
      BigLineSegment(BigPoint(0, 0), BigPoint(1, 0)),
      BigLineSegment(BigPoint(1, 0), BigPoint(1, 1)),
      BigLineSegment(BigPoint(1, 1), BigPoint(0, 1)),
      BigLineSegment(BigPoint(0, 1), BigPoint(0, 0))
    )
    IntersectionDetection.hasSelfIntersection(segments) shouldBe false

  it should "return true for a self-intersecting chain (bow-tie)" in:
    val segments = List(
      BigLineSegment(BigPoint(0, 0), BigPoint(2, 2)),
      BigLineSegment(BigPoint(2, 2), BigPoint(0, 2)),
      BigLineSegment(BigPoint(0, 2), BigPoint(2, 0)),
      BigLineSegment(BigPoint(2, 0), BigPoint(0, 0))
    )
    IntersectionDetection.hasSelfIntersection(segments) shouldBe true

  it should "handle empty or single segment lists" in:
    allAssert(
      IntersectionDetection.hasSelfIntersection(Nil) shouldBe false,
      IntersectionDetection.hasSelfIntersection(List(BigLineSegment(
        BigPoint(0, 0),
        BigPoint(1, 1)
      ))) shouldBe false
    )
