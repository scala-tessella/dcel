package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.TilingTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BigLineSegmentSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "BigLineSegment.unitPath"

  val angles: Vector[AngleDegree] = Vector(90, 90, 150, 60, 150).map(AngleDegree(_))

  it should "find a path of unit length sides" in:
    val start  = BigLineSegment(BigPoint.origin, BigPoint(1, 0))
    val result =
      List(
        BigPoint(0, 0),
        BigPoint(1, 0),
        BigPoint(1, 1),
        BigPoint(0.5, 1.86602540378),
        BigPoint(0, 1)
      )
    start.unitPath(angles).zip(result).forall(_.almostEquals(_)) shouldBe true

  behavior of "BigLineSegment.intersects"

  it should "treat near-collinear crossings as intersections" in:
    val delta = BigDecimal(BigDecimalGeometry.ACCURACY / 4)
    val s1    = BigLineSegment(BigPoint(0, 0), BigPoint(2, 0))
    val s2    = BigLineSegment(BigPoint(1, -delta), BigPoint(1, delta))
    s1.intersects(s2) shouldBe true

  it should "find a path of unit length sides from a shifted start" in:
    val start  = BigLineSegment(BigPoint(0, 1), BigPoint(1, 1))
    val result =
      List(
        BigPoint(0, 1),
        BigPoint(1, 1),
        BigPoint(1, 2),
        BigPoint(0.5, 2.86602540378),
        BigPoint(0, 2)
      )
    start.unitPath(angles).zip(result).forall(_.almostEquals(_)) shouldBe true

  it should "find a path of unit length sides from an inverted start" in:
    val start  = BigLineSegment(BigPoint.origin, BigPoint(0, 1))
    val result =
      List(
        BigPoint(0, 0),
        BigPoint(0, 1),
        BigPoint(-1, 1),
        BigPoint(-1.86602540378, 0.5),
        BigPoint(-1, 0)
      )
    start.unitPath(angles).zip(result).forall(_.almostEquals(_)) shouldBe true

  /** Pins the numerical contract of the unit-path primitive independently of `SimplePolygon.fromUntrusted`
    * and `TilingBuilderSpec`'s centagon ring. For a regular N-gon on unit sides, the distance from the last
    * generated vertex back to the start must equal 1 to within `ACCURACY`. See ADR-0009.
    */
  behavior of "BigLineSegment.unitPath closure for regular N-gons"

  for n <- Seq(3, 10, 46, 92, 100, 200, 500) do
    it should s"close within ACCURACY for a regular $n-gon" in:
      val start        = BigLineSegment(BigPoint.origin, BigPoint(1, 0))
      val vertices     = start.unitPath(RegularPolygon(n).angles)
      val closingEdge  = vertices.last.distanceTo(vertices.head)
      val closureError = (closingEdge - BigDecimal(1.0)).abs
      closureError.toDouble should be <= BigDecimalGeometry.ACCURACY
