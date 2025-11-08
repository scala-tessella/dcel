package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.TilingTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BigLineSegmentSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "BigLineSegment.unitPath"

  val angles: Vector[AngleDegree] = Vector(90, 90, 150, 60, 150).map(AngleDegree(_))

  it should "find a path of unit length sides" in {
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
  }

  it should "find a path of unit length sides from a shifted start" in {
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
  }


  it should "find a path of unit length sides from an inverted start" in {
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
  }
