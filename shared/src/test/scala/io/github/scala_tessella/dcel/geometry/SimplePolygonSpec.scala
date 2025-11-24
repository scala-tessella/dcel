package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.geometry.{AngleDegree, SimplePolygon}
import io.github.scala_tessella.dcel.TilingTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SimplePolygonSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "SimplePolygon.areOppositeShifted"

  it should "return None if the two opposite sequences of turns are of different" in {
    val xs = Vector(-45, 45, -45, 45).map(AngleDegree(_))
    val ys = Vector(0, 45, -45, 45).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(xs, ys) shouldBe None
  }

  it should "return None if each sequence has length <= 1" in {
    val s = Vector(45).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(s, s) shouldBe None
  }

  it should "detect a shift of 1 if turns are all 0°" in {
    val s  = Vector(0, 0).map(AngleDegree(_))
    val s1 = Vector(0, 0, 0).map(AngleDegree(_))
    allAssert(
      SimplePolygon.areOppositeShifted(s, s) shouldBe Some(1),
      SimplePolygon.areOppositeShifted(s1, s1) shouldBe Some(1)
    )
  }

  it should "return None for other opposite sequences containing the same turns (not fitting)" in {
    val s  = Vector(90, 90, 90, 90).map(AngleDegree(_))
    val s1 = Vector(45, 45, 45, 45).map(AngleDegree(_))
    allAssert(
      SimplePolygon.areOppositeShifted(s, s) shouldBe None,
      SimplePolygon.areOppositeShifted(s1, s1) shouldBe None
    )
  }

  it should "detect a shift of 1" in {
    val s = Vector(45, -45, 45).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(s, s) shouldBe Some(1)
  }

  it can "detect a shift of 2 in sequences of length 3" in {
    val s = Vector(45, 45, -45).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(s, s) shouldBe Some(2)
  }

  it can "NOT detect a shift of 2 in sequences of length 4" in {
    val s = Vector(45, 45, 45, -45).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(s, s) shouldBe None
  }

  it can "detect another shift of 2 in sequences of length 4" in {
    val s = Vector(45, 45, -45, 45).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(s, s) shouldBe None
  }

  it should "detect a shift of 2" in {
    val s = Vector(45, 0, -45, 0, 45, 0).map(AngleDegree(_))
    SimplePolygon.areOppositeShifted(s, s) shouldBe Some(2)
  }

  behavior of "SimplePolygon.areOpposite"

  it should "return None if the two opposite sequences of turns are of different size" in {
    val xs = Vector(-45, 45, -45, 45).map(AngleDegree(_))
    val ys = Vector(45, -45, 45).map(AngleDegree(_))
    SimplePolygon.areOpposite(xs, ys) shouldBe None
  }

  it should "detect a shift of 0 in fitting sequences" in {
    val xs = Vector(-45, 45, -45).map(AngleDegree(_))
    val ys = Vector(45, -45, 45).map(AngleDegree(_))
    SimplePolygon.areOpposite(xs, ys) shouldBe Some(0)
  }

  it should "detect a shift of 0 if turns are all 0°" in {
    val s = Vector(0, 0).map(AngleDegree(_))
    val s1 = Vector(0, 0, 0).map(AngleDegree(_))
    allAssert(
      SimplePolygon.areOpposite(s, s) shouldBe Some(0),
      SimplePolygon.areOpposite(s1, s1) shouldBe Some(0)
    )
  }

