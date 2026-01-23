package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.TilingTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RegularPolygonSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "RegularPolygon.apply"

  it should "create a polygon when sides are >= 3" in:
    RegularPolygon(3).toSides shouldBe 3

  it should "reject invalid side counts" in:
    an[IllegalArgumentException] should be thrownBy RegularPolygon(2)

  behavior of "RegularPolygon.fromInteriorAngle"

  it should "derive the number of sides from a valid interior angle" in:
    allAssert(
      RegularPolygon.fromInteriorAngle(AngleDegree(60)).toSides shouldBe 3,
      RegularPolygon.fromInteriorAngle(AngleDegree(90)).toSides shouldBe 4
    )

  it should "reject interior angles that do not form a valid regular polygon" in:
    an[IllegalArgumentException] should be thrownBy RegularPolygon.fromInteriorAngle(AngleDegree(180))
