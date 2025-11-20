package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.RegularPolygon
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingDoubleSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingDCEL.growDouble"

  it should "return an empty tiling" in {
    TilingDCEL.empty.growDouble.value.isEmpty shouldBe true
  }

  it should "fail for an hexagon" in {
    TilingBuilder.createRegularPolygon(RegularPolygon(6)).growDouble.isLeft shouldBe true
  }

  it should "not fail for a square" in {
    val doubled = TilingBuilder.createRegularPolygon(RegularPolygon(4)).growDouble
    println(doubled)
    doubled.isRight shouldBe true
  }

  it should "not fail for a 2x2 square net" in {
    val doubled = TilingBuilder.createRhombusNet(2, 2).growDoubleAlt
    println(doubled)
    doubled.isRight shouldBe true
  }

  it should "not fail for a 3x3 square net" in {
    val doubled = TilingBuilder.createRhombusNet(3, 3).growDouble
    println(doubled)
    doubled.isRight shouldBe true
  }
