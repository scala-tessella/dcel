package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.RegularPolygon
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingDoubleSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingDCEL.quadrupleArea"

  it should "keep an empty tiling" in {
    TilingDCEL.empty.quadrupleArea.value.isEmpty shouldBe true
  }

  it should "fail for an hexagon" in {
    TilingBuilder.createRegularPolygon(RegularPolygon(6)).quadrupleArea.isLeft shouldBe true
  }

  it should "quadruple a square" in {
    val doubled = TilingBuilder.createRegularPolygon(RegularPolygon(4)).quadrupleArea
    val result  = doubled.value
    allAssert(
      result.halfEdges.size shouldBe 24,
      result.vertices.size shouldBe 9,
      result.innerFaces.size shouldBe 4,
      TilingValidation.validate(result).isRight shouldBe true
    )
  }

  it should "quadruple a 2x2 square net" in {
    val doubled = TilingBuilder.createRhombusNet(2, 2).quadrupleArea
    val result  = doubled.value
    allAssert(
      result.halfEdges.size shouldBe 80,
      result.vertices.size shouldBe 25,
      result.innerFaces.size shouldBe 16,
      TilingValidation.validate(result).isRight shouldBe true
    )
  }

  it should "quadruple a 2x2 hexagon net" in {
    val doubled = TilingBuilder.createHexagonNet(2, 2).quadrupleArea
    val result  = doubled.value
    allAssert(
      result.halfEdges.size shouldBe 126,
      result.vertices.size shouldBe 48,
      result.innerFaces.size shouldBe 16,
      TilingValidation.validate(result).isRight shouldBe true
    )
  }
