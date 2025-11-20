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
    val result  = doubled.value
    allAssert(
      result.halfEdges.size shouldBe 14,
      result.vertices.size shouldBe 6,
      result.innerFaces.size shouldBe 2,
      TilingValidation.validate(result).isRight shouldBe true
    )
  }

  it should "not fail for a 2x2 square net" in {
    val doubled = TilingBuilder.createRhombusNet(2, 2).growDouble
    val result  = doubled.value
    allAssert(
      result.halfEdges.size shouldBe 44,
      result.vertices.size shouldBe 15,
      result.innerFaces.size shouldBe 8,
      TilingValidation.validate(result).isRight shouldBe true
    )
  }

  it should "not fail for a 3x3 square net" in {
    val doubled = TilingBuilder.createRhombusNet(3, 3).growDouble
    val result  = doubled.value
    allAssert(
      result.halfEdges.size shouldBe 90,
      result.vertices.size shouldBe 28,
      result.innerFaces.size shouldBe 18,
      TilingValidation.validate(result).isRight shouldBe true
    )
  }

  it should "not fail for a 2x2 hexagon net" in {
    val doubled = TilingBuilder.createHexagonNet(2, 2).growDouble
    val result  = doubled.value
    allAssert(
      result.halfEdges.size shouldBe 70,
      result.vertices.size shouldBe 28,
      result.innerFaces.size shouldBe 8,
      TilingValidation.validate(result).isRight shouldBe true
    )
  }

  behavior of "TilingDCEL.quadrupleArea"

  it should "keep an empty tiling" in {
    TilingDCEL.empty.quadrupleArea.value.isEmpty shouldBe true
  }

  it should "fail for an hexagon" in {
    TilingBuilder.createRegularPolygon(RegularPolygon(6)).quadrupleArea.isLeft shouldBe true
  }

  it should "quadruple a square" in {
    val doubled = TilingBuilder.createRegularPolygon(RegularPolygon(4)).quadrupleArea
    val result = doubled.value
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
