package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon, SimplePolygon}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingUniformitySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingDCEL.uniformityTree"

  it should "find an uniform 1 tiling" in {

    /** <img src="file:../../../../../resources/uniform1.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => (i + 3 * j) % 7 == 0)
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 1,
      result.innerFaces.size shouldBe 101
    )
  }

  it should "find the same tiling with opposite chirality by inverting the axes" in {

    /** <img src="file:../../../../../resources/uniform1_specular.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => (j + 3 * i) % 7 == 0)
    println(result.toSVG(showUniformity = true))
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 1,
      result.innerFaces.size shouldBe 101
    )
  }

  it should "find an uniform 2 tiling" in {

    /** <img src="file:../../../../../resources/uniform2.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => i % 3 == 0 && j % 3 == 0)
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 2,
      result.innerFaces.size shouldBe 112
    )
  }

  it should "find an uniform 3 tiling" in {

    /** <img src="file:../../../../../resources/uniform3.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => (i + 4 * j) % 7 == 0)
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 3,
      result.innerFaces.size shouldBe 104
    )
  }

  it should "find an uniform 4 tiling" in {

    /** <img src="file:../../../../../resources/uniform4.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => (i + 7 * j) % 9 == 0)
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 4,
      result.innerFaces.size shouldBe 116
    )
  }

  it should "find an uniform 5 tiling" in {

    /** <img src="file:../../../../../resources/uniform5.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => i % 10 == (j * 8) % 10)
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 5,
      result.innerFaces.size shouldBe 120
    )
  }

  it should "find an uniform 6 tiling" in {

    /** <img src="file:../../../../../resources/uniform6.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => (i + 3 * j) % 13 == 0)
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 6,
      result.innerFaces.size shouldBe 131
    )
  }
