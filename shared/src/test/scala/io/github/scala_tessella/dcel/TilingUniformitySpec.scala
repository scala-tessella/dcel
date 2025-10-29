package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.RegularPolygon
import io.github.scala_tessella.dcel.structure.VertexId
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

  it should "find an issue" in {
    /** <img src="file:../../../../../resources/uniform_issue.svg"/> */
    val result =
      TilingBuilder.createHoledTriangleNet(9, 11)((i, j) => (i - j) % 3 == 0)
        .maybeAddRegularPolygon(VertexId("V24"), VertexId("V25"), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId("V25"), VertexId("V35"), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId("V27"), VertexId("V28"), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId("V28"), VertexId("V38"), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId("V54"), VertexId("V55"), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId("V55"), VertexId("V65"), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId("V57"), VertexId("V58"), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId("V58"), VertexId("V68"), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId("V84"), VertexId("V85"), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId("V85"), VertexId("V95"), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId("V87"), VertexId("V88"), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId("V88"), VertexId("V98"), RegularPolygon(3)).value
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 5,
      result.innerFaces.size shouldBe 39
    )
  }
