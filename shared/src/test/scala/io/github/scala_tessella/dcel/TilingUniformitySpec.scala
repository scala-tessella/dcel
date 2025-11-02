package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingUniformity.*
import io.github.scala_tessella.dcel.Tree.{Branch, Leaf}
import io.github.scala_tessella.dcel.geometry.RegularPolygon
import io.github.scala_tessella.dcel.structure.VertexId
import io.github.scala_tessella.dcel.conversion.TilingSVG.toUniformityAnimation
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

  /** <img src="file:../../../../../resources/uniform6.svg"/> */
  val uniformity6: TilingDCEL = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => (i + 3 * j) % 13 == 0)

  it should "find an uniform 6 tiling" in {
    val tree = uniformity6.uniformityTree
    allAssert(
      tree.sizeLeaves shouldBe 6,
      uniformity6.innerFaces.size shouldBe 131,
      tree shouldBe
        Branch(
          List(),
          List(
            Branch(
              List("V12", "V13", "V14", "V15", "V16", "V17", "V22", "V39", "V49", "V62", "V72", "V87", "V88", "V89"),
              List(
                Leaf(List("V23", "V64", "V47", "V54", "V57")),
                Leaf(List("V24", "V53", "V58", "V65", "V46")),
                Leaf(List("V48", "V63", "V55", "V56"))
              )
            ),
            Branch(
              List("V18", "V19", "V29", "V32", "V33", "V43", "V52", "V59", "V68", "V73", "V74", "V78", "V79", "V82", "V84", "V85", "V86"),
              List(
                Leaf(List("V25", "V38", "V45", "V66")),
                Leaf(List("V26", "V44", "V67", "V37")),
                Leaf(List("V27", "V34", "V75", "V77", "V36"))
              )
            )
          )
        )
    )
  }

  it should "find that 3.3.6.6.i has uniformity 5" in {

    /** <img src="file:../../../../../resources/uniform5_3.3.6.6.i.svg"/> */
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

  behavior of "TilingUniformity.uniformityTreeUncompressed"

  it should "find at distance 0" in {

    uniformity6.uniformityTreeUncompressed(Option(0)).compress(_ ::: _) shouldBe
      Branch(
        List(),
        List(
          Leaf(
            List(
              "V12",
              "V13",
              "V14",
              "V15",
              "V16",
              "V17",
              "V22",
              "V39",
              "V49",
              "V62",
              "V72",
              "V87",
              "V88",
              "V89"
            )
          ),
          Leaf(
            List(
              "V18",
              "V19",
              "V29",
              "V32",
              "V33",
              "V43",
              "V52",
              "V59",
              "V68",
              "V73",
              "V74",
              "V78",
              "V79",
              "V82",
              "V84",
              "V85",
              "V86"
            )
          )
        )
      )
  }

  it should "find at distance 1" in {

    uniformity6.uniformityTreeUncompressed(Option(1)).compress(_ ::: _) shouldBe
      Branch(
        List(),
        List(
          Branch(
            List(
              "V12",
              "V13",
              "V14",
              "V15",
              "V16",
              "V17",
              "V22",
              "V39",
              "V49",
              "V62",
              "V72",
              "V87",
              "V88",
              "V89"
            ),
            List(
              Leaf(List("V23", "V64")),
              Leaf(List("V24", "V53", "V58", "V65")),
              Leaf(List("V48", "V63"))
            )
          ),
          Branch(
            List(
              "V18",
              "V19",
              "V29",
              "V32",
              "V33",
              "V43",
              "V52",
              "V59",
              "V68",
              "V73",
              "V74",
              "V78",
              "V79",
              "V82",
              "V84",
              "V85",
              "V86"
            ),
            List(
              Leaf(List("V25", "V38")),
              Leaf(List("V26", "V44", "V67")),
              Leaf(List("V27", "V34", "V75", "V77"))
            )
          )
        )
      )
  }

  it should "find at distance 2" in {

    uniformity6.uniformityTreeUncompressed(Option(2)).compress(_ ::: _) shouldBe
      Branch(
        List(),
        List(
          Branch(
            List(
              "V12",
              "V13",
              "V14",
              "V15",
              "V16",
              "V17",
              "V22",
              "V39",
              "V49",
              "V62",
              "V72",
              "V87",
              "V88",
              "V89"
            ),
            List(
              Leaf(List("V23", "V64", "V47", "V54", "V57")),
              Leaf(List("V24", "V53", "V58", "V65")),
              Leaf(List("V48", "V63", "V55", "V56"))
            )
          ),
          Branch(
            List(
              "V18",
              "V19",
              "V29",
              "V32",
              "V33",
              "V43",
              "V52",
              "V59",
              "V68",
              "V73",
              "V74",
              "V78",
              "V79",
              "V82",
              "V84",
              "V85",
              "V86"
            ),
            List(
              Leaf(List("V25", "V38", "V45", "V66")),
              Leaf(List("V26", "V44", "V67", "V37")),
              Leaf(List("V27", "V34", "V75", "V77", "V36"))
            )
          )
        )
      )
  }

  it should "find at distance 3" in {

    uniformity6.uniformityTreeUncompressed(Option(3)).compress(_ ::: _) shouldBe
      Branch(
        List(),
        List(
          Branch(
            List(
              "V12",
              "V13",
              "V14",
              "V15",
              "V16",
              "V17",
              "V22",
              "V39",
              "V49",
              "V62",
              "V72",
              "V87",
              "V88",
              "V89"
            ),
            List(
              Leaf(List("V23", "V64", "V47", "V54", "V57")),
              Leaf(List("V24", "V53", "V58", "V65", "V46")),
              Leaf(List("V48", "V63", "V55", "V56"))
            )
          ),
          Branch(
            List(
              "V18",
              "V19",
              "V29",
              "V32",
              "V33",
              "V43",
              "V52",
              "V59",
              "V68",
              "V73",
              "V74",
              "V78",
              "V79",
              "V82",
              "V84",
              "V85",
              "V86"
            ),
            List(
              Leaf(List("V25", "V38", "V45", "V66")),
              Leaf(List("V26", "V44", "V67", "V37")),
              Leaf(List("V27", "V34", "V75", "V77", "V36"))
            )
          )
        )
      )
  }

  it should "find at distance 4" in {
    uniformity6.uniformityTreeUncompressed(Option(4)).compress(_ ::: _) shouldEqual
      uniformity6.uniformityTreeUncompressed(Option(3)).compress(_ ::: _)
  }

  behavior of "TilingDCEL.scanUniformityTreeAlt"

  it should "efficiently scan uniformity at all distances" in {
    uniformity6.scanUniformityTree shouldEqual
      (0 to 3).toList.map { distance =>

        uniformity6.uniformityTreeUncompressed(Option(distance)).compress(_ ::: _)
      }
  }

  it should "scan uniformity leaves at all distances" in {
    uniformity6.scanUniformityTree.map(_.flattenLeaves) shouldEqual
      List(
        List(
          List(
            "V12",
            "V13",
            "V14",
            "V15",
            "V16",
            "V17",
            "V22",
            "V39",
            "V49",
            "V62",
            "V72",
            "V87",
            "V88",
            "V89"
          ),
          List(
            "V18",
            "V19",
            "V29",
            "V32",
            "V33",
            "V43",
            "V52",
            "V59",
            "V68",
            "V73",
            "V74",
            "V78",
            "V79",
            "V82",
            "V84",
            "V85",
            "V86"
          )
        ),
        List(
          List("V23", "V64"),
          List("V24", "V53", "V58", "V65"),
          List("V48", "V63"),
          List("V25", "V38"),
          List("V26", "V44", "V67"),
          List("V27", "V34", "V75", "V77")
        ),
        List(
          List("V23", "V64", "V47", "V54", "V57"),
          List("V24", "V53", "V58", "V65"),
          List("V48", "V63", "V55", "V56"),
          List("V25", "V38", "V45", "V66"),
          List("V26", "V44", "V67", "V37"),
          List("V27", "V34", "V75", "V77", "V36")
        ),
        List(
          List("V23", "V64", "V47", "V54", "V57"),
          List("V24", "V53", "V58", "V65", "V46"),
          List("V48", "V63", "V55", "V56"),
          List("V25", "V38", "V45", "V66"),
          List("V26", "V44", "V67", "V37"),
          List("V27", "V34", "V75", "V77", "V36")
        )
      )
  }

  it should "pippo" in {
    println(uniformity6.toUniformityAnimation())
    true shouldBe true
  }
