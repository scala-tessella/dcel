package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingUniformity.uniformityTreeUncompressed
import io.github.scala_tessella.dcel.Tree.{Branch, Leaf}
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

  /** <img src="file:../../../../../resources/uniform6.svg"/> */
  val uniformity6: TilingDCEL = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => (i + 3 * j) % 13 == 0)

  it should "find an uniform 6 tiling" in {
    allAssert(
      uniformity6.uniformityTree.sizeLeaves shouldBe 6,
      uniformity6.innerFaces.size shouldBe 131
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

  behavior of "TilingUniformity.uniformityTreeUncompressed"

  it should "find at distance 0" in {

    uniformity6.uniformityTreeUncompressed(Option(0)) shouldBe
      Branch(
        List(),
        List(
          Branch(
            List("V12", "V13", "V14", "V15", "V16", "V17", "V22", "V39", "V49", "V62", "V72", "V87", "V88", "V89"),
            List()
          ),
          Branch(
            List("V18", "V19", "V29", "V32", "V33", "V43", "V52", "V59", "V68", "V73", "V74", "V78", "V79", "V82", "V84", "V85", "V86"),
            List()
          )
        )
      )
  }

  it should "find at distance 1" in {

    uniformity6.uniformityTreeUncompressed(Option(1)) shouldBe
      Branch(
        List(),
        List(
          Branch(
            List("V12", "V13", "V14", "V15", "V16", "V17", "V22", "V39", "V49", "V62", "V72", "V87", "V88", "V89"),
            List(
              Branch(
                List("V23", "V64"),
                List()
              ),
              Branch(
                List("V24", "V53", "V58", "V65"),
                List()
              ),
              Branch(
                List("V48", "V63"),
                List()
              )
            )
          ),
          Branch(
            List("V18", "V19", "V29", "V32", "V33", "V43", "V52", "V59", "V68", "V73", "V74", "V78", "V79", "V82", "V84", "V85", "V86"),
            List(
              Branch(
                List("V25", "V38"),
                List()
              ),
              Branch(
                List("V26", "V44", "V67"),
                List()
              ),
              Branch(
                List("V27", "V34", "V75", "V77"),
                List()
              )
            )
          )
        )
      )
  }

  it should "find at distance 2" in {

    uniformity6.uniformityTreeUncompressed(Option(2)) shouldBe
      Branch(
        List(),
        List(
          Branch(
            List("V12", "V13", "V14", "V15", "V16", "V17", "V22", "V39", "V49", "V62", "V72", "V87", "V88", "V89"),
            List(
              Branch(
                List("V23", "V64"),
                List(
                  Leaf(List("V47", "V54", "V57"))
                )
              ),
              Branch(
                List("V24", "V53", "V58", "V65"),
                List(
                  Branch(
                    List(),
                    List()
                  )
                )
              ),
              Branch(
                List("V48", "V63"),
                List(
                  Leaf(List("V55", "V56"))
                )
              )
            )
          ),
          Branch(
            List("V18", "V19", "V29", "V32", "V33", "V43", "V52", "V59", "V68", "V73", "V74", "V78", "V79", "V82", "V84", "V85", "V86"),
            List(
              Branch(
                List("V25", "V38"),
                List(
                  Leaf(List("V45", "V66"))
                )
              ),
              Branch(
                List("V26", "V44", "V67"),
                List(
                  Leaf(List("V37"))
                )
              ),
              Branch(
                List("V27", "V34", "V75", "V77"),
                List(
                  Leaf(List("V36"))
                )
              )
            )
          )
        )
      )
  }

  it should "find at distance 3" in {

    uniformity6.uniformityTreeUncompressed(Option(3)) shouldBe
      Branch(
        List(),
        List(
          Branch(
            List("V12", "V13", "V14", "V15", "V16", "V17", "V22", "V39", "V49", "V62", "V72", "V87", "V88", "V89"),
            List(
              Branch(
                List("V23", "V64"),
                List(
                  Leaf(List("V47", "V54", "V57"))
                )
              ),
              Branch(
                List("V24", "V53", "V58", "V65"),
                List(
                  Branch(
                    List(),
                    List(
                      Leaf(List("V46"))
                    )
                  )
                )
              ),
              Branch(
                List("V48", "V63"),
                List(
                  Leaf(List("V55", "V56"))
                )
              )
            )
          ),
          Branch(
            List("V18", "V19", "V29", "V32", "V33", "V43", "V52", "V59", "V68", "V73", "V74", "V78", "V79", "V82", "V84", "V85", "V86"),
            List(
              Branch(
                List("V25", "V38"),
                List(
                  Leaf(
                    List("V45", "V66"))
                )
              ),
              Branch(
                List("V26", "V44", "V67"),
                List(
                  Leaf(List("V37"))
                )
              ),
              Branch(
                List("V27", "V34", "V75", "V77"),
                List(
                  Leaf(List("V36"))
                )
              )
            )
          )
        )
      )
  }

  it should "find at distance 4" in {

    uniformity6.uniformityTreeUncompressed(Option(4)) shouldEqual
      uniformity6.uniformityTreeUncompressed(Option(3))
  }
