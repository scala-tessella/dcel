package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingUniformity.*
import io.github.scala_tessella.dcel.Tree.{Branch, Leaf}
import io.github.scala_tessella.dcel.geometry.RegularPolygon
import io.github.scala_tessella.dcel.structure.VertexId
//import io.github.scala_tessella.dcel.conversion.TilingSVG.toUniformityAnimation
//import io.github.scala_tessella.dcel.conversion.TilingSVGPlatform
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingUniformitySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingDCEL.uniformityTree"

  it should "find an uniform 1 tiling" in:

    /** Uniform 1 <img src="file:../../../../../resources/uniform1.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => (i + 3 * j) % 7 == 0)
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 1,
      result.innerFaces.size shouldBe 101
    )

  it should "find the same tiling with opposite chirality by inverting the axes" in:

    /** Uniform 1 specular <img src="file:../../../../../resources/uniform1_specular.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => (j + 3 * i) % 7 == 0)
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 1,
      result.innerFaces.size shouldBe 101
    )

  it should "find an uniform 2 tiling" in:

    /** Uniform 2 <img src="file:../../../../../resources/uniform2.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => i % 3 == 0 && j % 3 == 0)
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 2,
      result.innerFaces.size shouldBe 112
    )

  it should "find an uniform 3 tiling" in:

    /** Uniform 3 <img src="file:../../../../../resources/uniform3.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => (i + 4 * j) % 7 == 0)
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 3,
      result.innerFaces.size shouldBe 104
    )

  it should "find an uniform 4 tiling" in:

    /** Uniform 4 <img src="file:../../../../../resources/uniform4.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => (i + 7 * j) % 9 == 0)
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 4,
      result.innerFaces.size shouldBe 116
    )

  it should "find an uniform 5 tiling" in:

    /** Uniform 5 <img src="file:../../../../../resources/uniform5.svg"/> */
    val result = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => i % 10 == (j * 8) % 10)
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 5,
      result.innerFaces.size shouldBe 120
    )

  /** Uniform 6 <img src="file:../../../../../resources/uniform6.svg"/> */
  val uniformity6: TilingDCEL = TilingBuilder.createHoledTriangleNet(9, 9)((i, j) => (i + 3 * j) % 13 == 0)

  it should "find an uniform 6 tiling" in:
    val tree = uniformity6.uniformityTree
    allAssert(
      tree.sizeLeaves shouldBe 6,
      uniformity6.innerFaces.size shouldBe 131,
      tree.orderedForComparison shouldBe
        Branch(
          List(),
          List(
            Branch(
              List(12, 13, 14, 15, 16, 17, 22, 39, 49, 62, 72, 87, 88, 89),
              List(
                Leaf(List(23, 47, 54, 57, 64)),
                Leaf(List(24, 46, 53, 58, 65)),
                Leaf(List(48, 55, 56, 63))
              )
            ),
            Branch(
              List(18, 19, 29, 32, 33, 43, 52, 59, 68, 73, 74, 78, 79, 82, 84, 85, 86),
              List(
                Leaf(List(25, 38, 45, 66)),
                Leaf(List(26, 37, 44, 67)),
                Leaf(List(27, 34, 36, 75, 77))
              )
            )
          )
        )
    )

  it should "find that 3.3.6.6.i has uniformity 5" in:

    /** Uniform 5 3.3.6.6.i <img src="file:../../../../../resources/uniform5_3.3.6.6.i.svg"/> */
    val result =
      TilingBuilder.createHoledTriangleNet(9, 11)((i, j) => (i - j) % 3 == 0)
        .maybeAddRegularPolygon(VertexId(24), VertexId(25), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId(25), VertexId(35), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId(27), VertexId(28), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId(28), VertexId(38), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId(54), VertexId(55), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId(55), VertexId(65), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId(57), VertexId(58), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId(58), VertexId(68), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId(84), VertexId(85), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId(85), VertexId(95), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId(87), VertexId(88), RegularPolygon(3)).value
        .maybeAddRegularPolygon(VertexId(88), VertexId(98), RegularPolygon(3)).value
    allAssert(
      result.uniformityTree.sizeLeaves shouldBe 5,
      result.innerFaces.size shouldBe 39
    )

  behavior of "TilingUniformity.uniformityTreeUncompressed"

  it should "find at distance 0 uncompressed" in:
    uniformity6
      .uniformityTreeUncompressed(Option(0))
      .orderedForComparison shouldBe
      Branch(
        List(),
        List(
          Branch(
            List(12, 13, 14, 15, 16, 17, 22, 39, 49, 62, 72, 87, 88, 89),
            List()
          ),
          Branch(
            List(18, 19, 29, 32, 33, 43, 52, 59, 68, 73, 74, 78, 79, 82, 84, 85, 86),
            List()
          )
        )
      )

  it should "find at distance 0" in:
    uniformity6.uniformityTreeUncompressed(Option(0))
      .compress:
        _ ::: _
      .orderedForComparison shouldBe
      Branch(
        List(),
        List(
          Leaf(
            List(12, 13, 14, 15, 16, 17, 22, 39, 49, 62, 72, 87, 88, 89)
          ),
          Leaf(
            List(18, 19, 29, 32, 33, 43, 52, 59, 68, 73, 74, 78, 79, 82, 84, 85, 86)
          )
        )
      )

  it should "find at distance 1" in:
    uniformity6.uniformityTreeUncompressed(Option(1))
      .compress:
        _ ::: _
      .orderedForComparison shouldBe
      Branch(
        List(),
        List(
          Branch(
            List(12, 13, 14, 15, 16, 17, 22, 39, 49, 62, 72, 87, 88, 89),
            List(
              Leaf(List(23, 64)),
              Leaf(List(24, 53, 58, 65)),
              Leaf(List(48, 63))
            )
          ),
          Branch(
            List(18, 19, 29, 32, 33, 43, 52, 59, 68, 73, 74, 78, 79, 82, 84, 85, 86),
            List(
              Leaf(List(25, 38)),
              Leaf(List(26, 44, 67)),
              Leaf(List(27, 34, 75, 77))
            )
          )
        )
      )

  it should "find at distance 2" in:
    uniformity6.uniformityTreeUncompressed(Option(2))
      .compress:
        _ ::: _
      .orderedForComparison shouldBe
      Branch(
        List(),
        List(
          Branch(
            List(12, 13, 14, 15, 16, 17, 22, 39, 49, 62, 72, 87, 88, 89),
            List(
              Leaf(List(23, 47, 54, 57, 64)),
              Leaf(List(24, 53, 58, 65)),
              Leaf(List(48, 55, 56, 63))
            )
          ),
          Branch(
            List(18, 19, 29, 32, 33, 43, 52, 59, 68, 73, 74, 78, 79, 82, 84, 85, 86),
            List(
              Leaf(List(25, 38, 45, 66)),
              Leaf(List(26, 37, 44, 67)),
              Leaf(List(27, 34, 36, 75, 77))
            )
          )
        )
      )

  it should "find at distance 3" in:
    uniformity6.uniformityTreeUncompressed(Option(3))
      .compress:
        _ ::: _
      .orderedForComparison shouldBe
      Branch(
        List(),
        List(
          Branch(
            List(12, 13, 14, 15, 16, 17, 22, 39, 49, 62, 72, 87, 88, 89),
            List(
              Leaf(List(23, 47, 54, 57, 64)),
              Leaf(List(24, 46, 53, 58, 65)),
              Leaf(List(48, 55, 56, 63))
            )
          ),
          Branch(
            List(18, 19, 29, 32, 33, 43, 52, 59, 68, 73, 74, 78, 79, 82, 84, 85, 86),
            List(
              Leaf(List(25, 38, 45, 66)),
              Leaf(List(26, 37, 44, 67)),
              Leaf(List(27, 34, 36, 75, 77))
            )
          )
        )
      )

  it should "find at distance 4" in:
    uniformity6.uniformityTreeUncompressed(Option(4))
      .compress:
        _ ::: _
      .shouldEqual:
        uniformity6.uniformityTreeUncompressed(Option(3))
          .compress:
            _ ::: _

//  behavior of "problematic tiling"
//
//  val xmlMetadata: String = loadFile(s"metadata/3.6.3.6_uniformity_issue.xml")
//
//  /** Uniformity issue <img src="file:../../../../../resources/uniformityIssue.svg"/> */
//  def problematicTiling: TilingDCEL = TilingSVGPlatform.fromMetadata(xmlMetadata).value
//  //    tiling.uniformityTree.sizeLeaves shouldBe 1
//
//  it should "have uniformity 1" in:
//    problematicTiling.uniformityTree.sizeLeaves shouldBe 1
//
//  it should "find the structure at distance 3 originating from vertex V7" in:
//    val struct = problematicTiling.getDcelAtVertex(VertexId(7), 3).value
//    struct.innerFaces.size shouldBe 38
//
//  it should "find the structure at distance 3 originating from vertex V1" in:
//    val struct = problematicTiling.getDcelAtVertex(V1, 3).value
//    struct.innerFaces.size shouldBe 38

  behavior of "TilingDCEL.scanUniformityTree"

  it should "efficiently scan uniformity at all distances" in:
    uniformity6.scanUniformityTree
      .map:
        _.orderedForComparison
      .shouldEqual:
        (0 to 3).toList.map: distance =>
          uniformity6.uniformityTreeUncompressed(Option(distance))
            .compress(_ ::: _)
            .orderedForComparison

  it should "scan uniformity leaves at all distances" in:
    uniformity6.scanUniformityTree
      .map:
        _.orderedForComparison
      .map:
        _.flattenLeaves
      .shouldEqual:
        List(
          List(
            List(12, 13, 14, 15, 16, 17, 22, 39, 49, 62, 72, 87, 88, 89),
            List(18, 19, 29, 32, 33, 43, 52, 59, 68, 73, 74, 78, 79, 82, 84, 85, 86)
          ),
          List(
            List(23, 64),
            List(24, 53, 58, 65),
            List(48, 63),
            List(25, 38),
            List(26, 44, 67),
            List(27, 34, 75, 77)
          ),
          List(
            List(23, 47, 54, 57, 64),
            List(24, 53, 58, 65),
            List(48, 55, 56, 63),
            List(25, 38, 45, 66),
            List(26, 37, 44, 67),
            List(27, 34, 36, 75, 77)
          ),
          List(
            List(23, 47, 54, 57, 64),
            List(24, 46, 53, 58, 65),
            List(48, 55, 56, 63),
            List(25, 38, 45, 66),
            List(26, 37, 44, 67),
            List(27, 34, 36, 75, 77)
          )
        )

  behavior of "TilingUniformity.gonality"

  it should "find gonality" in:
    uniformity6.gonalityUnsafe shouldBe
      List(List(3, 3, 3, 3, 3, 3), List(3, 3, 3, 3, 6))
