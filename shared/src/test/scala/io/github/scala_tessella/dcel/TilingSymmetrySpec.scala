package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingDeletion.deleteEdge
import io.github.scala_tessella.dcel.TilingSymmetry.*
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon}
import io.github.scala_tessella.dcel.structure.VertexId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingSymmetrySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingSymmetry.rotationalSymm"

  it should "calculate the rotational symmetry for a square" in:
    square.rotationalSymm shouldBe 4

  it should "calculate the rotational symmetry for a 2x2 square" in:
    val square2x2 = TilingBuilder.createRhombusNet(2, 2)
    square2x2.rotationalSymm shouldBe 4

  it should "calculate the rotational symmetry for a 3x3 square" in:
    val square3x3 = TilingBuilder.createRhombusNet(3, 3)
    square3x3.rotationalSymm shouldBe 4

  it should "calculate the rotational symmetry for a 4x4 square" in:
    val square4x4 = TilingBuilder.createRhombusNet(4, 4)
    square4x4.rotationalSymm shouldBe 4

  it should "calculate the rotational symmetry for a 3x2 rectangle" in:
    val rectangle3x2 = TilingBuilder.createRhombusNet(3, 2)
    rectangle3x2.rotationalSymm shouldBe 2

  it should "calculate the rotational symmetry for a triangle" in:
    triangle.rotationalSymm shouldBe 3

  it should "calculate the rotational symmetry for a triangle net" in:
    val triangleNet = TilingBuilder.createTriangleNet(3, 3)
    triangleNet.rotationalSymm shouldBe 2

  it should "calculate the rotational symmetry for an hexagon" in:
    allAssert(
      hexagon.rotationalSymm shouldBe 6,
      hexagon.rotationalVertexIds shouldBe List(V1, V6, V5, V4, V3, V2)
    )

  it should "calculate the rotational symmetry for a 2x1 hexagon net" in:
    val hex2x1 = TilingBuilder.createHexagonNet(2, 1)
    hex2x1.rotationalSymm shouldBe 2

  it should "calculate the rotational symmetry for a 4x4 hexagon net" in:
    val hex4x4 = TilingBuilder.createHexagonNet(4, 4)
    hex4x4.rotationalSymm shouldBe 2

  it should "calculate the rotational symmetry for a modified 4x4 square" in:
    val square4x4modified = TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value
    square4x4modified.rotationalSymm shouldBe 1

  it should "calculate the rotational symmetry for another modified 4x4 square" in:
    val square4x4modified = TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value
      .deleteEdge(VertexId("V17"), VertexId("V22")).value
    square4x4modified.rotationalSymm shouldBe 1

  it should "calculate the rotational symmetry for a third modified 4x4 square" in:
    val square4x4modified = TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value
      .deleteEdge(VertexId("V17"), VertexId("V22")).value
      .deleteEdge(VertexId("V19"), VertexId("V20")).value
      .deleteEdge(VertexId("V4"), VertexId("V9")).value
    square4x4modified.rotationalSymm shouldBe 4

  it should "calculate the rotational symmetry for a fourth modified 4x4 square" in:
    val square4x4modified = TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value
      .deleteEdge(VertexId("V19"), VertexId("V20")).value
    allAssert(
      square4x4modified.rotationalSymm shouldBe 2,
      square4x4modified.rotationalVertexIds shouldBe List(V6, "V20")
    )
