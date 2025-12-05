package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingDeletion.deleteEdge
import io.github.scala_tessella.dcel.TilingSymmetry.*
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

  it should "calculate the rotational symmetry for an equilateral triangle" in:
    triangle.rotationalSymm shouldBe 3

  it should "calculate the rotational symmetry for a triangle net" in:
    val triangleNet = TilingBuilder.createTriangleNet(3, 3)
    triangleNet.rotationalSymm shouldBe 2

  it should "calculate the rotational symmetry for a regular hexagon" in:
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

  /** <img src="file:../../../../../resources/symmetry/oneAsymmHole.svg"/> */
  val oneAsymmetricHole: TilingDCEL =
    TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value

  it should "calculate the rotational symmetry for a 4x4 square with one asymmetric hole" in:
    oneAsymmetricHole.rotationalSymm shouldBe 1

  /** <img src="file:../../../../../resources/symmetry/twoAsymmHoles.svg"/> */
  val twoAsymmetricHoles: TilingDCEL =
    TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value
      .deleteEdge(VertexId("V17"), VertexId("V22")).value

  it should "calculate the rotational symmetry for a 4x4 square with two asymmetrical holes" in:
    twoAsymmetricHoles.rotationalSymm shouldBe 1

  /** <img src="file:../../../../../resources/symmetry/fourRotationalHoles.svg"/> */
  val fourRotationalHoles: TilingDCEL =
    TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value
      .deleteEdge(VertexId("V17"), VertexId("V22")).value
      .deleteEdge(VertexId("V19"), VertexId("V20")).value
      .deleteEdge(VertexId("V4"), VertexId("V9")).value

  it should "calculate the rotational symmetry for a 4x4 square with four rotated holes" in:
    fourRotationalHoles.rotationalSymm shouldBe 4

  /** <img src="file:../../../../../resources/symmetry/twoRotationalHoles.svg"/> */
  val twoRotationalHoles: TilingDCEL =
    TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value
      .deleteEdge(VertexId("V19"), VertexId("V20")).value

  it should "calculate the rotational symmetry for a 4x4 square with two rotated holes" in:
    allAssert(
      twoRotationalHoles.rotationalSymm shouldBe 2,
      twoRotationalHoles.rotationalVertexIds shouldBe List(V6, "V20")
    )

  /** <img src="file:../../../../../resources/symmetry/twoReflectionalHoles.svg"/> */
  val twoReflectionalHoles: TilingDCEL =
    TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value
      .deleteEdge(VertexId("V9"), VertexId("V10")).value

  it should "calculate the rotational symmetry for a 4x4 square with two reflected holes" in :
    allAssert(
      twoReflectionalHoles.rotationalSymm shouldBe 1,
      twoReflectionalHoles.rotationalVertexIds shouldBe List(V6)
    )

  behavior of "TilingSymmetry.reflectionalSymm"

  it should "calculate the reflectional symmetry for a square" in:
    square.reflectionalSymm shouldBe 4

  it should "calculate the reflectional symmetry for a 2x2 square" in:
    val square2x2 = TilingBuilder.createRhombusNet(2, 2)
    square2x2.reflectionalSymm shouldBe 4

  it should "calculate the reflectional symmetry for a 3x3 square" in:
    val square3x3 = TilingBuilder.createRhombusNet(3, 3)
    square3x3.reflectionalSymm shouldBe 4

  it should "calculate the reflectional symmetry for a 4x4 square" in:
    val square4x4 = TilingBuilder.createRhombusNet(4, 4)
    square4x4.reflectionalSymm shouldBe 4

  it should "calculate the reflectional symmetry for a 3x2 rectangle" in:
    val rectangle3x2 = TilingBuilder.createRhombusNet(3, 2)
    rectangle3x2.reflectionalSymm shouldBe 2

  it should "calculate the reflectional symmetry for an equilateral triangle" in:
    triangle.reflectionalSymm shouldBe 3

  it should "calculate the reflectional symmetry for a triangle net" in:
    val triangleNet = TilingBuilder.createTriangleNet(3, 3)
    triangleNet.reflectionalSymm shouldBe 0

  it should "calculate the reflectional symmetry for a regular hexagon" in:
    hexagon.reflectionalSymm shouldBe 6

  it should "calculate the reflectional symmetry for a 2x1 hexagon net" in:
    val hex2x1 = TilingBuilder.createHexagonNet(2, 1)
    hex2x1.reflectionalSymm shouldBe 2

  it should "calculate the reflectional symmetry for a 4x4 hexagon net" in:
    val hex4x4 = TilingBuilder.createHexagonNet(4, 4)
    hex4x4.reflectionalSymm shouldBe 1

  it should "calculate the reflectional symmetry for a modified 4x4 square" in:
    oneAsymmetricHole.reflectionalSymm shouldBe 0

  it should "calculate the reflectional symmetry for another modified 4x4 square" in:
    twoAsymmetricHoles.reflectionalSymm shouldBe 0

  it should "calculate the reflectional symmetry for a third modified 4x4 square" in:
    fourRotationalHoles.reflectionalSymm shouldBe 0

  it should "calculate the reflectional symmetry for a fourth modified 4x4 square" in:
    twoRotationalHoles.reflectionalSymm shouldBe 0

  it should "calculate the reflectional symmetry for a 4x4 square with two reflected holes" in :
    twoReflectionalHoles.reflectionalSymm shouldBe 1
