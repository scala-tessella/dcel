package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingDeletion.deleteEdge
import io.github.scala_tessella.dcel.TilingSymmetry.*
import io.github.scala_tessella.dcel.structure.VertexId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingSymmetrySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingSymmetry.rotationalSymm"

  it should "calculate the rotational symmetry for a square" in:
    square.rotationalSymmetryOrder shouldBe 4

  it should "calculate the rotational symmetry for a 2x2 square" in:
    val square2x2 = TilingBuilder.createRhombusNet(2, 2)
    square2x2.rotationalSymmetryOrder shouldBe 4

  it should "calculate the rotational symmetry for a 3x3 square" in:
    val square3x3 = TilingBuilder.createRhombusNet(3, 3)
    square3x3.rotationalSymmetryOrder shouldBe 4

  it should "calculate the rotational symmetry for a 4x4 square" in:
    val square4x4 = TilingBuilder.createRhombusNet(4, 4)
    square4x4.rotationalSymmetryOrder shouldBe 4

  it should "calculate the rotational symmetry for a 3x2 rectangle" in:
    val rectangle3x2 = TilingBuilder.createRhombusNet(3, 2)
    rectangle3x2.rotationalSymmetryOrder shouldBe 2

  it should "calculate the rotational symmetry for an equilateral triangle" in:
    triangle.rotationalSymmetryOrder shouldBe 3

  it should "calculate the rotational symmetry for a triangle net" in:
    val triangleNet = TilingBuilder.createTriangleNet(3, 3)
    triangleNet.rotationalSymmetryOrder shouldBe 2

  it should "calculate the rotational symmetry for a regular hexagon" in:
    allAssert(
      hexagon.rotationalSymmetryOrder shouldBe 6,
      hexagon.rotationalVertexIds shouldBe List(V1, V6, V5, V4, V3, V2)
    )

  it should "calculate the rotational symmetry for a 2x1 hexagon net" in:
    val hex2x1 = TilingBuilder.createHexagonNet(2, 1)
    hex2x1.rotationalSymmetryOrder shouldBe 2

  it should "calculate the rotational symmetry for a 4x4 hexagon net" in:
    val hex4x4 = TilingBuilder.createHexagonNet(4, 4)
    hex4x4.rotationalSymmetryOrder shouldBe 2

  /** <img src="file:../../../../../resources/symmetry/oneAsymmHole.svg"/> */
  val oneAsymmetricHole: TilingDCEL =
    TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value

  it should "calculate the rotational symmetry for a 4x4 square with one asymmetric hole" in:
    oneAsymmetricHole.rotationalSymmetryOrder shouldBe 1

  /** <img src="file:../../../../../resources/symmetry/twoAsymmHoles.svg"/> */
  val twoAsymmetricHoles: TilingDCEL =
    TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value
      .deleteEdge(VertexId("V17"), VertexId("V22")).value

  it should "calculate the rotational symmetry for a 4x4 square with two asymmetrical holes" in:
    twoAsymmetricHoles.rotationalSymmetryOrder shouldBe 1

  /** <img src="file:../../../../../resources/symmetry/fourRotationalHoles.svg"/> */
  val fourRotationalHoles: TilingDCEL =
    TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value
      .deleteEdge(VertexId("V17"), VertexId("V22")).value
      .deleteEdge(VertexId("V19"), VertexId("V20")).value
      .deleteEdge(VertexId("V4"), VertexId("V9")).value

  it should "calculate the rotational symmetry for a 4x4 square with four rotated holes" in:
    fourRotationalHoles.rotationalSymmetryOrder shouldBe 4

  /** <img src="file:../../../../../resources/symmetry/twoRotationalHoles.svg"/> */
  val twoRotationalHoles: TilingDCEL =
    TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value
      .deleteEdge(VertexId("V19"), VertexId("V20")).value

  it should "calculate the rotational symmetry for a 4x4 square with two rotated holes" in:
    allAssert(
      twoRotationalHoles.rotationalSymmetryOrder shouldBe 2,
      twoRotationalHoles.rotationalVertexIds shouldBe List(V6, "V20")
    )

  /** <img src="file:../../../../../resources/symmetry/twoReflectionalHoles.svg"/> */
  val twoReflectionalHoles: TilingDCEL =
    TilingBuilder.createRhombusNet(4, 4)
      .deleteEdge(V6, VertexId("V7")).value
      .deleteEdge(VertexId("V9"), VertexId("V10")).value

  it should "calculate the rotational symmetry for a 4x4 square with two reflected holes" in:
    allAssert(
      twoReflectionalHoles.rotationalSymmetryOrder shouldBe 1,
      twoReflectionalHoles.rotationalVertexIds shouldBe List(V6)
    )

  behavior of "TilingSymmetry.rotationalVertexIds"

  it should "calculate the rotational vertex ids for a 4x4 square" in:
    val square4x4 = TilingBuilder.createRhombusNet(4, 4)
    square4x4.rotationalVertexIds shouldBe
      List(V6, "V22", "V20", V4)
    square4x4.rotationalVertexIdsAlt shouldBe
      List(
        BoundaryVertex(VertexId("V11")),
        BoundaryVertex(VertexId("V23")),
        BoundaryVertex(VertexId("V15")),
        BoundaryVertex(V3)
      )

  it should "calculate the rotational vertex ids for a 3x3 square" in:
    val square3x3 = TilingBuilder.createRhombusNet(3, 3)
    square3x3.rotationalVertexIds shouldBe
      List(V5, "V14", "V12", V3)
    square3x3.rotationalVertexIdsAlt shouldBe
      List(
        BoundaryEdge(V5, VertexId("V9")),
        BoundaryEdge(VertexId("V14"), VertexId("V15")),
        BoundaryEdge(VertexId("V12"), VertexId("V8")),
        BoundaryEdge(V3, V2)
      )

  behavior of "TilingSymmetry.reflectionalSymm"

  it should "calculate the reflectional symmetry for a square" in:
    square.reflectionalSymmetryOrder shouldBe 4

  it should "calculate the reflectional symmetry for a 2x2 square" in:
    val square2x2 = TilingBuilder.createRhombusNet(2, 2)
    square2x2.reflectionalSymmetryOrder shouldBe 4

  it should "calculate the reflectional symmetry for a 3x3 square" in:
    val square3x3 = TilingBuilder.createRhombusNet(3, 3)
    square3x3.reflectionalSymmetryOrder shouldBe 4

  it should "calculate the reflectional symmetry for a 4x4 square" in:
    val square4x4 = TilingBuilder.createRhombusNet(4, 4)
    square4x4.reflectionalSymmetryOrder shouldBe 4

  it should "calculate the reflectional symmetry for a 3x2 rectangle" in:
    val rectangle3x2 = TilingBuilder.createRhombusNet(3, 2)
    rectangle3x2.reflectionalSymmetryOrder shouldBe 2

  it should "calculate the reflectional symmetry for an equilateral triangle" in:
    triangle.reflectionalSymmetryOrder shouldBe 3

  it should "calculate the reflectional symmetry for a triangle net" in:
    val triangleNet = TilingBuilder.createTriangleNet(3, 3)
    triangleNet.reflectionalSymmetryOrder shouldBe 2

  it should "calculate the reflectional symmetry for a regular hexagon" in:
    hexagon.reflectionalSymmetryOrder shouldBe 6

  it should "calculate the reflectional symmetry for a 2x1 hexagon net" in:
    val hex2x1 = TilingBuilder.createHexagonNet(2, 1)
    hex2x1.reflectionalSymmetryOrder shouldBe 2

  it should "calculate the reflectional symmetry for a 4x4 hexagon net" in:
    val hex4x4 = TilingBuilder.createHexagonNet(4, 4)
    hex4x4.reflectionalSymmetryOrder shouldBe 2

  it should "calculate the reflectional symmetry for a modified 4x4 square" in:
    oneAsymmetricHole.reflectionalSymmetryOrder shouldBe 0

  it should "calculate the reflectional symmetry for another modified 4x4 square" in:
    twoAsymmetricHoles.reflectionalSymmetryOrder shouldBe 0

  it should "calculate the reflectional symmetry for a third modified 4x4 square" in:
    fourRotationalHoles.reflectionalSymmetryOrder shouldBe 0

  it should "calculate the reflectional symmetry for a fourth modified 4x4 square" in:
    twoRotationalHoles.reflectionalSymmetryOrder shouldBe 0

  it should "calculate the reflectional symmetry for a 4x4 square with two reflected holes" in:
    twoReflectionalHoles.reflectionalSymmetryOrder shouldBe 1
