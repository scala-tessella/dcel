package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingDeletion.deleteEdge
import io.github.scala_tessella.dcel.TilingSymmetry.*
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon}
import io.github.scala_tessella.dcel.structure.VertexId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingSymmetrySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingSymmetry.boundaryRotationalSymmetry"

  it should "return 3 for a triangle" in:
    triangle.boundaryRotationalSymmetry shouldBe 3

  it should "return 4 for a square" in:
    square.boundaryRotationalSymmetry shouldBe 4

  it should "return 6 for a hexagon" in:
    hexagon.boundaryRotationalSymmetry shouldBe 6

  it should "return 1 for an empty tiling" in:
    emptyTiling.boundaryRotationalSymmetry shouldBe 1

  it should "return correct rotational symmetry for a two-triangle tiling" in:
    val twoTriangles = triangle.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
    twoTriangles.boundaryRotationalSymmetry shouldBe 2

  it should "return correct rotational symmetry for bench configuration" in:
    val bench = hexagon
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V3, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(4)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V8"), RegularPolygon(4)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V11"), RegularPolygon(6)).value

    bench.boundaryRotationalSymmetry shouldBe 1

  it should "return correct rotational symmetry for a square net" in:
    val net = TilingBuilder.createRhombusNet(3, 3)
    net.boundaryRotationalSymmetry shouldBe 4

  it should "return correct rotational symmetry for a holed square net" in:
    val net = TilingBuilder.createRhombusNet(3, 3).deleteEdge(V2, V6).value
    net.boundaryRotationalSymmetry shouldBe 4
    net.boundaryVerticesRotationalSymmetry shouldBe 1

  it should "return correct rotational symmetry for a rhombi net" in:
    val net = TilingBuilder.createRhombusNet(3, 3, AngleDegree(60))
    net.boundaryRotationalSymmetry shouldBe 2

  behavior of "TilingSymmetry.boundaryReflectionalSymmetry"

  it should "return 3 for a triangle" in:
    triangle.boundaryReflectionalSymmetry shouldBe 3

  it should "return 4 for a square" in:
    square.boundaryReflectionalSymmetry shouldBe 4

  it should "return 6 for a hexagon" in:
    hexagon.boundaryReflectionalSymmetry shouldBe 6

  it should "return 1 for an empty tiling" in:
    emptyTiling.boundaryReflectionalSymmetry shouldBe 0

  it should "return correct reflectional symmetry for a two-triangle tiling" in:
    val twoTriangles = triangle.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
    twoTriangles.boundaryReflectionalSymmetry shouldBe 2

  it should "return correct reflectional symmetry for bench configuration" in:
    val bench = hexagon
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V3, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(4)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V8"), RegularPolygon(4)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V11"), RegularPolygon(6)).value

    bench.boundaryReflectionalSymmetry shouldBe 1

  it should "return correct reflectional symmetry for a square net of same width and height" in:
    val net = TilingBuilder.createRhombusNet(3, 3)
    net.boundaryReflectionalSymmetry shouldBe 4

  it should "return correct reflectional symmetry for a holed square net" in:
    val net = TilingBuilder.createRhombusNet(3, 3).deleteEdge(V2, V6).value
    net.boundaryReflectionalSymmetry shouldBe 4
    net.boundaryVerticesReflectionalSymmetry shouldBe 0

  it should "return correct reflectional symmetry for a square net of different width and height" in:
    val net = TilingBuilder.createRhombusNet(3, 4)
    net.boundaryReflectionalSymmetry shouldBe 2

  behavior of "TilingSymmetry with complex configurations"

  it should "handle tilings with holes" in:
    val net       = TilingBuilder.createRhombusNet(3, 6)
    val holeInNet = net.deleteEdge(VertexId("V14"), VertexId("V15")).value

    allAssert(
      holeInNet.boundaryRotationalSymmetry shouldBe 2,
      holeInNet.boundaryReflectionalSymmetry shouldBe 2
    )

  it should "handle triangle net" in:
    val triangleNet = TilingBuilder.createTriangleNet(4, 4)
    allAssert(
      triangleNet.boundaryRotationalSymmetry shouldBe 2,
      triangleNet.boundaryReflectionalSymmetry shouldBe 2
    )

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
