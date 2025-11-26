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
    val net = TilingBuilder.createRhombusNet(3, 3).deleteEdge(V2, VertexId("V6")).value
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
    val net = TilingBuilder.createRhombusNet(3, 3).deleteEdge(V2, VertexId("V6")).value
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
