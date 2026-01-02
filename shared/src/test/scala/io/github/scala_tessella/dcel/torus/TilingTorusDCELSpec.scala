package io.github.scala_tessella.dcel.torus

import io.github.scala_tessella.dcel.geometry.{RegularPolygon, SimplePolygon}
import io.github.scala_tessella.dcel.{TilingBuilder, TilingTestHelpers}
import io.github.scala_tessella.dcel.torus.TilingTorusBuilder.*
import io.github.scala_tessella.dcel.structure.{FaceId, VertexId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingTorusDCELSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingTorusDCEL.unorderedBoundaryVertices"

  it should "return the boundary of a 2x2 square net" in:
    val torus = createSquareNet(2, 2)
    torus.unorderedBoundaryVertices.map(_.id) shouldBe List(V1, V2, V3, V4)

  it should "return the boundary of a 3x2 square net" in:
    val torus = createSquareNet(3, 2)
    torus.unorderedBoundaryVertices.map(_.id) shouldBe List(V1, V3, V4, V6)

  it should "return the boundary of a 3x3 square net" in:
    val torus = createSquareNet(3, 3)
    torus.unorderedBoundaryVertices.map(_.id) shouldBe List(
      V1,
      V2,
      V3,
      V4,
      V6,
      VertexId("V7"),
      VertexId("V8"),
      VertexId("V9")
    )

  behavior of "TilingTorusDCEL.findVertex/findFace"

  it should "find existing vertices and faces" in:
    val torus = createSquareNet(2, 2)
    allAssert(
      torus.findVertex(V1).isRight shouldBe true,
      torus.findVertex(V2).isRight shouldBe true,
      torus.findVertex(V3).isRight shouldBe true,
      torus.findVertex(V4).isRight shouldBe true,
      torus.findFace(F1).isRight shouldBe true,
      torus.findFace(F2).isRight shouldBe true,
      torus.findFace(F3).isRight shouldBe true,
      torus.findFace(FaceId(4)).isRight shouldBe true
    )

  it should "fail on non-existing vertices and faces" in:
    val torus = createSquareNet(2, 2)
    allAssert(
      torus.findVertex(VertexId("VX")).isLeft shouldBe true,
      torus.findFace(FaceId(999)).isLeft shouldBe true
    )

//  behavior of "TilingTorusDCEL.toTilingDCEL"
//
//  it should "be converted" in:
//    val tiling = createSquareNet(4, 4).toTilingDCEL
//    println(tiling)
//    tiling.isRight shouldBe true
//

  behavior of "TilingTorusDCEL.fromTilingDCEL"

  it should "be converted from a square" in:
    val tilingDCEL = TilingBuilder.createRegularPolygon(RegularPolygon(4))
    val torus      = TilingTorusDCEL.fromTilingDCEL(tilingDCEL).value
//    val scale: Double = 1.0 / 1.0
//    println(result.value.toSVG3D(TorusSvg3DOptions().copy(
//      uScale = scale,
//      vScale = scale,
//      showVertexIds = true
//    )))
    allAssert(
      torus.faces.size shouldBe 1,
      torus.facesWithIncorrectCoords.size shouldBe 1,
      torus.faces.map(_.getVerticesUnsafe.map(_.id)) shouldBe
        List(
          List(V1, V1, V1, V1)
        ),
      torus.faces.map(_.anglesUnsafe) shouldBe
        List(
          List(90, 90, 90, 90)
        ),
      torus.vertices.size shouldBe 1,
      torus.halfEdges.size shouldBe 4,
      torus.halfEdges.forall(_.isLoop.get) shouldBe true
//      torus.toBox.width.almostEqual(1) shouldBe true,
//      torus.toBox.height.almostEqual(1) shouldBe true
    )

  it should "be converted from a 2x2 square" in:
    val tilingDCEL =
      TilingBuilder.createSimplePolygonUnsafe(SimplePolygon(90, 90, 90, 90).multiplySidesBy(2))
    val torus      = TilingTorusDCEL.fromTilingDCEL(tilingDCEL).value
//    val scale: Double = 1.0 / 2.0
//    println(result.value.toSVG3D(TorusSvg3DOptions().copy(
//      uScale = scale,
//      vScale = scale,
//      showVertexIds = true
//    )))
    allAssert(
      torus.faces.size shouldBe 1,
      torus.facesWithIncorrectCoords.size shouldBe 1,
      torus.faces.map(_.getVerticesUnsafe.map(_.id)) shouldBe
        List(
          List(V1, V6, V1, "V8", V1, V6, V1, "V8")
        ),
      torus.faces.map(_.anglesUnsafe) shouldBe
        List(
          List(90, 180, 90, 180, 90, 180, 90, 180)
        ),
      torus.vertices.size shouldBe 3,
      torus.halfEdges.size shouldBe 8,
      torus.halfEdges.exists(_.isLoop.get) shouldBe false
//      torus.toBox.width.almostEqual(2) shouldBe true,
//      torus.toBox.height.almostEqual(2) shouldBe true
    )

  it should "be converted from a 2x2 square net" in:
    val tilingDCEL = TilingBuilder.createRhombusNet(2, 2)
    val torus      = TilingTorusDCEL.fromTilingDCEL(tilingDCEL).value
//    val scale: Double = 1.0 / 2.0
//    println(result.value.toSVG3D(TorusSvg3DOptions().copy(
//      uScale = scale,
//      vScale = scale,
//      showVertexIds = true
//    )))
    allAssert(
      torus.faces.size shouldBe 4,
      torus.facesWithIncorrectCoords.size shouldBe 3,
      torus.faces.map(_.getVerticesUnsafe.map(_.id)) shouldBe
        List(
          List(V1, "V8", V5, V4),
          List("V8", V1, V4, V5),
          List(V4, V5, "V8", V1),
          List(V5, V4, V1, "V8")
        ),
      torus.vertices.size shouldBe 4,
      torus.halfEdges.size shouldBe 16,
      torus.halfEdges.exists(_.isLoop.get) shouldBe false
//      torus.toBox.width.almostEqual(2) shouldBe true,
//      torus.toBox.height.almostEqual(2) shouldBe true
    )

  it should "be converted from a 3x3 square net" in:
    val tilingDCEL = TilingBuilder.createRhombusNet(3, 3)
//    println(tilingDCEL.toTorusCheck)
    val torus      = TilingTorusDCEL.fromTilingDCEL(tilingDCEL).value
//    val scale: Double = 1.0 / 3.0
//    println(result.value.toSVG3D(TorusSvg3DOptions().copy(
//      uScale = scale,
//      vScale = scale,
//      showVertexIds = true
//    )))
    allAssert(
      torus.faces.size shouldBe 9,
      torus.facesWithIncorrectCoords.size shouldBe 5,
      torus.vertices.size shouldBe 9,
      torus.halfEdges.size shouldBe 36,
      torus.halfEdges.exists(_.isLoop.get) shouldBe false
//      torus.toBox.width.almostEqual(3) shouldBe true,
//      torus.toBox.height.almostEqual(3) shouldBe true
    )

  it should "be converted from a 3x2 square net" in:
    val tilingDCEL = TilingBuilder.createRhombusNet(3, 2)
//    println(tilingDCEL.toTorusCheck)
    val torus      = TilingTorusDCEL.fromTilingDCEL(tilingDCEL).value
    allAssert(
      torus.faces.size shouldBe 6,
      torus.vertices.size shouldBe 6,
      torus.halfEdges.size shouldBe 24,
      torus.halfEdges.exists(_.isLoop.get) shouldBe false
//      torus.toBox.width.almostEqual(3) shouldBe true,
//      torus.toBox.height.almostEqual(2) shouldBe true
    )

//  it should "be converted from a 2x1 hexagon net" in:
//    val tilingDCEL = TilingBuilder.createHexagonNet(2, 1)
////    println(tilingDCEL.toTorusCheck)
//    val torus      = TilingTorusDCEL.fromTilingDCEL(tilingDCEL).value
////    val vScale: Double = 1.0 / 1.5
////    val uScale: Double = 1.0 / (4 * 0.8660254037844386)
////    println(result.value.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
////    println(result)
//    allAssert(
//      torus.faces.size shouldBe 2,
//      torus.faces.map(_.getVerticesUnsafe.map(_.id)) shouldBe
//        List(
//          List(V1, V2, V3, V2, V1, V6),
//          List(V3, V6, V1, V6, V3, V2)
//        ),
//      torus.faces.map(_.anglesUnsafe) shouldBe
//        List(
//          List(120, 120, 120, 120, 120, 120),
//          List(120, 120, 120, 120, 120, 120)
//        ),
//      torus.facesWithIncorrectCoords.size shouldBe 2,
//      torus.vertices.size shouldBe 4,
//      torus.halfEdges.size shouldBe 12,
//      torus.halfEdges.exists(_.isLoop.get) shouldBe false
//    )
//
//  it should "be converted from a 2x2 hexagon net" in:
//    val tilingDCEL     = TilingBuilder.createHexagonNet(2, 2)
////    println(tilingDCEL.toTorusCheck)
//    val torus          = TilingTorusDCEL.fromTilingDCEL(tilingDCEL).value
//    val uScale: Double = 1.0 / 3.0
//    val vScale: Double = 1.0 / (4 * 0.8660254037844386)
////    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
//    allAssert(
//      torus.faces.size shouldBe 4,
//      torus.faces.map(_.getVerticesUnsafe.map(_.id)) shouldBe
//        List(
//          List(V1, V2, V3, V4, V5, V6),
//          List(V3, "V13", V1, V6, "V10", V4),
//          List(V5, V4, "V10", V2, V1, "V13"),
//          List("V10", V6, V5, "V13", V3, V2)
//        ),
//      torus.faces.map(_.anglesUnsafe) shouldBe
//        List(
//          List(120, 120, 120, 120, 120, 120),
//          List(120, 120, 120, 120, 120, 120),
//          List(120, 120, 120, 120, 120, 120),
//          List(120, 120, 120, 120, 120, 120)
//        ),
//      torus.vertices.size shouldBe 8,
//      torus.halfEdges.size shouldBe 24,
//      torus.halfEdges.exists(_.isLoop.get) shouldBe false
//    )

  it should "be converted from a 4x4 hexagon net" in:
    val tilingDCEL     = TilingBuilder.createHexagonNet(4, 4)
//    println(tilingDCEL.toTorusCheck)
    val torus          = TilingTorusDCEL.fromTilingDCEL(tilingDCEL).value
    val uScale: Double = 1.0 / (4 * 1.5)
    val vScale: Double = 1.0 / (4 * 2 * 0.8660254037844386)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.faces.size shouldBe 16,
      torus.vertices.size shouldBe 32,
      torus.halfEdges.size shouldBe 96,
      torus.halfEdges.exists(_.isLoop.get) shouldBe false
    )

  it should "be converted from a 8x8 hexagon net" in:
    val tilingDCEL = TilingBuilder.createHexagonNet(8, 8)
//    println(tilingDCEL.toTorusCheck)
    val torus      = TilingTorusDCEL.fromTilingDCEL(tilingDCEL).value
//    val uScale: Double = 1.0 / (8 * 1.5)
//    val vScale: Double = 1.0 / (8 * 2 * 0.8660254037844386)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.faces.size shouldBe 64,
      torus.vertices.size shouldBe 128,
      torus.halfEdges.size shouldBe 384,
      torus.halfEdges.exists(_.isLoop.get) shouldBe false
    )

  it should "be converted from a 1x1 triangle net" in:
    val tilingDCEL = TilingBuilder.createTriangleNet(1, 1)
    val torus      = TilingTorusDCEL.fromTilingDCEL(tilingDCEL).value
    allAssert(
      torus.faces.size shouldBe 2,
      torus.faces.map(_.getVerticesUnsafe.map(_.id)) shouldBe
        List(
          List(V1, V1, V1),
          List(V1, V1, V1)
        ),
      torus.faces.map(_.anglesUnsafe) shouldBe
        List(
          List(60, 60, 60),
          List(60, 60, 60)
        ),
      torus.vertices.size shouldBe 1,
      torus.halfEdges.size shouldBe 6,
      torus.halfEdges.forall(_.isLoop.get) shouldBe true
    )

  it should "be converted from a 2x1 triangle net" in:
    val tilingDCEL = TilingBuilder.createTriangleNet(2, 1)
    val torus      = TilingTorusDCEL.fromTilingDCEL(tilingDCEL).value
    allAssert(
      torus.faces.size shouldBe 4,
      torus.faces.map(_.getVerticesUnsafe.map(_.id)) shouldBe
        List(
          List(V1, V5, V1),
          List(V5, V5, V1),
          List(V5, V1, V5),
          List(V1, V1, V5)
        ),
      torus.faces.map(_.anglesUnsafe) shouldBe
        List(
          List(60, 60, 60),
          List(60, 60, 60),
          List(60, 60, 60),
          List(60, 60, 60)
        ),
      torus.vertices.size shouldBe 2,
      torus.halfEdges.size shouldBe 12,
      torus.halfEdges.count(_.isLoop.get) shouldBe 4
    )
