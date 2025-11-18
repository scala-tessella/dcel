package io.github.scala_tessella.dcel.torus

import io.github.scala_tessella.dcel.geometry.{RegularPolygon, SimplePolygon}
import io.github.scala_tessella.dcel.{TilingBuilder, TilingTestHelpers}
import io.github.scala_tessella.dcel.torus.TilingTorusBuilder.*
import io.github.scala_tessella.dcel.structure.{FaceId, VertexId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingTorusDCELSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingTorusDCEL.unorderedBoundaryVertices"

  it should "return the boundary of a 2x2 square net" in {
    val torus = createSquareNet(2, 2)
    torus.unorderedBoundaryVertices.map(_.id) shouldBe List(V1, V2, V3, V4)
  }

  it should "return the boundary of a 3x2 square net" in {
    val torus = createSquareNet(3, 2)
    torus.unorderedBoundaryVertices.map(_.id) shouldBe List(V1, V3, V4, VertexId("V6"))
  }

  it should "return the boundary of a 3x3 square net" in {
    val torus = createSquareNet(3, 3)
    torus.unorderedBoundaryVertices.map(_.id) shouldBe List(
      V1,
      V2,
      V3,
      V4,
      VertexId("V6"),
      VertexId("V7"),
      VertexId("V8"),
      VertexId("V9")
    )
  }

  behavior of "TilingTorusDCEL.findVertex/findFace"

  it should "find existing vertices and faces" in {
    val torus = createSquareNet(2, 2)
    allAssert(
      torus.findVertex(V1).isRight shouldBe true,
      torus.findVertex(V2).isRight shouldBe true,
      torus.findVertex(V3).isRight shouldBe true,
      torus.findVertex(V4).isRight shouldBe true,
      torus.findFace(F1).isRight shouldBe true,
      torus.findFace(F2).isRight shouldBe true,
      torus.findFace(F3).isRight shouldBe true,
      torus.findFace(FaceId("F4")).isRight shouldBe true
    )
  }

  it should "fail on non-existing vertices and faces" in {
    val torus = createSquareNet(2, 2)
    allAssert(
      torus.findVertex(VertexId("VX")).isLeft shouldBe true,
      torus.findFace(FaceId("FX")).isLeft shouldBe true
    )
  }

//  behavior of "TilingTorusDCEL.toTilingDCEL"
//
//  it should "be converted" in {
//    val tiling = createSquareNet(4, 4).toTilingDCEL
//    println(tiling)
//    tiling.isRight shouldBe true
//  }

  behavior of "TilingTorusDCEL.fromTilingDCEL"

  it should "be converted from a 1x1 square" in {
    val tilingDCEL    = TilingBuilder.createRegularPolygon(RegularPolygon(4))
    val result        = TilingTorusDCEL.fromTilingDCEL(tilingDCEL)
//    val scale: Double = 1.0 / 1.0
//    println(result.value.toSVG3D(TorusSvg3DOptions().copy(
//      uScale = scale,
//      vScale = scale,
//      showVertexIds = true
//    )))
    allAssert(
      result.isRight shouldBe true,
      result.value.faces.size shouldBe 1,
      result.value.faces.map(_.getVerticesUnsafe.map(_.id)) shouldBe
        List(
          List(V1, V1, V1, V1)
        ),
      result.value.faces.map(_.anglesUnsafe) shouldBe
        List(
          List(90, 90, 90, 90),
        ),
      result.value.vertices.size shouldBe 1,
      result.value.halfEdges.size shouldBe 4,
      result.value.halfEdges.forall(_.isLoop.get) shouldBe true
    )
  }

  it should "be converted from a 2x2 square" in {
    val tilingDCEL = TilingBuilder.createSimplePolygon(SimplePolygon(90, 90, 90, 90).multiplySidesBy(2)).toOption.get
    val result = TilingTorusDCEL.fromTilingDCEL(tilingDCEL)
//    val scale: Double = 1.0 / 2.0
//    println(result.value.toSVG3D(TorusSvg3DOptions().copy(
//      uScale = scale,
//      vScale = scale,
//      showVertexIds = true
//    )))
    allAssert(
      result.isRight shouldBe true,
      result.value.faces.size shouldBe 1,
      result.value.faces.map(_.getVerticesUnsafe.map(_.id)) shouldBe
        List(
          List(V1, "V6", V1, "V8", V1, "V6", V1, "V8")
        ),
      result.value.faces.map(_.anglesUnsafe) shouldBe
        List(
          List(90, 180, 90, 180, 90, 180, 90, 180),
        ),
      result.value.vertices.size shouldBe 3,
      result.value.halfEdges.size shouldBe 8,
      result.value.halfEdges.exists(_.isLoop.get) shouldBe false
    )
  }

  it should "be converted from a 2x2 square net" in {
    val tilingDCEL    = TilingBuilder.createRhombusNet(2, 2)
    val result        = TilingTorusDCEL.fromTilingDCEL(tilingDCEL)
//    val scale: Double = 1.0 / 2.0
//    println(result.value.toSVG3D(TorusSvg3DOptions().copy(
//      uScale = scale,
//      vScale = scale,
//      showVertexIds = true
//    )))
    allAssert(
      result.isRight shouldBe true,
      result.value.faces.size shouldBe 4,
      result.value.vertices.size shouldBe 4,
      result.value.halfEdges.size shouldBe 16,
      result.value.halfEdges.exists(_.isLoop.get) shouldBe false
    )
  }

  it should "be converted from a 3x3 square net" in {
    val tilingDCEL    = TilingBuilder.createRhombusNet(3, 3)
    val result        = TilingTorusDCEL.fromTilingDCEL(tilingDCEL)
//    val scale: Double = 1.0 / 3.0
//    println(result.value.toSVG3D(TorusSvg3DOptions().copy(
//      uScale = scale,
//      vScale = scale,
//      showVertexIds = true
//    )))
    allAssert(
      result.isRight shouldBe true,
      result.value.faces.size shouldBe 9,
      result.value.vertices.size shouldBe 9,
      result.value.halfEdges.size shouldBe 36,
      result.value.halfEdges.exists(_.isLoop.get) shouldBe false
    )
  }

  it should "be converted from a 3x2 square net" in {
    val tilingDCEL = TilingBuilder.createRhombusNet(3, 2)
    val result     = TilingTorusDCEL.fromTilingDCEL(tilingDCEL)
//    println(result)
    allAssert(
      result.isRight shouldBe true,
      result.value.faces.size shouldBe 6,
      result.value.vertices.size shouldBe 6,
      result.value.halfEdges.size shouldBe 24,
      result.value.halfEdges.exists(_.isLoop.get) shouldBe false
    )
  }

  it should "be converted from a 2x1 hexagon net" in {
    val tilingDCEL = TilingBuilder.createHexagonNet(2, 1)
    val result     = TilingTorusDCEL.fromTilingDCEL(tilingDCEL)
//    val vScale: Double = 1.0 / 1.5
//    val uScale: Double = 1.0 / (4 * 0.8660254037844386)
//    println(result.value.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
//    println(result)
    allAssert(
      result.isRight shouldBe true,
      result.value.faces.size shouldBe 2,
      result.value.faces.map(_.getVerticesUnsafe.map(_.id)) shouldBe
        List(
          List(V1, V2, V3, V2, V1, "V6"),
          List(V3, "V6", V1, "V6", V3, V2)
        ),
      result.value.faces.map(_.anglesUnsafe) shouldBe
        List(
          List(120, 120, 120, 120, 120, 120),
          List(120, 120, 120, 120, 120, 120)
        ),
      result.value.vertices.size shouldBe 4,
      result.value.halfEdges.size shouldBe 12,
      result.value.halfEdges.exists(_.isLoop.get) shouldBe false
    )
  }

  it should "be converted from a 2x2 hexagon net" in {
    val tilingDCEL = TilingBuilder.createHexagonNet(2, 2)
    val result     = TilingTorusDCEL.fromTilingDCEL(tilingDCEL)
//    val vScale: Double = 1.0 / (1.5 * 2)
//    val uScale: Double = 1.0 / (4 * 0.8660254037844386)
//    println(result.value.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      result.isRight shouldBe true,
      result.value.faces.size shouldBe 4,
      result.value.faces.map(_.getVerticesUnsafe.map(_.id)) shouldBe
        List(
          List(V1, V2, V3, V4, "V5", "V6"),
          List(V3, "V13", V1, "V6", "V10", V4),
          List("V5", V4, "V10", V2, V1, "V13"),
          List("V10", "V6", "V5", "V13", V3, V2)
        ),
      result.value.faces.map(_.anglesUnsafe) shouldBe
        List(
          List(120, 120, 120, 120, 120, 120),
          List(120, 120, 120, 120, 120, 120),
          List(120, 120, 120, 120, 120, 120),
          List(120, 120, 120, 120, 120, 120)
        ),
      result.value.vertices.size shouldBe 8,
      result.value.halfEdges.size shouldBe 24,
      result.value.halfEdges.exists(_.isLoop.get) shouldBe false
    )
  }

  it should "be converted from a 4x4 hexagon net" in {
    val tilingDCEL = TilingBuilder.createHexagonNet(4, 4)
    val result = TilingTorusDCEL.fromTilingDCEL(tilingDCEL)
//    val vScale: Double = 1.0 / (1.5 * 4)
//    val uScale: Double = 1.0 / (8 * 0.8660254037844386)
//    println(result.value.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      result.isRight shouldBe true,
      result.value.faces.size shouldBe 16,
      result.value.vertices.size shouldBe 32,
      result.value.halfEdges.size shouldBe 96,
      result.value.halfEdges.exists(_.isLoop.get) shouldBe false
    )
  }

  it should "be converted from a 8x8 hexagon net" in {
    val tilingDCEL = TilingBuilder.createHexagonNet(4, 8)
    val result = TilingTorusDCEL.fromTilingDCEL(tilingDCEL)
    //    val vScale: Double = 1.0 / (1.5 * 8)
    //    val uScale: Double = 1.0 / (8 * 0.8660254037844386)
    //    println(result.value.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      result.isRight shouldBe true,
      result.value.faces.size shouldBe 32,
      result.value.vertices.size shouldBe 64,
      result.value.halfEdges.size shouldBe 192,
      result.value.halfEdges.exists(_.isLoop.get) shouldBe false
    )
  }
