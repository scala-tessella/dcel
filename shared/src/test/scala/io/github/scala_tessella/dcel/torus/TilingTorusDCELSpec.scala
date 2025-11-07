package io.github.scala_tessella.dcel.torus

import io.github.scala_tessella.dcel.TilingTestHelpers
import io.github.scala_tessella.dcel.torus.TilingTorusDCEL.TorusSvg3DOptions
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
      torus.findVertex(VertexId("V1")).isRight shouldBe true,
      torus.findVertex(VertexId("V2")).isRight shouldBe true,
      torus.findVertex(VertexId("V3")).isRight shouldBe true,
      torus.findVertex(VertexId("V4")).isRight shouldBe true,
      torus.findFace(FaceId("F1")).isRight shouldBe true,
      torus.findFace(FaceId("F2")).isRight shouldBe true,
      torus.findFace(FaceId("F3")).isRight shouldBe true,
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

  behavior of "TilingTorusDCEL.toSVG3D"

  it should "draw a 1x1 square net on a torus" in {
    val torus = createSquareNet(1, 1)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 1,
      torus.faces.size shouldBe 1,
      torus.halfEdges.size shouldBe 4
    )
  }

  it should "draw a 1x2 square net on a torus" in {
    val torus = createSquareNet(1, 2)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 2,
      torus.faces.size shouldBe 2,
      torus.halfEdges.size shouldBe 8
    )
  }

  it should "draw a 2x1 square net on a torus" in {
    val torus = createSquareNet(2, 1)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 2,
      torus.faces.size shouldBe 2,
      torus.halfEdges.size shouldBe 8
    )
  }

  it should "draw a 2x2 square net on a torus" in {
    val torus         = createSquareNet(2, 2)
    val scale: Double = 1.0 / 2.0
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = scale, vScale = scale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 4,
      torus.faces.size shouldBe 4,
      torus.halfEdges.size shouldBe 16
    )
  }

  it should "draw a 3x3 square net on a torus" in {
    val torus         = createSquareNet(3, 3)
    val scale: Double = 1.0 / 3.0
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = scale, vScale = scale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 9,
      torus.faces.size shouldBe 9,
      torus.halfEdges.size shouldBe 36
    )
  }

  it should "draw a 4x4 square net on a torus" in {
    val torus         = createSquareNet(4, 4)
    val scale: Double = 1.0 / 4.0
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = scale, vScale = scale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 16,
      torus.faces.size shouldBe 16,
      torus.halfEdges.size shouldBe 64
    )
  }

  it should "draw a 8x8 square net on a torus" in {
    val torus         = createSquareNet(8, 8)
    val scale: Double = 1.0 / 8.0
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = scale, vScale = scale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 64,
      torus.faces.size shouldBe 64,
      torus.halfEdges.size shouldBe 256
    )
  }

  it should "draw a 16x4 square net on a torus" in {
    val torus          = createSquareNet(16, 4)
    val uScale: Double = 1.0 / 16.0
    val vScale: Double = 1.0 / 4.0
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 64,
      torus.faces.size shouldBe 64,
      torus.halfEdges.size shouldBe 256
    )
  }

  it should "draw a 4x16 square net on a torus" in {
    val torus          = createSquareNet(4, 16)
    val uScale: Double = 1.0 / 4.0
    val vScale: Double = 1.0 / 16.0
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 64,
      torus.faces.size shouldBe 64,
      torus.halfEdges.size shouldBe 256
    )
  }

  it should "draw a 1x2 triangle net on a torus" in {
    val torus          = createTriangleNet(1, 2)
    val uScale: Double = 1.0
    val vScale: Double = 1.0 / (2.0 * 0.8660254037844386)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 2,
      torus.faces.size shouldBe 4,
      torus.halfEdges.size shouldBe 12
    )
  }

  it should "draw a 2x2 triangle net on a torus" in {
    val torus          = createTriangleNet(2, 2)
    val uScale: Double = 1.0 / 2.0
    val vScale: Double = 1.0 / (2.0 * 0.8660254037844386)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 4,
      torus.faces.size shouldBe 8,
      torus.halfEdges.size shouldBe 24
    )
  }

  it should "draw a 4x4 triangle net on a torus" in {
    val torus          = createTriangleNet(4, 4)
    val uScale: Double = 1.0 / 4.0
    val vScale: Double = 1.0 / (4.0 * 0.8660254037844386)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 16,
      torus.faces.size shouldBe 32,
      torus.halfEdges.size shouldBe 96
    )
  }

  it should "draw a 8x8 triangle net on a torus" in {
    val torus          = createTriangleNet(8, 8)
    val uScale: Double = 1.0 / 8.0
    val vScale: Double = 1.0 / (8.0 * 0.8660254037844386)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 64,
      torus.faces.size shouldBe 128,
      torus.halfEdges.size shouldBe 384
    )
  }

  it should "draw a 16x4 triangle net on a torus" in {
    val torus          = createTriangleNet(16, 4)
    val uScale: Double = 1.0 / 16.0
    val vScale: Double = 1.0 / (4.0 * 0.8660254037844386)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 64,
      torus.faces.size shouldBe 128,
      torus.halfEdges.size shouldBe 384
    )
  }

  it should "draw a 4x16 triangle net on a torus" in {
    val torus          = createTriangleNet(4, 16)
    val uScale: Double = 1.0 / 4.0
    val vScale: Double = 1.0 / (16.0 * 0.8660254037844386)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 64,
      torus.faces.size shouldBe 128,
      torus.halfEdges.size shouldBe 384
    )
  }

  it should "draw a 1x2 hexagon net on a torus" in {
    val torus          = createHexagonNet(1, 2)
    val uScale: Double = 1.0 / 1.5
    val vScale: Double = 1.0 / (4 * 0.8660254037844386)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 4,
      torus.faces.size shouldBe 2,
      torus.halfEdges.size shouldBe 8
    )
  }

  it should "draw a 2x2 hexagon net on a torus" in {
    val torus          = createHexagonNet(2, 2)
    val uScale: Double = 1.0 / 3.0
    val vScale: Double = 1.0 / (4 * 0.8660254037844386)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 8,
      torus.faces.size shouldBe 4,
      torus.halfEdges.size shouldBe 24
    )
  }

  it should "draw a 4x4 hexagon net on a torus" in {
    val torus          = createHexagonNet(4, 4)
    val uScale: Double = 1.0 / 6.0
    val vScale: Double = 1.0 / (8 * 0.8660254037844386)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 32,
      torus.faces.size shouldBe 16,
      torus.halfEdges.size shouldBe 96
    )
  }

  it should "draw a 6x6 hexagon net on a torus" in {
    val torus          = createHexagonNet(6, 6)
    val uScale: Double = 1.0 / 9.0
    val vScale: Double = 1.0 / (12 * 0.8660254037844386)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 72,
      torus.faces.size shouldBe 36,
      torus.halfEdges.size shouldBe 216
    )
  }

  it should "draw a 12x4 hexagon net on a torus" in {
    val torus          = createHexagonNet(12, 4)
    val uScale: Double = 1.0 / 18.0
    val vScale: Double = 1.0 / (8 * 0.8660254037844386)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 96,
      torus.faces.size shouldBe 48,
      torus.halfEdges.size shouldBe 288
    )
  }

  behavior of "TilingTorusDCEL.toTilingDCEL"

  it should "be converted" in {
    val tiling = createSquareNet(4, 4).toTilingDCEL
    println(tiling)
    tiling.isRight shouldBe true
  }
