package io.github.scala_tessella.dcel.torus

import io.github.scala_tessella.dcel.TilingTestHelpers
import io.github.scala_tessella.dcel.torus.TilingTorusDCEL.TorusSvg3DOptions
import io.github.scala_tessella.dcel.structure.{FaceId, VertexId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingTorusDCELSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingTorusDCEL.build2x2Squares"

  it should "create a toroidal tiling with 4 vertices, 16 half-edges, and 4 faces" in {
    val torus = TilingTorusDCEL.build2x2Squares()
    allAssert(
      torus.vertices should have length 4,
      torus.halfEdges should have length 16,
      torus.faces should have length 4
    )
  }

  it should "be valid according to TilingTorusValidation" in {
    val torus = TilingTorusDCEL.build2x2Squares()
//    println(TilingTorusValidation.validate(torus))
    TilingTorusValidation.validate(torus).isRight shouldBe true
  }

  behavior of "TilingTorusDCEL.build1x1Square"

  it should "be valid according to TilingTorusValidation" in {
    val torus  = TilingTorusDCEL.build1x1Square()
    TilingTorusValidation.validate(torus).isRight shouldBe true
    val torus2 = TilingTorusDCEL.buildSquareNet(1, 1)
    TilingTorusValidation.validate(torus2).isRight shouldBe true
  }

  behavior of "TilingTorusDCEL.build4x1Triangles"

  it should "be valid according to TilingTorusValidation" in {
    val torus  = TilingTorusDCEL.build4x1Triangles()
//    println(TilingTorusValidation.validate(torus))
    TilingTorusValidation.validate(torus).isRight shouldBe true
    val torus2 = TilingTorusDCEL.buildSquareNet(2, 2)
    TilingTorusValidation.validate(torus2).isRight shouldBe true
  }

  behavior of "TilingTorusDCEL.build2x1Hexagons"

  it should "be valid according to TilingTorusValidation" in {
    val torus = TilingTorusDCEL.build2x1Hexagons()
    println(TilingTorusValidation.validate(torus))
    TilingTorusValidation.validate(torus).isRight shouldBe true
  }

  behavior of "TilingTorusDCEL.findVertex/findFace"

  it should "find existing vertices and faces" in {
    val torus = TilingTorusDCEL.build2x2Squares()
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
    val torus = TilingTorusDCEL.build2x2Squares()
    allAssert(
      torus.findVertex(VertexId("VX")).isLeft shouldBe true,
      torus.findFace(FaceId("FX")).isLeft shouldBe true
    )
  }

  behavior of "TilingTorusDCEL.toSVG3D"

  it should "draw a 1x1 square net on a torus" in {
    val torus = TilingTorusDCEL.buildSquareNet(1, 1)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 1,
      torus.faces.size shouldBe 1,
      torus.halfEdges.size shouldBe 4
    )
  }

  it should "draw a 1x2 square net on a torus" in {
    val torus = TilingTorusDCEL.buildSquareNet(1, 2)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 2,
      torus.faces.size shouldBe 2,
      torus.halfEdges.size shouldBe 8
    )
  }

  it should "draw a 2x1 square net on a torus" in {
    val torus = TilingTorusDCEL.buildSquareNet(2, 1)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 2,
      torus.faces.size shouldBe 2,
      torus.halfEdges.size shouldBe 8
    )
  }

  it should "draw a 2x2 square net on a torus" in {
    val torus = TilingTorusDCEL.buildSquareNet(2, 2)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 4,
      torus.faces.size shouldBe 4,
      torus.halfEdges.size shouldBe 16
    )
  }

  it should "draw a 3x3 square net on a torus" in {
    val torus = TilingTorusDCEL.buildSquareNet(3, 3)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 9,
      torus.faces.size shouldBe 9,
      torus.halfEdges.size shouldBe 36
    )
  }

  it should "draw a 4x4 square net on a torus" in {
    val torus = TilingTorusDCEL.buildSquareNet(4, 4)
    println(torus.toSVG3D())
    allAssert(
      torus.vertices.size shouldBe 16,
      torus.faces.size shouldBe 16,
      torus.halfEdges.size shouldBe 64
    )
  }

  it should "draw a 8x8 square net on a torus" in {
    val torus = TilingTorusDCEL.buildSquareNet(8, 8)
    println(torus.toSVG3D())
    allAssert(
      torus.vertices.size shouldBe 64,
      torus.faces.size shouldBe 64,
      torus.halfEdges.size shouldBe 256
    )
  }

  it should "draw a 1x2 triangle net on a torus" in {
    val torus = TilingTorusDCEL.buildTriangleNet(1, 2)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 2,
      torus.faces.size shouldBe 4,
      torus.halfEdges.size shouldBe 12
    )
  }

  it should "draw a 2x2 triangle net on a torus" in {
    val torus = TilingTorusDCEL.buildTriangleNet(2, 2)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 4,
      torus.faces.size shouldBe 8,
      torus.halfEdges.size shouldBe 24
    )
  }

  it should "draw a 4x4 triangle net on a torus" in {
    val torus = TilingTorusDCEL.buildTriangleNet(4, 4)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 16,
      torus.faces.size shouldBe 32,
      torus.halfEdges.size shouldBe 96
    )
  }

  it should "draw a 1x2 hexagon net on a torus" in {
    val torus = TilingTorusDCEL.buildHexagonNet(1, 2)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 4,
      torus.faces.size shouldBe 2,
      torus.halfEdges.size shouldBe 12
    )
  }

  it should "draw a 2x2 hexagon net on a torus" in {
    val torus = TilingTorusDCEL.buildHexagonNet(2, 2)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 8,
      torus.faces.size shouldBe 4,
      torus.halfEdges.size shouldBe 24
    )
  }

  it should "draw a 4x4 hexagon net on a torus" in {
    val torus = TilingTorusDCEL.buildHexagonNet(4, 4)
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 32,
      torus.faces.size shouldBe 16,
      torus.halfEdges.size shouldBe 96
    )
  }

  it should "draw a hexagon net on a torus" in {
    val torus = TilingTorusDCEL.build2x1Hexagons()
    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.findVertex(VertexId("VX")).isLeft shouldBe true,
      torus.findFace(FaceId("FX")).isLeft shouldBe true
    )
  }
