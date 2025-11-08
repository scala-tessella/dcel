package io.github.scala_tessella.dcel.torus

import io.github.scala_tessella.dcel.geometry.RegularPolygon
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

  behavior of "TilingTorusDCEL.toTilingDCEL"

  it should "be converted" in {
    val tiling = createSquareNet(4, 4).toTilingDCEL
    println(tiling)
    tiling.isRight shouldBe true
  }

  behavior of "TilingTorusDCEL.isTorusTilable"

  it should "find tilable a square" in {
    TilingTorusDCEL.isTorusTilable(TilingBuilder.createRegularPolygon(RegularPolygon(4))) shouldBe true
  }

  it should "find NOT tilable a pentagon" in {
    TilingTorusDCEL.isTorusTilable(TilingBuilder.createRegularPolygon(RegularPolygon(5))) shouldBe false
  }

  it should "find NOT tilable an hexagon" in {
    TilingTorusDCEL.isTorusTilable(TilingBuilder.createRegularPolygon(RegularPolygon(6))) shouldBe false
  }

  it should "find tilable a 4x4 square" in {
    TilingTorusDCEL.isTorusTilable(TilingBuilder.createRhombusNet(4, 4)) shouldBe true
  }

  it should "find tilable a rectangle" in {
    TilingTorusDCEL.isTorusTilable(TilingBuilder.createRhombusNet(10, 1)) shouldBe true
  }

  it should "find tilable a 2x1 hexagon net" in {
    TilingTorusDCEL.isTorusTilable(TilingBuilder.createHexagonNet(2, 1)) shouldBe true
  }
