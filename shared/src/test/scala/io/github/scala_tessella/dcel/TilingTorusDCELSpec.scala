package io.github.scala_tessella.dcel

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
    println(TilingTorusValidation.validate(torus))
    TilingTorusValidation.validate(torus).isRight shouldBe true
  }

  behavior of "TilingTorusDCEL.build1x1Square"

  it should "be valid according to TilingTorusValidation" in {
    val torus = TilingTorusDCEL.build1x1Square()
    TilingTorusValidation.validate(torus).isRight shouldBe true
  }

  behavior of "TilingTorusDCEL.build4x1Triangles"

  it should "be valid according to TilingTorusValidation" in {
    val torus = TilingTorusDCEL.build4x1Triangles()
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
