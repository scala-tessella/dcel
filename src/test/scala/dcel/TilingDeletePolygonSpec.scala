package io.github.scala_tessella
package dcel

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingDeletePolygonSpec extends AnyFlatSpec with Matchers with EitherValues:

  behavior of "TilingDCEL.maybeDeletePolygon"

  it should "fail to delete a face that does not exist" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
    val result = tiling.maybeDeletePolygon("F_NonExistent")
    result.isLeft shouldBe true
    result.left.value should include("not found")
  }

  it should "successfully delete a single square, leaving an empty tiling" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
    val result = tiling.maybeDeletePolygon("F_Poly")
    result.isRight shouldBe true

    val newTiling = result.value
    newTiling.innerFaces shouldBe empty
    newTiling.vertices.length shouldBe 0
    newTiling.halfEdges.length shouldBe 0 // After deleting inner face, only outer edges remain
    newTiling.boundary.map(_.id).length shouldBe 0
  }

  it should "fail to delete a face that is not on the boundary" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
      .maybeAddRegularPolygon(4, "V1").value
      .maybeAddRegularPolygon(4, "V2").value
      .maybeAddRegularPolygon(4, "V3").value
      .maybeAddRegularPolygon(4, "V0").value
    println(tiling.toSVG())
    val result = tiling.maybeDeletePolygon("F_Poly")
    result.isLeft shouldBe true
    result.left.value should include("is not adjacent to the outer boundary")
  }

  it should "fail to delete a face that would partition the tiling" in {
    val s1 = TilingBuilder.createRegularPolygon(4).value
    val s1s2 = s1.maybeAddRegularPolygon(4, "V1").value
    val s1s2s3 = s1s2.maybeAddRegularPolygon(4, "V4").value
    val result = s1s2s3.maybeDeletePolygon("F_Poly_1")
    result.isLeft shouldBe true
    result.left.value should include("would partition the tiling")
  }

  it should "successfully delete a boundary face" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
      .maybeAddRegularPolygon(4, "V1").value
    tiling.innerFaces.length shouldBe 2

    val result = tiling.maybeDeletePolygon("F_Poly_1")
    result.isRight shouldBe true
    val newTiling = result.value
    newTiling.innerFaces.length shouldBe 1
    newTiling.innerFaces.head.id shouldBe "F_Poly"
    newTiling.vertices.length shouldBe 4
    newTiling.boundary.length shouldBe 4
  }
