package io.github.scala_tessella
package dcel

import TilingDeletion.*

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingDeletionSpec extends AnyFlatSpec with Matchers with EitherValues:

  // Helper method to verify DCEL validity
  private def verifyValidTiling(tiling: TilingDCEL): Unit =
    val structuralCheck = TilingDCEL.validate(tiling)
    structuralCheck.isRight shouldBe true

    val spatialCheck = TilingDCEL.spatiallyValidate(tiling)
    spatialCheck.isRight shouldBe true

  behavior of "TilingDCEL.deletePolygon"

  it should "fail to delete a face that does not exist" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
    val result = tiling.deletePolygon("F_NonExistent")
    result.isLeft shouldBe true
    result.left.value should include("not found")
  }

  it should "successfully delete a single square, leaving an empty tiling" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
    val result = tiling.deletePolygon(Face.firstInnerId)
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
    val result = tiling.deletePolygon(Face.firstInnerId)
    result.isLeft shouldBe true
    result.left.value should include("is not adjacent to the outer boundary")
  }

  it should "fail to delete a face that would partition the tiling in two parts joined by a vertex" in {
    val s1 = TilingBuilder.createRegularPolygon(4).value
    val s1s2 = s1.maybeAddRegularPolygon(4, "V1").value
    val s1s2s3 = s1s2.maybeAddRegularPolygon(4, "V4").value
    val result = s1s2s3.deletePolygon("F2")
    result.isLeft shouldBe true
    result.left.value should include("would partition the tiling")
  }

  it should "fail to delete a face that would partition the tiling in two disjoint parts" in {
    val s1 = TilingBuilder.createRegularPolygon(4).value
    val s1s2 = s1.maybeAddRegularPolygon(4, "V1").value
    val s1s2s3 = s1s2.maybeAddRegularPolygon(4, "V5").value
    val result = s1s2s3.deletePolygon("F2")
    result.isLeft shouldBe true
    result.left.value should include("would partition the tiling")
  }

  it should "successfully delete an added boundary face" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
      .maybeAddRegularPolygon(4, "V1").value
    tiling.innerFaces.length shouldBe 2

    val result = tiling.deletePolygon("F2")
    result.isRight shouldBe true
    val newTiling = result.value
    verifyValidTiling(newTiling)

    newTiling.innerFaces.length shouldBe 1
    newTiling.innerFaces.head.id shouldBe Face.firstInnerId
    newTiling.vertices.length shouldBe 4
    newTiling.boundary.length shouldBe 4
  }

  it should "successfully delete the other boundary face" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
      .maybeAddRegularPolygon(4, "V1").value
    tiling.innerFaces.length shouldBe 2

    val result = tiling.deletePolygon(Face.firstInnerId)
    result.isRight shouldBe true
    val newTiling = result.value
    verifyValidTiling(newTiling)

    newTiling.innerFaces.length shouldBe 1
    newTiling.innerFaces.head.id shouldBe "F2"
    newTiling.vertices.length shouldBe 4
    newTiling.boundary.length shouldBe 4
  }
