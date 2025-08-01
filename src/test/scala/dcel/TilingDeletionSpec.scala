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
      .maybeAddRegularPolygon(4, "V2").value
      .maybeAddRegularPolygon(4, "V3").value
      .maybeAddRegularPolygon(4, "V4").value
      .maybeAddRegularPolygon(4, "V1").value
    val result = tiling.deletePolygon(Face.firstInnerId)
    result.isLeft shouldBe true
    result.left.value should include("is not adjacent to the outer boundary")
  }

  it should "fail to delete a face that would partition the tiling in two parts joined by a vertex" in {
    val s1 = TilingBuilder.createRegularPolygon(4).value
    val s1s2 = s1.maybeAddRegularPolygon(4, "V2").value
    val s1s2s3 = s1s2.maybeAddRegularPolygon(4, "V5").value
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
      .maybeAddRegularPolygon(4, "V2").value
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
      .maybeAddRegularPolygon(4, "V2").value
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

  behavior of "TilingDCEL.deleteEdge"

  it should "fail to delete an edge if a vertex does not exist" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
    val result = tiling.deleteEdge("V1", "V_NonExistent")
    result.isLeft shouldBe true
    result.left.value should include("Vertex with ID V_NonExistent not found.")
  }

  it should "fail to delete an edge if the vertices are not connected" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
    val result = tiling.deleteEdge("V1", "V3")
    result.isLeft shouldBe true
    result.left.value should include("Edge between vertices V1 and V3 not found.")
  }

  it should "successfully delete a boundary edge by deleting the adjacent face" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
      .maybeAddRegularPolygon(4, "V2").value // Two squares
    tiling.innerFaces.length shouldBe 2

    // Deleting a boundary edge, e.g., (V1, V2) from the first square
    val result = tiling.deleteEdge("V1", "V2")
    result.isRight shouldBe true
    val newTiling = result.value
    verifyValidTiling(newTiling)

    newTiling.innerFaces.length shouldBe 1
    newTiling.innerFaces.head.id shouldBe "F2" // F1 is deleted
    newTiling.vertices.length shouldBe 4
    newTiling.boundary.length shouldBe 4
  }

  it should "successfully delete an inner edge, merging two faces" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
      .maybeAddRegularPolygon(4, "V2").value // Two squares sharing edge (V2, V3)
    tiling.innerFaces.length shouldBe 2
    tiling.vertices.length shouldBe 6

    // Deleting the inner edge (V2, V3)
    val result = tiling.deleteEdge("V2", "V3")
    result.isRight shouldBe true
    val newTiling = result.value
    verifyValidTiling(newTiling)

    newTiling.innerFaces.length shouldBe 1
    newTiling.vertices.length shouldBe 6
    newTiling.halfEdges.length shouldBe 12 // A hexagon has 6*2=12 half-edges
    newTiling.boundary.length shouldBe 6
  }

  it should "merge only two faces when deleting an edge at a junction" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value // F1
      .maybeAddRegularPolygon(4, "V2").value // F2 on edge (V2,V3)
      .maybeAddRegularPolygon(4, "V1").value // F3 on edge (V1,V4)
    tiling.innerFaces.length shouldBe 3

    // Delete edge (V2, V3) between F1 and F2
    val result = tiling.deleteEdge("V2", "V3")
    result.isRight shouldBe true
    val newTiling = result.value
    verifyValidTiling(newTiling)

    newTiling.innerFaces.length shouldBe 2 // F1 and F2 merged, F3 remains
    newTiling.vertices.length shouldBe 8
    newTiling.boundary.length shouldBe 8
  }