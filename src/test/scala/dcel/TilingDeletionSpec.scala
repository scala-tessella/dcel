package io.github.scala_tessella
package dcel

import TilingAddition.*
import TilingDeletion.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingDeletionSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  // Helper method to verify DCEL validity
  private def verifyValidTiling(tiling: TilingDCEL): Unit =
    val comprehensiveCheck = TilingDCEL.validate(tiling)
    comprehensiveCheck.isRight shouldBe true

  behavior of "TilingDCEL.deletePolygon"

  it should "fail to delete a face that does not exist" in {
    val result = square.deletePolygon("F_NonExistent")
    result.isLeft shouldBe true
    result.left.value should include("not found")
  }

  it should "successfully delete a single square, leaving an empty tiling" in {
    val result = square.deletePolygon(Face.firstInnerId)
    result.isRight shouldBe true

    val newTiling = result.value
    newTiling.innerFaces shouldBe empty
    newTiling.vertices.length shouldBe 0
    newTiling.halfEdges.length shouldBe 0 // After deleting inner face, only outer edges remain
    newTiling.boundary.map(_.id).length shouldBe 0
  }

  it should "fail to delete a face that is not on the boundary" in {
    val tiling = square
      .addRegularPolygonToBoundary("V2", 4).value
      .addRegularPolygonToBoundary("V3", 4).value
      .addRegularPolygonToBoundary("V4", 4).value
      .addRegularPolygonToBoundary("V1", 4).value
    val result = tiling.deletePolygon(Face.firstInnerId)
    result.isLeft shouldBe true
    result.left.value should include("is not adjacent to the outer boundary")
  }

  it should "fail to delete a face that would partition the tiling in two parts joined by a vertex" in {
    val s1s2 = square.addRegularPolygonToBoundary("V2", 4).value
    val s1s2s3 = s1s2.addRegularPolygonToBoundary("V2", 4).value
    val result = s1s2s3.deletePolygon("F2")
    result.isLeft shouldBe true
    result.left.value should include("would partition the tiling in two halves connected by just a vertex")
  }

  it should "fail to delete a face that would partition the tiling in two disjoint parts" in {
    val s1s2 = square.addRegularPolygonToBoundary("V1", 4).value
    val s1s2s3 = s1s2.addRegularPolygonToBoundary("V5", 4).value
    val result = s1s2s3.deletePolygon("F2")
    result.isLeft shouldBe true
    result.left.value should include("would partition the tiling in two disconnected halves")
  }

  it should "delete a face that would NOT partition the tiling in two parts" in {
    val s1s2 = square.addRegularPolygonToBoundary("V2", 4).value
    val s1s2s3 = s1s2.addRegularPolygonToBoundary("V2", 4).value
    val s1s2s3s4 = s1s2s3.addRegularPolygonToBoundary("V2", 4).value
    val result = s1s2s3s4.deletePolygon("F2")
    result.isRight shouldBe true
  }

  it should "delete another face that would NOT partition the tiling in two parts" in {
    val s1s2 = hexagon.addRegularPolygonToBoundary("V2", 6).value
    val s1s2s3 = s1s2.addRegularPolygonToBoundary("V2", 6).value
    val result = s1s2s3.deletePolygon("F2")
    result.isRight shouldBe true
  }

  it should "successfully delete an added boundary face" in {
    val tiling = square
      .addRegularPolygonToBoundary("V2", 4).value
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
    val tiling = square
      .addRegularPolygonToBoundary("V2", 4).value
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

  it should "delete an irregular polygon" in {
    val result = triangle
      .addSimplePolygonToBoundary("V2", 15, 165, 15, 165).value
      .addSimplePolygonToBoundary("V3", 165, 15, 165, 15).value
      .addRegularPolygonToBoundary("V7", 4).value
      .addRegularPolygonToBoundary("V9", 4).value
      .addRegularPolygonToBoundary("V2", 4).value
      .deletePolygon("F2")

    result.isRight shouldBe true
    val tiling = result.value
    //    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
    //    println(validate(tiling))
    verifyValidTiling(tiling)
  }

  behavior of "TilingDCEL.deleteEdge"

  it should "fail to delete an edge if a vertex does not exist" in {
    val result = square.deleteEdge("V1", "V_NonExistent")
    result.isLeft shouldBe true
    result.left.value should include("Vertex with ID V_NonExistent not found.")
  }

  it should "fail to delete an edge if the vertices are not connected" in {
    val result = square.deleteEdge("V1", "V3")
    result.isLeft shouldBe true
    result.left.value should include("Edge between vertices V1 and V3 not found.")
  }

  it should "fail to delete an edge if it has no incident face" in {
    val tiling = square
    val v1 = tiling.findVertex("V1").get
    val v2 = tiling.findVertex("V2").get
    // Manually corrupt the DCEL for testing purposes
    val edge = tiling.findEdgeBetween(v1, v2).get
    edge.incidentFace = None

    val result = tiling.deleteEdge("V1", "V2")
    result.isLeft shouldBe true
    result.left.value should include("Edge has no incident face")
  }

  it should "successfully delete a boundary edge by deleting the adjacent face" in {
    val tiling = square
      .addRegularPolygonToBoundary("V2", 4).value // Two squares
    tiling.innerFaces.length shouldBe 2

    // Deleting a boundary edge, e.g., (V1, V2) from the first square
    val result = tiling.deleteEdge("V3", "V4")
    result.isRight shouldBe true
    val newTiling = result.value
    verifyValidTiling(newTiling)

    newTiling.innerFaces.length shouldBe 1
    newTiling.innerFaces.head.id shouldBe "F2" // F1 is deleted
    newTiling.vertices.length shouldBe 4
    newTiling.boundary.length shouldBe 4
  }

  it should "successfully delete a boundary edge" in {
    val tiling = square
      .addRegularPolygonToBoundary("V2", 4).value // Two squares
    tiling.innerFaces.length shouldBe 2

    // Deleting a boundary edge, e.g., (V1, V2) from the first square
    val result = tiling.deleteEdge("V4", "V3")
    result.isRight shouldBe true
    val newTiling = result.value
    verifyValidTiling(newTiling)

    newTiling.innerFaces.length shouldBe 1
    newTiling.innerFaces.head.id shouldBe "F2" // F1 is deleted
    newTiling.vertices.length shouldBe 4
    newTiling.boundary.length shouldBe 4
  }

  it should "successfully delete a single inner edge, merging two faces" in {
    val tiling = square
      .addRegularPolygonToBoundary("V2", 4).value // Two squares sharing edge (V2, V3)
    tiling.innerFaces.length shouldBe 2
    tiling.vertices.length shouldBe 6

    // Deleting the inner edge (V1, V2)
    val result = tiling.deleteEdge("V1", "V2")
    result.isRight shouldBe true
    val newTiling = result.value
//    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
//    println(TilingDCEL.validate(newTiling))
    verifyValidTiling(newTiling)

    newTiling.innerFaces.length shouldBe 1
    newTiling.vertices.length shouldBe 6
    newTiling.halfEdges.length shouldBe 12 // A hexagon has 6*2=12 half-edges
    newTiling.boundary.length shouldBe 6
  }

  it should "successfully delete multiple single inner edges" in {
    val tiling = triangle
      .addRegularPolygonToBoundary("V1", 3).value
      .addRegularPolygonToBoundary("V1", 3).value
      .addRegularPolygonToBoundary("V1", 3).value
      .addRegularPolygonToBoundary("V1", 3).value
      .addRegularPolygonToBoundary("V1", 3).value

    // Deleting the inner edges
    val result = tiling
      .deleteEdge("V1", "V3").value
      .deleteEdge("V1", "V4").value
      .deleteEdge("V1", "V5").value
      .deleteEdge("V1", "V6")
    result.isRight shouldBe true
    val newTiling = result.value
//    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
//    println(TilingDCEL.validate(newTiling))
    verifyValidTiling(newTiling)
  }

  it should "successfully delete a path of inner edges" in {
    val tiling = square
      .addRegularPolygonToBoundary("V1", 4).value
      .addRegularPolygonToBoundary("V1", 4).value
      .addRegularPolygonToBoundary("V2", 4).value

    // Deleting the inner edges
    val result = tiling
      .deleteEdge("V1", "V2").value
      .deleteEdge("V1", "V5").value
      .deleteEdge("V1", "V4")
    result.isRight shouldBe true
    val newTiling = result.value
//    println(newTiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
//    println(TilingDCEL.validate(newTiling))
    verifyValidTiling(newTiling)
  }

  it should "fail to delete edges if the surviving face is not a simple polygon" in {
    val tiling = triangle
      .addRegularPolygonToBoundary("V1", 3).value
      .addRegularPolygonToBoundary("V1", 3).value
      .addRegularPolygonToBoundary("V1", 3).value
      .addRegularPolygonToBoundary("V1", 3).value
      .addRegularPolygonToBoundary("V1", 3).value
      .addRegularPolygonToBoundary("V2", 6).value
      .addRegularPolygonToBoundary("V6", 6).value
      .addRegularPolygonToBoundary("V2", 4).value
      .addRegularPolygonToBoundary("V15", 4).value
      .addRegularPolygonToBoundary("V16", 4).value
      .addRegularPolygonToBoundary("V18", 4).value
      .addRegularPolygonToBoundary("V16", 4).value
      .addRegularPolygonToBoundary("V23", 4).value

    // Deleting the inner edges
    val result = tiling
      .deleteEdge("V4", "V5").value
      .deleteEdge("V2", "V3")
    result.isLeft shouldBe true
    result.left.value should include("is not simple")
  }
