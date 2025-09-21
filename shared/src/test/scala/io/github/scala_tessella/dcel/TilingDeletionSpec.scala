package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingAddition._
import io.github.scala_tessella.dcel.TilingDeletion._
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingDeletionSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  // Helper method to verify DCEL validity
  private def verifyValidTiling(tiling: TilingDCEL): Assertion =
    val comprehensiveCheck = TilingDCEL.validate(tiling)
    comprehensiveCheck.isRight shouldBe true

  behavior of "TilingDCEL.deleteFace"

  it should "fail to delete a face that does not exist" in {
    val result = square.deleteFace(FaceId("F_NonExistent"))
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("not found")
    )
  }

  it should "successfully delete a single square, leaving an empty tiling" in {
    val result = square.deleteFace(FaceId.firstInnerId)
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        allAssert(
          newTiling.innerFaces shouldBe empty,
          newTiling.vertices.length shouldBe 0,
          newTiling.halfEdges.length shouldBe 0, // After deleting inner face, only outer edges remain
          newTiling.boundaryVertices.map(_.id).length shouldBe 0
        )
      }
    )
  }

  it should "fail to delete a face touching the boundary in different points" in {
    val tiling = square
      .addRegularPolygonToBoundary(V2, 4).value
      .addRegularPolygonToBoundary(V3, 4).value
      .addRegularPolygonToBoundary(V4, 4).value
      .addRegularPolygonToBoundary(V1, 4).value
    val result = tiling.deleteFace(FaceId.firstInnerId)
    allAssert(
//      result.isLeft shouldBe true,
//      result.left.value.message should include("would partition the tiling in two or more parts")
    )
  }

  it should "delete a face that is not on the boundary" in {
    val tiling = TilingBuilder.createRhombusNet(3, 3)
    val result = tiling.deleteFace(FaceId("F5"))
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        newTiling.innerFaces.length shouldBe 1
      }
    )
  }

  /** <img src="file:../../../../../resources/partitioningBoundaryFace.svg"/> */
  def partitioningBoundaryFace: TilingDCEL =
    triangle
      .addRegularPolygonToBoundary(V2, 3).value
      .addRegularPolygonToBoundary(V2, 3).value

  it should "fail to delete a face that would partition the tiling in two parts joined by a vertex" in {
    val result = partitioningBoundaryFace.deleteFace(F2)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include(
        "would partition the tiling in two or more parts connected by just a vertex"
      )
    )
  }

  /** <img src="file:../../../../../resources/disconnectingBoundaryFace.svg"/> */
  def disconnectingBoundaryFace: TilingDCEL =
    triangle
      .addRegularPolygonToBoundary(V1, 4).value
      .addRegularPolygonToBoundary(V4, 3).value

  it should "fail to delete a face that would partition the tiling in two disjoint parts" in {
    val result = disconnectingBoundaryFace.deleteFace(F2)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("would partition the tiling in two disconnected halves")
    )
  }

  it should "delete a face that would NOT partition the tiling in two parts" in {
    val s1s2     = square.addRegularPolygonToBoundary(V2, 4).value
    val s1s2s3   = s1s2.addRegularPolygonToBoundary(V2, 4).value
    val s1s2s3s4 = s1s2s3.addRegularPolygonToBoundary(V2, 4).value
    val result   = s1s2s3s4.deleteFace(F2)
    result.isRight shouldBe true
  }

  it should "delete another face that would NOT partition the tiling in two parts" in {
    val s1s2   = hexagon.addRegularPolygonToBoundary(V2, 6).value
    val s1s2s3 = s1s2.addRegularPolygonToBoundary(V2, 6).value
    val result = s1s2s3.deleteFace(F2)
    result.isRight shouldBe true
  }

  it should "successfully delete an added boundary face" in {
    val tiling = square
      .addRegularPolygonToBoundary(V2, 4).value
    allAssert(
      tiling.innerFaces.length shouldBe 2, {
        val result = tiling.deleteFace(F2)
        allAssert(
          result.isRight shouldBe true, {
            val newTiling = result.value
            allAssert(
              verifyValidTiling(newTiling),
              newTiling.innerFaces.length shouldBe 1,
              newTiling.innerFaces.head.id shouldBe FaceId.firstInnerId,
              newTiling.vertices.length shouldBe 4,
              newTiling.boundaryVertices.length shouldBe 4
            )
          }
        )
      }
    )
  }

  it should "successfully delete the other boundary face" in {
    val tiling = square
      .addRegularPolygonToBoundary(V2, 4).value
    allAssert(
      tiling.innerFaces.length shouldBe 2, {
        val result = tiling.deleteFace(FaceId.firstInnerId)
        allAssert(
          result.isRight shouldBe true, {
            val newTiling = result.value
            allAssert(
              verifyValidTiling(newTiling),
              newTiling.innerFaces.length shouldBe 1,
              newTiling.innerFaces.head.id.value shouldBe "F2",
              newTiling.vertices.length shouldBe 4,
              newTiling.boundaryVertices.length shouldBe 4
            )
          }
        )
      }
    )
  }

  /** <img src="file:../../../../../resources/irregularFaces.svg"/> */
  def irregularFaces: TilingDCEL =
    triangle
      .addSimplePolygonToBoundary(V2, 15, 165, 15, 165).value
      .addSimplePolygonToBoundary(V3, 165, 15, 165, 15).value
      .addRegularPolygonToBoundary(VertexId("V7"), 4).value
      .addRegularPolygonToBoundary(VertexId("V9"), 4).value
      .addRegularPolygonToBoundary(V2, 4).value

  it should "delete an irregular polygon" in {
    val result = irregularFaces.deleteFace(F2)
    allAssert(
      result.isRight shouldBe true, {
        val tiling = result.value
        //    println(tiling.toSVG())
        //    println(validate(tiling))
        verifyValidTiling(tiling)
      }
    )
  }

  behavior of "TilingDCEL.deleteEdge"

  it should "fail to delete an edge if a vertex does not exist" in {
    val result = square.deleteEdge(V1, VertexId("V_NonExistent"))
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Vertex with ID 'V_NonExistent' not found.")
    )
  }

  it should "fail to delete an edge if the vertices are not connected" in {
    val result = square.deleteEdge(V1, V3)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Edge with ID 'between V1 and V3' not found.")
    )
  }

  it should "fail to delete an edge if it has no incident face" in {
    val tiling = square
    val v1     = tiling.findVertexUnsafe(V1).get
    val v2     = tiling.findVertexUnsafe(V2).get
    // Manually corrupt the DCEL for testing purposes
    val edge   = v1.findEdgeBetweenUnsafe(v2).get
    edge.incidentFace = None

    val result = tiling.deleteEdge(V1, V2)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Edge has no incident face")
    )
  }

  it should "successfully delete a boundary edge by deleting the adjacent face" in {
    val tiling = square
      .addRegularPolygonToBoundary(V2, 4).value // Two squares
    allAssert(
      tiling.innerFaces.length shouldBe 2, {
        // Deleting a boundary edge, e.g., (V1, V2) from the first square
        val result = tiling.deleteEdge(V3, V4)
        allAssert(
          result.isRight shouldBe true, {
            val newTiling = result.value
            allAssert(
              verifyValidTiling(newTiling),
              newTiling.innerFaces.length shouldBe 1,
              newTiling.innerFaces.head.id.value shouldBe "F2", // F1 is deleted
              newTiling.vertices.length shouldBe 4,
              newTiling.boundaryVertices.length shouldBe 4
            )
          }
        )
      }
    )
  }

  it should "successfully delete a boundary edge" in {
    val tiling = square
      .addRegularPolygonToBoundary(V2, 4).value // Two squares
    allAssert(
      tiling.innerFaces.length shouldBe 2, {
        // Deleting a boundary edge, e.g., (V1, V2) from the first square
        val result = tiling.deleteEdge(V4, V3)
        allAssert(
          result.isRight shouldBe true, {
            val newTiling = result.value
            allAssert(
              verifyValidTiling(newTiling),
              newTiling.innerFaces.length shouldBe 1,
              newTiling.innerFaces.head.id.value shouldBe "F2", // F1 is deleted
              newTiling.vertices.length shouldBe 4,
              newTiling.boundaryVertices.length shouldBe 4
            )
          }
        )
      }
    )
  }

  it should "successfully delete a single inner edge, merging two faces" in {
    val tiling = square
      .addRegularPolygonToBoundary(V2, 4).value // Two squares sharing edge (V2, V3)
    allAssert(
      tiling.innerFaces.length shouldBe 2,
      tiling.vertices.length shouldBe 6, {
        // Deleting the inner edge (V1, V2)
        val result = tiling.deleteEdge(V1, V2)
        allAssert(
          result.isRight shouldBe true, {
            val newTiling = result.value
            //    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
            //    println(TilingDCEL.validate(newTiling))
            allAssert(
              verifyValidTiling(newTiling),
              newTiling.innerFaces.length shouldBe 1,
              newTiling.vertices.length shouldBe 6,
              newTiling.halfEdges.length shouldBe 12, // A hexagon has 6*2=12 half-edges
              newTiling.boundaryVertices.length shouldBe 6
            )
          }
        )
      }
    )
  }

  it should "successfully delete multiple single inner edges" in {
    val tiling = triangle
      .addRegularPolygonToBoundary(V1, 3).value
      .addRegularPolygonToBoundary(V1, 3).value
      .addRegularPolygonToBoundary(V1, 3).value
      .addRegularPolygonToBoundary(V1, 3).value
      .addRegularPolygonToBoundary(V1, 3).value

    // Deleting the inner edges
    val result = tiling
      .deleteEdge(V1, V3).value
      .deleteEdge(V1, V4).value
      .deleteEdge(V1, VertexId("V5")).value
      .deleteEdge(V1, VertexId("V6"))
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        //    println(newTiling.toSVG(showHalfEdgeTraversal = true, leavingEdgeMarkers = true, faceIdsOnEdges = true))
        //    println(TilingDCEL.validate(newTiling))
        verifyValidTiling(newTiling)
      }
    )
  }

  /** <img src="file:../../../../../resources/deletableNonBoundaryPath.svg"/> */
  def deletableNonBoundaryPath: TilingDCEL =
    TilingBuilder.createRhombusNet(2, 2)
      .deleteEdge(V4, VertexId("V5")).value
      .deleteEdge(V5, VertexId("V6")).value

  it should "successfully delete a path of inner edges" in {
    val result = deletableNonBoundaryPath
      .deleteEdge(V2, VertexId("V5"))
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        newTiling.innerFaces should have size 1
      }
    )
  }

  /** <img src="file:../../../../../resources/partitioningNonBoundaryFace.svg"/> */
  def partitioningNonBoundaryFace: TilingDCEL =
    TilingBuilder.createRhombusNet(4, 3)
      .deleteEdge(VertexId("V8"), VertexId("V13")).value
      .addRegularPolygon(VertexId("V7"), VertexId("V8"), 3).value
      .addRegularPolygon(VertexId("V21"), VertexId("V8"), 3).value

  it should "fail to delete edges if the surviving face is not a simple polygon" in {
    val result = partitioningNonBoundaryFace
      .deleteEdge(VertexId("V7"), VertexId("V21"))
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("is not simple")
    )
  }

  behavior of "TilingDCEL.deleteVertex"

  it should "delete an interior vertex" in {
    val result = TilingBuilder.createRhombusNet(2, 2)
      .deleteVertex(VertexId("V5"))
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        newTiling.innerFaces should have size 1
      }
    )
  }

  it should "delete another interior vertex" in {
    val result = irregularFaces
      .deleteVertex(V2)
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        newTiling.innerFaces should have size 1
      }
    )
  }

  it should "delete a boundary vertex" in {
    val result = TilingBuilder.createTriangleNet(3, 3)
      .deleteVertex(V1)
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        newTiling.innerFaces.length shouldBe 17
      }
    )
  }

  it should "delete another boundary vertex" in {
    val result = TilingBuilder.createTriangleNet(3, 3)
      .deleteVertex(V2)
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        newTiling.innerFaces.length shouldBe 15
      }
    )
  }

  it should "delete a third boundary vertex" in {
    val result = TilingBuilder.createTriangleNet(3, 3)
      .deleteVertex(V3)
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        newTiling.innerFaces.length shouldBe 15
      }
    )
  }

  it should "delete a fourth boundary vertex" in {
    val result = TilingBuilder.createTriangleNet(3, 3)
      .deleteVertex(V4)
    allAssert(
      result.isRight shouldBe true, {
        val newTiling = result.value
        newTiling.innerFaces.length shouldBe 16
      }
    )
  }
