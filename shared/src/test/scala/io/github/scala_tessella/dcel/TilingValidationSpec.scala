package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingValidationSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingDCEL.validate"

  it should "succeed for a valid single polygon tiling" in {
    validate(square) shouldBe Right(())
  }

  it should "succeed for a valid multi-polygon tiling" in {
    val twoSquares = square.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(4)).value
    validate(twoSquares) shouldBe Right(())
  }

  it should "fail if a vertex has no leaving edge" in {
    val tiling = square
    tiling.vertices.head.leaving = None
    val result = validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message shouldBe "Vertex V1 at coords (0, 0) [Missing leaving edge]"
    )
  }

  it should "fail if an edge has no twin" in {
    val tiling = square
    tiling.halfEdges.head.twin = None
    val result = validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message shouldBe "HalfEdge V1 -> ? [Missing twin edge]"
    )
  }

  it should "fail if an edge's next/prev relationship is broken" in {
    val tiling = square
    val edge   = tiling.halfEdges.head
    edge.next.get.prev = None // Break the link
    val result = validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message shouldBe "HalfEdge V2 -> V3 [Missing previous edge]"
    )
  }

  it should "fail if an inner face has an incorrect sum of angles" in {
    val tiling = square
    // Tamper with an angle
    tiling.innerFaces.head.outerComponent.get.angle = Some(AngleDegree(89))
    val result = validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("The sum of interior angles is incorrect")
    )
  }

  it should "fail if an inner face has a full circle angle" in {
    val tiling = square
    // Tamper with angles to make one a full circle while keeping the sum correct
    val edges  = tiling.innerFaces.head.halfEdges.value
    edges(0).angle = Some(AngleDegree(360))
    edges(1).angle = Some(AngleDegree(0))
    edges(2).angle = Some(AngleDegree(90))
    edges(3).angle = Some(AngleDegree(-90))
    val result = validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("cannot have full circles as interior angles")
    )
  }

  it should "fail if a face edge does not point back to the face" in {
    val tiling = square
    // Make an inner edge "forget" its face
    tiling.innerFaces.head.outerComponent.get.incidentFace = None
    val result = validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message shouldBe "HalfEdge V1 -> V2 [Missing incident face]"
    )
  }

  it should "fail if the boundary angles do not sum correctly" in {
    val twoSquares      = square.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(4)).value
    // V2 is on the boundary. The inner edge from V2 belongs to the first square.
    val v2              = twoSquares.findVertexUnsafe(V2).get
    val innerEdgeFromV2 = v2.incidentEdgesUnsafe.find(_.incidentFace.exists(_.id == F1)).get

    // Distort the angle, which affects both the face and boundary angle sums
    innerEdgeFromV2.angle = Some(AngleDegree(80))

    val result = validate(twoSquares)
    allAssert(
      result.isLeft shouldBe true, {
        val error         = result.left.value.message
        // Check that at least one of the expected errors is present, as iteration order is not guaranteed
        val faceError     = "Face F1 has an invalid polygon: The sum of interior angles is incorrect"
        val boundaryError = "Boundary angles sum: The sum of interior angles is incorrect"
        (error.contains(faceError) || error.contains(boundaryError)) shouldBe true
      }
    )
  }

  it should "fail if a boundary angle is undefined" in {
    val tiling = square
    tiling.boundaryEdgesSafer.value.head.angle = None
    val result = validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include(
        "HalfEdge V1 -> V4 [Missing angle]"
      )
    )
  }

  it should "fail if a boundary angle is a full circle (360 degrees)" in {
    val tiling = square
    tiling.boundaryEdgesSafer.value.head.angle = Some(AngleDegree(360))
    val result = validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Full circle boundary angles")
    )
  }

  it should "fail if a boundary angle is a full circle (0 degrees)" in {
    val tiling = square
    tiling.boundaryEdgesSafer.value.head.angle = Some(AngleDegree(0))
    val result = validate(tiling)
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message should include("Full circle boundary angles")
    )
  }
