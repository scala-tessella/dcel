package io.github.scala_tessella
package dcel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues

class TilingAddPolygonSpec extends AnyFlatSpec with Matchers with EitherValues:

  behavior of "TilingDCEL.maybeAddRegularPolygon"

  it should "fail to add a polygon with fewer than 3 sides" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
    val result = tiling.maybeAddRegularPolygon(2, "V1")
    result.isLeft shouldBe true
    result.left.value should include("must have at least 3 sides")
  }

  it should "fail if the specified boundary edge does not exist" in {
    val tiling = TilingBuilder.createRegularPolygon(4).value
    val result = tiling.maybeAddRegularPolygon(4, "V_NonExistent")
    result.isLeft shouldBe true
    result.left.value should include("No boundary edge found")
  }

  it should "successfully add a triangle to the side of another triangle" in {
    // Start with a single triangle (V0-V1-V2)
    val initialTiling = TilingBuilder.createRegularPolygon(3).value
    initialTiling.vertices.length shouldBe 3
    initialTiling.innerFaces.length shouldBe 1
    initialTiling.boundary.map(_.id) shouldBe Vector("V0", "V2", "V1")

    // Add another triangle onto the edge starting at V1 (which is edge V1 -> V2)
    val result = initialTiling.maybeAddRegularPolygon(3, "V1")
    result.isRight shouldBe true

    val newTiling = result.value
    // Should add 1 new vertices (V3)
    newTiling.vertices.length shouldBe 4
    // Should add 1 new inner face
    newTiling.innerFaces.length shouldBe 2

    // The new boundary should bypass V1 and V2
    val newBoundaryIds = newTiling.boundary.map(_.id)
    newBoundaryIds shouldBe Vector("V0", "V2", "V1", "V3")
    newTiling.halfEdges.length shouldBe 10
    newTiling.innerFaces.map(_.halfEdges.map(_.angle).mkString(", ")) shouldBe List("60, 60, 60", "60, 60, 60")
    newTiling.innerFaces.map(_.halfEdges.map(_.incidentFace.get.id).mkString(", ")) shouldBe List("F_Poly, F_Poly, F_Poly", "F_Poly_1, F_Poly_1, F_Poly_1")
    newTiling.outerFace.halfEdges.map(_.angle).mkString(", ") shouldBe "300, 240, 300, 240"
    newTiling.outerFace.halfEdges.map(_.incidentFace.get.id).mkString(", ") shouldBe "F_Outer, F_Outer, F_Outer, F_Outer"
  }

  it should "successfully add a square to the side of another square" in {
    // Start with a single square (V0-V1-V2-V3)
    val initialTiling = TilingBuilder.createRegularPolygon(4).value
    initialTiling.vertices.length shouldBe 4
    initialTiling.innerFaces.length shouldBe 1
    initialTiling.boundary.map(_.id) shouldBe Vector("V0", "V3", "V2", "V1")

    // Add another square onto the edge starting at V1 (which is edge V1 -> V2)
    val result = initialTiling.maybeAddRegularPolygon(4, "V1")
    result.isRight shouldBe true

    val newTiling = result.value
    // Should add 2 new vertices (V4, V5)
    newTiling.vertices.length shouldBe 6
    // Should add 1 new inner face
    newTiling.innerFaces.length shouldBe 2

    // The new boundary should bypass V1 and V2
    val newBoundaryIds = newTiling.boundary.map(_.id)
    newBoundaryIds shouldBe Vector("V0", "V3", "V2", "V1", "V5", "V4")
    newTiling.halfEdges.length shouldBe 14
  }

  it should "fail if the added polygon crosses the boundary" in {
    val result =
      TilingBuilder.createRegularPolygon(4).value
        .maybeAddRegularPolygon(4, "V1").value
        .maybeAddRegularPolygon(4, "V1").value
        .maybeAddRegularPolygon(5, "V1")
    result.isRight shouldBe false
  }

  it should "successfully add an hexagon with coincident vertices" in {
    // Start with a single hexagon (V0-V1-V2-V3-V4-V5)
    val initialTiling = TilingBuilder.createRegularPolygon(6).value
    initialTiling.vertices.length shouldBe 6
    initialTiling.innerFaces.length shouldBe 1
    initialTiling.boundary.map(_.id) shouldBe Vector("V0", "V5", "V4", "V3", "V2", "V1")

    val result = initialTiling
      .maybeAddRegularPolygon(6, "V1").value
      .maybeAddRegularPolygon(6, "V3").value
      .maybeAddRegularPolygon(6, "V1", true)
    result.isRight shouldBe true

    val newTiling = result.value
    newTiling.boundary.map(_.id) shouldBe
      Vector("V0", "V5", "V4", "V3", "V13", "V12", "V11", "V10", "V2", "V10", "V15", "V14", "V9", "V8", "V7", "V6")
    newTiling.vertices.length shouldBe 16
    // Should add 1 new inner face
    newTiling.innerFaces.length shouldBe 4
    newTiling.faces.map(_.id).mkString(", ") shouldBe "F_Outer, F_Poly, F_Poly_1, F_Poly_2, F_Poly_3"
    newTiling.checkForDuplicateHalfEdges()
    newTiling.halfEdges.length shouldBe 38
  }

  it should "successfully add at a specular vertex the same hexagon with coincident vertices" in {
    // Start with a single hexagon (V0-V1-V2-V3-V4-V5)
    val initialTiling = TilingBuilder.createRegularPolygon(6).value
    initialTiling.vertices.length shouldBe 6
    initialTiling.innerFaces.length shouldBe 1
    initialTiling.boundary.map(_.id) shouldBe Vector("V0", "V5", "V4", "V3", "V2", "V1")

    val result = initialTiling
      .maybeAddRegularPolygon(6, "V1").value
      .maybeAddRegularPolygon(6, "V3").value
      .maybeAddRegularPolygon(6, "V10", true)
    result.isRight shouldBe true

    val newTiling = result.value
    newTiling.boundary.map(_.id) shouldBe
      Vector("V0", "V5", "V4", "V3", "V13", "V12", "V11", "V10", "V15", "V14", "V2", "V1", "V9", "V8", "V7", "V6")
    newTiling.vertices.length shouldBe 16
    // Should add 1 new inner face
    newTiling.innerFaces.length shouldBe 4
    newTiling.faces.map(_.id).mkString(", ") shouldBe "F_Outer, F_Poly, F_Poly_1, F_Poly_2, F_Poly_3"
    newTiling.checkForDuplicateHalfEdges()
    newTiling.halfEdges.length shouldBe 38
  }

  it should "successfully add at a different vertex the same hexagon with coincident vertices in both directions" in {
    // Start with a single hexagon (V0-V1-V2-V3-V4-V5)
    val initialTiling = TilingBuilder.createRegularPolygon(6).value
    initialTiling.vertices.length shouldBe 6
    initialTiling.innerFaces.length shouldBe 1
    initialTiling.boundary.map(_.id) shouldBe Vector("V0", "V5", "V4", "V3", "V2", "V1")

    val result = initialTiling
      .maybeAddRegularPolygon(6, "V1").value
      .maybeAddRegularPolygon(6, "V3").value
      .maybeAddRegularPolygon(6, "V2", true)
    result.isRight shouldBe true

    val newTiling = result.value
    newTiling.boundary.map(_.id) shouldBe
      Vector("V0", "V5", "V4", "V3", "V13", "V12", "V11", "V10", "V10", "V15", "V14", "V1", "V9", "V8", "V7", "V6")
    // Should add 1 new inner face
    newTiling.innerFaces.length shouldBe 4
    newTiling.vertices.length shouldBe 16
    // Should add 1 new inner face
    newTiling.innerFaces.length shouldBe 4
    newTiling.faces.map(_.id).mkString(", ") shouldBe "F_Outer, F_Poly, F_Poly_1, F_Poly_2, F_Poly_3"
    newTiling.checkForDuplicateHalfEdges()
    newTiling.halfEdges.length shouldBe 38
  }

//  it should "successfully add a dodecagon and the ensuing triangle" in {
//    // Start with a single hexagon (V0-V1-V2-V3-V4-V5)
//    val initialTiling = TilingBuilder.createRegularPolygon(12).value
//    initialTiling.vertices.length shouldBe 12
//    initialTiling.innerFaces.length shouldBe 1
//    initialTiling.boundary.map(_.id) shouldBe Vector("V0", "V11", "V10", "V9", "V8", "V7", "V6", "V5", "V4", "V3", "V2", "V1")
//
//    val result = initialTiling
//      .maybeAddRegularPolygon(12, "V1").value
//      .maybeAddRegularPolygon(12, "V3", true)
//    result.isRight shouldBe true
//
//    val newTiling = result.value
//    // Should add 2 new inner face
//    newTiling.vertices.length shouldBe 30
//    newTiling.innerFaces.length shouldBe 4
//  }
//
//  it should "add an ensuing triangle" in {
//    val result =
//      TilingBuilder.createRegularPolygon(4).value
//        .maybeAddRegularPolygon(3, "V1").value
//        .maybeAddRegularPolygon(3, "V3").value
//        .maybeAddRegularPolygon(4, "V5").value
//        .maybeAddRegularPolygon(3, "V6").value
//        .maybeAddRegularPolygon(4, "V1").value
//        .maybeAddRegularPolygon(3, "V1", true)
//
//    val newTiling = result.value
//    // Should add 2 new inner face
//    newTiling.vertices.length shouldBe 11
//    newTiling.innerFaces.length shouldBe 8
//  }