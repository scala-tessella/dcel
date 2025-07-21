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
  }

  it should "fail if the added polygon crosses the boundary" in {
    val result =
      TilingBuilder.createRegularPolygon(4).value
        .maybeAddRegularPolygon(4, "V1").value
        .maybeAddRegularPolygon(4, "V1").value
        .maybeAddRegularPolygon(5, "V1")
    val newTiling = result.value
    println(newTiling.toSVG())
    result.isRight shouldBe false
  }
