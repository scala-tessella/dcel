package io.github.scala_tessella
package dcel

import TilingAddition.*

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingSmallestSpec extends AnyFlatSpec with Matchers with EitherValues:

  behavior of "TilingDCEL smallest significant examples"

  it should "add an enclosing square to a triangle, producing the smallest DCEL with a non boundary vertex" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle.addRegularPolygon("V1", "V2", 4)

    result.isRight shouldBe true
    //    val tiling = result.value
    //    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
  }

  it should "add a triangle to a triangle, producing the smallest DCEL with a non boundary edge" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle.addRegularPolygonToBoundary("V2", 3)

    result.isRight shouldBe true
    val tiling = result.value
//    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))

    // Check structure
    tiling.vertices should have size 4
    tiling.innerFaces should have size 2
    tiling.halfEdges should have size 10 // 4 boundary + 6 inner edges

    // Check boundary angles
    tiling.outerFace.halfEdgesSafe.map(_.angle.get.toString).mkString(", ") shouldBe "240, 300, 240, 300"
    tiling.outerFace.halfEdgesSafe.map(_.incidentFace.get.id).mkString(", ") shouldBe "F0, F0, F0, F0"

    // Check inner face angles
    tiling.innerFaces.map(_.halfEdgesSafe.map(_.angle.get.toString).mkString(", ")) shouldBe List("60, 60, 60", "60, 60, 60")
    tiling.innerFaces.map(_.halfEdgesSafe.map(_.incidentFace.get.id).mkString(", ")) shouldBe List("F1, F1, F1", "F2, F2, F2")

    // Check boundary
    val boundary = tiling.boundary
    boundary should have length 4
    boundary.map(_.id) should contain theSameElementsInOrderAs Vector("V1", "V3", "V2", "V4")
  }

  it should "add an enclosing square to a triangle and then a triangle, producing the smallest DCEL with a non boundary face" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle
      .addRegularPolygon("V1", "V2", 4).value
      .addRegularPolygonToBoundary("V2", 3)

    result.isRight shouldBe true
//    val tiling = result.value
//    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
  }
