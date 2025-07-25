package io.github.scala_tessella
package dcel

import TilingAddition.*

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingAdditionSpec extends AnyFlatSpec with Matchers with EitherValues {

  behavior of "TilingDCEL.addRegularPolygon"

  it should "add a triangle to a triangle, producing a valid DCEL" in {
    val triangle = TilingBuilder.createRegularPolygon(3).value
    val result = triangle.addRegularPolygon(3, "V1")

    result.isRight shouldBe true
    val tiling = result.value

    val check = TilingDCEL.validate(tiling)
    check shouldBe Right(())

    tiling.outerFace.halfEdgesSafe.map(_.angle.get).mkString(", ") shouldBe "270, 270, 270, 270"
//    tiling.outerFace.halfEdgesSafe.map(_.incidentFace.get.id).mkString(", ") shouldBe "F_Outer, F_Outer, F_Outer, F_Outer"
//    tiling.innerFaces.map(_.halfEdgesSafe.map(_.angle.get).mkString(", ")) shouldBe List("90, 90, 90, 90")
//    tiling.innerFaces.map(_.halfEdgesSafe.map(_.incidentFace.get.id).mkString(", ")) shouldBe List("F_Poly, F_Poly, F_Poly, F_Poly")

  }

  it should "fail to add a polygon with less than 3 sides" in {
    val square = TilingBuilder.createRegularPolygon(4).value
    val result = square.addRegularPolygon(2, "V1")
    result.isLeft shouldBe true
    result.left.value should include("must have at least 3 sides")
  }

  it should "fail to add a polygon on a non-existent vertex" in {
    val square = TilingBuilder.createRegularPolygon(4).value
    val result = square.addRegularPolygon(3, "V99")
    result.isLeft shouldBe true
    result.left.value should include("not found on the boundary")
  }

}
