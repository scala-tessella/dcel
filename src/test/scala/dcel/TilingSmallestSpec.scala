package io.github.scala_tessella
package dcel

import dcel.BigDecimalGeometry.{AngleDegree, BigPoint}
import dcel.TilingAddition.*
import ring_seq.RingSeq.*

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingSmallestSpec extends AnyFlatSpec with Matchers with EitherValues:

  behavior of "TilingDCEL.addRegularPolygon"

  it should "add the same irregular pentagon with a different orientation to a triangle, producing a valid DCEL" in {
    val triangle = TilingBuilder.createSimplePolygon(
      List(AngleDegree(90), AngleDegree(90), AngleDegree(30), AngleDegree(300), AngleDegree(30))
    ).value
    val result = triangle.addRegularPolygon(3, "V4")

    result.isRight shouldBe true
    val tiling = result.value
    println(TilingDCEL.validate(tiling))
    println(tiling.toSVG(leavingEdgeMarkers = true, faceIdsOnEdges = true))
  }
