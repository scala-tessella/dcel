package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingDeletion.deleteEdge
import io.github.scala_tessella.dcel.TilingEquivalency.isEquivalentTo
import io.github.scala_tessella.dcel.Tree.{Branch, Leaf}
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon}
import io.github.scala_tessella.dcel.structure.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingDoubleSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingDCEL.growDouble"

  it should "return an empty tiling" in {
    TilingDCEL.empty.growDouble.value.isEmpty shouldBe true
  }

  it should "fail for an hexagon" in {
    TilingBuilder.createRegularPolygon(RegularPolygon(6)).growDouble.isLeft shouldBe true
  }

  it should "not fail for a square" in {
    TilingBuilder.createRegularPolygon(RegularPolygon(4)).growDouble.isRight shouldBe true
  }

