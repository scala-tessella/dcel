package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingGenerator.expandRotationally
import io.github.scala_tessella.dcel.conversion.TilingSVG.toScalableVectorGraphicsElem
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe

class TilingGeneratorSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingGenerator.validSignatures"

  it should "calculate the pippo" in:
    TilingGenerator.validSignatures shouldBe
      Set(
        List(4, 4, 4, 4),
        List(3, 4, 6, 4),
        List(3, 3, 4, 12),
        List(3, 6, 3, 6),
        List(3, 3, 3, 3, 3, 3),
        List(3, 4, 4, 6),
        List(3, 4, 3, 12),
        List(3, 3, 4, 3, 4),
        List(3, 3, 3, 4, 4),
        List(3, 3, 6, 6),
        List(3, 3, 3, 3, 6)
      )

//  behavior of "TilingGenerator.findTilings"
//
//  it should "work" in:
//    val found = TilingGenerator.findTilings(2, 70)
//    found.size shouldBe 15
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/pippo$i")

  behavior of "TilingGenerator.expandRotationally"

  it should "generate some tilings for an triangle" in:
    val found = triangle.expandRotationally(3)
    found.size shouldBe 4
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/tri$i")

  it should "generate some tilings for a square" in:
    val found = square.expandRotationally(4)
    found.size shouldBe 3
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/sqr$i")

  it should "generate some tilings for an hexagon" in:
    val found = hexagon.expandRotationally(6)
    found.size shouldBe 3
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/hex$i")
