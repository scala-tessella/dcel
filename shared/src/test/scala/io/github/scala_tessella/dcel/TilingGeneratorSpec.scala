package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingGenerator.{expandRotationally, expandRotationallyMore}
//import io.github.scala_tessella.dcel.conversion.TilingSVG.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/hex1/hex1_$i")

  it should "generate 2-step tilings for an hexagon" in:
    val found = List(hexagon).expandRotationallyMore(6, 2)
    found.size shouldBe 5
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/hex2/hex2_$i")

  it should "generate 3-step tilings for an hexagon" in:
    val found = List(hexagon).expandRotationallyMore(6, 3)
    found.size shouldBe 9
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/hex3/hex3_$i")

  it should "generate 4-step tilings for an hexagon" in:
    val found = List(hexagon).expandRotationallyMore(6, 4)
    found.size shouldBe 17
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/hex4/hex4_$i")

  it should "generate 5-step tilings for an hexagon" in:
    val found = List(hexagon).expandRotationallyMore(6, 5)
    found.size shouldBe 22
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/hex5/hex5_$i")

  it should "generate 6-step tilings for an hexagon" in:
    val found = List(hexagon).expandRotationallyMore(6, 6)
    found.size shouldBe 28
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/hex6/hex6_$i")

  it should "generate 7-step tilings for an hexagon" in:
    val found = List(hexagon).expandRotationallyMore(6, 7)
    found.size shouldBe 39
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/hex7/hex7_$i")

  it should "generate 12-step tilings for a triangle" in:
    val found = List(triangle).expandRotationallyMore(3, 12)
    found.size shouldBe 387
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/tri12/tri12_$i")

  it should "generate 12-step tilings for a square" in:
    val found = List(square).expandRotationallyMore(4, 12)
    found.size shouldBe 104
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/sqr12/sqr12_$i")

  it should "generate 12-step tilings for an hexagon" in:
    val found = List(hexagon).expandRotationallyMore(6, 12)
    found.size shouldBe 179
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/hex12/hex12_$i")

  it should "generate 12-step 3u 3g tilings for an hexagon" in:
    val found = List(hexagon).expandRotationallyMore(6, 12, Option(3), Option(3))
    found.size shouldBe 9
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsElem(), s"generator/hex12/hex12_$i")

  it should "generate 12-step 2u 2g tilings for an hexagon" in:
    val found = List(hexagon).expandRotationallyMore(6, 12, Option(2), Option(2))
    found.size shouldBe 3
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsXml(), s"generator/hex12_2u2g_$i")

//  it should "generate 12-step 1u 1g tilings for an hexagon" in :
//    val found = List(hexagon).expandRotationallyMore(6, 12, Option(1), Option(1))
//    found.size shouldBe 6
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsXml(), s"generator/hex12_1u1g_$i")

//  it should "generate 12-step 5u 5g tilings for an hexagon" in:
//    val found = List(hexagon).expandRotationallyMore(6, 12, Option(5), Option(5))
//    found.size shouldBe 143
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsXml(), s"generator/hex12_5u5g_$i")

//  it should "generate 16-step 5u 5g tilings for an hexagon" in :
//    val found = List(hexagon).expandRotationallyMore(6, 16, Option(5), Option(5))
//    found.size shouldBe 245
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsXml(), s"generator/hex16_5u5g_$i")

//  it should "generate 24-step 5u 5g tilings for an hexagon" in :
//    val found = List(hexagon).expandRotationallyMore(6, 24, Option(5), Option(5))
//    found.size shouldBe 5
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsXml(), s"generator/hex24_5u5g_$i")

//  it should "generate 32-step 5u 5g tilings for an hexagon" in :
//    val found = List(hexagon).expandRotationallyMore(6, 32, Option(5), Option(5))
//    found.size shouldBe 151
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsXml(), s"generator/hex32_5u5g_$i")

//  it should "generate 16-step 1u 1g tilings for a square with 2 rotations" in:
//    val found = List(square).expandRotationallyMore(2, 16, Option(1), Option(1))
//    found.size shouldBe 9
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsXml(), s"generator/sqr2_16_1u1g_$i")

//  it should "generate 16-step 2u 2g tilings for a square with 2 rotations" in:
//    val found = List(square).expandRotationallyMore(2, 16, Option(2), Option(2))
//    found.size shouldBe 72
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsXml(), s"generator/sqr2_16_2u2g_$i")

//  it should "generate 16-step 3u 3g tilings for a square with 2 rotations" in:
//    val found = List(square).expandRotationallyMore(2, 16, Option(3), Option(3))
//    found.size shouldBe 254
//    found.indices.foreach: i =>
//      saveFileSVG(found(i).toScalableVectorGraphicsXml(), s"generator/sqr2_16_3u3g_$i")
