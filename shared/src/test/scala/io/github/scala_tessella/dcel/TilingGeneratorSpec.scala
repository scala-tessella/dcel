package io.github.scala_tessella.dcel

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

  behavior of "TilingGenerator.findTilings"

  it should "work" in:
    val found = TilingGenerator.findTilings(1, 70)
    found.size shouldBe 36
    println(found.head.toSVG())


