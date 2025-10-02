package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.TilingTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BigRadianSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private val acc = BigDecimal(1e-12)

  behavior of "BigRadian construction"

  it should "be created from Double and BigDecimal preserving value" in
    allAssert(
      BigRadian(0.0).toBigDecimal shouldBe BigDecimal(0.0),
      BigRadian(BigDecimal(1.2345)).toBigDecimal shouldBe BigDecimal(1.2345)
    )

  behavior of "BigRadian constants"

  it should "expose Tau and its fractions coherently" in
    allAssert(
      (BigRadian.TAU_2 * 2).toBigDecimal shouldEqual BigRadian.TAU.toBigDecimal,
      (BigRadian.TAU_4 * 2).toBigDecimal shouldEqual BigRadian.TAU_2.toBigDecimal,
      (BigRadian.TAU / 3).toBigDecimal shouldEqual BigRadian.TAU_3.toBigDecimal,
      (BigRadian.TAU_2 / 3).toBigDecimal shouldEqual BigRadian.TAU_6.toBigDecimal
    )

  behavior of "BigRadian arithmetic"

  it should "add and subtract" in {
    val a = BigRadian(1.2)
    val b = BigRadian(0.7)
    allAssert(
      (a + b).toBigDecimal shouldEqual BigDecimal(1.9),
      (a - b).toBigDecimal shouldEqual BigDecimal(0.5),
      (b - a).toBigDecimal shouldEqual BigDecimal(-0.5)
    )
  }

  it should "multiply and divide by Int" in {
    val r = BigRadian(2.4)
    allAssert(
      (r * 2).toBigDecimal shouldEqual BigDecimal(4.8),
      (r / 2).toBigDecimal shouldEqual BigDecimal(1.2)
    )
  }

  behavior of "BigRadian normalization and modulo"

  it should "normalize to [0, TAU)" in {
    val twoPi = BigRadian.TAU.toBigDecimal
    val x     = BigRadian.TAU_4          // +pi/2
    val y     = BigRadian(-1.0)          // negative small
    val z     = BigRadian(twoPi + 0.123) // > 2pi
    allAssert(
      x.normalizeTau.toBigDecimal shouldEqual BigRadian.TAU_4.toBigDecimal,
      y.normalizeTau.toBigDecimal >= BigDecimal(0) shouldBe true,
      y.normalizeTau.toBigDecimal < twoPi shouldBe true,
      (z.normalizeTau.toBigDecimal - BigDecimal(0.123)).abs <= acc shouldBe true
    )
  }

  it should "normalize to (-Pi, Pi]" in {
    val nearPi   = BigRadian.TAU_2
    val overPi   = BigRadian.TAU_2 + BigRadian(0.001)
    val underNeg = BigRadian(-3.5) // arbitrary negative
    val n1       = nearPi.normalizePi.toBigDecimal
    val n2       = overPi.normalizePi.toBigDecimal
    val n3       = underNeg.normalizePi.toBigDecimal
    val pi       = BigRadian.TAU_2.toBigDecimal
    allAssert(
      n1 > -pi && n1 <= pi shouldBe true,
      n2 <= pi && n2 > -pi shouldBe true,
      n3 <= pi && n3 > -pi shouldBe true
    )
  }

  it should "compute modTau consistent with remainder by TAU" in {
    val r  = BigRadian.TAU_4 + BigRadian(0.3)
    val rt = r.modTau.toBigDecimal
    val ex = r.toBigDecimal % BigRadian.TAU.toBigDecimal
    rt shouldEqual ex
  }

  behavior of "BigRadian comparisons and almostEquals"

  it should "provide Ordering by underlying BigDecimal" in {
    val xs = List(BigRadian(1.0), BigRadian(0.5), BigRadian(2.0)).sorted
    xs.map(_.toBigDecimal) shouldBe List(BigDecimal(0.5), BigDecimal(1.0), BigDecimal(2.0))
  }

  it should "check approximate equality with default accuracy" in {
    val a = BigRadian(1.0)
    val b = BigRadian(1.0 + 1e-13)
    val c = BigRadian(1.0 + 1e-6)
    allAssert(
      a.almostEquals(b) shouldBe true,
      a.almostEquals(c) shouldBe false
    )
  }

  it should "check approximate equality with custom accuracy" in {
    val a = BigRadian(2.0)
    val b = BigRadian(2.0000001)
    allAssert(
      a.almostEquals(b, BigDecimal(1e-5)) shouldBe true,
      a.almostEquals(b, BigDecimal(1e-8)) shouldBe false
    )
  }

  behavior of "BigRadian with degrees interop (sanity)"

  it should "match known degree-to-radian values" in
    allAssert(
      AngleDegree(0).toBigRadian.almostEquals(BigRadian(0), acc.toDouble) shouldBe true,
      AngleDegree(90).toBigRadian.almostEquals(BigRadian.TAU_4, acc.toDouble) shouldBe true,
      AngleDegree(180).toBigRadian.almostEquals(BigRadian.TAU_2, acc.toDouble) shouldBe true
    )
