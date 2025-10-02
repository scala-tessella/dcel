package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.TilingTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.math.Rational

class AngleDegreeSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private val acc = 1e-12

  behavior of "AngleDegree construction and accessors"

  it should "be created from Int and Rational and expose the underlying Rational" in
    allAssert(
      AngleDegree(0).toRational shouldBe Rational(0),
      AngleDegree(90).toRational shouldBe Rational(90),
      AngleDegree(Rational(45, 2)).toRational shouldBe Rational(45, 2)
    )

  behavior of "AngleDegree arithmetic"

  it should "add and subtract preserving exact Rational semantics" in {
    val a = AngleDegree(90)
    val b = AngleDegree(45)
    allAssert(
      (a + b).toRational shouldBe Rational(135),
      (a - b).toRational shouldBe Rational(45),
      (b - a).toRational shouldBe Rational(-45)
    )
  }

  it should "negate (inverted) correctly" in
    allAssert(
      AngleDegree(30).inverted.toRational shouldBe Rational(-30),
      AngleDegree(0).inverted.toRational shouldBe Rational(0)
    )

  it should "multiply and divide by Int exactly" in
    allAssert(
      (AngleDegree(30) * 3).toRational shouldBe Rational(90),
      (AngleDegree(90) / 3).toRational shouldBe Rational(30),
      (AngleDegree(Rational(45, 2)) * 2).toRational shouldBe Rational(45),
      (AngleDegree(Rational(45, 2)) / 2).toRational shouldBe Rational(45, 4)
    )

  behavior of "AngleDegree angular utilities"

  it should "normalise to [0, 360)" in
    allAssert(
      AngleDegree(0).normalised.toRational shouldBe Rational(0),
      AngleDegree(90).normalised.toRational shouldBe Rational(90),
      AngleDegree(360).normalised.toRational shouldBe Rational(0),
      AngleDegree(450).normalised.toRational shouldBe Rational(90),
      AngleDegree(-90).normalised.toRational shouldBe Rational(270),
      AngleDegree(-360).normalised.toRational shouldBe Rational(0),
      AngleDegree(-450).normalised.toRational shouldBe Rational(270)
    )

  it should "detect full circles regardless of multiples of 360" in
    allAssert(
      AngleDegree(0).isFullCircle shouldBe true,
      AngleDegree(360).isFullCircle shouldBe true,
      AngleDegree(720).isFullCircle shouldBe true,
      AngleDegree(-360).isFullCircle shouldBe true,
      AngleDegree(361).isFullCircle shouldBe false,
      AngleDegree(180).isFullCircle shouldBe false
    )

  it should "compute conjugate (360 - d) and supplement (180 - d)" in
    allAssert(
      AngleDegree(90).conjugate.toRational shouldBe Rational(270),
      AngleDegree(0).conjugate.toRational shouldBe Rational(360),
      AngleDegree(360).conjugate.toRational shouldBe Rational(0),
      AngleDegree(60).supplement.toRational shouldBe Rational(120),
      AngleDegree(180).supplement.toRational shouldBe Rational(0),
      AngleDegree(270).supplement.toRational shouldBe Rational(-90)
    )

  behavior of "AngleDegree <-> BigRadian conversion"

  it should "convert degrees to BigRadian with expected values" in
    allAssert(
      AngleDegree(0).toBigRadian.almostEquals(BigRadian(0), acc) shouldBe true,
      AngleDegree(90).toBigRadian.almostEquals(BigRadian.TAU_4, acc) shouldBe true,
      AngleDegree(180).toBigRadian.almostEquals(BigRadian.TAU_2, acc) shouldBe true
    )

  it should "construct from BigRadian (with possible rounding)" in
    allAssert(
      AngleDegree(BigRadian(0)).toRational.toDouble === 0.0 +- acc shouldBe true,
      AngleDegree(
        BigRadian.TAU_4
      ).toRational.toDouble === 90.0 +- 1e-8 shouldBe true, // accept small numeric drift
      AngleDegree(BigRadian.TAU_2).toRational.toDouble === 180.0 +- 1e-8 shouldBe true
    )

  behavior of "AngleDegree collection helpers"

  it should "sum a sequence exactly with sumExact" in {
    val angles = Seq(AngleDegree(30), AngleDegree(60), AngleDegree(90))
    angles.sumExact.toRational shouldBe Rational(180)
  }
