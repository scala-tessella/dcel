package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.BigPoint
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Exactness checks for [[ZetaPoint]] — the integer ℤ[ζ₁₂] coordinates underpinning the fixed-Λ torus engine.
  */
class ZetaPointSpec extends AnyFlatSpec with Matchers:

  private def close(a: BigPoint, b: BigPoint): Boolean = (a.x - b.x).abs < BigDecimal("1e-12") &&
    (a.y - b.y).abs < BigDecimal("1e-12")

  behavior of "ZetaPoint"

  it should "embed the 12 unit directions at the right angles" in:
    for s <- 0 until 12 do
      val rad      = math.toRadians(30.0 * s)
      val expected = BigPoint(BigDecimal(math.cos(rad)), BigDecimal(math.sin(rad)))
      withClue(s"slot $s: ") {
        close(ZetaPoint.step(s).toBigPoint, expected) shouldBe true
      }

  it should "close a regular hexagon exactly (sum of 60°-spaced unit steps is the origin)" in:
    val sum = List(0, 2, 4, 6, 8, 10).map(ZetaPoint.step).foldLeft(ZetaPoint.origin)(_ + _)
    sum shouldBe ZetaPoint.origin

  it should "close a unit equilateral triangle exactly" in:
    // edges along 0°, 120°, 240°
    val sum = List(0, 4, 8).map(ZetaPoint.step).foldLeft(ZetaPoint.origin)(_ + _)
    sum shouldBe ZetaPoint.origin

  it should "multiply by ζ as a 30° rotation (ζ^s · ζ = ζ^{s+1})" in:
    for s <- 0 until 12 do
      ZetaPoint.step(s).timesZeta shouldBe ZetaPoint.step(s + 1)

  it should "decide congruence mod Λ by exact integer linear algebra" in:
    val v = ZetaPoint(1, 0, 0, 0) // (1, 0)
    val w = ZetaPoint(0, 0, 0, 1) // (0, 1)
    // Lattice points are congruent to the origin.
    ZetaPoint(2, 0, 0, 3).congruentMod(ZetaPoint.origin, v, w) shouldBe true
    v.congruentMod(ZetaPoint.origin, v, w) shouldBe true
    (v + v - w).congruentMod(ZetaPoint.origin, v, w) shouldBe true
    // ζ² = (½, √3/2) is off this integer lattice.
    ZetaPoint(0, 0, 1, 0).congruentMod(ZetaPoint.origin, v, w) shouldBe false
    // Two points differing by a lattice vector are congruent to each other.
    val p = ZetaPoint(0, 1, 0, 0)
    (p + v + v - w).congruentMod(p, v, w) shouldBe true
    (p + ZetaPoint(0, 0, 1, 0)).congruentMod(p, v, w) shouldBe false
