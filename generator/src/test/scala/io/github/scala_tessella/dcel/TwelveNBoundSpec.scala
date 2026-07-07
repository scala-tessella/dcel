package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.DelaneySymbols.{DSymbol, Orbit}
import io.github.scala_tessella.dcel.VertexTypes.VertexSignature
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The 12n bound (ADR-0039, Phase 0): a Krotenheerdt n-uniform tiling's MINIMAL Delaney–Dress symbol has at
  * most `12n` chambers. Proof, in this codebase's terms (each step is a test section below):
  *
  *   1. the `(1,2)`-orbits (vertex orbits) PARTITION the chambers — every chamber sits at exactly one vertex
  *      orbit, so `size = Σ_orbits length`;
  *   2. per orbit, `length ≤ 2·r` ([[DelaneySymbols.Orbit]]: a cycle alternates σ₁/σ₂ around the vertex, a
  *      chain is even shorter), `r ≤ m₁₂` (`m₁₂ = r·v`, `v ≥ 1`), and `m₁₂ ≤ 6` (the vertex degree of a
  *      regular-polygon tiling — six 60° triangle angles fill 360°, nothing smaller exists);
  *   3. hence `size ≤ Σ 2·m₁₂ ≤ 12·#orbits = 12n` (Krotenheerdt: n vertex orbits).
  *
  * Consequence (the pivot's keystone): the WHOLE n ≤ 7 problem lives in symbols of ≤ 84 chambers — the
  * search-space bound is n, not covolume — and enumerating to `12n` CERTIFIES a count. The covolume corollary
  * (`V_cell = Σᵢ [G:Λ]/|stabᵢ| ≤ 12n`, point groups ≤ 12) has its tight witness in the existing
  * [[BucketAssemblySpec]] `reproducesK1("4.6.12", 12)` — cell = 12 vertices at n = 1.
  *
  * The properties are checked EXHAUSTIVELY over every tiling the oracle budget reaches (stronger than random
  * generation): fast at `maxSize = 12` (n = 1 complete), and an `ignore`d deep run at the complete-n≤3
  * budget.
  */
class TwelveNBoundSpec extends AnyFlatSpec with Matchers:

  private val maxSize = 12

  /** Everything the fast budget yields — maxN is NOT capped below 7: the bound must hold for every tiling. */
  private lazy val syms: List[(Int, List[VertexSignature], DSymbol)] =
    DelaneySymbols.enumerateSymbols(maxN = 7, maxSize = maxSize)

  private def orbits12(ds: DSymbol): Vector[Orbit] = ds.orbs.filter(o => o.i == 1 && o.j == 2)

  private def checkBound(n: Int, sigs: List[VertexSignature], ds: DSymbol): Unit =
    withClue(s"n=$n sigs=$sigs size=${ds.size}: "):
      val orbs = orbits12(ds)
      // step 1 — partition
      orbs.flatMap(_.elements).sorted shouldBe (1 to ds.size).toVector
      // step 2 — per-orbit chain
      for (o, idx) <- orbs.zipWithIndex do
        val m12 = ds.m(1, 2, o.elements.head)
        withClue(s"orbit $idx (${o.elements}): "):
          o.length should be <= 2 * o.r
          o.r should be <= m12
          m12 shouldBe sigs(idx).size // degree read two ways: the symbol vs the vertex signature
          m12 should be <= 6
      // step 3 — the bound (sharp form, then 12n)
      orbs.size shouldBe n
      ds.size should be <= 2 * sigs.map(_.size).sum
      ds.size should be <= 12 * n

  behavior of "the fixture (fast oracle, maxSize 12)"

  it should "be non-vacuous: n = 1 complete (11) plus n ≥ 2 tilings at this budget" in:
    syms.count(_._1 == 1) shouldBe 11
    syms.count(_._1 >= 2) should be > 0

  behavior of "the 12n bound (ADR-0039)"

  it should "hold — with every proof step — for every tiling the fast budget reaches" in:
    syms.foreach(checkBound)

  // Tightness: the per-orbit bound `length ≤ 2·m₁₂` is ATTAINED (the ≤ chain is not slack everywhere). The
  // witness is the snub 3.3.3.3.6 (p6, chiral ⇒ trivial vertex stabilizer): one 12-orbit of 2·deg = 10
  // chambers, so its minimal symbol has exactly 10. (The covolume corollary's tight witness, 4.6.12 at
  // V_cell = 12 = 12·1, is the standing BucketAssemblySpec `reproducesK1("4.6.12", 12)`.)
  it should "attain the per-orbit bound: the snub's minimal symbol is exactly 2·degree = 10 chambers" in:
    val snubSig = List(3, 3, 3, 3, 6)
    val snub    = syms.filter((n, sigs, _) => n == 1 && sigs == List(snubSig))
    snub should have size 1
    val ds      = snub.head._3
    ds.size shouldBe 10
    val o       = orbits12(ds).head
    o.length shouldBe 2 * o.r
    o.length shouldBe 2 * ds.m(1, 2, o.elements.head)

  // ≈17 min (the maxSize-24 D-set tree; VERIFIED GREEN 2026-07-07). Un-ignore to verify the bound over the
  // COMPLETE n ≤ 3 oracle: all 70 tilings satisfy every step, and the max chamber count (≤ 24) sits well
  // under 12·3 = 36 — i.e. the bound's budget is sufficient (and generous) exactly where completeness is known.
  ignore should "hold across the complete n ≤ 3 oracle (11/20/39 at maxSize 24, max chambers < 36)" in:
    val deep = DelaneySymbols.enumerateSymbols(maxN = 3, maxSize = 24)
    deep.groupBy(_._1).view.mapValues(_.size).toMap shouldBe Map(1 -> 11, 2 -> 20, 3 -> 39)
    deep.foreach(checkBound)
    deep.map(_._3.size).max should be < 12 * 3
