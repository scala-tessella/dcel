package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Validation of the ADR-0025 de-risking spike: BOUNDED-V dart assembly ([[BucketAssembly]]).
  *
  * What this de-risks, and the verdict the assertions below lock in:
  *   - SOUNDNESS (decisive PASS): the angle-valid non-tilings `3.3.6.6` / `3.4.4.6` cannot be assembled into
  *     a torus — exactly the ADR-0022 win, now reproduced combinatorially with no overlap test.
  *   - CORRECTNESS / IDENTITY (decisive PASS): the assembler reproduces known tilings — including the octagon
  *     `4.8.8` the ζ engines cannot represent — with minimal-symbol canonical keys that match the
  *     [[DelaneySymbols]] oracle KEY-FOR-KEY, and gives the 2-uniform `{4⁴; 3³.4²}` bucket its correct
  *     multiplicity (2 distinct tilings). So the map→D-symbol bridge + dedup is right.
  *   - INNER-ASSEMBLY SIZE (per-V cost solved; the WALL is intrinsic): the ordered antiparallel port prune +
  *     PARTIAL-MAP CANONICAL DEDUP + FAIL-FAST FACE-CLOSURE (a face must close at exactly `before(d)` darts,
  *     which is invariant along a face) make a cell that FITS the V window tiny — `3.4.6.4` fell 2e6 → 168.
  *     The engine is sound, COMPLETE (`{3⁶;3².4.3.4}` found at its minimal cell V=7) and exactly identified.
  *     But the cells of interest are large-V: only 6 of 20 two-uniform buckets fit V≤6, and per-V cost grows
  *     ≈4–5× per +1. Since the torus-cell vertex count V IS the covolume, this is the SAME exponential wall
  *     as the fixed-Λ engines (ADR-0020) — pruning bought a large constant, not the asymptotics. See
  *     ADR-0025.
  */
class BucketAssemblySpec extends AnyFlatSpec with Matchers:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  // oracle keys for n = 1 (maxSize 12 reaches all 11 Archimedean, including 4.8.8)
  private lazy val oracleN1: List[(Int, Set[VertexSignature], String)] = DelaneySymbols.keyedTilings(1, 12)

  private def oracleKeysFor(types: Set[String]): Set[String] =
    val t = types.map(sig)
    oracleN1.filter(_._2 == t).map(_._3).toSet

  behavior of "BucketAssembly (ADR-0025 bounded-V dart assembly) — soundness"

  it should "NOT assemble any torus for the angle-valid non-tiling 3.3.6.6" in:
    BucketAssembly.enumerateBucket(Set(sig("3.3.6.6")), maxV = 4).tilings shouldBe empty

  it should "NOT assemble any torus for the angle-valid non-tiling 3.4.4.6" in:
    BucketAssembly.enumerateBucket(Set(sig("3.4.4.6")), maxV = 4).tilings shouldBe empty

  behavior of "BucketAssembly — reproduces k = 1 tilings with the oracle's canonical key"

  private def reproducesK1(name: String, maxV: Int): Unit =
    it should s"assemble exactly $name, keyed identically to the DelaneySymbols oracle" in:
      val r = BucketAssembly.enumerateBucket(Set(sig(name)), maxV)
      r.budgetHit shouldBe false
      r.tilings.map(_.types).toSet shouldBe Set(Set(sig(name)))
      r.keys shouldBe oracleKeysFor(Set(name))
      r.keys.size shouldBe 1

  reproducesK1("4.4.4.4", 2)     // square
  reproducesK1("3.3.3.3.3.3", 2) // triangular
  reproducesK1("6.6.6", 3)       // hexagonal
  reproducesK1("4.8.8", 4)       // truncated square — the octagon the ζ engines cannot do
  reproducesK1("3.6.3.6", 4)     // trihexagonal
  reproducesK1("3.4.6.4", 6)     // rhombitrihexagonal — larger cell (V≤6); tractable only with the dedup

  behavior of "BucketAssembly — k = 2 multiplicity"

  it should "assemble both distinct {4⁴; 3³.4²} tilings (the bucket appears twice in A068600)" in:
    val r = BucketAssembly.enumerateBucket(Set(sig("4.4.4.4"), sig("3.3.3.4.4")), maxV = 4)
    r.budgetHit shouldBe false
    every(r.tilings.map(_.n)) shouldBe 2
    every(r.tilings.map(_.types)) shouldBe Set(sig("4.4.4.4"), sig("3.3.3.4.4"))
    r.keys.size shouldBe 2 // two distinct adjacencies sharing the same vertex-type set

  behavior of "BucketAssembly — fail-fast pruning makes a FITTING cell tiny"

  // ordered ports + partial-map dedup + face-closure: a cell that fits the V window costs only hundreds of
  // states (3.4.6.4 fell from ~2e6 raw to ~1.7e2), and the search reaches almost only genuine closures.
  it should "assemble fitting-cell buckets in a few hundred states" in:
    BucketAssembly.enumerateBucket(Set(sig("4.4.4.4")), 2).states should be < 100L
    BucketAssembly.enumerateBucket(Set(sig("6.6.6")), 3).states should be < 30L
    BucketAssembly.enumerateBucket(Set(sig("4.8.8")), 4).states should be < 60L
    BucketAssembly.enumerateBucket(Set(sig("3.6.3.6")), 4).states should be < 500L
    BucketAssembly.enumerateBucket(Set(sig("3.4.6.4")), 6).states should be < 1000L

  behavior of "BucketAssembly — completeness at a large (V=7) minimal cell"

  // Confirms the engine is COMPLETE, not merely cheap: a 2-uniform tiling whose minimal torus cell needs 7
  // vertices is found at V=7 (absent for V≤6 because the cell genuinely does not fit — not a bug). Also the
  // evidence for the large-cell wall: such cells cost ~1e5 states and grow ~5×/V.
  it should "find {3⁶; 3².4.3.4} only once its V=7 cell is reached" in:
    BucketAssembly.enumerateBucket(Set(sig("3.3.3.3.3.3"), sig("3.3.4.3.4")), 6).tilings shouldBe empty
    val r = BucketAssembly.enumerateBucket(Set(sig("3.3.3.3.3.3"), sig("3.3.4.3.4")), 7)
    r.tilings.size shouldBe 1
    every(r.tilings.map(_.types)) shouldBe Set(sig("3.3.3.3.3.3"), sig("3.3.4.3.4"))
    every(r.tilings.map(_.n)) shouldBe 2
