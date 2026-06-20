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
  *   - INNER-ASSEMBLY SIZE (the open risk — PARTIAL): with the ordered antiparallel port prune the search is
  *     tiny for small-cell buckets (square/hexagonal/octagon: tens–hundreds of states) but the raw
  *     port-matched perfect-matching enumeration still SCATTERS for triangle-rich / large-minimal-cell
  *     buckets (`3.6.3.6` ~1e5, `3.4.6.4` ~2e6, `{3⁶;3⁴.6}` over budget). See the measurement test: states do
  *     not yet cleanly track V, so the ADR's "few assemblies per V" needs the further controls (partial-map
  *     canonical dedup, MRV) before n = 4–7 is reachable. The numbers here are the evidence for that next
  *     decision.
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

  behavior of "BucketAssembly — k = 2 multiplicity"

  it should "assemble both distinct {4⁴; 3³.4²} tilings (the bucket appears twice in A068600)" in:
    val r = BucketAssembly.enumerateBucket(Set(sig("4.4.4.4"), sig("3.3.3.4.4")), maxV = 4)
    r.budgetHit shouldBe false
    every(r.tilings.map(_.n)) shouldBe 2
    every(r.tilings.map(_.types)) shouldBe Set(sig("4.4.4.4"), sig("3.3.3.4.4"))
    r.keys.size shouldBe 2 // two distinct adjacencies sharing the same vertex-type set

  behavior of "BucketAssembly — inner-assembly SIZE measurement (the open risk)"

  it should "be tiny for small-cell buckets but scatter for large/triangle-rich ones" in:
    // small cells: the assembly is genuinely small — the ADR's hoped-for regime
    BucketAssembly.enumerateBucket(Set(sig("4.4.4.4")), 2).states should be < 2000L
    BucketAssembly.enumerateBucket(Set(sig("6.6.6")), 3).states should be < 200L
    BucketAssembly.enumerateBucket(Set(sig("4.8.8")), 4).states should be < 2000L
    // single-port triangle cell: ports cannot prune ⇒ the V-layer above the minimal cell already scatters
    BucketAssembly.enumerateBucket(Set(sig("3.6.3.6")), 4).states should be > 10000L
