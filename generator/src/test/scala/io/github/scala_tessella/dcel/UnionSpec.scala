package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Phase-3 UNION validation. Tests the [[UnionDriver]] plumbing + certifies the first FULLY-complete count
  * via the union: n = 1 = 11/11 INCLUDING the octagon `4.8.8` (which the ℤ[ζ₁₂] grower cannot represent but
  * bounded-V can). The heavy n = 2 = 20/20 certification is `UnionProbe` (run on demand). Both engines are
  * sound and key in the shared D-symbol space, so a deduped union of size `counts(n)` is the exact set.
  */
class UnionSpec extends AnyFlatSpec with Matchers:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)
  private lazy val oracleN1: Set[String]      =
    DelaneySymbols.keyedTilings(1, 12).filter(_._1 == 1).map(_._3).toSet

  behavior of "UnionDriver.candidateTypeSets (the bounded-V buckets per level)"

  it should "give the 11 Archimedean singletons at n=1 and the distinct 2-type sets at n=2" in:
    UnionDriver.candidateTypeSets(1).toSet shouldBe TilingReference.n1.map(Set(_))
    UnionDriver.candidateTypeSets(1) should have size 11
    val n2 = UnionDriver.candidateTypeSets(2)
    n2 shouldBe TilingReference.n2.distinct
    all(n2.map(_.size)) shouldBe 2
    UnionDriver.candidateTypeSets(3) should not be empty // parser yields the n=3 rows

  behavior of "the union — n=1 = 11/11 CERTIFIED (incl. the octagon bounded-V provides)"

  // bounded-V is complete at n=1 (incl 4.8.8); it alone certifies 11/11. The union with the grower (which
  // contributes the 10 in-scope, a subset) cannot reduce it. SOUND + size 11 = counts(1) ⇒ exact set.
  it should "have bounded-V reach exactly the 11 oracle n=1 keys (incl 4.8.8)" in:
    val keys = UnionDriver.boundedVKeys(UnionDriver.candidateTypeSets(1), maxV = 12)
    keys shouldBe oracleN1
    keys should have size TilingReference.counts(1) // 11
    // the octagon is the bounded-V-only contribution (grower is ℤ[ζ₁₂], no 4.8.8)
    val octKey = DelaneySymbols.keyedTilings(1, 12).find(_._2 == Set(sig("4.8.8"))).map(_._3).get
    keys should contain(octKey)

  behavior of "rotationTable (the n=2 rotation-symmetry reference, via bounded-V + realizeCell)"

  // Small maxV (fast) reaches the small-cell n=2 — incl. the multiplicity-2 {4⁴;3³.4²} (BOTH siblings). Each
  // entry must be a valid 2-type set with NON-EMPTY centres of valid orders (soundness via realizeCell is
  // already tested). The full 20-row table is RotationTableProbe (run on demand at higher maxV).
  it should "report sound rotation centres for the small-cell n=2 (incl both {4⁴;3³.4²} siblings)" in:
    val table = UnionDriver.rotationTable(n = 2, maxV = 6)
    table should not be empty
    table.values.foreach: (types, centres) =>
      types.size shouldBe 2
      centres should not be empty
      centres.foreach((kind, order) => withClue(s"($kind,$order): ")(Set(2, 3, 4, 6) should contain(order)))
    // {4⁴;3³.4²} is multiplicity 2 and small-cell ⇒ both siblings present with their (distinct) centres
    table.values.count(_._1 == Set(sig("4.4.4.4"), sig("3.3.3.4.4"))) shouldBe 2

  behavior of "the union operator (set union of two sound key sets, deduped)"

  it should "be the deduped union and never exceed soundness (⊆ oracle at n=1)" in:
    // a tiny grower slice (cheap) ∪ bounded-V(n=1); union ⊆ oracle and ⊇ each part
    val grower = UnionDriver.growerKeysByN(maxN = 1, maxFaces = 12, parallelism = 4).getOrElse(1, Set.empty)
    val bucket = UnionDriver.boundedVKeys(UnionDriver.candidateTypeSets(1), maxV = 12)
    val union  = grower ++ bucket
    grower.subsetOf(union) shouldBe true
    bucket.subsetOf(union) shouldBe true
    union.subsetOf(oracleN1) shouldBe true // both engines sound ⇒ union sound
    union shouldBe oracleN1 // bucket already = 11 ⇒ union = 11/11 certified
