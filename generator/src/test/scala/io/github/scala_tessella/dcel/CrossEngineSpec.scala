package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Phase-1 CROSS-ENGINE correctness audit (the generalized chirality regression). The three surviving sound
  * engines all dedup in the SAME canonical-key space ([[DelaneySymbols.minimalSymbol]] / `canonicalKey`):
  *
  *   - the generate-all D-symbol oracle (`keyedTilings`, exact and complete through n ≤ 3 but cheap only to n =
  *     2 at a small chamber budget),
  *   - the oriented-slice generator (`orientedRegularSymbols`, ADR-0023), and
  *   - the bounded-V dart assembler (`BucketAssembly.enumerateBucket`, ADR-0025/0030).
  *
  * So on any tiling two of them both reach, their canonical keys must be IDENTICAL. The 2026-06-21 chirality
  * bug ([[BucketAssembly]] assembled 0 tori for the oriented-chiral types `4.6.12` / `3.4.4.6` / `3.3.4.12`)
  * was a COMPLETENESS failure that a per-engine "soundness only" check could not catch, and it hid because
  * the spec covered only 6 of the 11 Archimedean. This spec is the antidote the feedback called for: assert
  * three-way key-for-key AGREEMENT across the full chirality axis — the n = 1 Archimedean (which include the
  * chiral `4.6.12`) and the n = 2 type-sets carrying a chiral vertex type (`3.4.4.6` / `3.3.4.12` /
  * `3.4.3.12`) — so any engine silently dropping a chiral tiling fails loudly here.
  *
  * All numbers below are MEASURED (`CrossEngineMeasure`), not guessed, so the spec is fast and deterministic.
  */
class CrossEngineSpec extends AnyFlatSpec with Matchers:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  // ---- shared canonical-key views of each engine (lazy: computed once, reused across assertions) ----

  // generate-all oracle: complete at n = 1 (maxSize 12), and reaches every chiral n = 2 type-set at maxSize 14
  private lazy val oracle: List[(Int, Set[VertexSignature], String)]       = DelaneySymbols.keyedTilings(2, 14)
  private def oracleKeys(n: Int, types: Set[VertexSignature]): Set[String] =
    oracle.filter(r => r._1 == n && r._2 == types).map(_._3).toSet

  // oriented-slice: reaches all 11 (n=1) and every chiral n=2 type-set at oriSize 26
  private lazy val oriented: List[(Int, List[VertexSignature], String)]      =
    DelaneySymbols.orientedRegularSymbols(maxN = 2, maxSize = 26)
  private def orientedKeys(n: Int, types: Set[VertexSignature]): Set[String] =
    oriented.filter(r => r._1 == n && r._2.toSet == types).map(_._3).toSet

  // the n = 2 type-sets containing an oriented-CHIRAL vertex type (the axis the bug lived on). Each pairs the
  // set with the bounded-V cell size where its tilings first close (`None` ⇒ cell V > 8, out of fast reach),
  // and the known multiplicity (count of distinct tilings sharing the type-set).
  private val chiralN2: List[(Set[VertexSignature], Option[Int], Int)] = List(
    (Set("3.4.4.6", "3.6.3.6").map(sig), Some(5), 2),
    (Set("3.12.12", "3.4.3.12").map(sig), Some(8), 1),
    (Set("3.4.6.4", "3.4.4.6").map(sig), None, 1),
    (Set("3.3.3.3.3.3", "3.3.4.12").map(sig), None, 1)
  )

  behavior of "cross-engine agreement — n = 1 (the 11 Archimedean, including the chiral 4.6.12)"

  it should "have the generate-all oracle and the oriented-slice generator produce the IDENTICAL 11 keys" in:
    val oracleN1   = oracle.filter(_._1 == 1).map(_._3).toSet
    val orientedN1 = oriented.filter(_._1 == 1).map(_._3).toSet
    oracleN1.size shouldBe 11
    orientedN1 shouldBe oracleN1 // key-for-key — a dropped chiral 4.6.12 would break this

  behavior of "cross-engine agreement — chiral n = 2 type-sets (the generalized chirality regression)"

  // The oracle and the oriented-slice generator BOTH reach every chiral n=2 type-set (different routes:
  // hyperbolic-universe D-set generation vs the oriented rotation slice). They must agree key-for-key.
  for (types, _, mult) <- chiralN2 do
    it should s"have oracle and oriented-slice agree key-for-key on $types (multiplicity $mult)" in:
      val o = oracleKeys(2, types)
      val r = orientedKeys(2, types)
      withClue(s"oracle=$o oriented=$r: ")(o shouldBe r)
      o.size shouldBe mult

  // For the chiral sets whose cell fits the fast V window, the INDEPENDENT bounded-V assembler must produce the
  // SAME keys — the strongest check, since neither the oriented slice nor bounded-V is the generate-all oracle
  // (two scalable engines agreeing on a chiral multi-type tiling, key-for-key).
  for case (types, Some(v), mult) <- chiralN2 do
    it should s"have bounded-V assembly reproduce $types at V=$v, keyed identically to the oracle" in:
      val r = BucketAssembly.enumerateBucket(types, maxV = v, stateBudget = 30_000_000L)
      r.budgetHit shouldBe false
      r.keys.size shouldBe mult
      r.keys shouldBe oracleKeys(2, types)

  behavior of "cross-engine soundness — no engine emits a key the oracle rejects at n = 1"

  it should "have every oriented-slice n = 1 key present in the complete n = 1 oracle (no spurious)" in:
    val oracleN1   = oracle.filter(_._1 == 1).map(_._3).toSet
    val orientedN1 = oriented.filter(_._1 == 1).map(_._3).toSet
    orientedN1.subsetOf(oracleN1) shouldBe true
