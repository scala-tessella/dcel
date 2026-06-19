package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.DelaneySymbols.Frac
import io.github.scala_tessella.dcel.VertexTypes.{
  VertexSignature, isCompleteVertex, normalize, validSignatures
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Validation of the Delaney–Dress symbol enumerator ([[DelaneySymbols]], ADR-0022) — the intrinsic,
  * coordinate-free combinatorial-map route to the Krotenheerdt tilings (OEIS A068600).
  *
  * The headline correctness fact is n = 1: it reproduces ALL 11 Archimedean tilings (including `4.8.8`, which
  * the ζ[ζ₁₂] engines cannot represent), exactly — no spurious cells (the 3.3.6.6 / 3.4.4.6 false-period
  * overlaps that plagued the geometric engines cannot even be constructed here). Completeness for n ≥ 2 needs
  * a larger `maxSize` than is tractable without the euclidean-pruning optimization (future work), so those
  * are exercised as soundness (subset + validity) rather than exact counts.
  */
class DelaneySymbolsSpec extends AnyFlatSpec with Matchers:

  // a chamber budget that reaches all of n = 1 quickly; n = 2 is partial at this size
  private val maxSize = 12

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  behavior of "DelaneySymbols.Frac (exact curvature rationals)"

  it should "normalize, add, subtract and report sign/zero" in:
    Frac.make(2, 4) shouldBe Frac(1, 2)
    Frac.make(6, 3) shouldBe Frac(2, 1)
    Frac.make(1, -2) shouldBe Frac(-1, 2)
    (Frac.make(1, 2) - Frac.make(1, 2)).isZero shouldBe true
    (Frac.make(1, 3) + Frac.make(1, 6)).isZero shouldBe false
    (Frac.make(1, 3) + Frac.make(1, 6)) shouldBe Frac(1, 2)
    (Frac.make(-1, 2) + Frac.make(1, 3)).signum shouldBe -1
    (Frac.make(1, 2) - Frac.make(1, 3)).signum shouldBe 1
    Frac.make(0, 5).isZero shouldBe true

  behavior of "DelaneySymbols.enumerate (n = 1 — the 11 Archimedean tilings)"

  private lazy val n1 = DelaneySymbols.enumerateDetailed(maxN = 1, maxSize = maxSize)

  it should "reproduce exactly the 11 Archimedean vertex configurations (A068600(1))" in:
    n1.size shouldBe 11 // exactly 11 — minimal-symbol dedup, no inflation
    n1.flatMap(_.vertices).toSet shouldBe TilingReference.n1

  it should "include 4.8.8 — the octagon tiling the ζ[ζ₁₂] engines cannot represent" in:
    n1.flatMap(_.vertices) should contain(sig("4.8.8"))

  it should "be sound: never the angle-valid-but-non-tiling types 3.3.6.6 / 3.4.4.6 at n = 1" in:
    val types = n1.flatMap(_.vertices).toSet
    types should not contain sig("3.3.6.6")
    types should not contain sig("3.4.4.6")

  it should "carry exactly one vertex orbit of one valid type per n = 1 tiling" in:
    all(n1.map(_.n)) shouldBe 1
    all(n1.map(_.vertices.size)) shouldBe 1
    all(n1.map(_.vertices.head.size)) should be >= 3 // a vertex has ≥ 3 polygons
    n1.foreach(t => withClue(s"$t: ")(isCompleteVertex(t.vertices.head) shouldBe true))

  behavior of "DelaneySymbols soundness (every result is a genuine Krotenheerdt tiling)"

  private lazy val all2 = DelaneySymbols.enumerateDetailed(maxN = 2, maxSize = maxSize)

  it should "satisfy the A068600 condition: n = vertex-orbit count = distinct-type count" in:
    all2.foreach: t =>
      t.vertices.size shouldBe t.n
      t.vertices.toSet.size shouldBe t.n

  it should "use only valid 360° vertex types from the {3,4,6,8,12} alphabet" in:
    all2.flatMap(_.vertices).foreach: v =>
      withClue(s"$v: ")(validSignatures.contains(v) shouldBe true)

  it should "develop on a non-empty minimal symbol" in:
    all(all2.map(_.chambers)) should be > 0

  it should "never exceed the A068600 counts at any maxSize" in:
    val byN = all2.groupBy(_.n).view.mapValues(_.size).toMap
    byN.foreach((n, c) => withClue(s"n=$n: ")(c should be <= TilingReference.counts(n)))

  behavior of "DelaneySymbols monotonicity & n = 2 partial agreement"

  it should "find no fewer tilings as the chamber budget grows" in:
    val small = DelaneySymbols.enumerate(2, 10).groupBy(_._1).view.mapValues(_.size).toMap
    val large = DelaneySymbols.enumerate(2, maxSize).groupBy(_._1).view.mapValues(_.size).toMap
    for n <- small.keys do withClue(s"n=$n: ")(large.getOrElse(n, 0) should be >= small(n))

  it should "produce only genuine 2-uniform vertex-type pairs (subset of the known 20)" in:
    val referenceTypeSets = TilingReference.n2.toSet
    val foundTypeSets     = all2.filter(_.n == 2).map(_.types).toSet
    withClue(s"found $foundTypeSets not all in reference: ")(
      foundTypeSets.subsetOf(referenceTypeSets) shouldBe true
    )
    // and it has genuinely reached into the 2-uniform tilings (not vacuous)
    foundTypeSets should contain(Set(sig("3.3.3.4.4"), sig("4.4.4.4")))

  behavior of "DelaneySymbols completeness (slow — run on demand)"

  // ≈3 min at maxSize 22 (the D-set generation tree is the cost; see ADR-0022). Un-ignore to verify that the
  // engine reproduces the FULL count, not just a subset — the decisive A068600(2) = 20 check.
  ignore should "enumerate exactly the 20 two-uniform tilings (A068600(2)) at maxSize 22" in:
    val twos = DelaneySymbols.enumerate(2, 22).count(_._1 == 2)
    twos shouldBe TilingReference.counts(2) // 20
