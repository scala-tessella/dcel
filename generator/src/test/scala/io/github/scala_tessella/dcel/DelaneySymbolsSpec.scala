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

  behavior of "DelaneySymbols completeness & element-for-element agreement (slow — run on demand)"

  // parse the reference's compact Wikipedia notation: "3^2.4.3.4" -> 3.3.4.3.4, "3.4^2.6" -> 3.4.4.6, etc.
  private def parseVertex(v: String): VertexSignature      =
    normalize(v.trim.split('.').toList.flatMap { tok =>
      tok.split('^') match
        case Array(b)    => List(b.toInt)
        case Array(b, e) => List.fill(e.toInt)(b.toInt)
        case _           => Nil
    })
  private def parseTiling(t: String): Set[VertexSignature] = t.split(';').map(parseVertex).toSet

  // ≈3 min at maxSize 22 (the D-set generation tree is the cost; see ADR-0022). Un-ignore to verify the engine
  // reproduces the FULL count AND the exact vertex-type sets — not just a subset.
  ignore should "agree element-for-element with the reference at n = 2 (exact) and n = 3 (no spurious)" in:
    val found            = DelaneySymbols.enumerate(maxN = 3, maxSize = 22)
    def distinct(n: Int) = found.filter(_._1 == n).map(_._2).toSet

    // n = 2: exactly the 20 two-uniform tilings, and exactly the reference's distinct vertex-type sets.
    found.count(_._1 == 2) shouldBe TilingReference.counts(2) // 20
    distinct(2) shouldBe TilingReference.n2.toSet

    // n = 3: every reference vertex-type set is found and none is spurious (the 38/39 gap is a geometric
    // duplicate sharing a type-set, not a missing/wrong configuration).
    val ref3 = TilingReference.rawWikipediaN3to5(3).map(parseTiling).toSet
    withClue("spurious n=3 type-sets: ")((distinct(3) -- ref3) shouldBe empty)
    withClue("missing n=3 type-sets: ")((ref3 -- distinct(3)) shouldBe empty)

  behavior of "DelaneySymbols.orientedRegularSymbolsParallel (parallel == sequential)"

  // Guards the parallel + instrumented oriented generator: it must return the SAME tilings as the sequential
  // one (same canonical keys, same count — no race-duplicated or dropped symbols). oriSize 24 runs in ~1-2 s.
  it should "return exactly the same tilings (keys, types, count) as the sequential generator" in:
    val seq = DelaneySymbols.orientedRegularSymbols(maxN = 7, maxSize = 24)
    val par = DelaneySymbols.orientedRegularSymbolsParallel(maxN = 7, maxSize = 24, parallelism = 4)
    par.map(_._3).toSet shouldBe seq.map(_._3).toSet // identical canonical-key set
    par.map(t => (t._1, t._2.toSet)).toSet shouldBe seq.map(t => (t._1, t._2.toSet)).toSet
    par.size shouldBe seq.size // no duplicate inflation
