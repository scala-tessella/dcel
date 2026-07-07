package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.SymbolAssembly.*
import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Phase-2 SAT symbol assembler (ADR-0039), validated fixture-first: the ORACLE's minimal symbols are a
  * perfect ground-truth population — every one must decompose into the star model (each 12-orbit isomorphic
  * to an enumerated folding of its type) and SATISFY every σ₀ constraint the SAT instance encodes, BEFORE any
  * solver enumeration is trusted. Then gate G1: the solver reproduces n = 1 key-for-key vs the oracle.
  */
class SymbolAssemblySpec extends AnyFlatSpec with Matchers:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  behavior of "the star model (hand-computed leaves)"

  it should "build the unfolded star of 3.6.3.6" in:
    val st = unfoldedStar(sig("3.6.3.6"))
    st.size shouldBe 8
    st.degree shouldBe 4
    st.m01 shouldBe Vector(3, 3, 6, 6, 3, 3, 6, 6)
    st.s1(0) shouldBe 1
    st.s2(1) shouldBe 2
    st.s2(0) shouldBe 7

  it should "find the figure symmetry groups (hand-counted orders)" in:
    starSymmetries(sig("3.3.3.3.3.3")).size shouldBe 12 // D₆
    starSymmetries(sig("4.4.4.4")).size shouldBe 8      // D₄
    starSymmetries(sig("3.3.3.3.6")).size shouldBe 2    // one mirror through the hexagon
    starSymmetries(sig("3.3.4.12")).size shouldBe 1     // fully asymmetric
    starSymmetries(sig("3.6.3.6")).size shouldBe 4      // C₂ rotation + the two corner-pair mirrors
    starSymmetries(sig("4.6.12")).size shouldBe 1 // chiral: three distinct polygons, no mirror

  it should "enumerate foldings: the asymmetric type has exactly its unfolded star" in:
    val f = starFoldings(sig("3.3.4.12"))
    f should have size 1
    f.head.size shouldBe 8

  it should "enumerate foldings: the snub figure has the unfolded star and one mirror chain" in:
    val f = starFoldings(sig("3.3.3.3.6"))
    f.map(_.size).sorted shouldBe Vector(5, 10)

  it should "include the fully-folded 1-chamber star for 4⁴ (both σ fixed)" in:
    val f = starFoldings(sig("4.4.4.4"))
    f.exists(st => st.size == 1 && st.s1(0) == 0 && st.s2(0) == 0) shouldBe true

  // Regression (the G2 19/20 bug): the two mirror 4-chains of 4⁴ — ends fixed by σ₁ (mirror through the
  // faces) vs by σ₂ (mirror along the edges) — are DIFFERENT stars; an op-blind canonical key conflated
  // them and dropped the folding the size-9 {3³.4²;4⁴} sibling needs.
  it should "distinguish the two mirror 4-chain foldings of 4⁴, and keep the rotation 4-cycle" in:
    val f4                    = starFoldings(sig("4.4.4.4")).filter(_.size == 4)
    def fixes(v: Vector[Int]) = v.indices.exists(c => v(c) == c)
    f4.exists(st => fixes(st.s1) && !fixes(st.s2)) shouldBe true  // σ₁-ends chain
    f4.exists(st => fixes(st.s2) && !fixes(st.s1)) shouldBe true  // σ₂-ends chain
    f4.exists(st => !fixes(st.s1) && !fixes(st.s2)) shouldBe true // C₂-rotation 4-cycle
    f4.map(canonicalStarKey).distinct.size shouldBe f4.size

  it should "satisfy the folding invariants for every viable type: r₁₂ divides the degree" in:
    for
      t  <- TypeCompatibility.viableFigures
      st <- starFoldings(t)
    do
      withClue(s"$t folding size ${st.size}: "):
        // walk the σ₁σ₂ rotation to find r₁₂ (order of σ₁∘σ₂ on the orbit)
        var cur = 0
        var r   = 0
        var go  = true
        while go do
          cur = st.s2(st.s1(cur))
          r += 1
          if cur == 0 then go = false
          if r > 2 * st.size then fail("no closure")
        st.degree % r shouldBe 0
        st.size should be <= 2 * st.degree

  behavior of "σ₀ predicates (hand-computed)"

  it should "accept the hand-built 4⁴ minimal symbol (1 chamber, all σ self)" in:
    // arrays 1-based; index 0 unused
    sigma0Valid(Vector(0, 1), Vector(0, 1), Vector(0, 1), Vector(0, 4)) shouldBe true

  it should "reject a σ₀ crossing different face sizes" in:
    // two chambers with m01 3 and 4 matched by σ₀
    sigma0Valid(Vector(0, 2, 1), Vector(0, 1, 2), Vector(0, 2, 1), Vector(0, 3, 4)) shouldBe false

  behavior of "THE ORACLE FIXTURE (encoding-completeness before any solver run)"

  // Every minimal symbol the fast oracle reaches (n=1 complete + partial n≥2) must (a) decompose into stars
  // that the folding enumerator generates for its types, and (b) satisfy every σ₀ constraint the SAT
  // instance encodes. A failure here means the encoding would MISS a real tiling — the one unacceptable bug.
  private lazy val oracle = DelaneySymbols.enumerateSymbols(maxN = 7, maxSize = 12)

  it should "decompose every oracle symbol's 12-orbits into enumerated foldings" in:
    oracle should not be empty
    for (n, sigs, ds) <- oracle do
      val stars = inducedStars(ds)
      stars.size shouldBe n
      for (st, idx) <- stars.zipWithIndex do
        withClue(s"types=$sigs orbit=$idx: "):
          val expected = starFoldings(sigs(idx)).map(canonicalStarKey).toSet
          expected should contain(canonicalStarKey(st))

  it should "satisfy every σ₀ constraint on every oracle symbol" in:
    for (_, sigs, ds) <- oracle do
      val m  = ds.size
      val s0 = Vector.tabulate(m + 1)(d => if d == 0 then 0 else ds.get(0, d))
      val s1 = Vector.tabulate(m + 1)(d => if d == 0 then 0 else ds.get(1, d))
      val s2 = Vector.tabulate(m + 1)(d => if d == 0 then 0 else ds.get(2, d))
      val mm = Vector.tabulate(m + 1)(d => if d == 0 then 0 else ds.m(0, 1, d))
      withClue(s"types=$sigs: ")(sigma0Valid(s0, s1, s2, mm) shouldBe true)

  behavior of "gate G1 (n = 1): the solver vs the oracle, key-for-key"

  it should "reproduce exactly the 11 Archimedean keys from the 11 fair candidates" in:
    val oracleKeys = DelaneySymbols.keyedTilings(maxN = 1, maxSize = 12).map(_._3).toSet
    oracleKeys should have size 11
    val results    = enumerate(1)
    results.values.exists(_.capped) shouldBe false
    val solvedKeys = results.values.flatMap(_.keys).toSet
    solvedKeys shouldBe oracleKeys
    // and per-set: each singleton candidate yields exactly its own tiling's key
    for (ts, r) <- results do withClue(s"$ts: ")(r.keys should have size 1)

  behavior of "gate G2 (n = 2): the solver vs A068600(2) = 20"

  it should "find exactly the 20 2-uniform tilings across the 25 fair candidates, multiplicities included" in:
    val results = enumerate(2)
    results.values.exists(_.capped) shouldBe false
    // a tiling's type-set is intrinsic, so per-set key counts are disjoint: the total is the A068600 count
    results.values.map(_.keys.size).sum shouldBe 20
    // the reference lists each realized type-set once PER TILING (multiplicity) — must match exactly
    val found   = results.toList.flatMap((ts, r) => List.fill(r.keys.size)(ts))
    found should contain theSameElementsAs TilingReference.n2

  behavior of "gate G3 (n = 3): the solver vs A068600(3) = 39"

  it should "find exactly the 39 3-uniform tilings across the 95 fair candidates, multiplicities included" in:
    val results = enumerate(3)
    results.values.exists(_.capped) shouldBe false
    results.values.map(_.keys.size).sum shouldBe 39
    val found   = results.toList.flatMap((ts, r) => List.fill(r.keys.size)(ts))
    val ref     = TilingReference.rawWikipediaN3to5(3).map(N4DriverProbe.parseRow)
    found should contain theSameElementsAs ref
