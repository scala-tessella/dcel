package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TypeCompatibility.*
import io.github.scala_tessella.dcel.VertexTypes
import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The fair top-down type-set derivation (ADR-0040 = ADR-0039 Phase 1), validated ground-up: hand-computed
  * leaves first, then each stage against its DOCUMENTED expectation (Kepler/Sommerville's 21, Sommerville's
  * six impossible figures, Krotenheerdt's 4.8²-isolation and the Archimedean 11), then the load-bearing
  * no-false-negative sweep: every reference type-set for n ≤ 5 must be a candidate (the filters are
  * necessary-only, so a miss would be a soundness bug, not an accuracy gap).
  */
class TypeCompatibilitySpec extends AnyFlatSpec with Matchers:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  behavior of "the leaves (hand-computed)"

  it should "extract adjacent pairs of a figure" in:
    adjacentPairs(sig("3.6.3.6")) shouldBe Set((3, 6))
    adjacentPairs(sig("4.4.4.4")) shouldBe Set((4, 4))
    adjacentPairs(sig("3.3.6.6")) shouldBe Set((3, 3), (3, 6), (6, 6))

  it should "extract the neighbor pair of each polygon occurrence" in:
    // (3,3,6,6): both 3s are flanked by {3,6}... no — first 3 by {6,3}, second by {3,6}: same unordered pair
    occurrenceNeighborPairs(sig("3.3.6.6"), 3) shouldBe Set((3, 6))
    occurrenceNeighborPairs(sig("3.3.6.6"), 6) shouldBe Set((3, 6))
    // (3,6,3,6): each 3 flanked by two 6s, each 6 by two 3s
    occurrenceNeighborPairs(sig("3.6.3.6"), 3) shouldBe Set((6, 6))
    occurrenceNeighborPairs(sig("3.6.3.6"), 6) shouldBe Set((3, 3))
    // (3,3,4,12): the two 3s differ — {12,3} and {3,4}
    occurrenceNeighborPairs(sig("3.3.4.12"), 3) shouldBe Set((3, 12), (3, 4))
    occurrenceNeighborPairs(sig("3.3.4.12"), 5) shouldBe empty

  it should "decide corona satisfiability on hand-verified cases" in:
    // pure 3².6²: the triangle's 3 neighbor tiles must pairwise mix {3,6} — a 2-colouring of an odd cycle
    coronaSatisfiable(3, Set(sig("3.3.6.6"))) shouldBe false
    // ...but its hexagon corona alternates 3,6 around an EVEN cycle — satisfiable (the refutation is the 3)
    coronaSatisfiable(6, Set(sig("3.3.6.6"))) shouldBe true
    // pure 3⁶: all-triangle corona
    coronaSatisfiable(3, Set(sig("3.3.3.3.3.3"))) shouldBe true
    // pure 5².10: the pentagon corona alternates 5,10 around an odd cycle
    coronaSatisfiable(5, Set(sig("5.5.10"))) shouldBe false
    // pure 3.4².6: the triangle corona alternates 4,6 around an odd cycle
    coronaSatisfiable(3, Set(sig("3.4.4.6"))) shouldBe false
    // pinning: corner 1 forced to an unsupported pair is unsatisfiable even when the free corona is fine
    coronaSatisfiable(3, Set(sig("3.3.3.3.3.3")), pinned = Some((3, 6))) shouldBe false

  behavior of "stage A — the arithmetic figures (no alphabet assumption)"

  it should "derive exactly the 21 figures from the angle equation alone" in:
    arithmeticFigures should have size 21

  it should "include the exotic solutions a hardcoded alphabet would exclude, up to the 42-gon" in:
    arithmeticFigures should contain allOf
      (
        sig("3.7.42"),
        sig("3.8.24"),
        sig("3.9.18"),
        sig("3.10.15"),
        sig(
          "4.5.20"
        ),
        sig("5.5.10")
      )
    arithmeticFigures.flatten.max shouldBe 42

  it should "distinguish cyclic arrangements of the same multiset" in:
    arithmeticFigures should contain allOf (sig("3.3.4.12"), sig("3.4.3.12"))
    arithmeticFigures should contain allOf (sig("3.3.6.6"), sig("3.6.3.6"))
    arithmeticFigures should contain allOf (sig("3.3.3.4.4"), sig("3.3.4.3.4"))
    arithmeticFigures should contain allOf (sig("3.4.4.6"), sig("3.4.6.4"))

  behavior of "stage B — tiling-viable figures (Sommerville's elimination, mechanized)"

  it should "kill exactly the six documented impossible figures, leaving 15" in:
    viableFigures should have size 15
    (arithmeticFigures -- viableFigures) shouldBe Set(
      sig("3.7.42"),
      sig("3.8.24"),
      sig("3.9.18"),
      sig("3.10.15"),
      sig("4.5.20"),
      sig("5.5.10")
    )

  it should "DERIVE the polygon alphabet {3,4,6,8,12} that VertexTypes hardcodes" in:
    derivedPolygonAlphabet shouldBe VertexTypes.polygonSides.toSet

  it should "agree with VertexTypes.validSignatures figure-for-figure" in:
    viableFigures shouldBe VertexTypes.validSignatures

  behavior of "stage C — candidate type-sets"

  it should "derive candidates(1) = exactly the 11 Archimedean types (the four mix-only figures refuted)" in:
    candidates(1) shouldBe TilingReference.n1.map(Set(_))
    // the four documented mix-only figures fail alone (triangle-corona parity), and are viable in mixes
    val mixOnly = Set(sig("3.3.4.12"), sig("3.3.6.6"), sig("3.4.3.12"), sig("3.4.4.6"))
    mixOnly.foreach(f => withClue(s"$f: ")(isCandidate(Set(f)) shouldBe false))
    mixOnly.subsetOf(viableFigures) shouldBe true

  it should "derive Krotenheerdt's 4.8²-isolation: no mixed candidate ever contains it" in:
    // among the 15 survivors only 4.8² contains an 8 ⇒ its vertices only neighbour its own kind ⇒ singleton
    viableFigures.count(_.contains(8)) shouldBe 1
    for n <- 2 to 4 do withClue(s"n=$n: ")(candidates(n).exists(_.contains(sig("4.8.8"))) shouldBe false)
    // ⇒ the m ≤ 14 ceiling: the only 15-set (everything) is not a candidate
    isCandidate(viableFigures) shouldBe false

  // The load-bearing soundness direction: the filters are NECESSARY-only, so every reference type-set must
  // pass. n=2 from the tested reference pairs; n=3..5 from the (audited) Wikipedia rows.
  it should "never prune a realizable type-set: all reference sets for n = 2..5 are candidates" in:
    val byN: Map[Int, Set[Set[VertexSignature]]] =
      Map(2 -> TilingReference.n2.toSet) ++
        (3 to 5).map(n => n -> TilingReference.rawWikipediaN3to5(n).map(N4DriverProbe.parseRow).toSet).toMap
    for (n, sets) <- byN do
      val cands = candidates(n)
      sets.foreach: ts =>
        withClue(s"n=$n $ts: "):
          ts.size shouldBe n
          cands should contain(ts)

  // Characterization (pinned 2026-07-07): the measured candidate inventory — the over-generation Phase 2
  // inherits (realized, for comparison: 11 / 15 / 36 / 21 / 12 DISTINCT type-sets at n = 1..5). Candidates
  // at n = 8 are NON-empty by design: the A068600 n=8 zero is the types-=-orbits rigidity, not type-set
  // incompatibility (ADR-0039 Phase 0 correction) — refuting these is Phase-2 work.
  it should "pin the candidate inventory sizes (the measured over-generation)" in:
    (1 to 8).map(n => n -> candidates(n).size).toMap shouldBe Map(
      1 -> 11,
      2 -> 25,
      3 -> 95,
      4 -> 289,
      5 -> 686,
      6 -> 1224,
      7 -> 1624,
      8 -> 1617
    )
