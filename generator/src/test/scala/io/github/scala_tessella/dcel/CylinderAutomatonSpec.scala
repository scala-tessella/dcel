package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.KrotenheerdtTorusMapSearch.FaceZ
import io.github.scala_tessella.dcel.VertexTypes.VertexSignature
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** SPIKE validation for ADR-0038 — does the cylinder automaton's load-bearing machinery hold on the smallest
  * real cases BEFORE building the full engine?
  *
  *   - **B (cylinder soundness):** PASSES — `isConsistentCyl` accepts valid layers, rejects overlaps mod-⟨h⟩.
  *   - **C (closing ⇒ right key):** PASSES — strips grown on the circumference-2 cylinder close, via the
  *     grower's `verifyCell` + `torusMapClassify`, to the SAME D-symbol keys the oracle assigns, for ALL FOUR
  *     period-≤2 banded 1-uniform tilings (4⁴, 3³.4², 3.6.3.6, 3⁶). The grow→close→key pipeline is validated.
  *   - **A (finiteness):** the SPIKE's patch-growth front does NOT bound — completing the lowest vertex
  *     advances downward but leaves the seed's far side permanently incomplete, so the tracked front spans an
  *     unbounded y-range (`CylProbe`: 57→118→…→1244 as maxFaces rises). FINDING: the transfer-matrix state
  *     must be the advancing TOP PROFILE ([[StripBand.Profile]]), grown one-directionally with the completed
  *     region forgotten — not the whole patch. The full engine is built on profile states; see ADR-0038.
  */
class CylinderAutomatonSpec extends AnyFlatSpec with Matchers:

  import CylinderAutomaton.{enumerateAtH, isConsistentCyl}

  // grow at circumference 2 (= 2·unit period): squares/triangles (extent < 2) never wrap, and a period-1 tiling
  // appears as a period-2 strip that `primitiveBasis` reduces back to its primitive |h|=1 cell when closing.
  private val h1 = ZetaPoint(2, 0, 0, 0)

  /** Oracle D-symbol keys for the 1-uniform tilings, keyed by type-set (the authority). */
  private lazy val oracle1: Map[Set[VertexSignature], String] =
    DelaneySymbols.keyedTilings(1, 16).map((_, types, key) => (types.toSet, key)).toMap

  behavior of "CylinderAutomaton.isConsistentCyl (assumption B — mod-h soundness)"

  it should "accept a single square but reject two squares overlapping a shared wedge" in:
    val sq  = FaceZ(4, StripBand.polygon(ZetaPoint.origin, 0, 4))
    isConsistentCyl(List(sq), h1) shouldBe true
    val bad = FaceZ(4, StripBand.polygon(ZetaPoint.origin, 1, 4)) // rotated 30° ⇒ wedge collision
    isConsistentCyl(List(sq, bad), h1) shouldBe false

  behavior of "CylinderAutomaton.enumerateAtH — closing ⇒ oracle D-symbol key (assumption C)"

  // grow the circumference-2 cylinder once; the assertions below read from this single run
  private lazy val run = enumerateAtH(h1, maxN = 1, maxFaces = 24)

  it should "close ALL FOUR period-≤2 banded 1-uniform tilings to the oracle's D-symbol keys" in:
    val want = Map(
      "4⁴"      -> List(4, 4, 4, 4),
      "3³.4²"   -> List(3, 3, 3, 4, 4),
      "3.6.3.6" -> List(3, 6, 3, 6),
      "3⁶"      -> List(3, 3, 3, 3, 3, 3)
    )
    want.foreach: (name, sig) =>
      val ts = Set(VertexTypes.normalize(sig))
      withClue(s"$name not closed to its oracle key: ")(run.emitted.keySet should contain(oracle1(ts)))

  it should "emit ONLY genuine 1-uniform D-symbol keys (sound — no spurious closings)" in:
    val oracleKeys = oracle1.values.toSet
    run.emitted should not be empty
    run.emitted.keySet.foreach(k => withClue(s"spurious key $k: ")(oracleKeys should contain(k)))
