package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Validation of the exact-coordinate torus engine ([[KrotenheerdtTorusSearch]], ADR-0019 Option B prototype)
  * against the proven DCEL fixed-Λ engine ([[KrotenheerdtLatticeSearch]]).
  */
class KrotenheerdtTorusSearchSpec extends AnyFlatSpec with Matchers:

  private def torus(n: Int, k: Int, maxCovol: Double): List[Set[VertexSignature]] =
    KrotenheerdtTorusSearch.enumerate(n, k, maxCovol).tilings.map(_._1)

  behavior of "KrotenheerdtTorusSearch (exact-coordinate fixed-Λ engine)"

  it should "enumerate exactly the four small-cell 1-uniform tilings, incl. the multi-vertex 6.6.6" in:
    val found = torus(1, 3, 2.6)
    found.map(_.map(_.sorted)).toSet shouldBe Set(
      Set(List(3, 3, 3, 3, 3, 3)),
      Set(List(4, 4, 4, 4)),
      Set(List(3, 3, 3, 4, 4)),
      Set(List(6, 6, 6))
    )
    found.size shouldBe 4

  it should "agree with the DCEL engine on the torus keys for the small n=1 cells" in:
    val torusKeys = KrotenheerdtTorusSearch.enumerate(1, 3, 2.6).tilings.map(_._2).toSet
    val dcelKeys  = KrotenheerdtLatticeSearch
      .enumerate(1, 3, 2.6, parallelism = 4)
      .tilings
      // the DCEL engine also reaches 4.8.8 (octagon) at this covolume; the torus engine excludes it by design
      .filterNot((types, _) => types.exists(_.contains(8)))
      .map(_._2)
      .toSet
    torusKeys shouldBe dcelKeys

  it should "reproduce the per-n results in a single combined all-n pass (bucketed by n)" in:
    // One search with the ≤maxN type prune must, bucketed by n, give exactly the same keys per n as the
    // dedicated fixed-n runs — the property that lets one sweep produce the whole A068600 table.
    val (k, covol, maxN) = (4, 6.0, 2)
    val combined         = KrotenheerdtTorusSearch.enumerateCombined(maxN, k, covol, parallelism = 4)
    for n <- 1 to maxN do
      val fixedKeys    = KrotenheerdtTorusSearch.enumerate(n, k, covol, parallelism = 4).tilings.map(_._2).toSet
      val combinedKeys = combined.collect { case (m, _, key) if m == n => key }.toSet
      withClue(s"n=$n: ")(combinedKeys shouldBe fixedKeys)

  behavior of "KrotenheerdtTorusSearch.enumerateBanded (strip-stacking — ADR-0037)"

  // enumerateBanded is `enumerate` RESTRICTED to band-aligned lattices, so by construction its keys must be a
  // SUBSET of the full engine's (no new/spurious tilings — the restriction can only drop lattices) and it must
  // still REACH the band-aligned cells (the short-edge-period ones). Fast at n=1, covol 2.6.
  it should "emit only a subset of the full fixed-Λ engine's keys (sound, no spurious)" in:
    val full   = KrotenheerdtTorusSearch.enumerate(1, 3, 2.6, parallelism = 4).tilings.map(_._2).toSet
    val banded = KrotenheerdtTorusSearch.enumerateBanded(1, 3, 2.6, parallelism = 4).tilings.map(_._2).toSet
    banded should not be empty
    withClue(s"banded keys not ⊆ full engine keys (spurious!): ")(banded.subsetOf(full) shouldBe true)

  it should "reach the short-edge-period banded n=1 cells (4⁴, 3³.4², 3⁶)" in:
    val typeSets = KrotenheerdtTorusSearch.enumerateBanded(
      1,
      3,
      2.6,
      parallelism = 4
    ).tilings.map(_._1.map(_.sorted)).toSet
    typeSets should contain(Set(List(4, 4, 4, 4)))    // square grid — rows of squares, period 1
    typeSets should contain(Set(List(3, 3, 3, 4, 4))) // elongated triangular — the archetypal banded tiling
    typeSets should contain(Set(List(3, 3, 3, 3, 3, 3))) // triangular — period 1 along an edge

  it should "be monotone in maxBandLen (a longer in-band period reaches a superset)" in:
    val short = KrotenheerdtTorusSearch.enumerateBanded(
      1,
      3,
      2.6,
      maxBandLen = 1,
      parallelism = 4
    ).tilings.map(_._2).toSet
    val long  = KrotenheerdtTorusSearch.enumerateBanded(
      1,
      3,
      2.6,
      maxBandLen = 6,
      parallelism = 4
    ).tilings.map(_._2).toSet
    withClue(s"maxBandLen=1 keys not ⊆ maxBandLen=6 keys: ")(short.subsetOf(long) shouldBe true)

  behavior of "TilingReference n=3 audit correction (ReferenceAuditProbe, 2026-06-25)"

  // The oracle (sound + complete for n≤3) corrected two compensating Wikipedia transcription errors in the n=3
  // list. Lock the corrected multiplicities so the fix can't silently regress.
  private def n3sig(s: String): Set[VertexSignature] =
    s.split(';').map(v => normalize(v.trim.split('.').map(_.toInt).toList)).toSet
  it should "have the audit-corrected n=3 multiplicities (oracle is authority)" in:
    TilingReference.counts(3) shouldBe 39
    UnionDriver.multiplicity(3, n3sig("3.3.3.3.3.3; 3.3.3.4.4; 4.4.4.4")) shouldBe 4 // was 3 (corrected)
    UnionDriver.multiplicity(3, n3sig("3.3.6.6; 3.4.4.6; 3.6.3.6")) shouldBe 2 // was 3 (corrected)
