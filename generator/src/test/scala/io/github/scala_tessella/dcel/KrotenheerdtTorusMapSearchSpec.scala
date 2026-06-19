package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Validation of the direct torus-map engine ([[KrotenheerdtTorusMapSearch]], ADR-0021) along the validation
  * ladder: each rung gates the next. Rung 1 here validates the reused "verify-a-developed-cell" tail
  * ([[KrotenheerdtTorusMapSearch.verifyCell]]) on the two single-face cells, against the fixed-Λ engine's
  * keys.
  */
class KrotenheerdtTorusMapSearchSpec extends AnyFlatSpec with Matchers:

  /** The fixed-Λ engine's canonical key for the 1-uniform tiling whose single vertex type is `sig`. */
  private def fixedKeyOf(sig: VertexSignature): String =
    KrotenheerdtTorusSearch
      .enumerate(1, 3, 2.6)
      .tilings
      .collectFirst { case (types, key) if types == Set(VertexTypes.normalize(sig)) => key }
      .getOrElse(fail(s"fixed-Λ engine produced no 1-uniform tiling of type $sig"))

  behavior of "KrotenheerdtTorusMapSearch.verifyCell (rung 1 — single-face cells)"

  it should "verify the single-square cell 4.4.4.4 with the fixed-Λ key" in:
    val (faces, g1, g2) = KrotenheerdtTorusMapSearch.squareCell
    val result          = KrotenheerdtTorusMapSearch.verifyCell(faces, g1, g2, maxN = 2)
    result match
      case Some((n, types, key, _)) =>
        n shouldBe 1
        types shouldBe Set(List(4, 4, 4, 4))
        key shouldBe fixedKeyOf(List(4, 4, 4, 4))
      case None                     => fail("verifyCell rejected the single-square cell")

  it should "verify the single-hexagon cell 6.6.6 (two torus vertices) with the fixed-Λ key" in:
    val (faces, g1, g2) = KrotenheerdtTorusMapSearch.hexagonCell
    val result          = KrotenheerdtTorusMapSearch.verifyCell(faces, g1, g2, maxN = 2)
    result match
      case Some((n, types, key, _)) =>
        n shouldBe 1
        types shouldBe Set(List(6, 6, 6))
        key shouldBe fixedKeyOf(List(6, 6, 6))
      case None                     => fail("verifyCell rejected the single-hexagon cell")

  behavior of "KrotenheerdtTorusMapSearch.verifyCell (rung 2 — multi-face cells)"

  it should "verify the 2-triangle rhombus cell 3⁶ with the fixed-Λ key" in:
    val (faces, g1, g2) = KrotenheerdtTorusMapSearch.triangleCell
    val result          = KrotenheerdtTorusMapSearch.verifyCell(faces, g1, g2, maxN = 2)
    result match
      case Some((n, types, key, _)) =>
        n shouldBe 1
        types shouldBe Set(List(3, 3, 3, 3, 3, 3))
        key shouldBe fixedKeyOf(List(3, 3, 3, 3, 3, 3))
      case None                     => fail("verifyCell rejected the 2-triangle rhombus cell")

  behavior of "KrotenheerdtTorusMapSearch.enumerate (rung 3 — cross-check vs the fixed-Λ engine)"

  it should "reproduce the four small-cell 1-uniform tilings (types)" in:
    val found = KrotenheerdtTorusMapSearch.enumerate(maxN = 1, maxFaces = 24, maxCovolume = 2.6)
    found.map((_, types, _) => types.map(_.sorted)).toSet shouldBe Set(
      Set(List(3, 3, 3, 3, 3, 3)),
      Set(List(4, 4, 4, 4)),
      Set(List(3, 3, 3, 4, 4)),
      Set(List(6, 6, 6))
    )

  it should "agree key-for-key with the fixed-Λ engine on the small n=1 cells" in:
    val mapKeys   = KrotenheerdtTorusMapSearch.enumerate(1, 24, 2.6).map((_, _, key) => key).toSet
    val fixedKeys = KrotenheerdtTorusSearch.enumerate(1, 3, 2.6).tilings.map(_._2).toSet
    mapKeys shouldBe fixedKeys

  behavior of "TilingReference (A068600 ground truth)"

  it should "have the authoritative Krotenheerdt counts and consistent n=1/n=2 data" in:
    TilingReference.counts shouldBe Map(1 -> 11, 2 -> 20, 3 -> 39, 4 -> 33, 5 -> 15, 6 -> 10, 7 -> 7)
    TilingReference.n1.size shouldBe 11
    TilingReference.n1NoOctagon.size shouldBe 10
    TilingReference.n2.size shouldBe 20
    TilingReference.vertexTypes.size shouldBe 14
    // every n=2 tiling is a pair of valid vertex types drawn from the alphabet
    all(TilingReference.n2.map(_.size)) shouldBe 2
    TilingReference.n2.flatten.toSet.subsetOf(TilingReference.vertexTypes) shouldBe true
    // the documented Wikipedia appendix has the right number of rows per n (matching A068600)
    TilingReference.rawWikipediaN3to5.view.mapValues(_.size).toMap shouldBe Map(3 -> 39, 4 -> 33, 5 -> 15)

  behavior of "KrotenheerdtTorusMapSearch.enumerate (soundness — no spurious tilings)"

  it should "produce only genuine Archimedean tilings at n=1 (no 3.3.6.6 / 3.4.4.6)" in:
    // The overlap / false-period check (ADR-0021) must reject the angle-valid-but-non-tiling vertex types.
    // Soundness only (a subset of the 10): completeness of the map search at high covolume is a separate
    // matter (it misses the most complex cell 4.6.12), handled by the fixed-Λ engines / the gluing engine.
    val found = KrotenheerdtTorusMapSearch.enumerate(maxN = 1, maxFaces = 18, maxCovolume = 4.0)
    val types = found.flatMap((_, t, _) => t).toSet
    withClue(s"found types $types: ")(types.subsetOf(TilingReference.n1NoOctagon) shouldBe true)
    types should contain(VertexTypes.normalize(List(3, 6, 3, 6))) // a genuine high-covol cell IS reached
