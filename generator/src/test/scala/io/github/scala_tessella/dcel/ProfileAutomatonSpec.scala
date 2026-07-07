package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.ProfileAutomaton.{PV, Profile, fillLowest}
import io.github.scala_tessella.dcel.VertexTypes.normalize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Test-first build of the profile-state engine ([[ProfileAutomaton]], ADR-0038). Rung 1: the fill-vertex
  * SURGERY is correct on the square row — filling a flat profile vertex places two squares whose shared upper
  * corner becomes a new profile vertex with the right below-fan, and the right neighbour absorbs a square.
  */
class ProfileAutomatonSpec extends AnyFlatSpec with Matchers:

  private val c4 = ZetaPoint(4, 0, 0, 0) // circumference 4 (no wrap for unit squares/triangles)

  /** A square-row profile vertex at `(x, 0)`: two squares below (slots 6..11), open arc up (0..5). */
  private def sqv(x: Int) = PV(ZetaPoint(x, 0, 0, 0), List((6, 4), (9, 4)))
  private val sqRow       = Profile(c4, Vector(sqv(0), sqv(1), sqv(2), sqv(3)))

  behavior of "ProfileAutomaton.fillLowest (Rung 1 — surgery on the square row)"

  it should "offer the all-square completion (type 4.4.4.4) among the fills of the lowest vertex" in:
    fillLowest(sqRow).map(_._2) should contain(normalize(List(4, 4, 4, 4)))

  it should "create the new top vertex (0,1) with two squares below it (slots 6..11)" in:
    val (prof, _, _) =
      fillLowest(sqRow).find(_._2 == normalize(List(4, 4, 4, 4))).getOrElse(fail("no square fill"))
    val top          = prof.verts.find(v => v.pos == ZetaPoint(0, 0, 0, 1)).getOrElse(fail("no (0,1) vertex"))
    top.covered shouldBe Set(6, 7, 8, 9, 10, 11)

  it should "grow the right neighbour (1,0)'s fan by one square (now covering slots 3..11, open 0..2)" in:
    val (prof, _, _) = fillLowest(sqRow).find(_._2 == normalize(List(4, 4, 4, 4))).get
    val r            = prof.verts.find(v => v.pos == ZetaPoint(1, 0, 0, 0)).getOrElse(fail("no (1,0) vertex"))
    r.covered shouldBe Set(3, 4, 5, 6, 7, 8, 9, 10, 11)

  it should "place exactly two unit squares for the square fill" in:
    val (_, _, faces) = fillLowest(sqRow).find(_._2 == normalize(List(4, 4, 4, 4))).get
    faces.map(_.size) shouldBe List(4, 4)

  behavior of "ProfileAutomaton.enumerateFrom (Rung 2 — cycle detection + closing)"

  /** Oracle D-symbol keys for the 1-uniform tilings, keyed by type-set. */
  private lazy val oracle1: Map[Set[List[Int]], String] =
    DelaneySymbols.keyedTilings(1, 16).map((_, types, key) => (types.toSet, key)).toMap

  it should "close the square grid 4⁴ from the square-row profile (cycle ⇒ verifyCell ⇒ oracle key)" in:
    val emitted = ProfileAutomaton.enumerateFrom(sqRow, maxN = 1)
    emitted.keySet should contain(oracle1(Set(normalize(List(4, 4, 4, 4)))))

  // a triangle-row profile: three triangles below each vertex (slots 6..11), open arc up
  private def trv(x: Int) = PV(ZetaPoint(x, 0, 0, 0), List((6, 3), (8, 3), (10, 3)))
  private val triRow      = Profile(c4, Vector(trv(0), trv(1), trv(2), trv(3)))

  it should "close the triangular 3⁶ from the triangle-row profile" in:
    ProfileAutomaton.enumerateFrom(triRow, maxN = 1).keySet should
      contain(oracle1(Set(normalize(List(3, 3, 3, 3, 3, 3)))))

  it should "discover elongated-triangular 3³.4² from the SAME square-row seed (triangle-fill branch)" in:
    // proves the engine isn't hand-fed one path: from square-below, the [3,3,3] completion makes 3.3.3.4.4
    // vertices, and that branch closes to a different oracle tiling
    val emitted = ProfileAutomaton.enumerateFrom(sqRow, maxN = 1, maxSteps = 90)
    emitted.keySet should contain(oracle1(Set(normalize(List(3, 3, 3, 4, 4)))))

  behavior of "ProfileAutomaton.enumerate (Rung 4 — automatic seeds from the StripBand catalogue)"

  it should "reach the integer-period banded 1-uniform tilings at c=4 with NO hand-built seeds" in:
    // c=4 (integer) reaches the period-1 banded tilings; 3.6.3.6 / 6.6.6 have period √3 and need c=2√3 (a later
    // non-integer leg of the sweep). The n=3 GATE cells all have integer |h|∈{1,2}, so integer c covers them.
    val emitted = ProfileAutomaton.enumerate(cInt = 4, maxN = 1)
    val want    = List(List(4, 4, 4, 4), List(3, 3, 3, 3, 3, 3), List(3, 3, 3, 4, 4))
    want.foreach: sig =>
      withClue(s"${sig.mkString(".")} not reached: ")(
        emitted.keySet should contain(oracle1(Set(normalize(sig))))
      )

  behavior of
    "ProfileAutomaton maxBand (multi-row expansion — the affordable-band measurement rests on these)"

  private val sq       = normalize(List(4, 4, 4, 4))
  private val sqTS     = Set(sq)
  // an integer-period banded 3-type-set (all triangle/square ⇒ closes at integer c=4; one of the gate's gap sets)
  private val triSqHex = Set(List(3, 3, 3, 3, 3, 3), List(3, 3, 3, 4, 4), List(4, 4, 4, 4)).map(normalize)

  private def cyclesOf(feedDbg: String): Int =
    "cycles=(\\d+)".r.findFirstMatchIn(
      feedDbg
    ).map(_.group(1).toInt).getOrElse(fail(s"no cycles= in [$feedDbg]"))

  /** T1 — NON-NO-OP: maxBand>1 must actually multiply the band-height variants through the FULL enumerate
    * path (expandBands → replayCycle → dedup). Needs a fixture with an INTERIOR band: a pure self-loop (e.g.
    * the square row) is the whole-walk spine and is correctly a no-op (a taller pure-square band is still
    * 4⁴). The multi-band triangle/square set has interior bands (a self-loop followed by more edges), so a
    * taller band is a distinct (more faces) cycle ⇒ strictly more distinct cycles reach `close`. Guards
    * against the failure where "maxBand buys nothing" is really a wiring no-op rather than a true finding.
    */
  it should
    "actually exercise band expansion end-to-end (feedDebug reports MORE distinct cycles at maxBand=2)" in:
      val seeds = ProfileAutomaton.seedsC(c4)
      val d1    = ProfileAutomaton.feedDebug(c4, triSqHex, seeds, "", maxBand = 1)
      val d2    = ProfileAutomaton.feedDebug(c4, triSqHex, seeds, "", maxBand = 2)
      withClue(s"d1=[$d1] d2=[$d2]: ")(cyclesOf(d2) should be > cyclesOf(d1))

  /** T2 — MONOTONICITY: a taller band can only ADD cells, never drop one. So the "matched" count the (b)
    * measurement reports at maxBand=2/3 is a TRUE floor on the maxBand=1 baseline (no silent regression).
    */
  it should "be monotone in maxBand (emitted key set at a higher maxBand ⊇ the lower)" in:
    def keys(mb: Int) =
      ProfileAutomaton.enumerateForTypeSetC(c4, triSqHex, maxNodes = 8000, maxBand = mb).keySet
    val (k1, k2, k3)  = (keys(1), keys(2), keys(3))
    withClue(s"baseline must be non-empty to make this bite: k1=$k1 ")(k1 should not be empty)
    withClue(s"k1=$k1 ⊄ k2=$k2: ")(k1.subsetOf(k2) shouldBe true)
    withClue(s"k2=$k2 ⊄ k3=$k3: ")(k2.subsetOf(k3) shouldBe true)

  /** T3 — SOUNDNESS + primitiveBasis FOLD-BACK: the ONLY tiling of the pure-square type-set is the 4⁴ grid,
    * so the engine must emit EXACTLY that one key at every maxBand — a doubled square band (k=2) is NOT a new
    * cell, `close`/`primitiveBasis` must fold it back to 4⁴, and no spurious doubled-band artifact may
    * appear. This is the guarantee that every EXTRA "matched" the measurement reports at maxBand>1 is a REAL
    * tiling, not noise.
    */
  it should
    "emit EXACTLY the 4⁴ key for the pure-square type-set at every maxBand (no spurious doubled band)" in:
      val expected = Set(oracle1(sqTS))
      for mb <- List(1, 2, 3) do
        withClue(s"maxBand=$mb: ")(
          ProfileAutomaton.enumerateForTypeSetC(c4, sqTS, maxNodes = 4000, maxBand = mb).keySet shouldBe
            expected
        )
