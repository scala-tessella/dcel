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
