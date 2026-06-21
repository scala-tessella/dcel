package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize, validSignatures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Phase-2 de-risk GATE as a deterministic spec (the [[feedback_test_before_probes]] discipline: test the new
  * methods before any long probe). Validates the symmetry-first geometric grower
  * ([[KrotenheerdtTorusMapSearch.enumerateBySymmetry]], m=6, central hexagon at the origin) on invariants
  * that hold regardless of exact counts.
  *
  * NOTE on keys: like the other geometric torus engines, the grower closes via `verifyCell`, whose canonical
  * key is the GEOMETRIC content key (`size:centroid`), NOT the `DelaneySymbols` D-symbol `canonicalKey`. That
  * geometric key space is already cross-validated against the oracle (`KrotenheerdtTorusSearchSpec`:
  * n=1=10/11, n=2=18/20 key-equivalent), so for the GATE we check by VERTEX-TYPE-SET (engine-independent) and
  * soundness; unifying the grower's output into the shared D-symbol key space (build `op` →
  * `classifyClosedMap`) is a Phase-3 dedup task, not needed to decide GO/NO-GO.
  *
  * Gate: (1) SOUND — every n=1 tiling has exactly one vertex type, a real {3,4,6,12} Archimedean (no
  * false-period 3.3.6.6/3.4.4.6 leakage); (2) REPRODUCES the hexagon-centred m=6 Archimedean by type-set (6³
  * / 3.6.3.6 / 3.4.6.4 — all have an order-6 hexagon centre); (3) DETERMINISTIC + BOUNDED (no scatter
  * explosion). (4.6.12 / 3.12.12 need the DODECAGON-centred seed — a follow-up once this gate is GO.)
  */
class SymmetryGrowerSpec extends AnyFlatSpec with Matchers:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  // the 10 Archimedean reachable in the ℤ[ζ₁₂] world (octagon 4.8.8 excluded — needs ℤ[ζ₂₄])
  private val archimedean10: Set[VertexSignature] = TilingReference.n1NoOctagon

  private val maxFaces  = 48
  private lazy val m6n1 = KrotenheerdtTorusMapSearch.enumerateBySymmetry(m = 6, maxN = 1, maxFaces = maxFaces)

  behavior of "enumerateBySymmetry(m=6, central hexagon)"

  it should "be SOUND — every n=1 result is a single real Archimedean type (no non-tiling leakage)" in:
    m6n1.tilings should not be empty
    m6n1.tilings.foreach: (nn, types, _) =>
      withClue(s"$types: ")(nn shouldBe 1)
      types.size shouldBe 1
      withClue(s"spurious/non-Archimedean type $types: ")(types.subsetOf(archimedean10) shouldBe true)
      types.foreach(t => validSignatures should contain(t))

  it should "reproduce the hexagon-centred m=6 Archimedean by type-set" in:
    val foundTypeSets = m6n1.tilings.map(_._2).toSet
    List("6.6.6", "3.6.3.6", "3.4.6.4").foreach: t =>
      withClue(s"$t missing — found $foundTypeSets — ")(foundTypeSets should contain(Set(sig(t))))

  it should "be deterministic (same type-sets and states on re-run)" in:
    val again = KrotenheerdtTorusMapSearch.enumerateBySymmetry(m = 6, maxN = 1, maxFaces = maxFaces)
    again.tilings.map(_._2).toSet shouldBe m6n1.tilings.map(_._2).toSet
    again.states shouldBe m6n1.states

  it should "have BOUNDED search cost (tracks domain size, no scatter explosion)" in:
    withClue(s"states=${m6n1.states} budgetHit=${m6n1.budgetHit} — ")(m6n1.states should be < 200000L)

  behavior of "completeVertexTypes (the diagnostic lens — must be correct before trusting any trace)"

  import KrotenheerdtTorusMapSearch.FaceZ

  // central hexagon: 6 corners, each touched only by the hexagon (120°) ⇒ NO completed vertex
  private val hexAtOrigin               = FaceZ(6, Vector(0, 2, 4, 6, 8, 10).map(ZetaPoint.unit))
  // six unit triangles fanned around the origin ⇒ the origin is a completed 3⁶ vertex (outer corners are not)
  private val sixTriangles: List[FaceZ] = (0 until 6).toList.map(k =>
    FaceZ(3, Vector(ZetaPoint.origin, ZetaPoint.unit(2 * k), ZetaPoint.unit((2 * k + 2) % 12)))
  )

  it should "find NO completed vertex in the bare central hexagon" in:
    KrotenheerdtTorusMapSearch.completeVertexTypes(List(hexAtOrigin)) shouldBe empty

  it should "detect the 3⁶ vertex when six triangles fan the origin" in:
    KrotenheerdtTorusMapSearch.completeVertexTypes(sixTriangles) shouldBe Set(sig("3.3.3.3.3.3"))

  behavior of "seed catalogue + exact-affine rotation (the new methods — tested before any enumeration probe)"

  private lazy val seeds = KrotenheerdtTorusMapSearch.allSeeds

  // THE critical invariant for the exact-affine rotation r(p)=ζ^(12/m)p+t: each seed must be genuinely
  // C_m-INVARIANT under its own rotation, i.e. rot maps every seed face onto another seed face (same size +
  // same corner SET). If t is wrong (the dodecagon/vertex/edge cases), this fails — before we ever grow.
  it should "produce only seeds that are C_m-invariant under their own rotation" in:
    seeds should not be empty
    seeds.foreach: s =>
      val faceSets = s.faces.map(f => (f.size, f.corners.toSet)).toSet
      s.faces.foreach: f =>
        val rotated = (f.size, f.corners.map(s.rot).toSet)
        withClue(s"seed ${s.label}: rot does not preserve face $f — ")(faceSets should contain(rotated))

  it should "include the key polygon-centre seeds — incl. the DODECAGON centre at order 6" in:
    val labels = seeds.map(_.label).toSet
    labels should contain("poly12/m6") // dodecagon centre, m=6 (the 4.6.12 / 3.12.12 enabler)
    labels should contain("poly4/m4")  // square centre, m=4
    labels should contain("poly3/m3")  // triangle centre, m=3
    labels should contain("edge4") // two-squares domino, m=2

  it should "give the dodecagon an order-6 (not 12) centre — crystallographic restriction" in:
    val dodecOrders = seeds.collect { case s if s.label.startsWith("poly12/") => s.m }.toSet
    dodecOrders should contain(6)
    dodecOrders should not contain 12 // never order 12 in a periodic tiling

  it should "admit a 3⁶ vertex-centre at m=6 but NO vertex-centre for the C₁ vertex 3.4.6.4" in:
    val labels = seeds.map(_.label).toSet
    labels should contain("vtx3.3.3.3.3.3/m6")
    labels.filter(_.startsWith("vtx3.4.6.4/")) shouldBe empty // 3.4.6.4 corona has no nontrivial rotation

  behavior of "enumerateAllSeeds (the full-catalogue driver — fast smoke + live logger)"

  // small maxFaces: only the tiny cells close (6.6.6 @ 7 faces) — keeps the smoke fast. Asserts the driver is
  // sound (every result is a real Archimedean, cross-seed dedup leaves no spurious) and the live logger fires.
  it should "be sound, reach 6.6.6, and invoke the progress logger" in:
    var ticks = 0
    val res   = KrotenheerdtTorusMapSearch.enumerateAllSeeds(
      maxN = 1,
      maxFaces = 12,
      onSeed = (_, _, _) => (),
      log = _ => ticks += 1,
      logEveryMs = 50L // fast ticks so the daemon fires within the smoke's runtime
    )
    res.tilings should not be empty
    res.tilings.foreach((nn, types, _) =>
      withClue(s"$types: ")((nn, types.subsetOf(archimedean10)) shouldBe (1, true))
    )
    res.tilings.map(_._2).toSet should contain(Set(sig("6.6.6")))
    ticks should be > 0
