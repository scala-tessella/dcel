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
