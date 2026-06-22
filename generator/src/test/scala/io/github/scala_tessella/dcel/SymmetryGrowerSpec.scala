package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize, validSignatures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Phase-2 de-risk GATE as a deterministic spec (the [[feedback_test_before_probes]] discipline: test the new
  * methods before any long probe). Validates the symmetry-first geometric grower
  * ([[KrotenheerdtTorusMapSearch.enumerateBySymmetry]], m=6, central hexagon at the origin) on invariants
  * that hold regardless of exact counts.
  *
  * NOTE on keys: the grower now keys in the SHARED D-symbol space — `closeCell` validates soundness via
  * `verifyCell` (its `tilesWithoutOverlap` rejects false-period non-tilings the combinatorial classifier
  * would accept) and then keys via `torusMapClassify` (build the barycentric `op` →
  * `DelaneySymbols.classifyClosedMap`). So the grower's keys are IDENTICAL to the oracle's and the bounded-V
  * assembler's (asserted below) — the ADR-0032 key-unification step that enables the Phase-3 union and
  * certified counts.
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

  // The key-unification payoff (ADR-0032): the grower now keys via torusMapClassify (the shared D-symbol
  // space), so its keys must be IDENTICAL to the generate-all oracle's — the same key-for-key agreement the
  // other engines have (CrossEngineSpec). This is what lets the grower + bounded-V dedup in one space.
  it should "key the m=6 Archimedean identically to the DelaneySymbols oracle (shared key space)" in:
    val oracle     = DelaneySymbols.keyedTilings(1, 12)
    val oracleKey  = (t: String) => oracle.find(_._2 == Set(sig(t))).map(_._3).get
    val foundKeys  = m6n1.tilings.map(_._3).toSet
    List("6.6.6", "3.6.3.6", "3.4.6.4").foreach(t =>
      withClue(s"$t key: ")(foundKeys should contain(oracleKey(t)))
    )
    // soundness in the shared space: every grower key is a real oracle tiling
    val oracleKeys = oracle.map(_._3).toSet
    m6n1.tilings.foreach((_, _, k) =>
      withClue(s"grower key not in oracle: $k — ")(oracleKeys should contain(k))
    )

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

  behavior of
    "profileSeed (per-state cost measurement — must match the real driver before we trust its numbers)"

  it should "reproduce the seed's tilings (sound) and report a populated phase breakdown" in:
    val (res, phases) =
      KrotenheerdtTorusMapSearch.profileSeed(
        KrotenheerdtTorusMapSearch.polygonCenterSeed(4, 4),
        maxN = 1,
        maxFaces = 16
      )
    res.tilings should not be empty
    res.tilings.foreach((nn, types, _) =>
      withClue(s"$types: ")((nn, types.subsetOf(archimedean10)) shouldBe (1, true))
    )
    res.tilings.map(_._2).toSet should contain(Set(sig("4.4.4.4"))) // square centre reaches the square tiling
    phases.keySet shouldBe Set("corona", "tryClose", "grow", "canonicalKey")
    phases.values.sum should be > 0L

  it should "have profileClose mirror the same control flow (same state count as profileSeed)" in:
    val seed     = KrotenheerdtTorusMapSearch.polygonCenterSeed(4, 4)
    val (res, _) = KrotenheerdtTorusMapSearch.profileSeed(seed, maxN = 1, maxFaces = 16)
    val close    = KrotenheerdtTorusMapSearch.profileClose(seed, maxN = 1, maxFaces = 16)
    close("states") shouldBe res.states // identical DFS ⇒ identical state count
    close("verifyCalls") should be > 0L

  behavior of "torusMapClassify (D-symbol key unification — the ADR-0032 keystone for the Phase-3 union)"

  // Hand-built closed torus cells with DIFFERENT polygons + lattices, each keyed IDENTICALLY to the oracle —
  // proving the geometry→D-symbol bridge on more than one config (4.4.4.4 could pass by its square symmetry
  // alone). The grower-based test below additionally exercises torusMapClassify on 6.6.6/3.6.3.6/3.4.6.4
  // end-to-end ⇒ 5 distinct configs covered.
  private val u                                                        = (k: Int) => ZetaPoint.unit(k)
  private val originB                                                  = io.github.scala_tessella.dcel.geometry.BigPoint.origin
  private def oracleKeyOf(t: String): String                           =
    DelaneySymbols.keyedTilings(1, 12).find(_._2 == Set(sig(t))).map(_._3).get
  private def classify(faces: List[FaceZ], a: ZetaPoint, b: ZetaPoint) =
    KrotenheerdtTorusMapSearch.torusMapClassify(faces, a.toBigPoint, b.toBigPoint, originB)

  it should "key the unit-square 4.4.4.4 cell identically to the oracle (V=1,E=2,F=1; Λ=ζ⁰,ζ³)" in:
    val square = FaceZ(4, Vector(ZetaPoint.origin, u(0), u(0) + u(3), u(3)))
    val res    = classify(List(square), u(0), u(3))
    res.map(_._1) shouldBe Some(1)
    res.map(_._2.toSet) shouldBe Some(Set(sig("4.4.4.4")))
    res.map(_._3) shouldBe Some(oracleKeyOf("4.4.4.4"))

  it should "key the two-triangle 3⁶ (triangular) cell identically to the oracle (V=1,E=3,F=2; Λ=ζ⁰,ζ²)" in:
    // up [origin,ζ⁰,ζ²] + down [ζ⁰,origin,ζ¹⁰] share edge origin→ζ⁰; under Λ=(ζ⁰,ζ²) every corner ≡ origin
    // (ζ¹⁰ = ζ⁰−ζ²) ⇒ one degree-6 torus vertex = 3⁶.
    val up   = FaceZ(3, Vector(ZetaPoint.origin, u(0), u(2)))
    val down = FaceZ(3, Vector(u(0), ZetaPoint.origin, u(10)))
    val res  = classify(List(up, down), u(0), u(2))
    res.map(_._1) shouldBe Some(1)
    res.map(_._2.toSet) shouldBe Some(Set(sig("3.3.3.3.3.3")))
    res.map(_._3) shouldBe Some(oracleKeyOf("3.3.3.3.3.3"))

  behavior of "rotationCenters (ground-truth rotational symmetry — validated on known orbifolds)"

  // 4⁴ (p4m, rotation orbifold 442): order-4 at the square centre AND at the vertex, order-2 at the edge mid.
  it should "give 442 for the unit-square 4.4.4.4 cell (face4, vertex4, edge2)" in:
    val square = FaceZ(4, Vector(ZetaPoint.origin, u(0), u(0) + u(3), u(3)))
    KrotenheerdtTorusMapSearch.rotationCenters(List(square), u(0), u(3)) shouldBe
      Set(("face", 4), ("vertex", 4), ("edge", 2))

  // 3⁶ (p6m, rotation orbifold 632): order-6 at the vertex, order-3 at the triangle centre, order-2 at edge.
  it should "give 632 for the two-triangle 3⁶ cell (vertex6, face3, edge2)" in:
    val up   = FaceZ(3, Vector(ZetaPoint.origin, u(0), u(2)))
    val down = FaceZ(3, Vector(u(0), ZetaPoint.origin, u(10)))
    KrotenheerdtTorusMapSearch.rotationCenters(List(up, down), u(0), u(2)) shouldBe
      Set(("vertex", 6), ("face", 3), ("edge", 2))

  behavior of "enumerateAllSeedsParallel (work-stealing — sound, finds the budget-stable core)"

  // CONCURRENCY correctness, budget-robust. Exact parallel==sequential equality holds only for BUDGET-COMPLETE
  // runs: when `budgetHit` cuts a branch, whether a boundary tiling (e.g. 3³.4²-snub at cell ≈ maxFaces) closes
  // before the cut is exploration-order-dependent, so parallel and sequential legitimately differ on those —
  // both sound LOWER BOUNDS. So we assert the invariants that DON'T depend on order: (1) SOUND — every key the
  // parallel run emits is a real oracle tiling (a race would corrupt/duplicate a key); (2) it finds the cheap
  // CORE whose cells are well under budget (order-independent); (3) parallel ⊆ sequential's reachable set.
  it should "be sound and find the budget-stable core (matching the sequential driver)" in:
    val mf     = 14
    val seq    = KrotenheerdtTorusMapSearch.enumerateAllSeeds(maxN = 1, maxFaces = mf)
    val par    = KrotenheerdtTorusMapSearch.enumerateAllSeedsParallel(maxN = 1, maxFaces = mf, parallelism = 8)
    val oracle = DelaneySymbols.keyedTilings(1, 12).map(_._3).toSet
    par.tilings.foreach((_, _, k) =>
      withClue(s"parallel emitted non-oracle key $k — ")(oracle should contain(k))
    )
    seq.tilings.foreach((_, _, k) =>
      withClue(s"sequential emitted non-oracle key $k — ")(oracle should contain(k))
    )
    val core   = Set("6.6.6", "4.4.4.4", "3.3.3.3.3.3").map(t => Set(sig(t))) // cells ≪ 14 ⇒ order-independent
    withClue(
      s"parallel core: ${par.tilings.map(_._2).toSet} — "
    )(core.subsetOf(par.tilings.map(_._2).toSet) shouldBe true)
    withClue(
      s"sequential core: ${seq.tilings.map(_._2).toSet} — "
    )(core.subsetOf(seq.tilings.map(_._2).toSet) shouldBe true)

  // REGRESSION (the n=3 4-hour run's finding): `visited` was shared across seeds keyed by canonicalKey alone,
  // but growBySymmetry depends on the seed's (rot, m) — so an earlier seed claiming an isometric partial patch
  // pruned a LATER seed's path to its OWN tiling (e.g. 4.6.12 from the dodecagon seed), an order-dependent
  // COMPLETENESS bug (it made n=1 = 9 not 10). Per-seed visited fixes it: at maxFaces=44 every n=1 cell is
  // budget-complete, so the engine reaches all 10 in-scope Archimedean (4.8.8 octagon aside) key-for-key,
  // deterministically — a certified n=1 = 10/10 (in-scope) by the symmetry engine alone.
  it should "reach all 10 in-scope Archimedean at n=1 incl 4.6.12 (no cross-seed pruning)" in:
    val res       = KrotenheerdtTorusMapSearch.enumerateAllSeedsParallel(maxN = 1, maxFaces = 44, parallelism = 8)
    val oracle    = DelaneySymbols.keyedTilings(1, 12)
    val foundKeys = res.tilings.filter(_._1 == 1).map(_._3).toSet
    TilingReference.n1NoOctagon.foreach: ts =>
      withClue(s"$ts missing (cross-seed pruning?) — ")(foundKeys should
        contain(oracle.find(_._2 == Set(ts)).map(_._3).get))
    withClue("4.6.12 — the cross-seed-pruning poster child — ")(
      foundKeys should contain(oracle.find(_._2 == Set(sig("4.6.12"))).map(_._3).get)
    )

  // The wall-clock cap (for long unattended runs) must terminate promptly and NEVER emit a garbage key — a
  // partial (capped) run is a sound LOWER BOUND, so every key it emits is still a real oracle tiling.
  it should "stay sound under an early wall-clock cutoff (maxMillis)" in:
    val oracle = DelaneySymbols.keyedTilings(1, 12).map(_._3).toSet
    val capped =
      KrotenheerdtTorusMapSearch.enumerateAllSeedsParallel(
        maxN = 1,
        maxFaces = 40,
        parallelism = 8,
        maxMillis = 1L
      )
    capped.budgetHit shouldBe true // cut early by the deadline
    capped.tilings.foreach((_, _, k) =>
      withClue(s"capped run emitted non-oracle key $k — ")(oracle should contain(k))
    )
