package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.KrotenheerdtTorusMapSearch.FaceZ
import io.github.scala_tessella.dcel.ProfileAutomaton as PA
import io.github.scala_tessella.dcel.ProfileAutomaton.{PV, Profile}
import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

/** GROUND-UP tests for the cut-and-feed machinery (ADR-0038). Every leaf method is pinned on hand-computed
  * values FIRST (geometry, lattice, cut pieces), then the composites are checked — so a failure localises to
  * one piece. The decisive positive control is a MATCHED n=3 cell: anything the engine reaches via band-tops,
  * the harness MUST reproduce from the cell's own cut (else the harness, not the engine, is the limit).
  */
class CutFeedSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks:

  private val Z                                   = ZetaPoint
  private def st(s: Int): ZetaPoint               = ZetaPoint.step(s)
  private def ts(s: String): Set[VertexSignature] =
    s.split(',').toList.map(t => normalize(t.split('.').map(_.toInt).toList)).toSet

  /** A unit square with lower-left corner at `p`, sides along slots 0 and 3 (CCW). */
  private def unitSquare(p: ZetaPoint): FaceZ = FaceZ(4, Vector(p, p + st(0), p + st(0) + st(3), p + st(3)))

  // ----- leaf: geometry --------------------------------------------------------------------------------

  behavior of "ProfileAutomaton geometry leaves"

  it should "rotZk rotate a unit by k slots" in {
    for s <- 0 until 12; k <- 0 until 12 do PA.rotZk(st(s), k) shouldBe st((s + k) % 12)
    PA.rotZk(Z(1, 0, 0, 0), 0) shouldBe Z(1, 0, 0, 0)
    PA.rotZk(Z(1, 2, 3, 4), 12) shouldBe Z(1, 2, 3, 4)
  }

  it should "lenD give Euclidean length" in {
    PA.lenD(st(0)) shouldBe 1.0 +- 1e-9
    PA.lenD(ZetaPoint.origin) shouldBe 0.0 +- 1e-9
    PA.lenD(Z(0, 4, 0, -2)) shouldBe (2 * math.sqrt(3.0)) +- 1e-9 // the 2√3 horizontal
  }

  it should "collinear detect parallel vectors" in {
    PA.collinear(st(0), st(0) + st(0)) shouldBe true
    PA.collinear(st(0), st(6)) shouldBe true // antiparallel
    PA.collinear(st(0), st(3)) shouldBe false
  }

  it should "centroidZ sum the corners" in {
    PA.centroidZ(unitSquare(ZetaPoint.origin)) shouldBe (st(0) + st(0) + st(3) + st(3))
  }

  it should "divExactZ divide only when all components divide" in {
    PA.divExactZ(Z(2, 4, 0, -2), 2) shouldBe Some(Z(1, 2, 0, -1))
    PA.divExactZ(Z(1, 0, 0, 0), 2) shouldBe None
    PA.divExactZ(Z(2, 4, 6, 8), 2) shouldBe Some(Z(1, 2, 3, 4))
  }

  it should "sameFaceZ compare corner multisets order-independently" in {
    val a = unitSquare(ZetaPoint.origin)
    val b = FaceZ(4, a.corners.reverse)
    PA.sameFaceZ(a, b) shouldBe true
    PA.sameFaceZ(a, unitSquare(st(0))) shouldBe false
  }

  // ----- leaf: lattice ---------------------------------------------------------------------------------

  behavior of "ProfileAutomaton lattice leaves"

  it should "latticeSolve recover integer combos and reject non-members" in {
    val (a, b) = (st(0), st(3)) // unit square lattice
    PA.latticeSolve(st(0) + st(0), a, b) shouldBe Some((2, 0))
    PA.latticeSolve(st(3), a, b) shouldBe Some((0, 1))
    PA.latticeSolve(st(0) + st(3), a, b) shouldBe Some((1, 1))
    PA.latticeSolve(st(1), a, b) shouldBe None // 30°-unit is not in the square lattice
  }

  it should "latticeBasisZ return two shortest independent generators" in {
    val Some((v1, v2, pts)) = PA.latticeBasisZ(List(st(0), st(3))): @unchecked
    PA.lenD(v1) shouldBe 1.0 +- 1e-9
    PA.lenD(v2) shouldBe 1.0 +- 1e-9
    PA.collinear(v1, v2) shouldBe false
    pts should contain(st(0) + st(3))           // combos are present
    PA.latticeBasisZ(List(st(0))) shouldBe None // rank 1 ⇒ no basis
    PA.latticeBasisZ(Nil) shouldBe None
    // redundant generators ⇒ same unit-square lattice
    val Some((w1, w2, _)) = PA.latticeBasisZ(List(st(0), st(3), st(0) + st(3))): @unchecked
    PA.lenD(w1) shouldBe 1.0 +- 1e-9; PA.lenD(w2) shouldBe 1.0 +- 1e-9
  }

  it should "faceInCell test lattice-translate membership" in {
    val funds  = List(unitSquare(ZetaPoint.origin))
    val (a, b) = (st(0), st(3))
    PA.faceInCell(unitSquare(st(0) + st(3)), funds, a, b) shouldBe true                         // lattice translate
    PA.faceInCell(unitSquare(st(0) + st(0)), funds, a, b) shouldBe true
    PA.faceInCell(unitSquare(st(1)), funds, a, b) shouldBe false                                // non-lattice translate
    PA.faceInCell(FaceZ(3, Vector(ZetaPoint.origin, st(0), st(2))), funds, a, b) shouldBe false // wrong size
  }

  // ----- leaf: cut pieces ------------------------------------------------------------------------------

  behavior of "ProfileAutomaton cut pieces"

  private val c2 = Z(2, 0, 0, 0)

  it should "foldFaceModC bring the leftmost corner into [0,|c|)" in {
    val folded = PA.foldFaceModC(unitSquare(st(0) + st(0)), c2) // square at x∈[2,3]
    folded.corners.map(_.toBigPoint.x.toDouble).min shouldBe 0.0 +- 1e-9
    PA.foldFaceModC(unitSquare(ZetaPoint.origin), c2) shouldBe
      unitSquare(ZetaPoint.origin) // already in range
  }

  it should "tiledBandFaces produce DISTINCT folded faces (no horizontal double-cover)" in {
    val tiled = PA.tiledBandFaces(List(unitSquare(ZetaPoint.origin)), st(0), st(3), c2, K = 3)
    val keys  = tiled.map(_.corners.map(z => (z.a0, z.a1, z.a2, z.a3)).sorted)
    keys.distinct.size shouldBe keys.size // deduped
    all(tiled.map(_.corners.map(_.toBigPoint.x.toDouble).min)) should
      (be >= -1e-9 and be < 2.0)          // folded into [0,2)
  }

  /** A profile is well-formed iff non-empty, spans the circumference, and every vertex fan has
    * NON-OVERLAPPING covered slots and faces UP (the bug signatures were 11-square overlapping fans and
    * down-facing floor rows).
    */
  private def assertWellFormed(p: Profile, c: ZetaPoint): Unit =
    p.verts should not be empty
    p.widthX shouldBe c.toBigPoint.x.toDouble +- 1e-9
    p.verts.foreach { v =>
      val slots =
        v.fan.flatMap((s, m) => (0 until KrotenheerdtTorusMapSearch.gSlots(m)).map(k => (s + k) % 12))
      withClue(s"overlapping fan slots in $v: ")(slots.distinct.size shouldBe slots.size)
      withClue(s"non-up-facing vertex $v: ") {
        KrotenheerdtTorusMapSearch.coveredSlots(v.fan).size should be < 12
      }
    }

  it should "cutProfileAt yield the square row from a square tiling" in {
    val tiled = PA.tiledBandFaces(List(unitSquare(ZetaPoint.origin)), st(0), st(3), c2, K = 4)
    val prof  = PA.cutProfileAt(tiled, c2, Y = 0.5)
    prof shouldBe defined
    assertWellFormed(prof.get, c2)
    prof.get.verts should have size 2 // two unit squares across circumference 2
  }

  // ----- composite: representFrame + cut validity on REALIZED cells ------------------------------------

  /** (op, key) pairs for a type-set straight from the bucket assembler — `ops` is keyed by canonicalKey, so
    * no slow DelaneySymbols oracle is needed to get a realizable cell + its key.
    */
  private def opsOf(typeSet: Set[VertexSignature], maxV: Int, target: Int): Map[String, Array[Array[Int]]] =
    BucketAssembly.enumerateBucket(typeSet, maxV, targetCount = target).ops

  behavior of "ProfileAutomaton.representFrame (realized cells)"

  it should "give a horizontal c and WELL-FORMED cut profiles for every realized 3³.4² and 4⁴ cell" in {
    for typeSet <- List(ts("4.4.4.4"), ts("3.3.3.4.4")); (_, op) <- opsOf(typeSet, 8, 1) do
      val frame = PA.representFrame(op)
      frame shouldBe defined
      val f     = frame.get
      f.c.toBigPoint.y.toDouble shouldBe 0.0 +- 1e-9 // c horizontal
      f.profs should not be empty
      f.profs.foreach(p => assertWellFormed(p, f.c))
  }

  it should "give WELL-FORMED cut profiles for every realized n=3 {3⁶;3³.4²;4⁴} cell" in {
    val ops = opsOf(ts("3.3.3.3.3.3,3.3.3.4.4,4.4.4.4"), 12, 4)
    ops should not be empty
    for (_, op) <- ops do
      val frame = PA.representFrame(op)
      frame shouldBe defined
      frame.get.profs should not be empty
      frame.get.profs.foreach(p => assertWellFormed(p, frame.get.c))
  }

  // ----- composite: cut-and-feed round-trip ------------------------------------------------------------

  behavior of "ProfileAutomaton.cutFeedDiagnose (round-trip)"

  it should "reproduce 4⁴ and banded 3³.4² from their own cut (feed AND trace)" in {
    for typeSet <- List(ts("4.4.4.4"), ts("3.3.3.4.4")); (key, op) <- opsOf(typeSet, 8, 1) do
      val r = PA.cutFeedDiagnose(op, key, typeSet)
      info(s"$typeSet: $r")
      r.representable shouldBe true
      r.fedEmitsKey shouldBe true
      r.traceClosesKey shouldBe true
  }

  // ----- the cut-construction INVARIANT (what the high-aspect bug violated) ----------------------------

  behavior of "ProfileAutomaton cut-profile cell-consistency"

  private val n3 = ts("3.3.3.3.3.3,3.3.3.4.4,4.4.4.4")

  it should "profileCellConsistent accept a valid cut and reject an off-cell vertex" in {
    val funds     = List(unitSquare(ZetaPoint.origin))
    val tiled     = PA.tiledBandFaces(funds, st(0), st(3), c2, K = 4)
    val valid     = PA.cutProfileAt(tiled, c2, Y = 0.5).get
    PA.profileCellConsistent(valid, funds, st(0), st(3)) shouldBe true
    val corrupted =
      Profile(c2, valid.verts :+ PV(st(1), List((6, 4)))) // a square hung at a non-lattice vertex
    PA.profileCellConsistent(corrupted, funds, st(0), st(3)) shouldBe false
  }

  /** Every cut profile of every realized cell must be a VALID cut: each vertex fan reconstructs to a cell
    * face. This FAILS for the high-aspect n=3 cells before the slab+window fix (deep floor rows ⇒ off-lattice
    * fans).
    */
  it should "produce only CELL-CONSISTENT cut profiles for every realized cell" in {
    for
      (typeSet, maxV, target) <- List((ts("4.4.4.4"), 8, 1), (ts("3.3.3.4.4"), 8, 1), (n3, 12, 4))
      (_, op)                 <- opsOf(typeSet, maxV, target)
    do
      val f = PA.representFrame(op).get
      f.profs should not be empty
      f.profs.foreach: p =>
        withClue(s"non-cell cut profile for $typeSet (verts=${p.verts.size}): ") {
          PA.profileCellConsistent(p, f.rFaces, f.rv1, f.rv2) shouldBe true
        }
  }

  it should "cutProfileAt yield a cell-consistent profile at ANY height (property)" in {
    val frames  = opsOf(n3, 12, 4).values.toList.flatMap(PA.representFrame)
    frames should not be empty
    val tiledOf = frames.map(f => f -> PA.tiledBandFaces(f.rFaces, f.rv1, f.rv2, f.c)).toMap
    forAll(Gen.oneOf(frames), Gen.choose(-6.0, 6.0)) { (f, y) =>
      PA.cutProfileAt(tiledOf(f), f.c, y)
        .foreach(p => PA.profileCellConsistent(p, f.rFaces, f.rv1, f.rv2) shouldBe true)
    }
  }

  it should "trace back to a cycle from a cell-consistent cut for every realized n=3 cell" in {
    for (_, op) <- opsOf(n3, 12, 4) do
      val f       = PA.representFrame(op).get
      val steps   = f.profs.map(p => PA.traceCellDebug(p, f.rFaces, f.rv1, f.rv2, f.c, n3, 60).size)
      val cycled  = f.profs.exists(p => PA.traceCellCycle(p, f.rFaces, f.rv1, f.rv2, f.c, n3, "", 60)._1)
      val deepest = f.profs.maxBy(p => PA.traceCellDebug(p, f.rFaces, f.rv1, f.rv2, f.c, n3, 60).size)
      withClue(s"no cut profile traces to a cycle (step counts=$steps)\n${
          PA.dumpDeadEnd(deepest, f.rFaces, f.rv1, f.rv2, f.c, n3, 60)
        }\n") {
        cycled shouldBe true
      }
  }

  // ----- composite: cut-and-feed round-trip (n=3) ------------------------------------------------------

  it should "reproduce every MATCHED n=3 {3⁶;3³.4²;4⁴} cell from its own cut" in {
    val typeSet = ts("3.3.3.3.3.3,3.3.3.4.4,4.4.4.4")
    val ops     = opsOf(typeSet, 12, 4)
    val circs   = List(c2, Z(4, 0, 0, 0), Z(6, 0, 0, 0))
    val perC    = circs.map(c => c -> PA.enumerateForTypeSetC(c, typeSet, 12000).keySet).toMap
    val matched = perC.values.flatten.toSet
    matched should not be empty
    for (key, op) <- ops if matched.contains(key) do
      val r      = PA.cutFeedDiagnose(op, key, typeSet)
      val gateAt = circs.filter(c => perC(c).contains(key)).map(_.toBigPoint.x.toDouble)
      info(s"matched cutC=${r.c.map(_.toBigPoint.x.toDouble)} gateAt=$gateAt -> $r")
      withClue(s"matched cell $key not reproduced by its own cut: ") {
        (r.fedEmitsKey || r.traceClosesKey) shouldBe true
      }
  }
