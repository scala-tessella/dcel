package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.KrotenheerdtTorusMapSearch.FaceZ
import io.github.scala_tessella.dcel.ProfileAutomaton as PA
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

/** Ground-up tests for the generic covering-walk finder ([[ProfileAutomaton.coveringWalks]]) on synthetic Int
  * graphs (edges = `(to, color)`), independent of any tiling geometry. Key properties:
  *   - it traverses a SELF-LOOP / revisits a node when needed to cover a colour (a homogeneous `4⁴` row is a
  *     self-loop in the profile graph; a simple-cycle finder could never cover it);
  *   - it allows a `(node,colorset)` state up to `maxRepeat` times PER BRANCH (a tall band of `k` rows needs
  *     the self-loop `k` times) ⇒ distinct repeat counts give distinct walks/tilings, all enumerated;
  *   - it is NON-LOSSY across branches (the old `(node, colors-used)` BFS visited each state once and lost
  *     one);
  *   - every returned walk covers EXACTLY the target colours, and the search terminates (bounded).
  */
class CycleFinderSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks:

  private type E = (Int, Char)
  private def find(
      g: Map[Int, List[E]],
      start: Int,
      colors: Set[Char],
      maxLen: Int = 12,
      cap: Int = 100,
      maxRepeat: Int = 8
  ) = PA.coveringWalks[Int, E, Char](
    start,
    n => g.getOrElse(n, Nil),
    _._1,
    _._2,
    colors,
    maxLen,
    cap,
    maxRepeat
  )

  /** Every returned walk must cover exactly `colors` (the universal post-condition). */
  private def allCover(ws: List[List[E]], colors: Set[Char]): Unit =
    ws.foreach(w => w.map(_._2).toSet shouldBe colors)

  behavior of "ProfileAutomaton.coveringWalks"

  it should "find a single covering cycle" in {
    val g = Map(0 -> List((1, 'a')), 1 -> List((0, 'b')))
    find(g, 0, Set('a', 'b')) shouldBe List(List((1, 'a'), (0, 'b')))
  }

  it should "traverse a SELF-LOOP to cover its colour (the regression simpleCycles caused)" in {
    val g  = Map(0 -> List((1, 'a')), 1 -> List((1, 'b'), (0, 'c')))
    val ws = find(g, 0, Set('a', 'b', 'c'))
    allCover(ws, Set('a', 'b', 'c'))
    ws should contain(List((1, 'a'), (1, 'b'), (0, 'c'))) // the minimal (single self-loop) walk is present
  }

  it should "enumerate distinct band HEIGHTS via repeated self-loops (k=1..maxRepeat)" in {
    // 0-a->1, 1-b->1 (self), 1-a->0 : covering needs {a,b}; the self-loop may repeat ⇒ taller bands
    val g  = Map(0 -> List((1, 'a')), 1 -> List((1, 'b'), (0, 'a')))
    val ws = find(g, 0, Set('a', 'b'), maxRepeat = 3)
    allCover(ws, Set('a', 'b'))
    ws.map(_.length).sorted shouldBe List(3, 4, 5) // self-loop taken 1, 2, 3 times
  }

  it should "find a covering walk that REVISITS an intermediate node with a new colourset" in {
    val g  = Map(0 -> List((1, 'a')), 1 -> List((2, 'b'), (0, 'd')), 2 -> List((1, 'c')))
    val ws = find(g, 0, Set('a', 'b', 'c', 'd'))
    allCover(ws, Set('a', 'b', 'c', 'd'))
    ws should contain(List((1, 'a'), (2, 'b'), (1, 'c'), (0, 'd')))
  }

  it should "find BOTH covering walks that share a (node,colorset) state across branches (BFS-lossy case)" in {
    val g  = Map(
      0 -> List((1, 'a'), (2, 'b')),
      1 -> List((3, 'b')),
      2 -> List((3, 'a')),
      3 -> List((0, 'c'))
    )
    val ws = find(g, 0, Set('a', 'b', 'c'))
    allCover(ws, Set('a', 'b', 'c'))
    ws.toSet shouldBe Set(List((1, 'a'), (3, 'b'), (0, 'c')), List((2, 'b'), (3, 'a'), (0, 'c')))
  }

  it should "reject a walk that does not cover ALL colours" in {
    find(Map(0 -> List((1, 'a')), 1 -> List((0, 'b'))), 0, Set('a', 'b', 'c')) shouldBe empty
  }

  it should "prune any edge whose colour is outside the target set" in {
    val g = Map(0 -> List((1, 'a')), 1 -> List((0, 'b'), (2, 'x')), 2 -> List((0, 'b')))
    find(g, 0, Set('a', 'b')) shouldBe List(List((1, 'a'), (0, 'b')))
  }

  it should "respect maxLen (walk edge-count bound)" in {
    val g = Map(0 -> List((1, 'a')), 1 -> List((2, 'b')), 2 -> List((0, 'c'))) // 3-edge cycle
    find(g, 0, Set('a', 'b', 'c'), maxLen = 2) shouldBe empty
    find(g, 0, Set('a', 'b', 'c'), maxLen = 3) should have size 1
  }

  it should "respect cap" in {
    val g = Map(
      0 -> List((1, 'a'), (2, 'b')),
      1 -> List((3, 'b')),
      2 -> List((3, 'a')),
      3 -> List((0, 'c'))
    )
    find(g, 0, Set('a', 'b', 'c'), cap = 1) should have size 1
  }

  it should "terminate and bound repeats on a zero-gain self-loop" in {
    val g  = Map(0 -> List((1, 'a')), 1 -> List((1, 'a'), (0, 'b'))) // self-loop colour already coverable
    val ws = find(g, 0, Set('a', 'b'), maxRepeat = 4)
    allCover(ws, Set('a', 'b'))
    ws should not be empty
    ws.size should be <= 5 // bounded by maxRepeat, no infinite spin
  }

  it should "return empty when start has no edges" in {
    find(Map.empty[Int, List[E]], 0, Set('a')) shouldBe empty
  }

  // ----- multi-row finder leaves: band (sub-cycle) detection + band-height expansion --------------------

  behavior of "ProfileAutomaton.bandSegments / expandBands"
  private def to1(e: E): Int = e._1

  it should "find the SELF-LOOP band (period 1)" in {
    // 0-a->1, 1-b->1(self), 1-c->0 : the self-loop edge (index 1) is the band; the rest is the spine
    PA.bandSegments(0, List((1, 'a'), (1, 'b'), (0, 'c')), to1) shouldBe List((1, 1))
  }

  it should "find the 2-CYCLE band (period 2, e.g. a 3⁶ triangle band)" in {
    // 0-a->1, 1-b->2, 2-c->1, 1-d->0 : edges 1..2 (1->2->1) are the band
    PA.bandSegments(0, List((1, 'a'), (2, 'b'), (1, 'c'), (0, 'd')), to1) shouldBe List((1, 2))
  }

  it should "find BOTH bands in the aspect-5.6 structure (a 3⁶ 2-cycle AND a 4⁴ self-loop)" in {
    // node3→1, 1→2, 2→1, 1→0, 0→0(self), 0→3 : bands = [1..2] (3⁶) and [4..4] (4⁴ self); spine excluded
    val path = List((1, 'a'), (2, 'b'), (1, 'b'), (0, 'a'), (0, 'c'), (3, 'a'))
    PA.bandSegments(3, path, to1) shouldBe List((1, 2), (4, 4))
  }

  it should "find NO band in a simple cycle (only the spine)" in {
    PA.bandSegments(0, List((1, 'a'), (0, 'b')), to1) shouldBe Nil
  }

  it should "expand a single band to 1..maxRepeat consecutive traversals" in {
    val path = List((1, 'a'), (1, 'b'), (0, 'c')) // self-loop band at index 1
    PA.expandBands(0, path, to1, maxRepeat = 3) shouldBe List(
      List((1, 'a'), (1, 'b'), (0, 'c')),                    // k=1
      List((1, 'a'), (1, 'b'), (1, 'b'), (0, 'c')),          // k=2
      List((1, 'a'), (1, 'b'), (1, 'b'), (1, 'b'), (0, 'c')) // k=3
    )
    PA.expandBands(0, List((1, 'a'), (0, 'b')), to1, 5) shouldBe List(List((1, 'a'), (0, 'b'))) // no band
  }

  it should "expand TWO bands as the cartesian of their heights (the multi-row case)" in {
    val path = List((1, 'a'), (2, 'b'), (1, 'b'), (0, 'a'), (0, 'c'), (3, 'a')) // bands [1..2] and [4..4]
    val vs   = PA.expandBands(3, path, to1, maxRepeat = 2)
    vs should have size 4 // 2×2
    // the (k1=2, k2=2) variant: 3⁶ band twice + 4⁴ band twice
    vs should
      contain(List((1, 'a'), (2, 'b'), (1, 'b'), (2, 'b'), (1, 'b'), (0, 'a'), (0, 'c'), (0, 'c'), (3, 'a')))
  }

  it should "replayCycle accumulate Δ and STACK each edge's faces shifted by the cumulative Δ" in {
    val faceA          = FaceZ(3, Vector(ZetaPoint.origin, ZetaPoint.step(0), ZetaPoint.step(2)))
    val faceB          = FaceZ(4, Vector(ZetaPoint.origin, ZetaPoint.step(0), ZetaPoint.step(3)))
    val up             = ZetaPoint(0, 0, 0, 1) // a vertical step
    val e1             = PA.PEdge(Nil, up, List(faceA), List(3))
    val e2             = PA.PEdge(Nil, up, List(faceB), List(4))
    val (delta, faces) = PA.replayCycle(List(e1, e2))
    delta shouldBe ZetaPoint(0, 0, 0, 2) // Δ = e1.delta + e2.delta
    faces shouldBe List(faceA, FaceZ(4, faceB.corners.map(_ + up))) // faceA at 0, faceB shifted by e1.Δ
    PA.replayCycle(Nil) shouldBe (ZetaPoint.origin, Nil)
  }

  // ----- property-based: SOUNDNESS over random graphs --------------------------------------------------

  /** A walk is sound iff it is a real path `start → … → start` in `g` covering EXACTLY `colors`. */
  private def isSound(w: List[E], g: Map[Int, List[E]], start: Int, colors: Set[Char]): Boolean =
    w.nonEmpty && {
      var node = start
      val ok   = w.forall { e =>
        val good = g.getOrElse(node, Nil).contains(e); node = e._1; good
      }
      ok && node == start && w.map(_._2).toSet == colors
    }

  // bounded edge counts (≤3 per node) keep the walk set finite-and-modest so properties run fast
  private def edgesGen(nodes: Int, alphabet: Seq[Char]): Gen[List[E]] =
    Gen.choose(0, 3).flatMap(k => Gen.listOfN(k, Gen.zip(Gen.choose(0, nodes - 1), Gen.oneOf(alphabet))))
  private val graphGen: Gen[Map[Int, List[E]]]                        =
    Gen.sequence[List[List[E]], List[E]]((0 until 5).map(_ => edgesGen(5, "abc")))
      .map(_.zipWithIndex.map((es, i) => i -> es).toMap)

  it should "return only SOUND walks (real closed paths covering exactly the colours) for ANY graph" in
    forAll(graphGen, Gen.choose(0, 4), Gen.choose(1, 3)) { (g, start, maxRepeat) =>
      val colors = Set('a', 'b', 'c')
      val ws     = find(g, start, colors, maxLen = 8, cap = 50, maxRepeat = maxRepeat)
      ws.foreach { w =>
        withClue(s"unsound walk $w in $g from $start: ")(isSound(w, g, start, colors) shouldBe true)
        w.length should be <= 8
      }
    }

  // tiny graphs (3 nodes, alphabet {a,b}) so the FULL walk set is small and the cap below never binds
  private val tinyGraphGen: Gen[Map[Int, List[E]]] =
    Gen.sequence[List[List[E]], List[E]]((0 until 3).map(_ => edgesGen(3, "ab")))
      .map(_.zipWithIndex.map((es, i) => i -> es).toMap)

  it should "be MONOTONE in maxRepeat when the cap does not bind (algorithmic superset)" in
    // NOTE: monotonicity holds only with a non-binding cap — under a binding cap a larger maxRepeat REORDERS the
    // DFS and can crowd out a walk (the cap-crowding that the engine must respect). Here cap is non-binding.
    forAll(tinyGraphGen, Gen.choose(0, 2)) { (g, start) =>
      val colors = Set('a', 'b')
      val small  = find(g, start, colors, maxLen = 5, cap = 5000, maxRepeat = 1).toSet
      val big    = find(g, start, colors, maxLen = 5, cap = 5000, maxRepeat = 3).toSet
      small.subsetOf(big) shouldBe true
    }
