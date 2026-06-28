package io.github.scala_tessella.dcel

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
