package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.ProfileAutomaton as PA
import io.github.scala_tessella.dcel.ProfileAutomaton.{PV, Profile}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Characterization of [[ProfileAutomaton.canonKey]] — the node identity used for the profile graph + cycle
  * detection. Two contracts: (1) translation-invariance and vertex-order-independence (congruent profiles key
  * EQUAL); (2) FAITHFULNESS — geometrically distinct profiles key DIFFERENT, in particular two vertices with
  * the same fan SIZES but a different SLOT arrangement (the lossy `map(_._2).sorted` collapses these,
  * conflating distinct profiles and collapsing a cell's period-cycle into a non-simple loop the finder can't
  * traverse).
  */
class CanonKeySpec extends AnyFlatSpec with Matchers:

  private val c             = ZetaPoint(2, 0, 0, 0)
  private def sq(x: Int)    = ZetaPoint(x, 0, 0, 0)
  private def prof(vs: PV*) = Profile(c, vs.toVector)

  behavior of "ProfileAutomaton.canonKey — congruence (must hold either way)"

  it should "be reflexive" in {
    val p = prof(PV(sq(0), List((6, 4), (9, 4))), PV(sq(1), List((6, 4), (9, 4))))
    PA.canonKey(p) shouldBe PA.canonKey(p)
  }

  it should "be invariant under translation" in {
    val p       = prof(PV(sq(0), List((6, 4), (9, 3))), PV(sq(1), List((6, 3), (9, 4))))
    val shifted = Profile(c, p.verts.map(v => PV(v.pos + ZetaPoint(3, 0, 5, -2), v.fan)))
    PA.canonKey(shifted) shouldBe PA.canonKey(p)
  }

  it should "be independent of vertex order" in {
    val a = PV(sq(0), List((6, 4), (9, 3)))
    val b = PV(sq(1), List((6, 3), (9, 4)))
    PA.canonKey(prof(a, b)) shouldBe PA.canonKey(prof(b, a))
  }

  behavior of "ProfileAutomaton.canonKey — faithfulness"

  it should "distinguish different fan SIZES" in {
    val triRow = prof(PV(sq(0), List((6, 3), (9, 3))))
    val sqRow  = prof(PV(sq(0), List((6, 4), (9, 4))))
    PA.canonKey(triRow) should not be PA.canonKey(sqRow)
  }

  it should "distinguish the SAME fan sizes in different SLOT arrangements" in {
    // both vertices carry one triangle + one square below, but on opposite sides — geometrically distinct
    val a = prof(PV(sq(0), List((6, 3), (9, 4))))
    val b = prof(PV(sq(0), List((6, 4), (9, 3))))
    PA.canonKey(a) should not be PA.canonKey(b)
  }
