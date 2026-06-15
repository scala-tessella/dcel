package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Soundness regression for the fixed-Λ engine (ADR-0019). These runs are small (low `maxCovolume`), so they
  * stay fast while exercising the parts that were hard to get right: multi-vertex cells, sublattice and
  * chirality deduplication, and — above all — the exact vertex-orbit count that replaced 1-WL colour
  * refinement. Each tiling reported must be a genuine Krotenheerdt tiling with the correct multiplicity.
  */
class KrotenheerdtLatticeSearchSpec extends AnyFlatSpec with Matchers:

  private def compositions(n: Int, k: Int, maxCovol: Double): List[Set[VertexSignature]] =
    KrotenheerdtLatticeSearch.enumerate(n, k, maxCovol, parallelism = 4).tilings.map(_._1)

  behavior of "KrotenheerdtLatticeSearch (fixed-Λ engine)"

  it should "enumerate exactly the four small-cell 1-uniform tilings, incl. the multi-vertex 6.6.6" in:
    // 3⁶ (covol 0.866), 4⁴ (1.0), 3³.4² (1.866), 6.6.6 (2.598). The hexagonal cell has two vertices per cell,
    // so finding it at all exercises the union fan reconstruction; the triangle lattice's index-2/3
    // sublattices exercise primitive-basis deduplication (each must collapse to one key).
    val found = compositions(1, 3, 2.6)
    found.map(_.map(_.sorted)).toSet shouldBe Set(
      Set(List(3, 3, 3, 3, 3, 3)),
      Set(List(4, 4, 4, 4)),
      Set(List(3, 3, 3, 4, 4)),
      Set(List(6, 6, 6))
    )
    found.size shouldBe 4

  it should "reject the 3.3.3.3.6 / 3.6.3.6 pseudo-tiling (3 orbits, not 2)" in:
    // This cell (four 3.3.3.3.6 vertices + one 3.6.3.6) has two distinct 3.3.3.3.6 orbits, so it is 3-uniform
    // and absent from the published 20. 1-WL colour refinement merges the four into one class and wrongly
    // passes it as 2-uniform; the exact orbit count must keep it out. A genuine 2-uniform tiling of comparable
    // size must still appear, so the run is not silently empty.
    val found    = compositions(2, 4, 6.0)
    val spurious = Set(List(3, 3, 3, 3, 6), List(3, 6, 3, 6))
    found.map(_.map(_.sorted)) should not contain spurious
    found.map(_.map(_.sorted)) should contain(Set(List(3, 3, 3, 3, 3, 3), List(3, 3, 3, 4, 4)))
