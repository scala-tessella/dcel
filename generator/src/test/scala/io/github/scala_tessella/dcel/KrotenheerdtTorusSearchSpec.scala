package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Validation of the exact-coordinate torus engine ([[KrotenheerdtTorusSearch]], ADR-0019 Option B prototype)
  * against the proven DCEL fixed-Λ engine ([[KrotenheerdtLatticeSearch]]).
  */
class KrotenheerdtTorusSearchSpec extends AnyFlatSpec with Matchers:

  private def torus(n: Int, k: Int, maxCovol: Double): List[Set[VertexSignature]] =
    KrotenheerdtTorusSearch.enumerate(n, k, maxCovol).tilings.map(_._1)

  behavior of "KrotenheerdtTorusSearch (exact-coordinate fixed-Λ engine)"

  it should "enumerate exactly the four small-cell 1-uniform tilings, incl. the multi-vertex 6.6.6" in:
    val found = torus(1, 3, 2.6)
    found.map(_.map(_.sorted)).toSet shouldBe Set(
      Set(List(3, 3, 3, 3, 3, 3)),
      Set(List(4, 4, 4, 4)),
      Set(List(3, 3, 3, 4, 4)),
      Set(List(6, 6, 6))
    )
    found.size shouldBe 4

  it should "agree with the DCEL engine on the torus keys for the small n=1 cells" in:
    val torusKeys = KrotenheerdtTorusSearch.enumerate(1, 3, 2.6).tilings.map(_._2).toSet
    val dcelKeys  = KrotenheerdtLatticeSearch
      .enumerate(1, 3, 2.6, parallelism = 4)
      .tilings
      // the DCEL engine also reaches 4.8.8 (octagon) at this covolume; the torus engine excludes it by design
      .filterNot((types, _) => types.exists(_.contains(8)))
      .map(_._2)
      .toSet
    torusKeys shouldBe dcelKeys
