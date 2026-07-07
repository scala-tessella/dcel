package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.SymbolAssembly.*
import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The ADR-0041 generalization (m-Archimedean n-uniform, type MULTISETS), gated on Čtrnáct/Galebach's known
  * cells (validation oracle only): structural regressions first (the Krotenheerdt case is unchanged; the m=1
  * column is zero beyond n=1 — Krötenheerdt's own theorem), then the first ladder cells (3,2)=22, (4,2)=33,
  * (4,3)=85. The heavier rows run in `CellGateProbe`.
  */
class MultisetAssemblySpec extends AnyFlatSpec with Matchers:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  private def cellCount(n: Int, m: Int): Int =
    val cells = solveCell(n, m, parallelism = 4)
    cells.values.exists(_.capped) shouldBe false
    cells.values.map(_.keys.size).sum

  behavior of "structural regressions"

  it should "reduce to the Krotenheerdt solver on all-distinct multisets (same keys)" in:
    val ts = Set(sig("4.4.4.4"), sig("3.3.3.4.4"))
    solveMultiset(ts.toList).keys shouldBe solveTypeSet(ts).keys

  it should "derive Krötenheerdt's theorem: no n-uniform 1-Archimedean tilings for n > 1" in:
    cellCount(2, 1) shouldBe 0
    cellCount(3, 1) shouldBe 0

  it should "reproduce the diagonal as the Krotenheerdt cells" in:
    cellCount(2, 2) shouldBe 20
    cellCount(3, 3) shouldBe 39

  behavior of "the gate ladder (known off-diagonal cells, count-exact)"

  it should "reproduce (3,2) = 22" in:
    cellCount(3, 2) shouldBe 22

  it should "reproduce (4,2) = 33" in:
    cellCount(4, 2) shouldBe 33

  it should "reproduce (4,3) = 85" in:
    cellCount(4, 3) shouldBe 85
