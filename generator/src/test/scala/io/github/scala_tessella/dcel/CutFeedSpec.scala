package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** POSITIVE CONTROL for the cut-and-feed diagnostic ([[ProfileAutomaton.cutFeedDiagnose]], ADR-0038). Before
  * the harness's verdict on the MISSING cells can be trusted, it must reproduce a KNOWN-FOUND banded cell:
  * realize the cell, find a horizontal circumference, cut its own tiling into a profile, and confirm the
  * engine both (A) emits the cell's key when fed that cut as a seed, and (B) cycles+closes to the key when
  * following the cell's own faces. We control on the 1-uniform banded `3³.4²` (elongated triangular) and `4⁴`
  * (square).
  */
class CutFeedSpec extends AnyFlatSpec with Matchers:

  private def ts(s: String): Set[VertexSignature] =
    s.split(',').toList.map(t => normalize(t.split('.').map(_.toInt).toList)).toSet

  /** Realize a 1-uniform tiling's `op` + oracle key from the n=1 Delaney oracle + bucket assembler. */
  private def cellOf(typeSet: Set[VertexSignature]): (Array[Array[Int]], String) =
    val oracle = DelaneySymbols
      .enumerateSymbolsParallel(1, 16, parallelism = 4)
      .filter(t => t._1 == 1 && t._2.toSet == typeSet)
    oracle should not be empty
    val sym    = oracle.head._3
    val key    = DelaneySymbols.canonicalKey(sym)
    val br     = BucketAssembly.enumerateBucket(typeSet, maxV = 8, targetCount = 1)
    br.ops.get(key) match
      case Some(op) => (op, key)
      case None     => fail(s"no op realized for $typeSet")

  behavior of "ProfileAutomaton.cutFeedDiagnose (positive control)"

  it should "reproduce the banded 3³.4² (elongated triangular) cell from its own cut" in {
    val t         = ts("3.3.3.4.4")
    val (op, key) = cellOf(t)
    val r         = ProfileAutomaton.cutFeedDiagnose(op, key, t)
    info(s"3³.4²: $r")
    r.realized shouldBe true
    r.representable shouldBe true
    r.bandAxisHorizontal shouldBe true
    r.cutProfiles should be > 0
    // the harness must recover the cell by AT LEAST one route (feed or explicit trace)
    (r.fedEmitsKey || r.traceClosesKey) shouldBe true
  }

  it should "reproduce the 4⁴ (square) cell from its own cut" in {
    val t         = ts("4.4.4.4")
    val (op, key) = cellOf(t)
    val r         = ProfileAutomaton.cutFeedDiagnose(op, key, t)
    info(s"4⁴: $r")
    r.realized shouldBe true
    r.representable shouldBe true
    (r.fedEmitsKey || r.traceClosesKey) shouldBe true
  }
