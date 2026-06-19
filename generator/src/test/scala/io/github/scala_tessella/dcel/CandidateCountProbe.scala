package io.github.scala_tessella.dcel

import org.scalatest.flatspec.AnyFlatSpec

/** How the candidate-lattice count grows with (k, maxCovolume) — the scaling wall for high n. */
class CandidateCountProbe extends AnyFlatSpec:

  it should "report candidate-lattice counts" in:
    for
      k        <- List(4, 5, 6, 7)
      maxCovol <- List(8.0, 16.0, 24.0, 40.0)
    do
      val t0 = System.nanoTime()
      val c  = KrotenheerdtTorusSearch.candidateBasesZeta(k, maxCovol).size
      val ms = (System.nanoTime() - t0) / 1_000_000
      println(f"  k=$k maxCovol=$maxCovol%5.0f  ->  $c%6d bases  (${ms}%5d ms to enumerate)")
