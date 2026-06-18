package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Head-to-head wall-clock: the exact-coordinate torus engine vs the DCEL fixed-Λ engine, on the same (n, k,
  * maxCovol). Not an assertion-heavy spec — it prints timings and the (octagon-free) key agreement so the
  * ADR-0019 Option-B speedup thesis can be judged. Run with: sbt "generator/testOnly *TorusTimingProbe"
  */
class TorusTimingProbe extends AnyFlatSpec with Matchers:

  private def comps(ts: List[(Set[VertexSignature], String)]): Set[Set[List[Int]]] =
    ts.map(_._1.map(_.sorted)).toSet

  private def time[A](label: String)(body: => A): A =
    val t0 = System.nanoTime()
    val a  = body
    val ms = (System.nanoTime() - t0) / 1_000_000
    println(f"  [$label%-22s] ${ms}%6d ms")
    a

  private def compare(n: Int, k: Int, maxCovol: Double): Unit =
    println(s"=== n=$n k=$k maxCovol=$maxCovol ===")
    val torus     = time("torus (parallel 4)")(KrotenheerdtTorusSearch.enumerate(n, k, maxCovol, parallelism = 4))
    val dcel      = time("DCEL (parallel 4)")(KrotenheerdtLatticeSearch.enumerate(n, k, maxCovol, parallelism = 4))
    val dcelNoOct = dcel.tilings.filterNot((t, _) => t.exists(_.exists(_ == 8)))
    println(
      s"  torus: ${torus.tilings.size} tilings, ${torus.basesTried} bases, ${torus.statesExplored} states"
    )
    println(
      s"  DCEL : ${dcel.tilings.size} tilings (${dcelNoOct.size} octagon-free), ${dcel.basesTried} bases, ${dcel.statesExplored} states"
    )
    val torusKeys = torus.tilings.map(_._2).toSet
    val dcelKeys  = dcelNoOct.map(_._2).toSet
    val agree     = torusKeys == dcelKeys
    println(s"  torus keys == DCEL octagon-free keys: $agree")
    if !agree then
      def label(ts: List[(Set[VertexSignature], String)], key: String): String =
        ts.find(_._2 == key).map((t, _) => t.map(_.sorted).mkString("+")).getOrElse("?")
      (torusKeys -- dcelKeys).foreach(key => println(s"    torus-ONLY: ${label(torus.tilings, key)}  $key"))
      (dcelKeys -- torusKeys).foreach(key => println(s"    DCEL-ONLY : ${label(dcelNoOct, key)}  $key"))

  behavior of "torus vs DCEL timing"

  it should "compare on n=1 small and medium k" in:
    compare(1, 3, 2.6)
    compare(1, 4, 8.0)

  it should "compare on n=2 (the ADR-0019 profiled case)" in:
    compare(2, 4, 6.0)
