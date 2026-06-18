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
    val prop      = time("torus PROP (∥4)")(KrotenheerdtTorusSearch.enumerate(n, k, maxCovol, parallelism = 4))
    val poly      = time("torus 1-poly (∥4)")(KrotenheerdtTorusSearch.enumerate(
      n,
      k,
      maxCovol,
      parallelism = 4,
      completion = false
    ))
    val dcel      = time("DCEL (∥4)")(KrotenheerdtLatticeSearch.enumerate(n, k, maxCovol, parallelism = 4))
    val dcelNoOct = dcel.tilings.filterNot((t, _) => t.exists(_.exists(_ == 8)))
    println(s"  PROP : ${prop.tilings.size} tilings, ${prop.basesTried} bases, ${prop.statesExplored} states")
    println(s"  1-POLY ${poly.tilings.size} tilings, ${poly.basesTried} bases, ${poly.statesExplored} states")
    println(
      s"  DCEL : ${dcel.tilings.size} (${dcelNoOct.size} oct-free), ${dcel.basesTried} bases, ${dcel.statesExplored} states"
    )
    val propKeys  = prop.tilings.map(_._2).toSet
    val polyKeys  = poly.tilings.map(_._2).toSet
    val dcelKeys  = dcelNoOct.map(_._2).toSet
    println(s"  PROP keys == 1-POLY keys: ${propKeys ==
        polyKeys}   PROP keys == DCEL oct-free keys: ${propKeys == dcelKeys}")
    if propKeys != dcelKeys then
      def label(ts: List[(Set[VertexSignature], String)], key: String): String =
        ts.find(_._2 == key).map((t, _) => t.map(_.sorted).mkString("+")).getOrElse("?")
      (propKeys -- dcelKeys).foreach(key => println(s"    PROP-ONLY: ${label(prop.tilings, key)}  $key"))
      (dcelKeys -- propKeys).foreach(key => println(s"    DCEL-ONLY: ${label(dcelNoOct, key)}  $key"))

  behavior of "torus vs DCEL timing"

  it should "compare on n=1 small and medium k" in:
    compare(1, 3, 2.6)
    compare(1, 4, 8.0)

  it should "compare on n=2 (the ADR-0019 profiled case)" in:
    compare(2, 4, 6.0)
