package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature

/** Phase-2 COVERAGE probe: run the full seed catalogue ([[KrotenheerdtTorusMapSearch.enumerateAllSeeds]]) and
  * report which tilings it reaches, by vertex-type-set, against A068600 counts — plus per-seed states/time so
  * the per-state-cost concern is quantified. Cross-seed dedup is automatic (shared content key).
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.SymmetryCoverageProbe [maxN] [maxFaces]`
  */
object SymmetryCoverageProbe:

  def main(args: Array[String]): Unit =
    val maxN     = args.headOption.map(_.toInt).getOrElse(1)
    val maxFaces = args.lift(1).map(_.toInt).getOrElse(40)
    val par      = args.lift(2).map(_.toInt).getOrElse(math.max(1, Runtime.getRuntime.availableProcessors - 1))
    println(
      s"enumerateAllSeedsParallel(maxN=$maxN, maxFaces=$maxFaces, par=$par) over the full seed catalogue ..."
    )

    val t0   = System.nanoTime()
    val res  = KrotenheerdtTorusMapSearch.enumerateAllSeedsParallel(
      maxN = maxN,
      maxFaces = maxFaces,
      parallelism = par,
      log = println, // live daemon heartbeat every 10s: elapsed / seeds-done / states+rate / faces / tilings
      logEveryMs = 10000L
    )
    val secs = (System.nanoTime() - t0) / 1e9

    println(
      f"\n=== ${res.tilings.size} tilings, totalStates=${res.states}, budgetHit=${res.budgetHit}, ${secs}%.1fs ==="
    )
    val byN = res.tilings.groupBy(_._1)
    for n <- 1 to maxN do
      val ts = byN.getOrElse(n, Nil)
      println(s"n=$n: ${ts.size}/${TilingReference.counts(n)}")
      ts.map(_._2.map(_.mkString(".")).toList.sorted.mkString("; ")).sorted.foreach(s => println(s"    $s"))
    println("\n[coverage probe complete]")
