package io.github.scala_tessella.dcel

/** Combined all-n sweep over the torus engine ([[KrotenheerdtTorusSearch.enumerateCombined]], ADR-0020): one
  * search that yields every Krotenheerdt tiling with `n ≤ maxN` at once, bucketed by `n` — the expensive
  * near-miss/lattice work is paid once instead of once per `n`, which is the lever for reaching the whole
  * A068600 table (11, 20, 39, 33, 15, 10, 7 for n = 1..7) in a single run.
  *
  * Usage: `runMain io.github.scala_tessella.dcel.KrotenheerdtTorusSweepApp <maxN> <k> <maxCovolume>
  * [parallelism]`. The torus engine omits the octagon's `4.8.8`, so the n = 1 figure it can reach is 10.
  */
object KrotenheerdtTorusSweepApp:

  private val published = List(11, 20, 39, 33, 15, 10, 7)

  def main(args: Array[String]): Unit =
    val maxN        = args(0).toInt
    val k           = args(1).toInt
    val maxCovolume = args(2).toDouble
    val parallelism =
      args.lift(3).map(_.toInt).getOrElse(math.max(1, Runtime.getRuntime.availableProcessors / 2))

    val bases = KrotenheerdtTorusSearch.candidateBasesZeta(k, maxCovolume).size
    println(
      s"combined sweep n≤$maxN k=$k maxCovol=$maxCovolume parallelism=$parallelism; $bases candidate lattices"
    )

    val started = System.nanoTime
    val tilings =
      KrotenheerdtTorusSearch.enumerateCombined(maxN, k, maxCovolume, parallelism, msg => println(msg))
    val seconds = (System.nanoTime - started) / 1e9
    val byN     = tilings.groupBy(_._1).view.mapValues(_.size).toMap

    println(f"%n=== combined sweep: ${tilings.size} tilings in $seconds%.1f s ===")
    println("  n | found | A068600 | note")
    for n <- 1 to maxN do
      val found  = byN.getOrElse(n, 0)
      val target = published.lift(n - 1).map(_.toString).getOrElse("0")
      val note   = if n == 1 then "(torus omits octagon 4.8.8 → 10)" else ""
      println(f"  $n%d | $found%5d | $target%7s | $note")
