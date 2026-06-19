package io.github.scala_tessella.dcel

/** Driver for the exact-coordinate torus engine ([[KrotenheerdtTorusSearch]], ADR-0020) — the calibration and
  * production runner for OEIS A068600 at n ≥ 2. Prints the candidate-lattice count, then the tilings found
  * grouped by vertex-type composition, with timing.
  *
  * Usage: `runMain io.github.scala_tessella.dcel.KrotenheerdtTorusApp <n> <k> <maxCovolume> [parallelism]
  * [completion]`.
  *
  * A068600 targets (n = 1..7): 11, 20, 39, 33, 15, 10, 7. The torus engine omits the octagon's `4.8.8`, so
  * the n = 1 target it can reach is 10; for n ≥ 2 the octagon never appears, so the full count is in scope —
  * subject to `(k, maxCovolume)` being large enough to reach every cell (the largest are the dodecagon
  * cells).
  */
object KrotenheerdtTorusApp:

  private val published = List(11, 20, 39, 33, 15, 10, 7)

  def main(args: Array[String]): Unit =
    val n           = args(0).toInt
    val k           = args(1).toInt
    val maxCovolume = args(2).toDouble
    val parallelism = args.lift(3).map(_.toInt).getOrElse(Runtime.getRuntime.availableProcessors)
    val completion  = args.lift(4).forall(_.toBoolean)

    val bases  = KrotenheerdtTorusSearch.candidateBasesZeta(k, maxCovolume).size
    val target = published.lift(n - 1).map(_.toString).getOrElse("?")
    println(
      s"n=$n k=$k maxCovol=$maxCovolume parallelism=$parallelism completion=$completion " +
        s"(A068600 target: $target${if n == 1 then " — 10 octagon-free" else ""}); $bases candidate lattices"
    )

    val started = System.nanoTime
    val outcome =
      KrotenheerdtTorusSearch.enumerate(n, k, maxCovolume, parallelism, completion, log = msg => println(msg))
    val seconds = (System.nanoTime - started) / 1e9

    val byComposition =
      outcome.tilings
        .groupBy((types, _) => types.map(_.sorted).toList.sortBy(_.mkString))
        .toList
        .map((comp, ts) => (comp.map(_.mkString(".")).mkString(" , "), ts.size))
        .sortBy((comp, _) => comp)
    println(f"%n=== n=$n: ${outcome.tilings.size} tilings in $seconds%.1f s "
      + s"(${outcome.basesTried} bases, ${outcome.statesExplored} states) — target $target ===")
    byComposition.foreach((comp, count) => println(f"  $count%2d ×  $comp"))
