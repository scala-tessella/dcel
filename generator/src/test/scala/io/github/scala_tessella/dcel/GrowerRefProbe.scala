package io.github.scala_tessella.dcel

/** Characterise `symmetryRotationReference`'s cost/reach at a given maxFaces (per-seed live progress), so we
  * can pick a maxFaces that COMPLETES single-threaded yet reaches the large rotational n=2 cells.
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.GrowerRefProbe [n] [maxFaces]`
  */
object GrowerRefProbe:

  def main(args: Array[String]): Unit =
    val n        = args.headOption.map(_.toInt).getOrElse(2)
    val maxFaces = args.lift(1).map(_.toInt).getOrElse(36)
    val t0       = System.nanoTime()
    def secs     = (System.nanoTime() - t0) / 1e9
    println(s"symmetryRotationReference(n=$n, maxFaces=$maxFaces) — per-seed progress:")
    val ref      = KrotenheerdtTorusMapSearch.symmetryRotationReference(
      maxN = n,
      maxFaces = maxFaces,
      log = msg => println(f"  [${secs}%6.0fs] $msg")
    )
    println(f"\n=== reached ${ref.size} distinct n≤$n tilings in ${secs}%.0fs ===")
    ref.toList
      .map((_, v) => (v._1.map(_.mkString(".")).toList.sorted.mkString("; "), v._2, v._3))
      .sortBy(_._1)
      .foreach((lbl, centres, seed) => println(f"$lbl%-40s | ${centres.toList.sorted}%-40s | via $seed"))
    println("\n[done]")
