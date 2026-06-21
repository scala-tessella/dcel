package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Phase-1 BENCHMARK (the per-engine baseline the audit called for): one place that times the surviving sound
  * engines on this hardware and prints states/dsets + wall-clock, so cost claims in the ADRs are reproducible
  * rather than scattered. Modest sizes by default (finishes in a few minutes); pass a scale arg to push.
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.EngineBenchmark [oriSizeMax]`
  */
object EngineBenchmark:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  private def timed[A](label: String)(body: => A): A =
    val t0 = System.nanoTime()
    val r  = body
    val ms = (System.nanoTime() - t0) / 1e6
    println(f"  $label%-58s ${ms / 1000}%8.2fs")
    r

  private def mem(): Long =
    val rt = Runtime.getRuntime
    rt.gc(); rt.totalMemory() - rt.freeMemory()

  def main(args: Array[String]): Unit =
    val oriMax = args.headOption.map(_.toInt).getOrElse(28)
    val par    = math.max(1, Runtime.getRuntime.availableProcessors - 1)
    println(s"cores=${Runtime.getRuntime.availableProcessors}  parallelism=$par  oriMax=$oriMax\n")

    println("== generate-all D-symbol oracle (DelaneySymbols.generationStats) ==")
    for s <- List(12, 14, 16) do
      timed(s"generationStats(maxN=3, maxSize=$s)"):
        val (total, eucl, reg) = DelaneySymbols.generationStats(3, s)
        print(f"    dsets=$total%-9d euclidean=$eucl%-7d regular=$reg   ")

    println("\n== oriented-slice generator (sequential) ==")
    for s <- List(20, 24, oriMax) do
      timed(s"orientedGenerationStats(maxN=4, maxSize=$s)"):
        val (total, eucl, reg) = DelaneySymbols.orientedGenerationStats(4, s)
        print(f"    dsets=$total%-9d euclidean=$eucl%-7d regular=$reg   ")

    println("\n== oriented-slice generator (parallel, per-n recovery) ==")
    timed(s"orientedRegularSymbolsParallel(maxN=7, maxSize=$oriMax, par=$par)"):
      val res = DelaneySymbols.orientedRegularSymbolsParallel(7, oriMax, par)
      val byN = res.groupBy(_._1).view.mapValues(_.size).toMap
      print("    n→count: " + (1 to 7).map(n =>
        s"$n=${byN.getOrElse(n, 0)}/${TilingReference.counts(n)}"
      ).mkString(" ") + "   ")

    println("\n== bounded-V dart assembly (representative buckets) ==")
    val buckets: List[(String, Set[VertexSignature], Int)] = List(
      ("n1 4.6.12 (chiral, cell V=12)", Set(sig("4.6.12")), 12),
      ("n1 3.4.6.4 (cell V=6)", Set(sig("3.4.6.4")), 6),
      ("n2 {4^4;3^3.4^2} mult-2 (V=4)", Set(sig("4.4.4.4"), sig("3.3.3.4.4")), 4),
      ("n2 {3.4^2.6;3.6.3.6} chiral mult-2 (V=5)", Set(sig("3.4.4.6"), sig("3.6.3.6")), 5)
    )
    val m0                                                 = mem()
    for (name, b, v) <- buckets do
      timed(name):
        val r = BucketAssembly.enumerateBucket(b, maxV = v)
        print(
          f"    keys=${r.keys.size}%-3d states=${r.states}%-10d mapsClosed=${r.mapsClosed}%-6d budgetHit=${r.budgetHit}   "
        )
    println(f"  [bucket heap delta ≈ ${(mem() - m0) / 1e6}%.0f MB]")

    println("\n[benchmark complete]")
