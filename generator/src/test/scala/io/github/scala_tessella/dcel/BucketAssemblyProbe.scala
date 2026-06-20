package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Throwaway exploration of the ADR-0025 bounded-V dart assembler — prints minimal V, state counts and keys
  * so the spec can assert real numbers. Run: `generator/Test/runMain ...BucketAssemblyProbe`.
  */
object BucketAssemblyProbe:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  def main(args: Array[String]): Unit =
    val oracleSize = 14
    println(s"--- oracle keyedTilings(1, $oracleSize) ---")
    val oracle     = DelaneySymbols.keyedTilings(1, oracleSize)
    oracle.foreach((n, ts, key) => println(s"  n=$n types=${ts.map(_.mkString("."))} key=${key.take(40)}..."))

    def probe(name: String, types: Set[String], maxV: Int): Unit =
      val bucket = types.map(sig)
      val t0     = System.nanoTime()
      val r      = BucketAssembly.enumerateBucket(bucket, maxV, stateBudget = 5_000_000L)
      val ms     = (System.nanoTime() - t0) / 1000000
      println(
        f"$name%-28s maxV=$maxV  tilings=${r.tilings.size}%2d  states=${r.states}%9d  expanded=${r.expanded}%8d  mapsClosed=${r.mapsClosed}%7d  budgetHit=${r.budgetHit}  ${ms}ms"
      )
      r.tilings.foreach(f =>
        println(s"      n=${f.n} types=${f.types.map(_.mkString("."))} key=${f.key.take(40)}...")
      )

    println("--- k=1 reproduction ---")
    probe("4.4.4.4 (square)", Set("4.4.4.4"), 2)
    probe("3^6 (triangular)", Set("3.3.3.3.3.3"), 2)
    probe("6.6.6 (hexagonal)", Set("6.6.6"), 3)
    probe("4.8.8 (trunc square)", Set("4.8.8"), 4)
    probe("3.6.3.6 (trihex)", Set("3.6.3.6"), 4)
    probe("3.4.6.4 (rhombitrihex)", Set("3.4.6.4"), 6)

    println("--- k=1 soundness (angle-valid non-tilings ⇒ 0) ---")
    probe("3.3.6.6 (NON-tiling)", Set("3.3.6.6"), 4)
    probe("3.4.4.6 (NON-tiling)", Set("3.4.4.6"), 4)

    println("--- k=2 ---")
    probe("{4^4, 3^3.4^2}", Set("4.4.4.4", "3.3.3.4.4"), 4)
    probe("{3^6, 3^2.4.3.4}", Set("3.3.3.3.3.3", "3.3.4.3.4"), 4)
    probe("{3^6, 3^4.6} V<=5", Set("3.3.3.3.3.3", "3.3.3.3.6"), 5)
    probe("{3^6, 3^4.6} V<=6", Set("3.3.3.3.3.3", "3.3.3.3.6"), 6)

    println("--- k=3 gate (n=3) ---")
    probe("{3.4.6.4,3.6.3.6,4.6.12}", Set("3.4.6.4", "3.6.3.6", "4.6.12"), 6)
    probe("{3^6,3^2.4.3.4,3.4.6.4}", Set("3.3.3.3.3.3", "3.3.4.3.4", "3.4.6.4"), 6)
