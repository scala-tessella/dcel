package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Phase-1 measurement (scratch): pin the exact cross-engine reachability/keys used by `CrossEngineSpec`, so
  * the spec asserts measured facts (fast + correct) rather than guesses. Prints, for n = 1 and the chiral n =
  * 2 type-sets, what each surviving engine produces (oracle / oriented-slice / bounded-V bucket) in the
  * SHARED canonical-key space.
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.CrossEngineMeasure`
  */
object CrossEngineMeasure:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  // the n=2 type-sets containing an oriented-CHIRAL vertex type (3.4.4.6 / 3.3.4.12 / 3.4.3.12) — the chirality
  // axis the bug lived on; cross-engine agreement here is the generalized chirality regression.
  private val chiralN2: List[Set[String]] = List(
    Set("3.4.6.4", "3.4.4.6"),
    Set("3.4.4.6", "3.6.3.6"),
    Set("3.3.3.3.3.3", "3.3.4.12"),
    Set("3.12.12", "3.4.3.12")
  )

  def main(args: Array[String]): Unit =
    val t0 = System.nanoTime()

    println("=== ORACLE keyedTilings(2, 14) ===")
    val oracle   = DelaneySymbols.keyedTilings(2, 14)
    val oracleN1 = oracle.filter(_._1 == 1)
    println(s"n=1: ${oracleN1.size} keys")
    val oracleN2 = oracle.filter(_._1 == 2)
    println(s"n=2: ${oracleN2.size} keys")
    chiralN2.foreach: set =>
      val t  = set.map(sig)
      val ks = oracleN2.filter(_._2 == t).map(_._3).toSet
      println(s"  oracle $set -> ${ks.size} keys")

    println("\n=== ORIENTED-SLICE orientedRegularSymbols(2, 26) ===")
    val ori = DelaneySymbols.orientedRegularSymbols(2, 26)
    println(s"n=1: ${ori.count(_._1 == 1)}  n=2: ${ori.count(_._1 == 2)}")
    chiralN2.foreach: set =>
      val t  = set.map(sig)
      val ks = ori.filter(r => r._1 == 2 && r._2.toSet == t).map(_._3).toSet
      println(s"  oriented $set -> ${ks.size} keys  $ks")

    println("\n=== BOUNDED-V buckets (escalate maxV) ===")
    chiralN2.foreach: set =>
      val b    = set.map(sig)
      // escalate to find the minimal V where keys appear; cap to keep it fast
      var v    = b.size
      var done = false
      while v <= 8 && !done do
        val r = BucketAssembly.enumerateBucket(b, maxV = v, stateBudget = 30_000_000L)
        if r.keys.nonEmpty || r.budgetHit then
          println(
            s"  bucket $set V=$v -> ${r.keys.size} keys (states=${r.states}, budgetHit=${r.budgetHit}) ${r.keys}"
          )
        if r.keys.nonEmpty then done = true
        v += 1
      if !done then println(s"  bucket $set -> NONE through V=8")

    println(f"\n[done in ${(System.nanoTime() - t0) / 1e9}%.1fs]")
