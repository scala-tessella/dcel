package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Escalate V for a few failing 2-uniform buckets to find their true minimal cell size (completeness check +
  * the cost-vs-V curve that quantifies the large-cell wall). Runs per-V so we see exactly where each appears.
  */
object BucketEscalateProbe:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  def main(args: Array[String]): Unit =
    val cases = List(
      "{3.4.6.4;3^2.4.3.4}" -> Set("3.4.6.4", "3.3.4.3.4"),
      "{3.4.6.4;3^3.4^2}"   -> Set("3.4.6.4", "3.3.3.4.4"),
      "{3^6;3^2.4.3.4}"     -> Set("3.3.3.3.3.3", "3.3.4.3.4")
    )
    val maxV  = args.headOption.map(_.toInt).getOrElse(9)
    for (name, types) <- cases do
      val bucket = types.map(sig)
      println(s"=== $name ===")
      var v      = 2
      var found  = false
      while v <= maxV && !found do
        val t0 = System.nanoTime()
        // run ONLY this V layer by using a bucket call with maxV=v and subtracting? simpler: full up to v.
        val r  = BucketAssembly.enumerateBucket(bucket, v, 8_000_000L)
        val ms = (System.nanoTime() - t0) / 1000000
        println(
          f"  V<=$v  found=${r.tilings.size}%2d  states=${r.states}%10d  closed=${r.mapsClosed}%7d  budgetHit=${r.budgetHit}  ${ms}ms"
        )
        if r.tilings.nonEmpty then found = true
        v += 1
