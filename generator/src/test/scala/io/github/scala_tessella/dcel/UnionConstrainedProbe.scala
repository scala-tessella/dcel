package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature

/** TRUE per-level count via the UNION (by shared D-symbol key) of bounded-V and the TYPE-SET-CONSTRAINED
  * symmetry grower, per candidate type-set. bounded-V reaches the small cells, the constrained grower the
  * large rotational ones; both key in the same space (ADR-0032), so the per-type-set union is exact. Reports,
  * per type-set, bounded-V / grower / union counts vs multiplicity, and the grand total vs `TilingReference`.
  *
  * Run: `…UnionConstrainedProbe [n] [maxV] [maxFaces] [perSetMinutes]` (default n=4, V=12, maxFaces=80, 6
  * min)
  */
object UnionConstrainedProbe:

  private def label(ts: Set[VertexSignature]): String =
    ts.map(_.mkString(".")).toList.sorted.mkString("; ")

  def main(args: Array[String]): Unit =
    val n        = args.headOption.map(_.toInt).getOrElse(4)
    val maxV     = args.lift(1).map(_.toInt).getOrElse(12)
    val maxFaces = args.lift(2).map(_.toInt).getOrElse(80)
    val perSetMs = args.lift(3).map(_.toLong).getOrElse(6L) * 60000L
    val sets     = UnionDriver.candidateTypeSets(n)
    val t0       = System.nanoTime()
    def secs     = (System.nanoTime() - t0) / 1e9
    var total    = 0
    println(
      s"[union constrained] n=$n maxV=$maxV maxFaces=$maxFaces perSet=${perSetMs / 60000}min — ${sets.size} type-sets"
    )
    for (ts, i) <- sets.zipWithIndex do
      val mult = UnionDriver.multiplicity(n, ts)
      val bV   = BucketAssembly.enumerateBucket(ts, maxV, targetCount = mult).keys.toSet
      val gr   = KrotenheerdtTorusMapSearch
        .symmetryRotationReferenceParallel(
          maxN = n,
          maxFaces = maxFaces,
          maxMillis = perSetMs,
          targetTypes = ts
        )
        .filter(_._2._1 == ts)
        .keySet
      val u    = bV ++ gr
      total += u.size
      println(
        f"  [${secs}%6.0fs] ${i + 1}%2d/${sets.size} ${label(ts)}%-46s bV=${bV.size} grower=${gr.size} UNION=${u.size}/$mult"
      )
    val expected = TilingReference.counts(n)
    println(f"\n=== n=$n TRUE UNION (bounded-V ∪ constrained grower): $total/$expected, ${secs}%.0fs ===")
    if expected - total > 0 then
      println(s"(${expected - total} still short — neither engine reaches them at this config)")
    println("\n[done]")
