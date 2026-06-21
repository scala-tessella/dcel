package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** ADR-0030 n-uniform completion driver harness. Builds the candidate `n`-distinct-type buckets, runs the
  * parallel [[BucketAssembly.enumerateBuckets]] driver, and reports per-bucket coverage/cost and the
  * globally-deduped total against `TilingReference.counts(n)`.
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.N4DriverProbe [n] [maxV] [budgetM] [parallel]`
  */
object N4DriverProbe:

  /** Parse a Wikipedia compact vertex config ("3^2.4.3.4", "3.12^2", "4^4") into a bracelet-normalised sig.
    */
  private[dcel] def parseConfig(token: String): VertexSignature =
    normalize(token.trim.split('.').toList.flatMap { t =>
      val parts = t.split('^')
      if parts.length == 2 then List.fill(parts(1).toInt)(parts(0).toInt)
      else List(t.toInt)
    })

  private[dcel] def parseRow(row: String): Set[VertexSignature] = row.split(';').map(parseConfig).toSet

  // flexible-port types (hexagon/square mixes) drive the largest cells / fastest trees ⇒ slowest buckets;
  // ordering them LAST lets the cheap, productive buckets stream their tilings first.
  private val flexible =
    Set("3.3.6.6", "3.4.4.6", "3.4.6.4").map(s => normalize(s.split('.').map(_.toInt).toList))

  /** Distinct candidate type-sets for `n` (reference rows / n=2 oracle), ordered cheapest-first. */
  private[dcel] def buckets(n: Int): List[Set[VertexSignature]] =
    val raw = n match
      case 2 => TilingReference.n2.distinct
      case _ => TilingReference.rawWikipediaN3to5(n).map(parseRow).distinct
    raw.sortBy(b => b.count(flexible.contains))

  private def label(b: Set[VertexSignature]): String =
    b.map(_.mkString(".")).toList.sorted.mkString("{", ";", "}")

  def main(args: Array[String]): Unit =
    val n       = args.headOption.map(_.toInt).getOrElse(4)
    val maxV    = args.lift(1).map(_.toInt).getOrElse(16)
    val budget  = args.lift(2).map(_.toLong).getOrElse(50L) * 1_000_000L
    val par     = args.lift(3).map(_.toInt).getOrElse(math.max(1, Runtime.getRuntime.availableProcessors - 2))
    val bs      = buckets(n)
    val target  = TilingReference.counts(n)
    println(
      s"n=$n driver: ${bs.size} candidate buckets, maxV=$maxV budget=$budget parallel=$par target=$target"
    )
    val t0      = System.nanoTime()
    val res     = BucketAssembly.enumerateBuckets(
      bs,
      maxV,
      budget,
      par,
      onBucket = (b, r) =>
        val flag = if r.budgetHit then "BUDGET" else "done  "
        println(f"  [$flag] ${label(b)}%-46s found=${r.tilings.size}%2d states=${r.states}%11d ${
            if r.budgetHit then "(partial)" else ""
          }")
    )
    val secs    = (System.nanoTime() - t0) / 1e9
    val byN     = res.tilings.groupBy(_.n).view.mapValues(_.size).toMap
    println(
      f"%n=== n=$n: ${res.count} distinct tilings / target $target in ${secs}%.1fs (states=${res.totalStates}, anyBudgetHit=${res.anyBudgetHit}) ==="
    )
    println(s"by orbit-count n: ${byN.toList.sorted}")
    val missing = bs.filter(b => !res.perBucket.exists((bb, r) => bb == b && r.tilings.nonEmpty))
    if missing.nonEmpty then println(s"buckets with NO tiling found (need higher V/budget?): ${missing.size}")
