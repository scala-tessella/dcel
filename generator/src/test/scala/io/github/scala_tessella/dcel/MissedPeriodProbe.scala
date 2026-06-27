package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import io.github.scala_tessella.dcel.geometry.BigPoint

/** DE-RISK ADR-0038's `Hmax` assumption: measure the **shortest translation period** `|h|` (the band period =
  * cylinder circumference) of every grower-MISSED n=3 banded cell. The profile-automaton sweeps `h` over
  * horizontal translations `|h| ≤ Hmax`; this confirms the missed cells actually live at small `|h|`.
  *
  * For each of the 4 gap type-sets: oracle cells (authority), the grower's reached keys, and bounded-V's `op`
  * per cell → `realizeCell(op)` → exact lattice Λ=(vB,wB). The shortest non-zero lattice vector (brute force
  * over small integer combinations) is `|h|`; we also report the long/short aspect ratio (anisotropy).
  *
  * Run: `…MissedPeriodProbe [oracleMaxSize] [growerMaxFaces] [boundedVmaxV] [growerMillis]`
  */
object MissedPeriodProbe:
  private def ts(s: String): Set[VertexSignature] =
    s.split(',').toList.map(t => normalize(t.split('.').map(_.toInt).toList)).toSet

  private val gaps = List(
    ts("3.3.3.3.3.3,3.3.3.3.6,3.3.6.6"),
    ts("3.3.3.3.3.3,3.3.3.4.4,4.4.4.4"),
    ts("3.3.6.6,3.6.3.6,6.6.6"),
    ts("3.4.4.6,3.6.3.6,4.4.4.4")
  )

  /** Shortest non-zero lattice vector length and the longest "useful" one (the reduced basis), by brute force
    * over small integer combinations of the cell basis — Λ is rank 2 so [-6..6]² is ample.
    */
  private def latticeShape(vB: BigPoint, wB: BigPoint): (Double, Double, Double) =
    val (vx, vy) = (vB.x.toDouble, vB.y.toDouble)
    val (wx, wy) = (wB.x.toDouble, wB.y.toDouble)
    val covol    = math.abs(vx * wy - vy * wx)
    var shortest = Double.MaxValue
    for m <- -6 to 6; n <- -6 to 6 if !(m == 0 && n == 0) do
      val (x, y) = (m * vx + n * wx, m * vy + n * wy)
      val len    = math.hypot(x, y)
      if len > 1e-6 && len < shortest then shortest = len
    // the orthogonal extent = covolume / shortest (the band height direction)
    (shortest, covol / shortest, covol)

  def main(args: Array[String]): Unit =
    val oracleSize = args.headOption.map(_.toInt).getOrElse(24)
    val maxFaces   = args.lift(1).map(_.toInt).getOrElse(64)
    val maxV       = args.lift(2).map(_.toInt).getOrElse(16)
    val millis     = args.lift(3).map(_.toLong).getOrElse(120000L)
    println(s"MissedPeriodProbe: oracle=$oracleSize grower maxFaces=$maxFaces boundedV maxV=$maxV")

    val oracle = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = 12)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values.map(_.head).toList.filter(_._1 == 3)

    val rows = scala.collection.mutable.ListBuffer.empty[(String, String, String, Double, Double, Double)]
    for t <- gaps do
      val cells  = oracle.filter(_._2.toSet == t)
      val grower = KrotenheerdtTorusMapSearch
        .symmetryRotationReferenceParallel(3, maxFaces, parallelism = 12, maxMillis = millis, targetTypes = t)
        .filter(_._2._1 == t).keySet
      val br     = BucketAssembly.enumerateBucket(t, maxV, targetCount = cells.size)
      val tLabel = t.map(_.mkString(".")).toList.sorted.mkString("; ")
      for c <- cells do
        val key = DelaneySymbols.canonicalKey(c._3)
        val got = grower.contains(key)
        val orb = DelaneySymbols.orbifoldSignature(c._3)
        br.ops.get(key).flatMap(op => KrotenheerdtTorusMapSearch.realizeCell(op)) match
          case Some((_, vB, wB)) =>
            val (h, height, covol) = latticeShape(vB, wB)
            rows += ((if got then "ok" else "MISSED", tLabel, orb, h, height, covol))
          case None              =>
            rows += ((if got then "ok" else "MISSED-NOGEO", tLabel, orb, Double.NaN, Double.NaN, Double.NaN))

    println(
      f"\n${"mark"}%-13s ${"orbifold"}%-12s ${"|h| (band)"}%-11s ${"height"}%-9s ${"covol"}%-7s  type-set"
    )
    rows.sortBy(r => (r._1, r._4)).foreach: (mark, tl, orb, h, height, covol) =>
      val hs = if h.isNaN then "    —" else f"$h%9.3f"
      val ht = if height.isNaN then "   —" else f"$height%7.2f"
      val cv = if covol.isNaN then "  —" else f"$covol%5.1f"
      println(f"$mark%-13s $orb%-12s $hs   $ht  $cv   $tl")

    val missed = rows.filter(_._1.startsWith("MISSED"))
    val geo    = missed.filterNot(_._4.isNaN)
    println(s"\n=== ${missed.size} grower-missed cells; ${geo.size} with geometry ===")
    if geo.nonEmpty then
      println(
        f"   shortest |h| among missed = ${geo.map(_._4).min}%.3f   longest |h| among missed = ${geo.map(_._4).max}%.3f"
      )
      println(f"   all missed |h| ≤ 4 ? ${geo.forall(_._4 <= 4.0 + 1e-6)}")
      println(f"   max anisotropy (height/|h|) among missed = ${geo.map(r => r._5 / r._4).max}%.2f")
    if missed.exists(_._4.isNaN) then
      println(
        s"   NOTE: ${missed.count(_._4.isNaN)} missed cell(s) had no bounded-V geometry (period unmeasured here)"
      )
    println("[done]")
