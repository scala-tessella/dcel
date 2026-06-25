package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.KrotenheerdtTorusMapSearch.FaceZ
import io.github.scala_tessella.dcel.VertexTypes.VertexSignature
import io.github.scala_tessella.dcel.geometry.BigPoint

/** CHARACTERIZE the "banded" family the user spotted (single-direction strips ⇒ C₂-max) and test how cleanly
  * it explains the grower's n=3 gap. For every n=3 oracle tiling (the authority): (a) `maxConeOrder` from the
  * D-symbol (2 = C₂-max, the banded symmetry signature) — available for ALL; (b) a GEOMETRIC fault-line test
  * (a straight line of ≥ several collinear edges spanning the replicated patch = a band boundary), realized
  * via bounded-V op → realizeCell — available where bounded-V reaches the cell; (c) whether the GROWER
  * reaches it. Reports family size + cross-tabs (reached/missed × banded/not, × C₂/higher) so we see, with
  * data, whether a strip-stacking enumerator is worth building.
  *
  * Run: `…CharacterizeBandedProbe [oracleMaxSize] [growerMaxFaces] [boundedVmaxV] [growerPerSetSec]`
  */
object CharacterizeBandedProbe:
  private def label(t: Set[VertexSignature]): String = t.map(_.mkString(".")).toList.sorted.mkString("; ")

  /** A fault line = ≥ `minRun` collinear unit edges end-to-end (a band boundary). Replicates the cell 5×5 so
    * lines are long enough to measure; checks each of the 6 lattice directions. Heuristic (Double), for
    * classification only.
    */
  private def isBanded(faces: List[FaceZ], vB: BigPoint, wB: BigPoint): Boolean =
    val (vx, vy) = (vB.x.toDouble, vB.y.toDouble)
    val (wx, wy) = (wB.x.toDouble, wB.y.toDouble)
    val polys    = faces.map(_.corners.toList.map { c =>
      val p = c.toBigPoint; (p.x.toDouble, p.y.toDouble)
    })
    val segs     =
      for
        i <- -2 to 2; j <- -2 to 2; poly <- polys; k <- poly.indices
      yield
        val (ax, ay) = poly(k); val (bx, by) = poly((k + 1) % poly.size)
        val (tx, ty) = (i * vx + j * wx, i * vy + j * wy)
        ((ax + tx, ay + ty), (bx + tx, by + ty))
    val minRun   = 4.0 // unit edges; a fault line spans the patch, ordinary tilings turn at every vertex
    (0 until 6).exists: s =>
      val (ux, uy) = (math.cos(math.toRadians(30 * s)), math.sin(math.toRadians(30 * s)))
      val (px, py) = (-uy, ux)
      val parallel = segs.filter: (a, b) =>
        val (dx, dy) = (b._1 - a._1, b._2 - a._2); val len = math.hypot(dx, dy)
        len > 1e-6 && math.abs((dx * ux + dy * uy) / len) > 0.999
      val byOffset = parallel.groupBy((a, _) => math.round((a._1 * px + a._2 * py) / 0.1))
      byOffset.exists: (_, es) =>
        val ivs      = es.map((a, b) =>
          val (p1, p2) = (a._1 * ux + a._2 * uy, b._1 * ux + b._2 * uy); (math.min(p1, p2), math.max(p1, p2))
        ).sortBy(_._1)
        var (lo, hi) = ivs.head; var run = hi - lo
        for (a, b) <- ivs.tail do
          if a <= hi + 0.01 then { hi = math.max(hi, b); run = math.max(run, hi - lo) }
          else { lo = a; hi = b }
        run >= minRun

  def main(args: Array[String]): Unit =
    val oracleSize = args.headOption.map(_.toInt).getOrElse(24)
    val maxFaces   = args.lift(1).map(_.toInt).getOrElse(96)
    val maxV       = args.lift(2).map(_.toInt).getOrElse(12)
    val growerSec  = args.lift(3).map(_.toLong).getOrElse(90L)

    println(
      s"CharacterizeBandedProbe: oracle=$oracleSize grower maxFaces=$maxFaces boundedV maxV=$maxV ${growerSec}s/set"
    )
    val oracle   = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = 12)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values.map(_.head).toList.filter(_._1 == 3)
    val typeSets = oracle.map(_._2.toSet).distinct.sortBy(label)
    println(s"  n=3: ${oracle.size} tilings, ${typeSets.size} type-sets\n")

    // per cell: (maxConeOrder, banded: Option[Boolean], reached)
    val rows = scala.collection.mutable.ListBuffer.empty[(Int, Option[Boolean], Boolean)]
    for t <- typeSets do
      val cells   = oracle.filter(_._2.toSet == t)
      val reached = KrotenheerdtTorusMapSearch
        .symmetryRotationReferenceParallel(
          3,
          maxFaces,
          parallelism = 12,
          maxMillis = growerSec * 1000,
          targetTypes = t
        )
        .filter(_._2._1 == t).keySet
      val br      = BucketAssembly.enumerateBucket(t, maxV, targetCount = cells.size)
      for c <- cells do
        val key    = DelaneySymbols.canonicalKey(c._3)
        val order  = DelaneySymbols.maxConeOrder(c._3)
        val got    = reached.contains(key)
        val banded = br.ops.get(key).flatMap(op => KrotenheerdtTorusMapSearch.realizeCell(op)).map(
          (f, v, w) => isBanded(f, v, w)
        )
        rows += ((order, banded, got))
        val bStr   = banded.map(b => if b then "banded" else "not   ").getOrElse("  ?   ")
        println(f"  ${label(t)}%-50s C${order} $bStr ${if got then "reached" else "MISSED "}")

    val all                 = rows.toList
    def pct(a: Int, b: Int) = if b == 0 then "" else f" (${100.0 * a / b}%.0f%%)"
    println(f"\n=== n=3 characterization (${all.size} cells) ===")
    println(s"max cone order: " +
      all.groupBy(_._1).toList.sortBy(_._1).map((o, r) => s"C$o=${r.size}").mkString(" "))
    println(
      s"banded: yes=${all.count(_._2.contains(true))} no=${all.count(_._2.contains(false))} unrealized=${all.count(_._2.isEmpty)}"
    )
    val missed              = all.filterNot(_._3)
    val reached             = all.filter(_._3)
    println(f"\n  GROWER reached ${reached.size}, MISSED ${missed.size}")
    println(
      s"  of MISSED: C2=${missed.count(_._1 == 2)} higher=${missed.count(_._1 > 2)} | banded=${missed.count(_._2.contains(true))} not=${missed.count(_._2.contains(false))} ?=${missed.count(_._2.isEmpty)}"
    )
    println(
      s"  of REACHED: C2=${reached.count(_._1 == 2)} higher=${reached.count(_._1 > 2)} | banded=${reached.count(_._2.contains(true))} not=${reached.count(_._2.contains(false))} ?=${reached.count(_._2.isEmpty)}"
    )
    // how cleanly does "banded" predict missed?
    val bandedCells         = all.filter(_._2.contains(true))
    println(
      f"\n  BANDED family size: ${bandedCells.size}; of them grower MISSED ${bandedCells.count(!_._3)}${pct(bandedCells.count(!_._3), bandedCells.size)}, reached ${bandedCells.count(_._3)}"
    )
    val nonBanded           = all.filter(_._2.contains(false))
    println(
      f"  NON-banded (realized): ${nonBanded.size}; grower MISSED ${nonBanded.count(!_._3)}, reached ${nonBanded.count(_._3)}"
    )
    println("[done]")
