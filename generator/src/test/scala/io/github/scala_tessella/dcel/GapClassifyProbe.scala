package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import io.github.scala_tessella.dcel.geometry.BigPoint

/** Reframe the profile engine's recall correctly: it targets the BANDED (anisotropic) family, so it should be
  * measured against the banded gap cells, not all of them. Classify every n=3 gap cell banded vs isotropic by
  * its lattice ASPECT ratio (orthogonal extent / shortest vector |h|), tag whether the engine reaches it, and
  * report recall against the BANDED subset.
  *
  * Run: `…GapClassifyProbe [oracleMaxSize] [boundedVmaxV] [engineMaxNodes] [bandedAspect]`
  */
object GapClassifyProbe:
  private def ts(s: String): Set[VertexSignature] =
    s.split(',').toList.map(t => normalize(t.split('.').map(_.toInt).toList)).toSet

  private val gaps = List(
    ts("3.3.3.3.3.3,3.3.3.3.6,3.3.6.6"),
    ts("3.3.3.3.3.3,3.3.3.4.4,4.4.4.4"),
    ts("3.3.6.6,3.6.3.6,6.6.6"),
    ts("3.4.4.6,3.6.3.6,4.4.4.4")
  )

  private val circs =
    List(ZetaPoint(2, 0, 0, 0), ZetaPoint(4, 0, 0, 0), ZetaPoint(0, 4, 0, -2), ZetaPoint(0, 8, 0, -4))

  /** Shortest non-zero lattice vector |h|, and the orthogonal extent covol/|h| (the band height). */
  private def aspectOf(vB: BigPoint, wB: BigPoint): (Double, Double) =
    val (vx, vy) = (vB.x.toDouble, vB.y.toDouble); val (wx, wy) = (wB.x.toDouble, wB.y.toDouble)
    val covol    = math.abs(vx * wy - vy * wx)
    var shortest = Double.MaxValue
    for m <- -6 to 6; n <- -6 to 6 if !(m == 0 && n == 0) do
      val len = math.hypot(m * vx + n * wx, m * vy + n * wy)
      if len > 1e-6 && len < shortest then shortest = len
    (shortest, covol / shortest)

  def main(args: Array[String]): Unit =
    val oracleSize = args.headOption.map(_.toInt).getOrElse(24)
    val maxV       = args.lift(1).map(_.toInt).getOrElse(16)
    val maxNodes   = args.lift(2).map(_.toInt).getOrElse(8000)
    val bandedAsp  = args.lift(3).map(_.toDouble).getOrElse(1.5)
    println(
      s"GapClassifyProbe: oracle=$oracleSize boundedV=$maxV engine maxNodes=$maxNodes banded-aspect>$bandedAsp"
    )

    val oracle = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = 12)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values.map(_.head).toList.filter(_._1 == 3)

    var bMatch, bMiss, isoMatch, isoMiss, noGeo = 0
    for t <- gaps do
      val cells  = oracle.filter(_._2.toSet == t)
      val engine = circs.flatMap(c => ProfileAutomaton.enumerateForTypeSetC(c, t, maxNodes).keySet).toSet
      val br     = BucketAssembly.enumerateBucket(t, maxV, targetCount = cells.size)
      println(s"\n=== ${t.map(_.mkString(".")).toList.sorted.mkString("; ")} ===")
      for c <- cells do
        val key = DelaneySymbols.canonicalKey(c._3)
        val got = engine.contains(key)
        val orb = DelaneySymbols.orbifoldSignature(c._3)
        br.ops.get(key).flatMap(op => KrotenheerdtTorusMapSearch.realizeCell(op)) match
          case Some((_, vB, wB)) =>
            val (h, height) = aspectOf(vB, wB)
            val asp         = height / h
            val banded      = asp > bandedAsp
            if banded then if got then bMatch += 1 else bMiss += 1
            else if got then isoMatch += 1 else isoMiss += 1
            println(f"  ${if banded then "BANDED " else "isotrop"} aspect=${asp}%4.1f  engine=${
                if got then "MATCHED" else "missing"
              }  $orb")
          case None              =>
            noGeo += 1
            println(f"  (no geometry)            engine=${if got then "MATCHED" else "missing"}  $orb")
    println(f"\n=== BANDED: $bMatch matched / ${bMatch + bMiss} | isotropic: $isoMatch matched / ${isoMatch +
        isoMiss} | no-geometry: $noGeo ===")
    println("[done]")
