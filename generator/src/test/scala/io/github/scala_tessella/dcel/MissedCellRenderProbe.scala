package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.KrotenheerdtTorusMapSearch.FaceZ
import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import io.github.scala_tessella.dcel.geometry.BigPoint

import java.nio.file.{Files, Paths}

/** RENDER (and investigate) the n=3 tilings the grower misses. For each of the 4 gap type-sets: get the
  * oracle cells (authority), the GROWER's reached keys, and the rotation-AGNOSTIC bounded-V assembler's
  * tilings WITH their torus `op` (`BucketResult.ops`). For every oracle cell, realise its geometry via
  * `realizeCell(op)` (bounded-V op → exact ℤ[ζ₁₂] faces + lattice Λ) and write an SVG (a 3×3 block of
  * fundamental cells). The filename flags whether the GROWER reached it. A missed cell that bounded-V CAN
  * realise is a grower-specific failure (and we get to see it); one neither engine reaches would be a deeper
  * residual.
  *
  * Run: `…MissedCellRenderProbe [oracleMaxSize] [growerMaxFaces] [boundedVmaxV] [outDir]`
  */
object MissedCellRenderProbe:
  private def ts(s: String): Set[VertexSignature]    =
    s.split(',').toList.map(t => normalize(t.split('.').map(_.toInt).toList)).toSet
  private def slug(s: String): String                = f"${math.abs(s.hashCode)}%08x"
  private def label(t: Set[VertexSignature]): String = t.map(_.mkString(".")).toList.sorted.mkString("_")

  private val gaps = List(
    ts("3.3.3.3.3.3,3.3.3.3.6,3.3.6.6"),
    ts("3.3.3.3.3.3,3.3.3.4.4,4.4.4.4"),
    ts("3.3.6.6,3.6.3.6,6.6.6"),
    ts("3.4.4.6,3.6.3.6,4.4.4.4")
  )

  private val fill = Map(3 -> "#e8746b", 4 -> "#6ba3e8", 6 -> "#73c66b", 8 -> "#e8c46b", 12 -> "#b98be8")

  /** A 3×3 block of fundamental cells as an SVG string; polygons coloured by side count. */
  private def toSvg(faces: List[FaceZ], vB: BigPoint, wB: BigPoint): String =
    val (vx, vy)      = (vB.x.toDouble, vB.y.toDouble)
    val (wx, wy)      = (wB.x.toDouble, wB.y.toDouble)
    val polys         =
      for
        i <- -1 to 1; j <- -1 to 1; f <- faces
      yield
        val pts = f.corners.toList.map: c =>
          val p = c.toBigPoint
          (p.x.toDouble + i * vx + j * wx, p.y.toDouble + i * vy + j * wy)
        (f.size, pts)
    // dedup by rounded centroid (translated copies that coincide on shared cell boundaries)
    val seen          = scala.collection.mutable.HashSet.empty[(Long, Long)]
    val keep          = polys.filter: (_, pts) =>
      val cx = pts.map(_._1).sum / pts.size; val cy = pts.map(_._2).sum / pts.size
      seen.add((math.round(cx * 1000), math.round(cy * 1000)))
    val xs            = keep.flatMap(_._2.map(_._1)); val ys   = keep.flatMap(_._2.map(_._2))
    val (minX, maxX)  = (xs.min, xs.max); val (minY, maxY)     = (ys.min, ys.max)
    val scale         = 700.0 / math.max(maxX - minX, maxY - minY)
    val pad           = 20.0
    def tx(x: Double) = (x - minX) * scale + pad
    def ty(y: Double) = (maxY - y) * scale + pad // flip y for SVG
    val w             = (maxX - minX) * scale + 2 * pad; val h = (maxY - minY) * scale + 2 * pad
    val body          = keep.map: (size, pts) =>
      val ptsStr = pts.map((x, y) => f"${tx(x)}%.1f,${ty(y)}%.1f").mkString(" ")
      s"""<polygon points="$ptsStr" fill="${fill.getOrElse(size, "#ccc")}" stroke="#222" stroke-width="1"/>"""
    .mkString("\n")
    s"""<svg xmlns="http://www.w3.org/2000/svg" width="${w.toInt}" height="${h.toInt}" viewBox="0 0 ${w.toInt} ${h.toInt}">
       |<rect width="100%" height="100%" fill="white"/>
       |$body
       |</svg>""".stripMargin

  def main(args: Array[String]): Unit =
    val oracleSize = args.headOption.map(_.toInt).getOrElse(24)
    val maxFaces   = args.lift(1).map(_.toInt).getOrElse(96)
    val maxV       = args.lift(2).map(_.toInt).getOrElse(14)
    val outDir     = Paths.get(args.lift(3).getOrElse("generator/results/n3-gap-svg"))
    Files.createDirectories(outDir)
    println(
      s"MissedCellRenderProbe: oracle=$oracleSize grower maxFaces=$maxFaces boundedV maxV=$maxV -> $outDir"
    )

    val oracle = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = 12)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values.map(_.head).toList.filter(_._1 == 3)

    var rendered, missedRendered, bothMiss = 0
    for t <- gaps do
      val cells  = oracle.filter(_._2.toSet == t)
      val grower = KrotenheerdtTorusMapSearch
        .symmetryRotationReferenceParallel(
          3,
          maxFaces,
          parallelism = 12,
          maxMillis = 180000L,
          targetTypes = t
        )
        .filter(_._2._1 == t).keySet
      val br     = BucketAssembly.enumerateBucket(t, maxV, targetCount = cells.size)
      println(s"\n=== ${label(t)} : oracle=${cells.size} grower=${(grower &
          cells.map(c => DelaneySymbols.canonicalKey(c._3)).toSet).size} boundedV=${br.keys.size} ===")
      for c <- cells do
        val key  = DelaneySymbols.canonicalKey(c._3)
        val got  = grower.contains(key)
        val mark = if got then "ok" else "MISSED"
        br.ops.get(key).flatMap(op => KrotenheerdtTorusMapSearch.realizeCell(op)) match
          case Some((faces, vB, wB)) =>
            val file = outDir.resolve(s"n3-${label(t)}-$mark-${slug(key)}.svg")
            Files.writeString(file, toSvg(faces, vB, wB))
            rendered += 1
            if !got then missedRendered += 1
            println(s"  [$mark] ${DelaneySymbols.orbifoldSignature(c._3)}  -> ${file.getFileName}")
          case None                  =>
            if !got then bothMiss += 1
            println(
              s"  [$mark] ${DelaneySymbols.orbifoldSignature(c._3)}  (bounded-V did NOT reach it — no op to render)"
            )

    println(
      s"\n=== rendered $rendered SVGs ($missedRendered of them grower-MISSED cells); $bothMiss missed by BOTH engines ==="
    )
    println(s"open: $outDir")
    println("[done]")
