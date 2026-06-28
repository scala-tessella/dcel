package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.KrotenheerdtTorusMapSearch.FaceZ
import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import io.github.scala_tessella.dcel.geometry.BigPoint

import java.nio.file.{Files, Paths}

/** VISUAL of where the profile-state engine ([[ProfileAutomaton]]) stands on the banded n=3 GAP: render every
  * oracle cell of the 4 gap type-sets, tagged MATCHED (the engine reaches it, key-for-key) or MISSING.
  * Geometry is realised via the bounded-V op + `realizeCell` (same as `MissedCellRenderProbe`); colours by
  * polygon side.
  *
  * Run: `…GapCellRenderProbe [oracleMaxSize] [boundedVmaxV] [engineMaxNodes] [outDir]`
  */
object GapCellRenderProbe:
  private def ts(s: String): Set[VertexSignature] =
    s.split(',').toList.map(t => normalize(t.split('.').map(_.toInt).toList)).toSet

  private val gaps = List(
    ts("3.3.3.3.3.3,3.3.3.3.6,3.3.6.6"),
    ts("3.3.3.3.3.3,3.3.3.4.4,4.4.4.4"),
    ts("3.3.6.6,3.6.3.6,6.6.6"),
    ts("3.4.4.6,3.6.3.6,4.4.4.4")
  )

  // circumference sweep the engine uses: integers 2,4 + √3-family 2√3
  private val circs = List(ZetaPoint(2, 0, 0, 0), ZetaPoint(4, 0, 0, 0), ZetaPoint(0, 4, 0, -2))

  private val fill = Map(3 -> "#e8746b", 4 -> "#6ba3e8", 6 -> "#73c66b", 8 -> "#e8c46b", 12 -> "#b98be8")

  /** A 3×3 block of fundamental cells, polygons coloured by side count, with a tag banner. */
  private def toSvg(faces: List[FaceZ], vB: BigPoint, wB: BigPoint, tag: String, sub: String): String =
    val (vx, vy)      = (vB.x.toDouble, vB.y.toDouble)
    val (wx, wy)      = (wB.x.toDouble, wB.y.toDouble)
    val polys         =
      for i <- -1 to 1; j <- -1 to 1; f <- faces
      yield
        val pts = f.corners.toList.map: c =>
          val p = c.toBigPoint; (p.x.toDouble + i * vx + j * wx, p.y.toDouble + i * vy + j * wy)
        (f.size, pts)
    val seen          = scala.collection.mutable.HashSet.empty[(Long, Long)]
    val keep          = polys.filter: (_, pts) =>
      val cx = pts.map(_._1).sum / pts.size; val cy = pts.map(_._2).sum / pts.size
      seen.add((math.round(cx * 1000), math.round(cy * 1000)))
    val xs            = keep.flatMap(_._2.map(_._1)); val ys = keep.flatMap(_._2.map(_._2))
    val (minX, maxX)  = (xs.min, xs.max); val (minY, maxY)   = (ys.min, ys.max)
    val scale         = 360.0 / math.max(maxX - minX, maxY - minY)
    val pad           = 14.0; val banner                     = 30.0
    def tx(x: Double) = (x - minX) * scale + pad
    def ty(y: Double) = (maxY - y) * scale + pad + banner
    val w             = (maxX - minX) * scale + 2 * pad
    val h             = (maxY - minY) * scale + 2 * pad + banner
    val bg            = if tag == "MATCHED" then "#e6f6e6" else "#fdeaea"
    val fg            = if tag == "MATCHED" then "#1a7d1a" else "#b01818"
    val body          = keep.map: (size, pts) =>
      val ptsStr = pts.map((x, y) => f"${tx(x)}%.1f,${ty(y)}%.1f").mkString(" ")
      s"""<polygon points="$ptsStr" fill="${fill.getOrElse(size, "#ccc")}" stroke="#222" stroke-width="1"/>"""
    .mkString("\n")
    s"""<svg xmlns="http://www.w3.org/2000/svg" width="${w.toInt}" height="${h.toInt}" viewBox="0 0 ${w.toInt} ${h.toInt}">
       |<rect width="100%" height="100%" fill="$bg"/>
       |<text x="6" y="20" font-family="sans-serif" font-size="15" font-weight="bold" fill="$fg">$tag — $sub</text>
       |$body
       |</svg>""".stripMargin

  private def label(t: Set[VertexSignature]): String = t.map(_.mkString(".")).toList.sorted.mkString("; ")
  private def slug(s: String): String                = f"${math.abs(s.hashCode)}%08x"

  def main(args: Array[String]): Unit =
    val oracleSize = args.headOption.map(_.toInt).getOrElse(24)
    val maxV       = args.lift(1).map(_.toInt).getOrElse(16)
    val maxNodes   = args.lift(2).map(_.toInt).getOrElse(8000)
    val outDir     = Paths.get(args.lift(3).getOrElse("generator/results/gap-cells-svg"))
    Files.createDirectories(outDir)
    println(
      s"GapCellRenderProbe: oracle=$oracleSize boundedV maxV=$maxV engine maxNodes=$maxNodes -> $outDir"
    )

    val oracle = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = 12)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values.map(_.head).toList.filter(_._1 == 3)

    var matched, missing, nogeo = 0
    for t <- gaps do
      val cells  = oracle.filter(_._2.toSet == t)
      val engine = circs.flatMap(c => ProfileAutomaton.enumerateForTypeSetC(c, t, maxNodes).keySet).toSet
      val br     = BucketAssembly.enumerateBucket(t, maxV, targetCount = cells.size)
      val nMatch = cells.count(c => engine.contains(DelaneySymbols.canonicalKey(c._3)))
      println(f"\n=== ${label(t)}%-44s oracle=${cells.size} engine-matched=$nMatch ===")
      for c <- cells do
        val key = DelaneySymbols.canonicalKey(c._3)
        val tag = if engine.contains(key) then "MATCHED" else "MISSING"
        val orb = DelaneySymbols.orbifoldSignature(c._3)
        br.ops.get(key).flatMap(op => KrotenheerdtTorusMapSearch.realizeCell(op)) match
          case Some((faces, vB, wB)) =>
            Files.writeString(
              outDir.resolve(s"${label(t).replace("; ", "_")}-$tag-${slug(key)}.svg"),
              toSvg(faces, vB, wB, tag, label(t))
            )
            if tag == "MATCHED" then matched += 1 else missing += 1
            println(s"  [$tag] $orb")
          case None                  =>
            nogeo += 1
            println(s"  [$tag] $orb  (no bounded-V geometry to render)")
    println(s"\n=== MATCHED=$matched MISSING=$missing (no-geometry=$nogeo) — SVGs in $outDir ===")
    println("[done]")
