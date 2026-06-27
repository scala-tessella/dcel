package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.KrotenheerdtTorusMapSearch.FaceZ

import java.nio.file.{Files, Paths}

/** VISUAL catalogue of the strip-stacking band types ([[StripBand]], ADR-0037), for the user to inspect by
  * eye BEFORE any stacking. Enumerates [[StripBand.catalogue]] and renders each band type to an SVG (the
  * developed polygons over a few horizontal periods, the bottom profile drawn thick black, the top profile
  * thick grey), into `generator/results/band-catalogue-svg`. Runs only on the test-verified band generator.
  *
  * Run: `…BandCatalogueProbe [maxLen] [periods]` (default 4 / 4).
  */
object BandCatalogueProbe:

  private val fill = Map(3 -> "#e8746b", 4 -> "#6ba3e8", 6 -> "#73c66b", 12 -> "#b98be8")

  private def xy(z: ZetaPoint): (Double, Double) =
    val p = z.toBigPoint; (p.x.toDouble, p.y.toDouble)

  /** Render a band: `periods` copies side by side, profiles highlighted. */
  private def toSvg(band: StripBand.Band, periods: Int): String =
    val (px, py)                                                  = xy(band.period)
    val faces                                                     =
      for k <- 0 until periods; f <- band.faces
      yield FaceZ(f.size, f.corners.map(c => c + mul(band.period, k)))
    val pts                                                       = faces.flatMap(_.corners.map(xy))
    val (minX, mX)                                                = (pts.map(_._1).min - 0.3, pts.map(_._1).max + 0.3)
    val (minY, mY)                                                = (pts.map(_._2).min - 0.3, pts.map(_._2).max + 0.3)
    val scale                                                     = 70.0
    val pad                                                       = 10.0
    def tx(x: Double)                                             = (x - minX) * scale + pad
    def ty(y: Double)                                             = (mY - y) * scale + pad
    val w                                                         = ((mX - minX) * scale + 2 * pad).toInt
    val h                                                         = ((mY - minY) * scale + 2 * pad).toInt
    val polys                                                     = faces.map: f =>
      val pp = f.corners.map(xy).map((x, y) => f"${tx(x)}%.1f,${ty(y)}%.1f").mkString(" ")
      s"""<polygon points="$pp" fill="${fill.getOrElse(f.size, "#ccc")}" stroke="#222" stroke-width="1"/>"""
    .mkString("\n")
    // profile polylines (bottom over the rendered periods, and the reconstructed top)
    def chain(verts: List[(Double, Double)], col: String): String =
      if verts.sizeIs < 2 then ""
      else
        val d = verts.map((x, y) => f"${tx(x)}%.1f,${ty(y)}%.1f").mkString(" ")
        s"""<polyline points="$d" fill="none" stroke="$col" stroke-width="3" stroke-linecap="round"/>"""
    val bottomVerts                                               = (0 to periods).flatMap(k => band.bottomVertices.map(v => xy(v + mul(band.period, k))))
      .sortBy(_._1).toList
    val topVerts                                                  = (-1 to periods).flatMap(k => band.topVertexFans.map((v, _) => xy(v + mul(band.period, k))))
      .filter((x, _) => x >= minX && x <= mX).sortBy(_._1).toList
    s"""<svg xmlns="http://www.w3.org/2000/svg" width="$w" height="$h" viewBox="0 0 $w $h"><rect width="100%" height="100%" fill="white"/>
       |$polys
       |${chain(topVerts, "#999")}
       |${chain(bottomVerts, "#000")}
       |</svg>""".stripMargin

  private def mul(p: ZetaPoint, k: Int): ZetaPoint = ZetaPoint(p.a0 * k, p.a1 * k, p.a2 * k, p.a3 * k)

  private def slug(s: String): String = s.replaceAll("[^A-Za-z0-9]+", "_").stripPrefix("_").stripSuffix("_")

  def main(args: Array[String]): Unit =
    val maxLen  = args.headOption.map(_.toInt).getOrElse(4)
    val periods = args.lift(1).map(_.toInt).getOrElse(4)
    val outDir  = Paths.get("generator/results/band-catalogue-svg")
    Files.createDirectories(outDir)

    val cat = StripBand.catalogue(maxLen)
    println(s"BandCatalogueProbe: ${cat.size} band types (maxLen=$maxLen) -> $outDir\n")
    println(f"${"#"}%3s  ${"polygons"}%-12s ${"bottom arcs"}%-22s ${"top arcs"}%-22s faces/period")
    cat.zipWithIndex.foreach: (b, i) =>
      val poly =
        b.faceSizes.groupBy(identity).toList.sortBy(_._1).map((m, g) => s"$m^${g.size}").mkString(".")
      println(
        f"$i%3d  $poly%-12s ${b.bottomArcs.sorted.mkString("/")}%-22s ${b.topArcs.sorted.mkString("/")}%-22s ${b.faces.size}"
      )
      val name = f"band-$i%02d-${slug(poly)}-b${b.bottomArcs.sorted.mkString("_")}.svg"
      Files.writeString(outDir.resolve(name), toSvg(b, periods))
    println(s"\n[done] ${cat.size} SVGs in $outDir")
