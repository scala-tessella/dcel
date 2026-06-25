package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature

import java.nio.file.{Files, Paths}

/** TEST + VISUALIZE the strip-stacking enumerator ([[KrotenheerdtTorusSearch.enumerateBanded]], ADR-0037). NB
  * the engine emits the fixed-Λ GEOMETRIC content key, NOT the oracle's D-symbol key — so reach/soundness are
  * compared by TYPE-SET COUNT (engine vs the sound+complete n=3 oracle), which is key-space-independent:
  *   - SOUND: every type-set the engine emits is a real n=3 oracle type-set (no spurious type-set);
  *   - REACH: per type-set, engine count vs oracle count (it should reach the banded gap type-sets the grower
  *     misses).
  * Renders one SVG per found tiling (via the engine's onGeometry callback) into
  * generator/results/n3-strip-svg.
  *
  * Run: `…StripStackProbe [maxCovolume] [k] [maxBandLen] [oracleMaxSize] [parallelism]` (default 16 / 6 / 4 /
  * 24 / 12)
  */
object StripStackProbe:
  private def label(t: Set[VertexSignature]): String = t.map(_.mkString(".")).toList.sorted.mkString("; ")
  private def slug(s: String): String                = f"${math.abs(s.hashCode)}%08x"
  private val gapTypeSets                            = Set(
    "3.3.3.3.3.3; 3.3.3.3.6; 3.3.6.6",
    "3.3.3.3.3.3; 3.3.3.4.4; 4.4.4.4",
    "3.3.6.6; 3.6.3.6; 6.6.6",
    "3.4.4.6; 3.6.3.6; 4.4.4.4"
  )
  private val fill                                   = Map(3 -> "#e8746b", 4 -> "#6ba3e8", 6 -> "#73c66b", 12 -> "#b98be8")

  /** 3×3-cell SVG from the public geometry the engine hands back (size + Double corners) + basis. */
  private def toSvg(
      faces: List[(Int, Vector[(Double, Double)])],
      bV: (Double, Double),
      bW: (Double, Double)
  ): String =
    val polys         =
      for i <- -1 to 1; j <- -1 to 1; (size, pts) <- faces
      yield (size, pts.map((x, y) => (x + i * bV._1 + j * bW._1, y + i * bV._2 + j * bW._2)))
    val seen          = scala.collection.mutable.HashSet.empty[(Long, Long)]
    val keep          = polys.filter: (_, pts) =>
      val cx = pts.map(_._1).sum / pts.size; val cy = pts.map(_._2).sum / pts.size
      seen.add((math.round(cx * 1000), math.round(cy * 1000)))
    val xs            = keep.flatMap(_._2.map(_._1)); val ys                = keep.flatMap(_._2.map(_._2))
    val (minX, maxX)  = (xs.min, xs.max); val (minY, maxY)                  = (ys.min, ys.max)
    val scale         = 600.0 / math.max(maxX - minX, maxY - minY); val pad = 16.0
    def tx(x: Double) = (x - minX) * scale + pad
    def ty(y: Double) = (maxY - y) * scale + pad
    val w             = ((maxX - minX) * scale + 2 * pad).toInt; val h      = ((maxY - minY) * scale + 2 * pad).toInt
    val body          = keep.map: (size, pts) =>
      s"""<polygon points="${pts.map((x, y) => f"${tx(x)}%.1f,${ty(y)}%.1f").mkString(
          " "
        )}" fill="${fill.getOrElse(size, "#ccc")}" stroke="#222" stroke-width="1"/>"""
    .mkString("\n")
    s"""<svg xmlns="http://www.w3.org/2000/svg" width="$w" height="$h" viewBox="0 0 $w $h"><rect width="100%" height="100%" fill="white"/>
       |$body
       |</svg>""".stripMargin

  def main(args: Array[String]): Unit =
    val covol      = args.headOption.map(_.toDouble).getOrElse(16.0)
    val k          = args.lift(1).map(_.toInt).getOrElse(6)
    val maxBandLen = args.lift(2).map(_.toInt).getOrElse(4)
    val oracleSize = args.lift(3).map(_.toInt).getOrElse(24)
    val par        = args.lift(4).map(_.toInt).getOrElse(12)
    val outDir     = Paths.get("generator/results/n3-strip-svg")
    Files.createDirectories(outDir)
    println(s"StripStackProbe: enumerateBanded(n=3, covol=$covol, k=$k, maxBandLen=$maxBandLen) -> $outDir")

    val oracle       = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = par)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values.map(_.head).toList.filter(_._1 == 3)
    val oracleByType = oracle.groupBy(_._2.toSet).view.mapValues(_.size).toMap
    println(s"  oracle n=3: ${oracle.size} tilings, ${oracleByType.size} type-sets")

    var rendered = 0
    val t0       = System.nanoTime()
    val out      = KrotenheerdtTorusSearch.enumerateBanded(
      3,
      k,
      covol,
      maxBandLen,
      parallelism = par,
      log = println,
      onGeometry = (types, key, faces, bV, bW) =>
        val gap = if gapTypeSets.contains(label(types)) then "GAP-" else ""
        Files.writeString(
          outDir.resolve(s"strip-$gap${label(types).replace("; ", "_")}-${slug(key)}.svg"),
          toSvg(faces, bV, bW)
        )
        rendered += 1
    )
    val secs     = (System.nanoTime() - t0) / 1e9
    val bByType  = out.tilings.groupBy(_._1).view.mapValues(_.size).toMap

    val spuriousTypes = bByType.keySet -- oracleByType.keySet
    println(
      f"\n=== strip-stacking: ${out.tilings.size} tilings in ${secs}%.0fs (${out.basesTried} band lattices, ${out.statesExplored} states); $rendered SVGs ==="
    )
    println(s"  SOUND (by type-set): ${spuriousTypes.size} spurious type-set(s) (must be 0)")
    spuriousTypes.foreach(t => println(s"    ⚠ spurious: ${label(t)}"))
    val reachedTot    =
      oracleByType.keys.toList.map(ts => math.min(bByType.getOrElse(ts, 0), oracleByType(ts))).sum
    println(s"  REACH (by type-set count): $reachedTot/${oracle.size} of n=3")
    println("\n  per type-set (oracle vs strip, where strip>0 or a GAP set):")
    for ts <- oracleByType.keys.toList.sortBy(label) do
      val o = oracleByType(ts); val b = bByType.getOrElse(ts, 0)
      if b > 0 || gapTypeSets.contains(label(ts)) then
        println(f"    ${label(ts)}%-50s oracle=$o strip=$b${
            if gapTypeSets.contains(label(ts)) then "  <— GAP" else ""
          }")
    println(s"\nopen: $outDir")
    println("[done]")
