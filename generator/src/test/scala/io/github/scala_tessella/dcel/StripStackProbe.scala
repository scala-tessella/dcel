package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature

/** TEST the strip-stacking enumerator ([[KrotenheerdtTorusSearch.enumerateBanded]], ADR-0037) against the
  * sound+complete n=3 oracle (authority): SOUNDNESS (every key it emits is a real oracle tiling — no
  * spurious), REACH (how many of the 39 it gets, vs the grower's 32), and per-type-set focus on the 4
  * grower-gap type-sets (it should reach the banded cells the grower misses). Keys are in the shared D-symbol
  * space, so the comparison is key-for-key.
  *
  * Run: `…StripStackProbe [maxCovolume] [maxBandLen] [oracleMaxSize] [parallelism]` (default 12 / 4 / 24 /
  * 12)
  */
object StripStackProbe:
  private def label(t: Set[VertexSignature]): String = t.map(_.mkString(".")).toList.sorted.mkString("; ")
  private val gapTypeSets                            = Set(
    "3.3.3.3.3.3; 3.3.3.3.6; 3.3.6.6",
    "3.3.3.3.3.3; 3.3.3.4.4; 4.4.4.4",
    "3.3.6.6; 3.6.3.6; 6.6.6",
    "3.4.4.6; 3.6.3.6; 4.4.4.4"
  )

  def main(args: Array[String]): Unit =
    val covol      = args.headOption.map(_.toDouble).getOrElse(12.0)
    val maxBandLen = args.lift(1).map(_.toInt).getOrElse(4)
    val oracleSize = args.lift(2).map(_.toInt).getOrElse(24)
    val par        = args.lift(3).map(_.toInt).getOrElse(12)
    println(
      s"StripStackProbe: enumerateBanded(n=3, covol=$covol, maxBandLen=$maxBandLen), oracle maxSize=$oracleSize"
    )

    val oracle      = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = par)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values.map(_.head).toList.filter(_._1 == 3)
    val oracleKeys  = oracle.map(c => DelaneySymbols.canonicalKey(c._3)).toSet
    val oracleByKey = oracle.map(c => DelaneySymbols.canonicalKey(c._3) -> c._2.toSet).toMap
    println(s"  oracle n=3: ${oracle.size} tilings")

    val t0    = System.nanoTime()
    val out   =
      KrotenheerdtTorusSearch.enumerateBanded(3, 3, covol, maxBandLen, parallelism = par, log = println)
    val secs  = (System.nanoTime() - t0) / 1e9
    val bKeys = out.tilings.map(_._2).toSet

    val spurious = bKeys -- oracleKeys
    val reached  = bKeys & oracleKeys
    println(
      f"\n=== strip-stacking engine: ${bKeys.size} tilings in ${secs}%.0fs (${out.basesTried} band lattices, ${out.statesExplored} states) ==="
    )
    println(s"  SOUND: ${spurious.size} spurious (must be 0)   REACH: ${reached.size}/${oracle.size} of n=3")
    if spurious.nonEmpty then
      println("  ⚠ SPURIOUS KEYS (engine emitted non-oracle tilings — a soundness BUG):")
      spurious.take(5).foreach(k =>
        println(s"    $k  types=${out.tilings.find(_._2 == k).map(t => label(t._1)).getOrElse("?")}")
      )

    println("\n  per gap-type-set (oracle vs strip-stacking):")
    val byTypeBanded = bKeys.flatMap(k => oracleByKey.get(k)).groupBy(identity).view.mapValues(_.size).toMap
    val byTypeOracle = oracle.groupBy(_._2.toSet).view.mapValues(_.size).toMap
    for ts <- oracle.map(_._2.toSet).distinct.sortBy(label) do
      val o   = byTypeOracle.getOrElse(ts, 0)
      val b   = byTypeBanded.getOrElse(ts, 0)
      val tag = if gapTypeSets.contains(label(ts)) then "  <— GAP type-set" else ""
      if b > 0 || gapTypeSets.contains(label(ts)) then
        println(f"    ${label(ts)}%-50s oracle=$o strip=$b$tag")
    println("[done]")
