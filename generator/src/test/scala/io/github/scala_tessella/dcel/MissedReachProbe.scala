package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** PAYOFF VALIDATION for ADR-0038 (before the profile-engine build): does the cylinder approach REACH the
  * grower-missed n=3 cells, key-for-key in the shared D-symbol space? Uses the already-proven bounded
  * patch-growth ([[CylinderAutomaton.enumerateAtH]]) — no new engine.
  *
  *   - missed = (oracle n=3 cells in the 4 gap type-sets) − (grower-reached keys)
  *   - cylinder = ∪ over circumferences C of `enumerateAtH(C, maxN=3)` emitted D-symbol keys
  *   - report how many missed keys the cylinder reaches (and any it doesn't)
  *
  * Run: `…MissedReachProbe [oracleMaxSize] [growerMaxFaces] [growerMillis] [cylMaxFaces] [cylPerCap]`
  */
object MissedReachProbe:
  private def ts(s: String): Set[VertexSignature] =
    s.split(',').toList.map(t => normalize(t.split('.').map(_.toInt).toList)).toSet

  private val gaps = List(
    ts("3.3.3.3.3.3,3.3.3.3.6,3.3.6.6"),
    ts("3.3.3.3.3.3,3.3.3.4.4,4.4.4.4"),
    ts("3.3.6.6,3.6.3.6,6.6.6"),
    ts("3.4.4.6,3.6.3.6,4.4.4.4")
  )

  def main(args: Array[String]): Unit =
    val oracleSize = args.headOption.map(_.toInt).getOrElse(24)
    val maxFaces   = args.lift(1).map(_.toInt).getOrElse(64)
    val millis     = args.lift(2).map(_.toLong).getOrElse(45000L)
    val cylFaces   = args.lift(3).map(_.toInt).getOrElse(34)
    val cylPerCap  = args.lift(4).map(_.toLong).getOrElse(120000L)
    val circs      = args.lift(5).map(_.split(',').map(_.toInt).toList).getOrElse(List(2, 4))
    println(
      s"MissedReachProbe: oracle=$oracleSize grower maxFaces=$maxFaces cyl maxFaces=$cylFaces circs=$circs"
    )

    val oracle = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = 12)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values.map(_.head).toList.filter(_._1 == 3)

    // missed = gap-type-set oracle cells the grower does NOT reach
    val missed     = scala.collection.mutable.ListBuffer.empty[(String, String)] // (key, orbifold)
    for t <- gaps do
      val cells  = oracle.filter(_._2.toSet == t)
      val grower = KrotenheerdtTorusMapSearch
        .symmetryRotationReferenceParallel(3, maxFaces, parallelism = 12, maxMillis = millis, targetTypes = t)
        .filter(_._2._1 == t).keySet
      for c <- cells do
        val key = DelaneySymbols.canonicalKey(c._3)
        if !grower.contains(key) then missed += ((key, DelaneySymbols.orbifoldSignature(c._3)))
    val missedKeys = missed.map(_._1).toSet
    println(s"\n  grower-missed n=3 cells: ${missed.size}")

    // cylinder reach over the circumferences
    val cylKeys = scala.collection.mutable.HashSet.empty[String]
    for c <- circs do
      val t0   = System.nanoTime()
      val out  = CylinderAutomaton.enumerateAtH(
        ZetaPoint(c.toLong, 0, 0, 0),
        maxN = 3,
        maxFaces = cylFaces,
        perCap = cylPerCap
      )
      val secs = (System.nanoTime() - t0) / 1e9
      cylKeys ++= out.emitted.collect { case (k, (3, _)) => k } // n=3 cells only
      println(
        f"  C=$c: emitted=${out.emitted.size} keys (n=3: ${out.emitted.count(_._2._1 == 3)}), states=${out.states}, capped=${out.capped}, ${secs}%.0fs"
      )

    val reached   = missedKeys & cylKeys
    val unreached = missed.filterNot((k, _) => cylKeys.contains(k))
    val spurious  = cylKeys -- oracle.map(c => DelaneySymbols.canonicalKey(c._3)).toSet
    println(s"\n=== cylinder reaches ${reached.size}/${missed.size} grower-missed n=3 cells ===")
    unreached.foreach((k, orb) => println(s"  UNREACHED: $orb  $k"))
    println(
      s"  (cylinder emitted ${cylKeys.size} distinct n=3 keys total; ${spurious.size} not in the oracle n=3 set)"
    )
    println("[done]")
