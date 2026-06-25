package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature

/** DE-RISK closure-directed (best-first) growth (ADR-0034 §4) vs the DFS driver, head-to-head under the SAME
  * budget. The grower's residual is large-domain C₂ cells whose fundamental domain exceeds the feasible
  * maxFaces; DFS burns the budget on shallow non-closing patches. Best-first
  * ([[symmetryClosureDirectedParallel]]) orders the frontier by closure-proximity, so it should reach the
  * DEEP cells with far fewer states.
  *
  * Runs BOTH drivers (unconstrained, maxN) at the same maxFaces / time / state budget and reports, per n, how
  * many distinct tilings each reached (vs the A068600 reference), plus which keys are EXCLUSIVE to each — the
  * signal that best-first reaches cells DFS cannot in the budget. Sanity: at a fully-quiesced maxFaces both
  * must agree; at a high maxFaces (DFS explodes) best-first should pull ahead.
  *
  * Run: `... ClosureDirectedProbe [maxN] [maxFaces] [minutesEach] [parallelism] [maxStatesMillions]`
  */
object ClosureDirectedProbe:
  def main(args: Array[String]): Unit =
    val maxN     = args.headOption.map(_.toInt).getOrElse(3)
    val maxFaces = args.lift(1).map(_.toInt).getOrElse(96)
    val minutes  = args.lift(2).map(_.toLong).getOrElse(6L)
    val par      = args.lift(3).map(_.toInt).getOrElse(12)
    val maxSt    = args.lift(4).map(_.toLong).getOrElse(4L) * 1000000L
    val target   = args
      .lift(5)
      .filter(_.nonEmpty)
      .map(_.split(',').toList.map(t => VertexTypes.normalize(t.split('.').map(_.toInt).toList)).toSet)
      .getOrElse(Set.empty[VertexSignature])
    val ms       = minutes * 60000L
    val ref      = Map(1 -> 11, 2 -> 20, 3 -> 39, 4 -> 33, 5 -> 15, 6 -> 10, 7 -> 7)
    println(
      s"ClosureDirectedProbe: maxN=$maxN maxFaces=$maxFaces minutesEach=$minutes par=$par maxStates=$maxSt"
    )
    if target.nonEmpty then
      println(s"  CONSTRAINED to target: ${target.map(_.mkString(".")).mkString("; ")}")

    def report(name: String, r: Map[String, (Set[VertexSignature], Set[(String, Int)])], secs: Double): Unit =
      val byN = r.values.groupBy(_._1.size).view.mapValues(_.size).toMap
      println(f"=== $name: ${secs}%.0fs  total=${r.size} ===")
      for n <- 1 to maxN do println(f"   n=$n: ${byN.getOrElse(n, 0)}%2d/${ref(n)}")

    println("\n--- DFS driver (symmetryRotationReferenceParallel) ---")
    val d0  = System.nanoTime()
    val dfs = KrotenheerdtTorusMapSearch.symmetryRotationReferenceParallel(
      maxN,
      maxFaces,
      parallelism = par,
      log = println,
      maxMillis = ms,
      targetTypes = target
    )
    val dT  = (System.nanoTime() - d0) / 1e9
    report("DFS", dfs, dT)

    println("\n--- BEST-FIRST driver (symmetryClosureDirectedParallel) ---")
    val b0 = System.nanoTime()
    val bf = KrotenheerdtTorusMapSearch.symmetryClosureDirectedParallel(
      maxN,
      maxFaces,
      parallelism = par,
      log = println,
      maxMillis = ms,
      maxStates = maxSt,
      targetTypes = target
    )
    val bT = (System.nanoTime() - b0) / 1e9
    report("BEST-FIRST", bf, bT)

    println("\n=== HEAD-TO-HEAD ===")
    for n <- 1 to maxN do
      val dn  = dfs.values.count(_._1.size == n)
      val bn  = bf.values.count(_._1.size == n)
      val tag = if bn > dn then "  <== best-first AHEAD" else if bn < dn then "  <== DFS ahead" else ""
      println(f"   n=$n: DFS $dn%2d   best-first $bn%2d   (of ${ref(n)})$tag")
    println(s"   keys only in best-first: ${(bf.keySet -- dfs.keySet).size}")
    println(s"   keys only in DFS:        ${(dfs.keySet -- bf.keySet).size}")
    println("[done]")
