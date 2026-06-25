package io.github.scala_tessella.dcel

/** Split the generate-all oracle's wall into its two halves, to decide WHICH to attack (the curvature prune
  * was sound but wall-neutral — the cost is elsewhere). Times, at one maxSize:
  *   - GEN: [[DelaneySymbols.countDSetsParallel]] — walk the D-set tree + canonical check + euclidean gate,
  *     NO symbol processing.
  *   - FULL: [[DelaneySymbols.enumerateSymbolsParallel]] — the whole pipeline (adds DSymGenerator /
  *     isEuclidean / regularPolygonVertices / isMinimal / canonicalKey on the euclidean survivors).
  * PROC = FULL − GEN. If GEN ≈ FULL ⇒ the wall is the canonical generation tree (attack checkCanonicity). If
  * PROC ≫ GEN ⇒ the wall is euclidean-symbol redundancy (attack early minimal-symbol dedup).
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.GenProfileProbe [maxSize] [maxN] [parallelism]`
  */
object GenProfileProbe:
  def main(args: Array[String]): Unit =
    val maxSize = args.headOption.map(_.toInt).getOrElse(24)
    val maxN    = args.lift(1).map(_.toInt).getOrElse(4)
    val par     = args.lift(2).map(_.toInt).getOrElse(12)
    println(s"GenProfileProbe: maxSize=$maxSize maxN=$maxN parallelism=$par")

    val g0            = System.nanoTime()
    val (complete, e) = DelaneySymbols.countDSetsParallel(maxSize, par)
    val gen           = (System.nanoTime() - g0) / 1e9
    println(f"  GEN (walk+canonicity+euclidean gate): ${gen}%.1fs")
    println(f"    complete D-sets = $complete   euclideanFeasible = $e (${100.0 * e / complete}%.2f%%)")

    val f0       = System.nanoTime()
    val syms     = DelaneySymbols.enumerateSymbolsParallel(maxN, maxSize, parallelism = par)
    val full     = (System.nanoTime() - f0) / 1e9
    val distinct = syms.groupBy(t => DelaneySymbols.canonicalKey(t._3)).size
    println(f"  FULL (whole pipeline): ${full}%.1fs   raw symbols=${syms.size}  distinct tilings=$distinct")

    val proc = full - gen
    println(
      f"  => GEN ${gen}%.1fs (${100.0 * gen / full}%.0f%%)   PROC ${proc}%.1fs (${100.0 * proc / full}%.0f%%)"
    )
    if gen >= proc then
      println("  VERDICT: the canonical GENERATION TREE dominates ⇒ attack checkCanonicity O(size²)/node.")
    else
      println(
        "  VERDICT: euclidean-symbol PROCESSING dominates ⇒ attack early minimal-symbol dedup (redundancy)."
      )
    println("[done]")
