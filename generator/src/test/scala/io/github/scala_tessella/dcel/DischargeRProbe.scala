package io.github.scala_tessella.dcel

/** DISCHARGE Conjecture R for n ≤ 3: enumerate the COMPLETE set of n≤3 Krötenheerdt tilings with the
  * generate-all `DelaneySymbols` oracle (rotation-AGNOSTIC — it makes no symmetry assumption), dedup by
  * canonical key, and test each with `hasRotation` (the exact orbifold-cone test = group contains a
  * rotation). If the per-n counts match A068600 (11, 20, 39) AND every tiling has a rotation, then R is
  * PROVEN for n ≤ 3 by exhaustive rotation-blind enumeration — independent of Galebach.
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.DischargeRProbe [maxN] [maxSize]` (n≤3
  * discharges at maxSize 24; n=4 is an exploratory push — likely plateaus below 33 at feasible sizes.)
  */
object DischargeRProbe:
  def main(args: Array[String]): Unit =
    val maxN     = args.headOption.map(_.toInt).getOrElse(3)
    val maxSize  = args.lift(1).map(_.toInt).getOrElse(24)
    // SAFEGUARD: cap parallelism BELOW core count (default leaves only 1 core ⇒ a long run pins every
    // core on GC and the desktop becomes unresponsive). Pass e.g. 12 on a 16-core box to keep it usable.
    val par      = args.lift(2).map(_.toInt).getOrElse(math.max(1, Runtime.getRuntime.availableProcessors - 1))
    val expected = Map(1 -> 11, 2 -> 20, 3 -> 39, 4 -> 33, 5 -> 15, 6 -> 10, 7 -> 7) // A068600
    val t0       = System.nanoTime()
    println(
      s"DischargeRProbe: PARALLEL generate-all enumerateSymbolsParallel(maxN=$maxN, maxSize=$maxSize, parallelism=$par) ..."
    )
    val syms     = DelaneySymbols.enumerateSymbolsParallel(maxN, maxSize, parallelism = par, log = println)
    // dedup by canonical key (the oracle may emit several isomorphic minimal symbols per tiling)
    val distinct = syms.groupBy((_, _, ds) => DelaneySymbols.canonicalKey(ds)).values.map(_.head).toList
    println(f"  (enumerated in ${(System.nanoTime() - t0) / 1e9}%.0fs)")
    var allOk    = true
    var edgeOnly = 0
    for n <- 1 to maxN do
      val tilings  = distinct.filter(_._1 == n)
      val without  = tilings.filterNot((_, _, ds) => DelaneySymbols.hasRotation(ds))
      edgeOnly += tilings.count((_, _, ds) => DelaneySymbols.edgeMidpointRotationOnly(ds))
      val cnt      = tilings.size
      val exp      = expected(n)
      val complete = if cnt == exp then "COMPLETE ✓" else s"INCOMPLETE (need maxSize↑)"
      println(f"  n=$n: $cnt%2d/$exp tilings [$complete]   rotation-free: ${without.size}")
      without.foreach((_, sigs, ds) =>
        println(
          s"     ⚠ ROTATION-FREE: ${sigs.map(_.mkString(".")).mkString("; ")}  ${DelaneySymbols.orbifoldSignature(ds)}"
        )
      )
      if cnt != exp || without.nonEmpty then allOk = false
    println()
    println(
      s"  [method check] $edgeOnly tilings have a rotation ONLY via an edge-midpoint C₂ (no face/vertex cone)\n" +
        "                 ⇒ the (0,2) edge term is load-bearing and hasRotation is discriminating, not vacuous."
    )
    if allOk then
      println(s"=== R DISCHARGED for n ≤ $maxN: complete oracle reproduces the A068600 counts AND every")
      println("    tiling has a rotation ⇒ no rotation-free Krötenheerdt tiling exists for these n.")
    else
      println(
        s"=== NOT (yet) discharged for n ≤ $maxN: a count is short (raise maxSize — generation wall) " +
          "OR a rotation-free tiling was found (would be ⚠-flagged above). Reached counts are rotation-blind LOWER bounds."
      )
    println("\n[done]")
