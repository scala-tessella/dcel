package io.github.scala_tessella.dcel

/** DISCHARGE Conjecture R for n ≤ 3: enumerate the COMPLETE set of n≤3 Krötenheerdt tilings with the
  * generate-all `DelaneySymbols` oracle (rotation-AGNOSTIC — it makes no symmetry assumption), dedup by
  * canonical key, and test each with `hasRotation` (the exact orbifold-cone test = group contains a
  * rotation). If the per-n counts match A068600 (11, 20, 39) AND every tiling has a rotation, then R is
  * PROVEN for n ≤ 3 by exhaustive rotation-blind enumeration — independent of Galebach.
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.DischargeRProbe [maxSize]`
  */
object DischargeRProbe:
  def main(args: Array[String]): Unit =
    val maxSize  = args.headOption.map(_.toInt).getOrElse(12)
    val expected = Map(1 -> 11, 2 -> 20, 3 -> 39) // A068600(1..3)
    println(s"DischargeRProbe: generate-all oracle enumerateSymbols(maxN=3, maxSize=$maxSize) ...")
    val syms     = DelaneySymbols.enumerateSymbols(3, maxSize)
    // dedup by canonical key (the oracle may emit several isomorphic minimal symbols per tiling)
    val distinct = syms.groupBy((_, _, ds) => DelaneySymbols.canonicalKey(ds)).values.map(_.head).toList
    var allOk    = true
    var edgeOnly = 0
    for n <- 1 to 3 do
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
      println(
        "=== R DISCHARGED for n ≤ 3: complete oracle reproduces 11/20/39 AND every tiling has a rotation."
      )
      println("    (rotation-agnostic enumeration ⇒ no rotation-free Krötenheerdt tiling exists for n ≤ 3.)")
    else
      println(
        "=== NOT discharged: either a count is short (raise maxSize) or a rotation-free tiling was found."
      )
    println("\n[done]")
