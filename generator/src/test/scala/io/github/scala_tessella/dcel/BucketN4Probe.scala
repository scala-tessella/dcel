package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** ADR-0029 feasibility measurement: push the SOUND, EXACT bounded-V dart assembler ([[BucketAssembly]]) on
  * representative n = 4 type-sets to get the *V-needed* (minimal torus-cell vertex count where a tiling first
  * appears) and the *cost-per-V* curve. This is the decisive datum for whether a complete n = 4 can be
  * brute-forced inside the ~1-week budget: cost grows ≈ cᵛ, so V-needed and c fix the wall.
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.BucketN4Probe [maxV] [budgetMillions]`
  */
object BucketN4Probe:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  // Representative 4-type sets from TilingReference.rawWikipediaN3to5(4), spanning the symmetry range:
  //   - triangle/hex (high symmetry ⇒ expected small cell)
  //   - triangle/square (mid)
  //   - dodecagon-rich (low symmetry / large cell ⇒ worst case)
  private val cases: List[(String, Set[String])] = List(
    "tri/hex {3^6;3^4.6;3^2.6^2;6^3}"          ->
      Set("3.3.3.3.3.3", "3.3.3.3.6", "3.3.6.6", "6.6.6"),
    "tri/sq {3^6;3^3.4^2;3^2.4.3.4;4^4}"       ->
      Set("3.3.3.3.3.3", "3.3.3.4.4", "3.3.4.3.4", "4.4.4.4"),
    "dodec {3^2.4.12;3.4.3.12;3.4.6.4;4.6.12}" ->
      Set("3.3.4.12", "3.4.3.12", "3.4.6.4", "4.6.12"),
    // a representative HEAVY bucket (hexagon/square mix) that budget-hit found=0 at 120-200M in the driver
    "heavy {3^3.4^2;3^2.6^2;3.4^2.6;4.6.12}"   ->
      Set("3.3.3.4.4", "3.3.6.6", "3.4.4.6", "4.6.12"),
    // SANITY: the k=1 dodecagon tilings — does the assembler close 12-gon cells AT ALL?
    "k1 4.6.12"                                ->
      Set("4.6.12"),
    "k1 3.12.12"                               ->
      Set("3.12.12"),
    // SANITY: a MULTI-type bucket with a chiral type (3.4.4.6) — does the chirality fix let it close?
    "k2 {3.4^2.6;3.6.3.6}"                     ->
      Set("3.4.4.6", "3.6.3.6")
  )

  def main(args: Array[String]): Unit =
    val maxV   = args.headOption.map(_.toInt).getOrElse(12)
    val budget = args.lift(1).map(_.toLong).getOrElse(20L) * 1_000_000L
    val only   = args.lift(2).map(_.toInt) // optional case index (0=tri/hex,1=tri/sq,2=dodec); default all
    println(s"n=4 bounded-V feasibility: maxV=$maxV stateBudget=$budget only=${only.getOrElse("all")}")
    val chosen = only.fold(cases)(i => List(cases(i)))
    for (name, types) <- chosen do
      val bucket = types.map(sig)
      println(s"=== $name  (k=${bucket.size}) ===")
      var v      = bucket.size
      var found  = false
      var stop   = false
      while v <= maxV && !stop do
        val t0 = System.nanoTime()
        val r  = BucketAssembly.enumerateBucket(bucket, v, budget)
        val ms = (System.nanoTime() - t0) / 1000000
        println(
          f"  V<=$v%2d  found=${r.tilings.size}%2d  states=${r.states}%12d  expanded=${r.expanded}%11d  closed=${r.mapsClosed}%7d  budgetHit=${r.budgetHit}  ${ms}%6dms"
        )
        if r.tilings.nonEmpty then found = true
        // stop escalating once we hit the budget (cost has exceeded the measurement window for this curve)
        if r.budgetHit then stop = true
        v += 1
      println(s"  -> ${if found then "FOUND within window" else "NOT found within window"}")
