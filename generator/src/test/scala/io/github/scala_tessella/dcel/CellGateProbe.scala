package io.github.scala_tessella.dcel

/** ADR-0041 gate ladder + live targets: compute the m-Archimedean n-uniform cells for the given rows (m = 2
  * .. n−1; the diagonal is the validated Krotenheerdt run) and compare against the known values (Čtrnáct via
  * Wikipedia — validation only). Row 8 is the LIVE target: Wikipedia's breakdown sums to 2890 but the row
  * total (and A068599(8)) is 2850 — our independent numbers settle it.
  */
object CellGateProbe:
  private val known = Map(
    (3, 2) -> 22,
    (4, 2) -> 33,
    (4, 3) -> 85,
    (5, 2) -> 74,
    (5, 3) -> 149,
    (5, 4) -> 94,
    (6, 2) -> 100,
    (6, 3) -> 284,
    (6, 4) -> 187,
    (6, 5) -> 92,
    (7, 2) -> 175,
    (7, 3) -> 572,
    (7, 4) -> 426,
    (7, 5) -> 218,
    (7, 6) -> 74,
    // row 8 per Wikipedia (does NOT sum to the 2850 total — the discrepancy under test):
    (8, 2) -> 298,
    (8, 3) -> 1037,
    (8, 4) -> 795,
    (8, 5) -> 537,
    (8, 6) -> 203,
    (8, 7) -> 20
  )

  def main(args: Array[String]): Unit =
    val rows = if args.isEmpty then (5 to 8).toList else args.toList.map(_.toInt)
    for n <- rows do
      var rowSum = 0
      for m <- 2 until n do
        val t0    = System.nanoTime()
        val cells = SymbolAssembly.solveCell(n, m, parallelism = 8)
        val found = cells.values.map(_.keys.size).sum
        val caps  = cells.values.count(_.capped)
        rowSum += found
        val exp   = known.get((n, m)).map(e =>
          s" expected=$e ${if e == found then "OK" else "!!MISMATCH!!"}"
        ).getOrElse("")
        println(f"cell ($n,$m): FOUND=$found$exp capped=$caps in ${(System.nanoTime() - t0) / 1e9}%.0f s")
      println(s"row $n off-diagonal sum (m=2..${n - 1}) = $rowSum")
