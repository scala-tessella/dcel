package io.github.scala_tessella.dcel

/** Coverage check (ADR-0030): WHICH n=4 type-sets does the oriented-slice generator reach? The complementary-
  * engines hypothesis is that it covers the LARGE-CELL (high-symmetry, small minimal-symbol) tilings —
  * notably the 4.6.12/dodecagon-mixed ones — that the bounded-V dart assembler walls on (cell V≥19). So this
  * prints each reached n=4 tiling's sorted vertex-type set and flags the dodecagon-containing ones.
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.OrbifoldN4TypesProbe [oriSize]`
  */
object OrbifoldN4TypesProbe:

  def main(args: Array[String]): Unit =
    val oriSize = args.headOption.map(_.toInt).getOrElse(34)
    val par     = args.lift(1).map(_.toInt).getOrElse(math.max(1, Runtime.getRuntime.availableProcessors - 1))
    println(s"orientedRegularSymbolsParallel(maxN=4, oriSize=$oriSize, parallelism=$par) ...")
    val t0      = System.nanoTime()
    val res     = DelaneySymbols.orientedRegularSymbolsParallel(
      maxN = 4,
      maxSize = oriSize,
      parallelism = par,
      log = msg => { println(msg); System.out.flush() }
    )
    val ms      = (System.nanoTime() - t0) / 1000000
    val n4      = res.filter(_._1 == 4)
    println(s"n=4 reached ${n4.size}/33 in ${ms}ms")
    n4.map(_._2).map(_.map(_.mkString(".")).sorted).sortBy(_.mkString).foreach { types =>
      val dodec = types.exists(t => t.contains("12"))
      println(
        s"  ${types.mkString("; ")}${if dodec then "   <-- DODECAGON (bounded-V's large-cell wall)" else ""}"
      )
    }
