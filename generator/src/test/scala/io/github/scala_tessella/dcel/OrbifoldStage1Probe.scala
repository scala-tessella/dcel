package io.github.scala_tessella.dcel

/** ADR-0023 Stage 1 — the ORIENTED-slice generator. Validates it against the generate-all oracle key-for-key
  * (oriented results, keyed by their FULL minimal symbol, must be a subset of the oracle and recover the
  * rotation-symmetric tilings) and measures whether restricting generation to the oriented rotation orbifolds
  * shrinks the search vs generate-all.
  */
object OrbifoldStage1Probe:

  def main(args: Array[String]): Unit =
    val maxN    = args.lift(2).map(_.toInt).getOrElse(3)
    val oraSize = args.headOption.map(_.toInt).getOrElse(18)
    val oriSize = args.lift(1).map(_.toInt).getOrElse(20)

    println(s"oracle keyedTilings($maxN, $oraSize) ...")
    val oracle     = DelaneySymbols.keyedTilings(maxN, oraSize)
    val oracleKeys = oracle.map(_._3).toSet
    println(s"  oracle: ${oracle.size} tilings")

    println(s"oriented orientedRegularSymbols($maxN, $oriSize) ...")
    val t0       = System.nanoTime()
    val oriented = DelaneySymbols.orientedRegularSymbols(maxN, oriSize)
    val ms       = (System.nanoTime() - t0) / 1000000
    val oriKeys  = oriented.map(_._3).toSet
    println(s"  oriented: ${oriented.size} tilings in ${ms}ms")

    val recovered = oriKeys.intersect(oracleKeys)
    val spurious  = oriKeys.diff(oracleKeys)
    val missed    = oracleKeys.diff(oriKeys)
    println(s"  spurious (oriented not in oracle@$oraSize) = ${spurious.size}")
    // are the "spurious" actually REAL tilings beyond the oracle's size horizon? print them, and re-check
    // against a LARGER oracle.
    for (n, sigs, key) <- oriented if spurious.contains(key) do
      println(s"    spurious: n=$n types=${sigs.toSet.map(_.mkString("."))}")
    if spurious.nonEmpty then
      val bigSize       = oraSize + 6
      println(s"  re-checking spurious vs larger oracle keyedTilings($maxN, $bigSize) ...")
      val bigKeys       = DelaneySymbols.keyedTilings(maxN, bigSize).map(_._3).toSet
      val stillSpurious = spurious.diff(bigKeys)
      println(
        s"    of ${spurious.size} spurious, ${spurious.size - stillSpurious.size} are REAL (in oracle@$bigSize), ${stillSpurious.size} still unexplained ${
            if stillSpurious.isEmpty then "=> all real, oracle@" + oraSize + " was just too small"
            else "=> BUG"
          }"
      )
    println(s"  recovered ${recovered.size}/${oracleKeys.size} oracle tilings; missed ${missed.size}")
    for n <- 1 to maxN do
      val oraN = oracle.filter(_._1 == n).map(_._3).toSet
      val recN = oraN.intersect(oriKeys).size
      println(f"    n=$n  recovered $recN%2d / ${oraN.size}%2d")

    println("generation cost — generate-all vs oriented slice (total : euclidean : regular):")
    for sz <- List(10, 12, 14, 16) do
      val (gt, ge, gr) = DelaneySymbols.generationStats(maxN, sz)
      val (ot, oe, or) = DelaneySymbols.orientedGenerationStats(maxN, sz)
      println(f"  maxSize=$sz%2d  all: $gt%8d / $ge%6d / $gr%3d     oriented: $ot%8d / $oe%6d / $or%3d")
