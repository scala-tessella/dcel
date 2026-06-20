package io.github.scala_tessella.dcel

/** ADR-0023 Stage 1 — push the ORIENTED generator past n = 3, where generate-all (ADR-0022) and the bounded-V
  * assembler (ADR-0025) both wall. Counts the Krötenheerdt tilings it recovers per n at a given oriented
  * chamber budget, vs A068600 = 11,20,39,33,15,10,7. Counts at a finite budget are LOWER bounds (a tiling
  * appears once its oriented double fits); the question is how far n = 4+ comes into reach.
  */
object OrbifoldPushProbe:

  private val a068600 = Map(1 -> 11, 2 -> 20, 3 -> 39, 4 -> 33, 5 -> 15, 6 -> 10, 7 -> 7)

  def main(args: Array[String]): Unit =
    val oriSize = args.headOption.map(_.toInt).getOrElse(32)
    val maxN    = args.lift(1).map(_.toInt).getOrElse(7)
    println(s"orientedRegularSymbols(maxN=$maxN, oriSize=$oriSize) ...")
    val t0      = System.nanoTime()
    val res     = DelaneySymbols.orientedRegularSymbols(maxN, oriSize)
    val ms      = (System.nanoTime() - t0) / 1000000
    val byN     = res.groupBy(_._1).view.mapValues(_.size).toMap
    println(s"  ${res.size} tilings in ${ms}ms")
    for n <- 1 to maxN do
      val got = byN.getOrElse(n, 0)
      val tgt = a068600.getOrElse(n, 0)
      val tag = if got == tgt then "COMPLETE" else if got > 0 then s"partial" else "-"
      println(f"  n=$n  got $got%3d / $tgt%3d   $tag")
