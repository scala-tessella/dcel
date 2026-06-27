package io.github.scala_tessella.dcel

/** Quick diagnostic for the cylinder spike (ADR-0038): the reachable canonical-front count vs face budget
  * (does taut growth SATURATE ⇒ finite state space?) and the emitted D-symbol keys, with timing — no oracle.
  *
  * Run: `…CylProbe [circumference] [maxN]`
  */
object CylProbe:
  def main(args: Array[String]): Unit =
    val c      = args.headOption.map(_.toInt).getOrElse(2)
    val maxN   = args.lift(1).map(_.toInt).getOrElse(1)
    val perCap = args.lift(2).map(_.toLong).getOrElse(30000L)
    val h      = ZetaPoint(c.toLong, 0, 0, 0)
    println(s"CylProbe: circumference=$c maxN=$maxN perCap=$perCap")
    println("  reachable canonical fronts vs maxFaces (saturation ⇒ finite):")
    for mf <- List(4, 6, 8, 10, 12, 16) do
      val t0            = System.nanoTime()
      val (fronts, cap) = CylinderAutomaton.reachableFrontCount(h, maxN, mf, perCap)
      val secs          = (System.nanoTime() - t0) / 1e9
      println(f"    maxFaces=$mf%3d  fronts=$fronts%6d  capped=$cap  ${secs}%.1fs")
      System.out.flush()
    val t0     = System.nanoTime()
    val out    = CylinderAutomaton.enumerateAtH(h, maxN, maxFaces = 16, perCap)
    val secs   = (System.nanoTime() - t0) / 1e9
    println(
      f"  enumerateAtH(maxFaces=24): emitted=${out.emitted.size} keys, states=${out.states}, capped=${out.capped}, ${secs}%.1fs"
    )
    out.emitted.foreach((k, v) => println(s"    ${v._2.map(_.mkString("."))}  -> $k"))
    println("[done]")
