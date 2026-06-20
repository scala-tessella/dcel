package io.github.scala_tessella.dcel

object KrotProbe:
  def main(args: Array[String]): Unit =
    val n           = args.headOption.map(_.toInt).getOrElse(1)
    val maxV        = args.lift(1).map(_.toInt).getOrElse(30)
    val start       = System.nanoTime
    val factor      = args.lift(2).map(_.toDouble).getOrElse(3.5)
    val gate        = args.lift(3).map(_.toInt).getOrElse(60)
    val radius      = args.lift(4).map(_.toInt).getOrElse(5)
    val parallel    = args.lift(5).map(_.toInt).getOrElse(1)
    val coronaGate  = args.lift(6).exists(s => s == "1" || s.equalsIgnoreCase("true"))
    val coronaDepth = args.lift(7).map(_.toInt).getOrElse(3)
    val skipLargest = !args.lift(8).contains("0") // arg8="0" ⇒ full refinement (no skip)
    println(s"[config] coronaGrowthGate=$coronaGate coronaGateDepth=$coronaDepth skipLargest=$skipLargest")
    val out         =
      KrotenheerdtSearch.enumerate(
        n,
        maxV,
        factor,
        gate,
        radius,
        parallel,
        coronaGate,
        coronaDepth,
        skipLargest,
        log = msg => { println(msg); System.out.flush() }
      )
    val secs        = (System.nanoTime - start) / 1e9
    println(
      f"n=$n maxVertices=$maxV -> ${out.certified.size} tilings, states=${out.statesExplored}, ${secs}%.1f s"
    )
    println(s"rejections: ${out.rejections}")
    out.certified.foreach: c =>
      println(
        s"  ${c.vertexTypes.map(_.mkString(".")).toList.sorted.mkString("; ")} basis=(${c.basis._1.x.toDouble},${c.basis._1.y.toDouble})/(${c.basis._2.x.toDouble},${c.basis._2.y.toDouble}) key=${c.torusKey.take(120)}"
      )
