package io.github.scala_tessella.dcel

object KrotProbe:
  def main(args: Array[String]): Unit =
    val n     = args.headOption.map(_.toInt).getOrElse(1)
    val maxV  = args.lift(1).map(_.toInt).getOrElse(30)
    val start = System.nanoTime
    val out   = KrotenheerdtSearch.enumerate(n, maxV, msg => { println(msg); System.out.flush() })
    val secs  = (System.nanoTime - start) / 1e9
    println(
      f"n=$n maxVertices=$maxV -> ${out.certified.size} tilings, states=${out.statesExplored}, ${secs}%.1f s"
    )
    println(s"rejections: ${out.rejections}")
    out.certified.foreach: c =>
      println(
        s"  ${c.vertexTypes.map(_.mkString(".")).toList.sorted.mkString("; ")} basis=(${c.basis._1.x.toDouble},${c.basis._1.y.toDouble})/(${c.basis._2.x.toDouble},${c.basis._2.y.toDouble}) key=${c.torusKey.take(120)}"
      )
