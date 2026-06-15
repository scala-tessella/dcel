package io.github.scala_tessella.dcel

object KrotLatticeProbe:
  def main(args: Array[String]): Unit =
    val n        = args.headOption.map(_.toInt).getOrElse(1)
    val k        = args.lift(1).map(_.toInt).getOrElse(3)
    val maxCovol = args.lift(2).map(_.toDouble).getOrElse(16.0)
    val parallel = args.lift(3).map(_.toInt).getOrElse(1)
    val start    = System.nanoTime
    val out      =
      KrotenheerdtLatticeSearch.enumerate(
        n,
        k,
        maxCovol,
        parallel,
        msg => { println(msg); System.out.flush() }
      )
    val secs     = (System.nanoTime - start) / 1e9
    println(
      f"n=$n k=$k -> ${out.tilings.size} tilings, bases=${out.basesTried}, states=${out.statesExplored}, ${secs}%.1f s"
    )
    out.tilings
      .map((types, key) => (types.map(_.mkString(".")).toList.sorted.mkString("; "), key))
      .sortBy(_._1)
      .foreach((c, key) => println(s"  $c  |  ${key.take(90)}"))
