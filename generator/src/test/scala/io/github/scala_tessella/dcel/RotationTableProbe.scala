package io.github.scala_tessella.dcel

/** The COMPLETE rotational-symmetry reference table for the n-uniform tilings (default n=2 → all 20), for
  * visual verification. Each distinct tiling (D-symbol key) is a row: its vertex-type set and its rotation
  * centres as (centre-type, angle), where order m ↦ 360/m° (2→180°, 3→120°, 4→90°, 6→60°). Built
  * engine-independently via [[UnionDriver.rotationTable]] (bounded-V → op → realizeCell → rotationCenters).
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.RotationTableProbe [n] [maxV]`
  */
object RotationTableProbe:

  private val angle = Map(2 -> "180°", 3 -> "120°", 4 -> "90°", 6 -> "60°")

  def main(args: Array[String]): Unit =
    val n     = args.headOption.map(_.toInt).getOrElse(2)
    val maxV  = args.lift(1).map(_.toInt).getOrElse(16)
    println(s"rotationTable(n=$n, maxV=$maxV) ...")
    val t0    = System.nanoTime()
    val table = UnionDriver.rotationTable(n, maxV)
    val secs  = (System.nanoTime() - t0) / 1e9

    val expected = TilingReference.counts(n)
    println(f"\n=== n=$n rotation-symmetry table: ${table.size}/$expected tilings, ${secs}%.0fs ===")
    println(f"${"vertex-type set"}%-40s | rotation centres (centre-type : angle)")
    println("-" * 90)
    table.toList
      .map((_, v) => (v._1.map(_.mkString(".")).toList.sorted.mkString("; "), v._2))
      .sortBy(_._1)
      .foreach: (label, centres) =>
        val cs = centres.toList
          .sortBy((kind, ord) => (kind, -ord))
          .map((kind, ord) => s"$kind ${angle.getOrElse(ord, s"$ord?")}")
          .mkString(", ")
        println(f"$label%-40s | $cs")
    val missing  = expected - table.size
    if missing > 0 then
      println(
        s"\n(NOTE: $missing tiling(s) short of the reference — bounded-V did not reach them at maxV=$maxV)"
      )
    println("\n[done]")
