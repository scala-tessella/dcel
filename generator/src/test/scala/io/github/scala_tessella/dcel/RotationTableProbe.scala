package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature

/** The COMPLETE rotational-symmetry reference table for the n-uniform tilings (default n=2 → all 20), for
  * visual verification. Each distinct tiling (D-symbol key) is a row: its vertex-type set and its rotation
  * centres as (centre-type, angle), where order m ↦ 360/m° (2→180°, 3→120°, 4→90°, 6→60°). Built
  * engine-independently per type-set (bounded-V → torus op → realizeCell → rotationCenters), with a
  * per-type-set progress line so the run is NOT a blind wait.
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.RotationTableProbe [n] [maxV]`
  */
object RotationTableProbe:

  private val angle = Map(2 -> "180°", 3 -> "120°", 4 -> "90°", 6 -> "60°")

  def main(args: Array[String]): Unit =
    val n     = args.headOption.map(_.toInt).getOrElse(2)
    val maxV  = args.lift(1).map(_.toInt).getOrElse(20)
    val sets  = UnionDriver.candidateTypeSets(n)
    println(s"rotation-symmetry table n=$n, maxV=$maxV, ${sets.size} distinct type-sets ...")
    val t0    = System.nanoTime()
    // dkey -> (type-set, centres); built per type-set with a live progress line
    val table =
      scala.collection.mutable.LinkedHashMap.empty[String, (Set[VertexSignature], Set[(String, Int)])]
    for (ts, i) <- sets.zipWithIndex do
      val b0    = System.nanoTime()
      val mult  = UnionDriver.multiplicity(n, ts)
      val r     = BucketAssembly.enumerateBucket(ts, maxV, targetCount = mult)
      var got   = 0
      r.keys.foreach: key =>
        KrotenheerdtTorusMapSearch.realizeCell(r.ops(key)).foreach: (faces, pv, pw) =>
          table.getOrElseUpdate(key, (ts, KrotenheerdtTorusMapSearch.rotationCenters(faces, pv, pw)))
          got += 1
      val label = ts.map(_.mkString(".")).toList.sorted.mkString("; ")
      println(
        f"  [${(System.nanoTime() - t0) / 1e9}%5.0fs] ${i + 1}%2d/${sets.size} $label%-38s found=$got/$mult states=${r.states} budgetHit=${r.budgetHit} (+${(System.nanoTime() -
            b0) / 1e9}%.0fs)"
      )

    val expected = TilingReference.counts(n)
    println(
      f"\n=== n=$n rotation-symmetry table: ${table.size}/$expected tilings, ${(System.nanoTime() - t0) / 1e9}%.0fs ==="
    )
    println(f"${"vertex-type set"}%-40s | rotation centres (centre-type : angle)")
    println("-" * 92)
    table.toList
      .map((_, v) => (v._1.map(_.mkString(".")).toList.sorted.mkString("; "), v._2))
      .sortBy(_._1)
      .foreach: (label, centres) =>
        val cs = centres.toList.sortBy((kind, ord) => (kind, -ord)).map((kind, ord) =>
          s"$kind ${angle.getOrElse(ord, s"$ord?")}"
        ).mkString(", ")
        println(f"$label%-40s | $cs")
    if expected - table.size > 0 then
      println(
        s"\n(NOTE: ${expected - table.size} short of the reference — bounded-V did not reach them at maxV=$maxV)"
      )
    println("\n[done]")
