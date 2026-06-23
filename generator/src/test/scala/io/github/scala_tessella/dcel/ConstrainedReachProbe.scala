package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature

/** TYPE-SET-CONSTRAINED reach for level `n`: run the symmetry grower ONCE PER candidate type-set, constrained
  * to that target (`targetTypes`), so each per-target search is tiny — letting `maxFaces` go high enough to
  * close the large C₂ cells that the unconstrained global search can't afford. Prints, per type-set, the
  * reached/multiplicity and time; then the full rotation table (reached n-uniform tilings + centres) and the
  * count vs `TilingReference`. The pivot from brute global depth (ADR-0032, 2026-06-23).
  *
  * Run: `…ConstrainedReachProbe [n] [maxFaces] [perSetMinutes]`  (default n=4, maxFaces=96, 10 min/type-set)
  */
object ConstrainedReachProbe:

  private val angle = Map(2 -> "180°", 3 -> "120°", 4 -> "90°", 6 -> "60°")

  private def label(ts: Set[VertexSignature]): String =
    ts.map(_.mkString(".")).toList.sorted.mkString("; ")

  def main(args: Array[String]): Unit =
    val n        = args.headOption.map(_.toInt).getOrElse(4)
    val maxFaces = args.lift(1).map(_.toInt).getOrElse(96)
    val perSetMs = args.lift(2).map(_.toLong).getOrElse(10L) * 60000L
    val sets     = UnionDriver.candidateTypeSets(n)
    val t0       = System.nanoTime()
    def secs     = (System.nanoTime() - t0) / 1e9
    val table    = scala.collection.mutable.LinkedHashMap.empty[String, (Set[VertexSignature], Set[(String, Int)])]

    println(s"[constrained reach] n=$n maxFaces=$maxFaces perSet=${perSetMs / 60000}min — ${sets.size} type-sets")
    for (ts, i) <- sets.zipWithIndex do
      val b0   = System.nanoTime()
      val mult = UnionDriver.multiplicity(n, ts)
      val reached = KrotenheerdtTorusMapSearch.symmetryRotationReferenceParallel(
        maxN = n,
        maxFaces = maxFaces,
        maxMillis = perSetMs,
        targetTypes = ts,
        log = msg => println(f"        ${label(ts)}%-46s $msg")
      )
      val onTarget = reached.filter(_._2._1 == ts)
      onTarget.foreach((k, v) => table.getOrElseUpdate(k, v))
      println(
        f"  [${secs}%6.0fs] ${i + 1}%2d/${sets.size} ${label(ts)}%-46s reached=${onTarget.size}/$mult (+${(System.nanoTime() - b0) / 1e9}%.0fs)"
      )

    val expected = TilingReference.counts(n)
    val rows     = table.toList.map((_, v) => (label(v._1), v._2))
    println(f"\n=== n=$n CONSTRAINED rotation table: ${rows.size}/$expected tilings, ${secs}%.0fs ===")
    println(f"${"vertex-type set"}%-50s | rotation centres (centre : angle)")
    println("-" * 100)
    rows.sortBy(t => (t._1, t._2.toString)).foreach: (lbl, centres) =>
      val cs = centres.toList.sortBy((k, o) => (k, -o)).map((k, o) => s"$k ${angle.getOrElse(o, s"$o?")}").mkString(", ")
      println(f"$lbl%-50s | $cs")
    if expected - rows.size > 0 then println(s"\n(NOTE: ${expected - rows.size} short at maxFaces=$maxFaces)")
    val byOrder = rows.map((_, c) => if c.isEmpty then 0 else c.map(_._2).max)
    println(s"\npoint-group: " + List(6, 4, 3, 2).map(m => s"m$m=${byOrder.count(_ == m)}").mkString(" "))
    println("\n[done]")
