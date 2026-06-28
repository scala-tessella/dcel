package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.ProfileAutomaton.{Profile, fillLowest, seedsC}
import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Debug why the pure-hexagon type-set `3.3.6.6;3.6.3.6;6.6.6` growth STALLS at c=2√3 (ADR-0038): inspect the
  * seeds and their `fillLowest` successors — are the seeds valid hexagon cuts, and does growth have any
  * type-set-valid continuation?
  *
  * Run: `…HexStallProbe`
  */
object HexStallProbe:
  private val ts: Set[VertexSignature] =
    Set(normalize(List(3, 3, 6, 6)), normalize(List(3, 6, 3, 6)), normalize(List(6, 6, 6)))

  private def show(p: Profile): String =
    p.verts.toList
      .sortBy(v => (v.pos.toBigPoint.y.toDouble, v.pos.toBigPoint.x.toDouble))
      .map: v =>
        val b = v.pos.toBigPoint
        f"(${b.x.toDouble}%.2f,${b.y.toDouble}%.2f):[${v.fan.map(_._2).mkString(",")}]"
      .mkString(" ")

  def main(args: Array[String]): Unit =
    val c     = ZetaPoint(0, 4, 0, -2) // 2√3
    val seeds = seedsC(c)
    println(s"c=2√3, type-set={3.3.6.6,3.6.3.6,6.6.6}: ${seeds.size} seeds\n")

    seeds.zipWithIndex.foreach: (s, i) =>
      val succ = fillLowest(s)
      val inTs = succ.filter((_, t, _) => ts.contains(t))
      println(f"seed $i%d (${s.verts.size} verts): ${show(s)}")
      println(
        f"   fillLowest → ${succ.size} succ; types=${succ.map(_._2.mkString(".")).distinct.mkString(", ")}; in-ts=${inTs.size}"
      )

    // try to grow a pure-6.6.6 tiling: from each seed, repeatedly take a type-set successor (prefer 6.6.6),
    // print the trace until it dies or repeats a shape
    println("\n=== growth trace (taking a type-set-valid successor each step) ===")
    val hex = normalize(List(6, 6, 6))
    seeds.zipWithIndex.foreach: (s0, i) =>
      var cur  = s0
      var step = 0
      var dead = false
      while step < 12 && !dead do
        val opts = fillLowest(cur).filter((_, t, _) => ts.contains(t))
        if opts.isEmpty then { println(f"seed $i%d step $step%2d: DEAD-END — ${show(cur)}"); dead = true }
        else
          val pick = opts.find(_._2 == hex).getOrElse(opts.head)
          cur = pick._1; step += 1
      if !dead then println(f"seed $i%d: grew $step steps OK (no stall)")

    // does a WIDER circumference reach the 3-uniform mixed cell? (2√3 = 2 hexagons wide may be too narrow)
    println("\n=== emitted cells vs circumference (√3-family) ===")
    val circs = List(
      ("2√3", ZetaPoint(0, 4, 0, -2)),
      ("3√3", ZetaPoint(0, 6, 0, -3)),
      ("4√3", ZetaPoint(0, 8, 0, -4)),
      ("6√3", ZetaPoint(0, 12, 0, -6))
    )
    for (name, cc) <- circs do
      val t0   = System.nanoTime()
      val emit = ProfileAutomaton.enumerateForTypeSetC(cc, ts, maxNodes = 20000)
      val sec  = (System.nanoTime() - t0) / 1e9
      println(f"  c=$name%-4s seeds=${seedsC(cc).size}%2d emitted=${emit.size} (${sec}%.1fs)")
    println("[done]")
