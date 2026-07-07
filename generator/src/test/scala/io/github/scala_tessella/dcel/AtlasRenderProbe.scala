package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature

/** Render the COMPLETE A068600 atlas: re-enumerate each level with the SAT assembler and draw every found
  * tiling as an SVG (banner = level + type-set; filename = level, index, compact type-set) under
  * `results/a068600-atlas/`. Deterministic ordering (type-set string, then canonical key) so re-runs name
  * files identically.
  */
object AtlasRenderProbe:
  private def compress(sig: VertexSignature): String =
    sig
      .foldRight(List.empty[(Int, Int)]):
        case (p, (q, k) :: rest) if q == p => (q, k + 1) :: rest
        case (p, acc)                      => (p, 1) :: acc
      .map((p, k) => if k == 1 then s"$p" else s"$p^$k")
      .mkString(".")

  def main(args: Array[String]): Unit =
    val levels = if args.isEmpty then (1 to 7).toList else args.toList.map(_.toInt)
    val dir    = java.nio.file.Path.of("results", "a068600-atlas")
    for n <- levels do
      val t0      = System.nanoTime()
      val results = SymbolAssembly.enumerate(n, parallelism = 8)
      var idx     = 0
      val sorted  = results.toList
        .filter(_._2.tilings.nonEmpty)
        .sortBy((ts, _) => ts.toList.map(_.mkString(".")).sorted.mkString(";"))
      for
        (ts, r)    <- sorted
        (key, sym) <- r.tilings.toList.sortBy(_._1)
      do
        idx += 1
        val types = ts.toList.map(compress).sorted.mkString(";")
        SymbolRenderer.render(
          sym,
          dir.resolve(f"n$n-t$idx%02d-[$types].svg"),
          banner = s"A068600  n=$n  t$idx  [$types]  chambers=${sym.size}"
        )
      println(f"n=$n: rendered $idx tilings in ${(System.nanoTime() - t0) / 1e9}%.0f s")
