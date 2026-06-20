package io.github.scala_tessella.dcel

/** ADR-0023 orbit-bounded enumeration: does pruning generate-all by `closedVertexCount ≤ maxN` (a MONOTONIC,
  * early-firing bound on vertex-orbit count) shrink the tree enough to reach n = 4–7? Compares the bounded
  * tree to generate-all and tracks the regular tilings recovered as the chamber budget grows.
  */
object OrbitBoundedProbe:

  def main(args: Array[String]): Unit =
    val maxN  = args.headOption.map(_.toInt).getOrElse(7)
    val sizes = args.drop(1).map(_.toInt).toList match
      case Nil => List(14, 18, 22, 26)
      case xs  => xs
    for sz <- sizes do
      val t0           = System.nanoTime()
      val (obTot, obR) = DelaneySymbols.orbitBoundedStats(maxN, sz)
      val ms           = (System.nanoTime() - t0) / 1000000
      // generate-all comparison only where it is still cheap
      val allStr       =
        if sz <= 16 then
          val (gt, _, gr) = DelaneySymbols.generationStats(maxN, sz)
          f"generate-all=$gt%9d (reg=$gr%3d)"
        else "generate-all=(too big)"
      println(f"maxN≤$maxN maxSize=$sz%2d  orbit-bounded dsets=$obTot%9d reg=$obR%3d  $allStr  ${ms}ms")
