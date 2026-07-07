package io.github.scala_tessella.dcel

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}

/** Gate G4 and beyond: run the SAT assembler over the fair candidates for the given levels (default 4..8),
  * reporting per-level counts vs A068600 (33, 15, 10, 7, 0) and wall time. Candidate sets are independent —
  * solved on a fixed thread pool (each `solveTypeSet` owns its solver; the shared derivations are immutable).
  */
object G4GateProbe:
  def main(args: Array[String]): Unit =
    val levels = if args.isEmpty then (4 to 8).toList else args.toList.map(_.toInt)
    for n <- levels do
      val t0     = System.nanoTime()
      val cands  = TypeCompatibility.candidates(n).toVector
      val done   = new AtomicInteger(0)
      // 8 workers / ≤10g heap: the box has 31 GiB RAM and only 1 GiB swap — a 24g/14-worker run thrashed
      // the desktop and crashed the IDE (see reference_machine_memory_limits)
      val pool   = Executors.newFixedThreadPool(8)
      val tasks  = cands.map: ts =>
        pool.submit: () =>
          val s0 = System.nanoTime()
          val r  = SymbolAssembly.solveTypeSet(ts)
          val dt = (System.nanoTime() - s0) / 1e9
          if dt > 30 then
            println(
              f"  n=$n SLOW SET ($dt%.0f s, models=${r.models}) ${ts.map(_.mkString(".")).toList.sorted.mkString("{", "; ", "}")}"
            )
          val k  = done.incrementAndGet()
          if k % 100 == 0 then
            println(f"  n=$n progress $k/${cands.size} (${(System.nanoTime() - t0) / 1e9}%.0f s)")
          (ts, r)
      val perSet = tasks.map(_.get())
      pool.shutdown()
      pool.awaitTermination(1, TimeUnit.MINUTES)
      val found  = perSet.map(_._2.keys.size).sum
      val capped = perSet.count(_._2.capped)
      val secs   = (System.nanoTime() - t0) / 1e9
      println(
        f"n=$n: candidates=${cands.size} FOUND=$found expected=${TilingReference.counts.getOrElse(n, 0)} capped=$capped in $secs%.1f s"
      )
      for (ts, r) <- perSet if r.keys.nonEmpty do
        println(
          s"  REALIZED ${ts.map(_.mkString(".")).toList.sorted.mkString("{", "; ", "}")} x${r.keys.size}"
        )
      if n <= 5 then
        TilingReference.rawWikipediaN3to5.get(n).map(_.map(N4DriverProbe.parseRow)).foreach: rows =>
          val foundMultiset =
            perSet.flatMap((ts, r) => List.fill(r.keys.size)(ts)).groupBy(
              identity
            ).view.mapValues(_.size).toMap
          val refMultiset   = rows.groupBy(identity).view.mapValues(_.size).toMap
          val miss          =
            refMultiset.toList.flatMap((ts, k) =>
              if foundMultiset.getOrElse(ts, 0) < k then List(ts) else Nil
            )
          val extra         =
            foundMultiset.toList.flatMap((ts, k) =>
              if refMultiset.getOrElse(ts, 0) < k then List(ts) else Nil
            )
          if miss.nonEmpty then
            println(
              s"  MISSING vs reference: ${miss.map(_.map(_.mkString(".")).toList.sorted.mkString("{", ";", "}"))}"
            )
          if extra.nonEmpty then
            println(
              s"  EXTRA vs reference: ${extra.map(_.map(_.mkString(".")).toList.sorted.mkString("{", ";", "}"))}"
            )
          if miss.isEmpty && extra.isEmpty then println(s"  multiset vs reference: EXACT")
