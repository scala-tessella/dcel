package io.github.scala_tessella.dcel

/** Gate G4 and beyond: run the SAT assembler over the fair candidates for n = 4..8 sequentially, reporting
  * per-level counts vs A068600 (33, 15, 10, 7, 0) and wall time. Per-level early report so a long run still
  * yields data.
  */
object G4GateProbe:
  def main(args: Array[String]): Unit =
    val levels = if args.isEmpty then (4 to 8).toList else args.toList.map(_.toInt)
    for n <- levels do
      val t0     = System.nanoTime()
      val cands  = TypeCompatibility.candidates(n)
      var found  = 0
      var capped = 0
      var done   = 0
      val perSet = collection.mutable.ListBuffer.empty[(Set[VertexTypes.VertexSignature], Int)]
      for ts <- cands do
        val r = SymbolAssembly.solveTypeSet(ts)
        if r.capped then capped += 1
        if r.keys.nonEmpty then perSet += ts -> r.keys.size
        found += r.keys.size
        done += 1
        if done                               % 50 == 0 then
          println(
            s"  n=$n progress $done/${cands.size} found=$found (${(System.nanoTime() - t0) / 1e9}%.0f s)"
          )
      val secs   = (System.nanoTime() - t0) / 1e9
      println(
        f"n=$n: candidates=${cands.size} FOUND=$found expected=${TilingReference.counts.getOrElse(n, 0)} capped=$capped in $secs%.1f s"
      )
      if n <= 5 then
        val ref = TilingReference.rawWikipediaN3to5.get(n).map(_.map(N4DriverProbe.parseRow))
        ref.foreach: rows =>
          val foundMultiset =
            perSet.toList.flatMap((ts, k) => List.fill(k)(ts)).groupBy(identity).view.mapValues(_.size).toMap
          val refMultiset   = rows.groupBy(identity).view.mapValues(_.size).toMap
          val miss          = refMultiset.toList.flatMap((ts, k) =>
            if foundMultiset.getOrElse(ts, 0) < k then List(ts) else Nil
          )
          val extra         = foundMultiset.toList.flatMap((ts, k) =>
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
