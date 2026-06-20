package io.github.scala_tessella.dcel

/** ADR-0023 corona-first spike: measures whether the euclidean (360°) prune cuts the PARTIAL D-set tree (the
  * premise of a corona-first generator) or only completed symbols. `coronaStats` walks DISTINCT partial
  * oriented D-sets that pass the early angle prune (visited-set dedup, fill-order-independent); compare its
  * node count to the oriented generator's COMPLETE-D-set count and its euclidean subset. If nodes ≈ the
  * complete count, the prune fires late (partial coronas are under-angle until closure) ⇒ corona-first cannot
  * help. If nodes ≈ the euclidean subset, it cuts the tree ⇒ corona-first is viable.
  */
object CoronaProbe:

  def main(args: Array[String]): Unit =
    for sz <- args.headOption.map(_.toInt).map(List(_)).getOrElse(List(16, 20, 24)) do
      val (allTotal, allEucl, allReg) = DelaneySymbols.orientedGenerationStats(3, sz)
      val (nodes, reg)                = DelaneySymbols.coronaStats(3, sz)
      val correct                     = if reg == allReg then "OK" else s"MISMATCH (reg=$reg vs $allReg)"
      println(
        f"maxSize=$sz%2d  oriented-complete=$allTotal%8d  euclidean-complete=$allEucl%7d  " +
          f"corona-partial-nodes=$nodes%9d  reg=$reg ($correct)"
      )
