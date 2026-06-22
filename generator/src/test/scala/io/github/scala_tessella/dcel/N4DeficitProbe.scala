package io.github.scala_tessella.dcel

/** Diagnose which n=4 tilings the symmetry grower + bounded-V miss: for each n=4 candidate type-set, print
  * its reference multiplicity. Cross-referenced with the reached counts (maxFaces=64 union table) this
  * pinpoints the deficit type-sets — the home of the missing tilings — so we can reason about which rotation
  * seed each needs (the lever, since more maxFaces was shown not to help).
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.N4DeficitProbe`
  */
object N4DeficitProbe:
  def main(args: Array[String]): Unit =
    val n    = args.headOption.map(_.toInt).getOrElse(4)
    val sets = UnionDriver.candidateTypeSets(n)
    val mult =
      sets.map(ts => ts.map(_.mkString(".")).toList.sorted.mkString("; ") -> UnionDriver.multiplicity(n, ts))
    println(
      s"n=$n: ${sets.size} distinct type-sets, multiplicities sum to ${mult.map(_._2).sum} (= count $n)"
    )
    mult.sortBy(_._1).foreach((lbl, m) => println(f"  $m%2d  $lbl"))
    println("\n[done]")
