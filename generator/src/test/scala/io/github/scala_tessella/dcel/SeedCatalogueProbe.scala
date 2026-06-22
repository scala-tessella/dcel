package io.github.scala_tessella.dcel

/** List the full current seed catalogue (`allSeeds`): label, rotation order m, #faces in the germ — grouped
  * by kind. Run: `generator/Test/runMain io.github.scala_tessella.dcel.SeedCatalogueProbe`
  */
object SeedCatalogueProbe:
  def main(args: Array[String]): Unit =
    val seeds                                                                       = KrotenheerdtTorusMapSearch.allSeeds
    println(s"TOTAL seeds: ${seeds.size}\n")
    def show(title: String, pred: KrotenheerdtTorusMapSearch.Seed => Boolean): Unit =
      val g = seeds.filter(pred)
      println(s"--- $title (${g.size}) ---")
      g.foreach(s => println(f"  ${s.label}%-22s  m=${s.m}  faces=${s.faces.size}"))
      println()
    show("polygon-centre (poly P / mM)", _.label.startsWith("poly"))
    show("edge-midpoint (edge P, m=2)", _.label.startsWith("edge"))
    show("vertex-centre (vtx SIG / mM)", _.label.startsWith("vtx"))
    println(s"by rotation order m: " + seeds.groupBy(_.m).view.mapValues(_.size).toMap.toList.sortBy(_._1))
    println("\n[done]")
