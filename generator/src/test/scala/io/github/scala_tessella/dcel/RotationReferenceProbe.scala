package io.github.scala_tessella.dcel

/** Phase-3 / symmetry-reference probe: print the ROTATIONAL SYMMETRY of each (free-grower-reachable)
  * n-uniform tiling — its [[KrotenheerdtTorusMapSearch.rotationCenters]] as (centre-type, order). The free
  * grower reaches the SMALL cells (incl. the ones the symmetry grower captures away), so this exposes the
  * rotation centres of the "missed" siblings (e.g. the square-centred {4⁴;3³.4²}) and tells us which SEED can
  * reach each tiling.
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.RotationReferenceProbe [maxN] [maxFaces]`
  */
object RotationReferenceProbe:

  def main(args: Array[String]): Unit =
    val maxN     = args.headOption.map(_.toInt).getOrElse(2)
    val maxFaces = args.lift(1).map(_.toInt).getOrElse(28)
    println(s"freeGrowerRotationReference(maxN=$maxN, maxFaces=$maxFaces) ...")
    val t0       = System.nanoTime()
    val ref      = KrotenheerdtTorusMapSearch.freeGrowerRotationReference(maxN, maxFaces)
    val secs     = (System.nanoTime() - t0) / 1e9

    println(f"\n=== ${ref.size} tilings, ${secs}%.1fs ===")
    // group the distinct tilings (by D-symbol key) under their vertex-type set; print each sibling's centres
    ref.toList
      .groupBy(_._2._1)
      .toList
      .sortBy((types, _) => types.map(_.mkString(".")).toList.sorted.mkString(";"))
      .foreach: (types, entries) =>
        val label = types.map(_.mkString(".")).toList.sorted.mkString("; ")
        println(s"\n$label  (${entries.size} distinct tiling(s)):")
        entries.foreach { case (_, (_, centers)) =>
          println(s"    rotationCenters = ${centers.toList.sorted}")
        }
