package io.github.scala_tessella.dcel

/** Phase-2 per-state PROFILE: run one seed through [[KrotenheerdtTorusMapSearch.profileSeed]] and print where
  * the per-state wall-clock goes (corona gate / tryClose / grow / canonicalKey), so the cost optimisation
  * targets the MEASURED hotspot rather than a guess.
  *
  * Run:
  * `generator/Test/runMain io.github.scala_tessella.dcel.SymmetryProfileProbe [seedLabel] [maxN] [maxFaces]`
  * (default: poly6/m6 — the central hexagon — at maxN=1 maxFaces=40)
  */
object SymmetryProfileProbe:

  def main(args: Array[String]): Unit =
    val label    = args.headOption.getOrElse("poly6/m6")
    val maxN     = args.lift(1).map(_.toInt).getOrElse(1)
    val maxFaces = args.lift(2).map(_.toInt).getOrElse(40)
    val seed     = KrotenheerdtTorusMapSearch.allSeeds.find(_.label == label)
      .getOrElse(sys.error(
        s"no seed labelled $label; have ${KrotenheerdtTorusMapSearch.allSeeds.map(_.label)}"
      ))

    println(s"profileSeed(${seed.label}, maxN=$maxN, maxFaces=$maxFaces) ...")
    val t0           = System.nanoTime()
    val (res, phase) = KrotenheerdtTorusMapSearch.profileSeed(seed, maxN, maxFaces)
    val ms           = (System.nanoTime() - t0) / 1e6

    println(f"\nstates=${res.states}  tilings=${res.tilings.size}  budgetHit=${res.budgetHit}  total=${ms /
        1000}%.1fs  (${res.states / (ms / 1000)}%.1f states/s)")
    val sum = phase.values.sum.max(1L)
    println("per-phase wall-clock (ms, % of measured):")
    phase.toList.sortBy(-_._2).foreach((k, v) => println(f"  $k%-14s ${v}%7d ms  ${100.0 * v / sum}%5.1f%%"))
    println(s"\ntilings: ${res.tilings.map(_._2.map(_.mkString(".")))}")

    println("\n--- tryClose sub-profile (boundaryGlueBases vs verifyCell loop) ---")
    val close      = KrotenheerdtTorusMapSearch.profileClose(seed, maxN, maxFaces)
    val callsPerSt = close("verifyCalls").toDouble / close("states").max(1L)
    println(
      f"  states=${close("states")} (should match the run above)  verifyCalls=${close("verifyCalls")}  ($callsPerSt%.1f calls/state)"
    )
    println(f"  boundaryGlueBases=${close("boundaryGlueBases_ms")}%7d ms")
    println(f"  verifyCell       =${close("verifyCell_ms")}%7d ms")
