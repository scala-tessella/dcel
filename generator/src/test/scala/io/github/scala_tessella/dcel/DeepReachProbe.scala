package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** FOCUSED reachability-at-depth spike (ADR-0035 #1 gate): run the existing DFS symmetry grower, constrained,
  * on ONLY the three n=3 DEFICIT type-sets (the large-domain chiral C₂ residual, stuck at 2/3, 1/2, 2/3 at
  * maxFaces=96), at a HIGH maxFaces and generous per-set cap. The question this answers: does DEPTH ALONE
  * reach these cells? If they close (3/3, 2/2, 3/3), orbifold-domain growth's ~2× (same tree, half the cost)
  * is a worthwhile build. If the tree explodes (still short after a long cap), no growth method helps and the
  * levers are the type-targeted oracle (#2) / SAT (#3). Reuses the DFS driver — orbifold-growth would explore
  * the SAME tree, so this is the honest proxy for its reach (see ADR-0035 §1 re-analysis).
  *
  * Run: `…DeepReachProbe [maxFaces] [perSetMinutes]` (default maxFaces=160, 12 min/set)
  */
object DeepReachProbe:
  private def ts(s: String): Set[VertexSignature]    =
    s.split(',').toList.map(t => normalize(t.split('.').map(_.toInt).toList)).toSet
  private def label(t: Set[VertexSignature]): String = t.map(_.mkString(".")).toList.sorted.mkString("; ")

  // the 3 n=3 deficit type-sets (sig form), with their A068600 multiplicity and the maxFaces=96 reach
  private val deficits = List(
    (ts("3.3.6.6,3.4.4.6,3.6.3.6"), 3, 2),
    (ts("3.3.6.6,3.6.3.6,6.6.6"), 2, 1),
    (ts("3.3.3.3.3.3,3.3.3.3.6,3.3.6.6"), 3, 2)
  )

  def main(args: Array[String]): Unit =
    val maxFaces = args.headOption.map(_.toInt).getOrElse(160)
    val perSetMs = args.lift(1).map(_.toLong).getOrElse(12L) * 60000L
    println(
      s"DeepReachProbe: maxFaces=$maxFaces perSet=${perSetMs / 60000}min — ${deficits.size} n=3 deficit sets"
    )
    for (t, mult, was96) <- deficits do
      val b0       = System.nanoTime()
      val reached  = KrotenheerdtTorusMapSearch.symmetryRotationReferenceParallel(
        maxN = 3,
        maxFaces = maxFaces,
        maxMillis = perSetMs,
        targetTypes = t,
        log = msg => println(f"    ${label(t)}%-40s $msg")
      )
      val onTarget = reached.count(_._2._1 == t)
      val secs     = (System.nanoTime() - b0) / 1e9
      val verdict  = if onTarget >= mult then "CLOSED ✓" else s"STILL SHORT (was $was96/$mult @96)"
      println(f">>> ${label(t)}%-40s reached=$onTarget/$mult  [$verdict]  ${secs}%.0fs\n")
    println("[done]")
