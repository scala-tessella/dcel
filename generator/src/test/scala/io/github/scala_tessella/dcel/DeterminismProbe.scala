package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Pin down the non-monotonic reach: run the suspicious n=4 type-set {3².4.3.4; 3².6²; 3.4².6; 6³}
  * constrained, REPEATEDLY at maxFaces ∈ {80,96} and parallelism ∈ {15,1}, printing the reached count + the
  * actual D-symbol KEYS each run. Distinguishes (a) a parallel RACE (same config varies run-to-run;
  * parallelism=1 stable) from (b) config-dependence (deterministic per config but 80≠96 ⇒ a canonicalKey
  * ambiguity / hash collision).
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.DeterminismProbe`
  */
object DeterminismProbe:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  def main(args: Array[String]): Unit =
    val ts                                                = Set(sig("3.3.4.3.4"), sig("3.3.6.6"), sig("3.4.4.6"), sig("6.6.6"))
    def run(maxFaces: Int, par: Int): (Int, List[String]) =
      val gr = KrotenheerdtTorusMapSearch
        .symmetryRotationReferenceParallel(
          maxN = 4,
          maxFaces = maxFaces,
          parallelism = par,
          maxMillis = 300000L,
          targetTypes = ts
        )
        .filter(_._2._1 == ts)
      (gr.size, gr.keySet.toList.sorted)
    for (maxFaces, par, reps) <- List((80, 15, 3), (96, 15, 3), (80, 1, 1), (96, 1, 1)) do
      for r <- 1 to reps do
        val (cnt, keys) = run(maxFaces, par)
        println(
          s"maxFaces=$maxFaces par=$par run=$r  reached=$cnt  keys=${keys.map(_.takeWhile(_ != ';')).mkString("[", ", ", "]")}"
        )
    println("\n[done]")
