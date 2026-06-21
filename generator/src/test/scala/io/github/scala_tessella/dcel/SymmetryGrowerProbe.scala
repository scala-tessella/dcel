package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Phase-2 DE-RISK spike (ADR-0023 architecture B). Runs the symmetry-first geometric grower
  * ([[KrotenheerdtTorusMapSearch.enumerateBySymmetry]]) at m=6 (central hexagon) and checks the GO/NO-GO
  * gate: (1) every tiling it finds is SOUND (its canonical key is a real oracle key, i.e.
  * classifyClosedMap-valid); (2) it reproduces the m=6 Archimedean (6³, 3.6.3.6, 3.4.6.4, and the both-walled
  * 4.6.12) key-for-key; (3) the search cost (states) is bounded — tracks domain size, does not explode.
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.SymmetryGrowerProbe [maxFaces]`
  */
object SymmetryGrowerProbe:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  def main(args: Array[String]): Unit =
    val maxFaces = args.headOption.map(_.toInt).getOrElse(48)

    // oracle keys for the m=6 Archimedean we expect the central-hexagon seed to reach (all are n=1)
    val oracleN1                             = DelaneySymbols.keyedTilings(1, 12)
    def oracleKey(t: String): Option[String] =
      oracleN1.find(_._2 == Set(sig(t))).map(_._3)
    val expect                               = List("6.6.6", "3.6.3.6", "3.4.6.4", "4.6.12").map(t => t -> oracleKey(t))
    // a fuller soundness oracle (n<=3) to check no spurious key
    val oracleN3                             = DelaneySymbols.keyedTilings(3, 18).map(_._3).toSet

    for n <- List(1, 2, 3) do
      val t0  = System.nanoTime()
      val res = KrotenheerdtTorusMapSearch.enumerateBySymmetry(m = 6, maxN = n, maxFaces = maxFaces)
      val ms  = (System.nanoTime() - t0) / 1e6
      println(
        f"\n=== m=6 maxN=$n maxFaces=$maxFaces : ${res.tilings.size} tilings, states=${res.states}, budgetHit=${res.budgetHit}, ${ms /
            1000}%.2fs ==="
      )
      res.tilings.foreach: (nn, types, key) =>
        val sound = oracleN3.contains(key)
        println(f"  n=$nn  ${types.map(_.mkString(".")).toList.sorted.mkString("; ")}%-45s ${
            if sound then "sound" else "SPURIOUS?"
          }")
      if n == 1 then
        println("  --- gate: expected m=6 Archimedean reached? ---")
        val foundKeys = res.tilings.map(_._3).toSet
        expect.foreach: (t, k) =>
          val ok = k.exists(foundKeys.contains)
          println(f"    $t%-10s ${if ok then "FOUND ✓" else "MISSING ✗"}")

    println("\n[spike complete]")
