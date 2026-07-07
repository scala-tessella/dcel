package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.SymbolAssembly.*
import io.github.scala_tessella.dcel.VertexTypes.normalize

/** G2 gap, step 2: for each {3³.4²;4⁴} sibling, rebuild MY frame from its own induced stars, relabel its σ₀
  * into frame coordinates, then check (i) classify(frame, σ₀) reproduces its key, and (ii) the SAT
  * enumeration contains that σ₀ — splitting a classify bug from an encoding bug.
  */
object G2DiagProbe:
  def main(args: Array[String]): Unit =
    val target = Set(normalize(List(3, 3, 3, 4, 4)), normalize(List(4, 4, 4, 4)))
    val deep   = DelaneySymbols
      .enumerateSymbols(maxN = 2, maxSize = 18)
      .filter((_, sigs, _) => sigs.toSet == target)
    val solved = solveTypeSet(target)
    println(s"solveTypeSet keys=${solved.keys.size} models=${solved.models}")
    for (_, sigs, ds) <- deep do
      val key              = DelaneySymbols.canonicalKey(ds)
      val found            = solved.keys.contains(key)
      println(s"— sibling size=${ds.size} FOUND-BY-SOLVER=$found")
      // frame = this symbol's own stars, laid out orbit by orbit
      val orbs12           = ds.orbs.filter(o => o.i == 1 && o.j == 2)
      val frame            = Frame(inducedStars(ds))
      val toFrame          = orbs12.flatMap(_.elements).zipWithIndex.map((d, i) => d -> (i + 1)).toMap
      val s0f              = Array.fill(frame.size + 1)(0)
      for (d, i) <- toFrame do s0f(i) = toFrame(ds.get(0, d))
      val cls              = classify(frame, s0f)
      println(
        s"   classify(frame, its own σ₀) = ${cls.map(_ => "KEY").getOrElse("NONE")}, matches=${cls.exists(_._1 ==
            key)}"
      )
      val (models, capped) = enumerateSigma0(frame)
      val hit              = models.exists(_.sameElements(s0f))
      println(s"   SAT models=${models.size} capped=$capped containsOwnSigma0=$hit")
