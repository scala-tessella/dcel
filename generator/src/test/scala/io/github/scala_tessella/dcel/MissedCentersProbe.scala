package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Reveal the rotation centres of the multiplicity-2 n=2 siblings the symmetry grower CAPTURES away
  * ({4⁴;3³.4²}, {3⁶;3³.4²}): bounded-V reaches them, exposes the torus op, realizeCell turns each into a
  * geometric cell, rotationCenters reads its centres. The question: does the captured sibling have a centre
  * (edge-midpoint / non-square) the grower could reach without the 4⁴-capture?
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.MissedCentersProbe`
  */
object MissedCentersProbe:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  def main(args: Array[String]): Unit =
    val sets = List(
      Set("4.4.4.4", "3.3.3.4.4"),
      Set("3.3.3.3.3.3", "3.3.3.4.4")
    )
    for ts <- sets do
      val r = BucketAssembly.enumerateBucket(ts.map(sig), maxV = 10)
      println(s"\n${ts.toList.sorted} — ${r.keys.size} distinct tiling(s):")
      r.keys.toList.sorted.foreach: key =>
        KrotenheerdtTorusMapSearch.realizeCell(r.ops(key)) match
          case Some((faces, pv, pw)) =>
            val centres = KrotenheerdtTorusMapSearch.rotationCenters(faces, pv, pw)
            println(
              s"   ${faces.map(_.size).sorted.mkString("[", ",", "]")} faces  centres=${centres.toList.sorted}"
            )
          case None                  => println(s"   (realizeCell failed for $key)")
    println("\n[done]")
