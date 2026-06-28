package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.ProfileAutomaton.{Profile, fillLowest, seedsC}
import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Debug the cut-and-feed harness on the 4⁴ positive control: print the realized lattice, the cut profiles
  * the harness builds, the engine's own working seed (`seedsC`), and the first cut profile's `fillLowest`
  * successors (with cell-consistency). Pinpoints why feed/trace fail.
  */
object CutFeedDebugProbe:
  private def ts(s: String): Set[VertexSignature] =
    s.split(',').toList.map(t => normalize(t.split('.').map(_.toInt).toList)).toSet

  private def show(p: Profile): String =
    p.verts.toList
      .sortBy(v => (v.pos.toBigPoint.y.toDouble, v.pos.toBigPoint.x.toDouble))
      .map { v =>
        val b = v.pos.toBigPoint
        f"(${b.x.toDouble}%.2f,${b.y.toDouble}%.2f):[${v.fan.map { case (s, m) => s"$m@$s" }.mkString(",")}]"
      }
      .mkString(" ")

  def main(args: Array[String]): Unit =
    val t      = ts("4.4.4.4")
    val oracle =
      DelaneySymbols.enumerateSymbolsParallel(1, 16, parallelism = 4).filter(_._1 == 1).filter(_._2.toSet ==
        t)
    val sym    = oracle.head._3
    val key    = DelaneySymbols.canonicalKey(sym)
    val op     = BucketAssembly.enumerateBucket(t, maxV = 8, targetCount = 1).ops(key)

    val Some((faces, deck, pvB, pwB)) = KrotenheerdtTorusMapSearch.realizeCellZ(op): @unchecked
    println(s"4⁴ key=$key")
    println(s"faces=${faces.map(_.size)} deck=${deck.map(d => (d.a0, d.a1, d.a2, d.a3))}")
    println(
      s"primitive lattice pvB=(${pvB.x.toDouble},${pvB.y.toDouble}) pwB=(${pwB.x.toDouble},${pwB.y.toDouble})"
    )

    val c2 = ZetaPoint(2, 0, 0, 0)
    println(s"\nengine seedsC(c=2): ${seedsC(c2).size} seeds")
    seedsC(c2).take(6).foreach(s => println(s"  SEED ${show(s)}"))
    println(s"engine finds 4⁴ at c=2: ${ProfileAutomaton.enumerateForTypeSetC(c2, t).keySet.contains(key)}")

    println(s"\nharness cut profiles:")
    val frame = ProfileAutomaton.representFrame(op).get
    println(
      s"  c=(${frame.c.a0},${frame.c.a1},${frame.c.a2},${frame.c.a3}) rv1=${frame.rv1} rv2=${frame.rv2}"
    )
    frame.profs.foreach { p =>
      println(s"  CUT ${show(p)}")
      val fedOne = ProfileAutomaton.enumerateFromSeeds(frame.c, t, List(p)).keySet
      println(
        s"      feeds→ ${fedOne.contains(key)} (emitted ${fedOne.size}: ${fedOne.take(3).mkString(", ")})"
      )
    }

    println(s"\nharness cutFeedDiagnose:")
    val r = ProfileAutomaton.cutFeedDiagnose(op, key, t)
    println(s"  $r")
    println("[done]")
