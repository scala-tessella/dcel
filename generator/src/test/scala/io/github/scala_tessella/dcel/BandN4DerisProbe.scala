package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import io.github.scala_tessella.dcel.geometry.BigPoint

/** DE-RISK the band engine for n≥4 (decision: is finishing the band engine worth it?). For n=4 type-sets
  * analogous to the n=3 banded gap families, realize cells (bounded-V), classify each by lattice ASPECT
  * (banded = anisotropic, aspect>1.5), and run the VALIDATED `cutFeedDiagnose` on them. The decisive
  * questions:
  *   - do n=4 BANDED cells stay REPRESENTABLE with a 30°-aligned band axis (same kind of obstacle as n=3 ⇒
  *     completing the n=3 obstacles — multi-row + finer √3 — would CARRY to n=4), or do they introduce a NEW
  *     obstacle (non-representable ⇒ the band engine does not extend)?
  *   - does the engine FEED (fully reproduce) any n=4 banded cell at all?
  * Precondition (BandN4Spec, must be green first): the cut machinery is SOUND on n=4 cells.
  *
  * Run: `…BandN4DerisProbe [maxV]`
  */
object BandN4DerisProbe:
  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  private val cases: List[(String, Set[VertexSignature])] = List(
    "tri/hex {3⁶;3⁴.6;3².6²;6³}"    -> Set("3.3.3.3.3.3", "3.3.3.3.6", "3.3.6.6", "6.6.6").map(sig),
    "tri/sq {3⁶;3³.4²;3².4.3.4;4⁴}" -> Set("3.3.3.3.3.3", "3.3.3.4.4", "3.3.4.3.4", "4.4.4.4").map(sig)
  )

  /** Shortest non-zero lattice vector |h| and the orthogonal extent covol/|h| (band height); aspect = ratio.
    */
  private def aspectOf(vB: BigPoint, wB: BigPoint): Double =
    val (vx, vy) = (vB.x.toDouble, vB.y.toDouble); val (wx, wy) = (wB.x.toDouble, wB.y.toDouble)
    val covol    = math.abs(vx * wy - vy * wx)
    var shortest = Double.MaxValue
    for m <- -6 to 6; n <- -6 to 6 if !(m == 0 && n == 0) do
      val len = math.hypot(m * vx + n * wx, m * vy + n * wy)
      if len > 1e-6 && len < shortest then shortest = len
    (covol / shortest) / shortest

  def main(args: Array[String]): Unit =
    val maxV                          = args.headOption.map(_.toInt).getOrElse(11)
    println(s"BandN4DerisProbe: maxV=$maxV")
    var banded, bandedRepr, bandedFed = 0
    for (name, ts) <- cases do
      val ops = BucketAssembly.enumerateBucket(ts, maxV, targetCount = 8).ops
      println(s"\n=== $name  realized=${ops.size} cells ===")
      for (key, op) <- ops do
        val asp = KrotenheerdtTorusMapSearch.realizeCell(op).map((_, vB, wB) => aspectOf(vB, wB))
        val r   = ProfileAutomaton.cutFeedDiagnose(op, key, ts, maxNodes = 20000)
        val tag = asp match
          case Some(a) if a > 1.5 => "BANDED "
          case Some(_)            => "isotrop"
          case None               => "no-geo "
        if asp.exists(_ > 1.5) then
          banded += 1
          if r.representable then bandedRepr += 1
          if r.fedEmitsKey then bandedFed += 1
        println(f"  $tag aspect=${asp.getOrElse(0.0)}%4.1f  repr=${r.representable}%-5s bandH=${
            r.bandAxisHorizontal
          }%-5s fed=${r.fedEmitsKey}%-5s cuts=${r.cutProfiles}  ${r.note}")
    println(f"\n=== BANDED n=4: total=$banded representable=$bandedRepr fed=$bandedFed ===")
    println("VERDICT: if representable≈banded ⇒ same obstacle class as n=3 (multi-row/√3 carries); " +
      "if representable≪banded ⇒ NEW n=4 obstacle (band axis not 30°-aligned).")
    println("[done]")
