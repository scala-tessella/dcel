package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** DIAGNOSE why the grower misses 3 of the 39 n=3 tilings (ADR-0035/0036). For each of the 3 deficit
  * type-sets, list the ORACLE's cells (our own sound generate-all engine — fair to use for debugging, not for
  * sourcing a count) with their INTRINSIC symmetry read straight off the minimal D-symbol
  * ([[DelaneySymbols.orbifoldSignature]] = cone/rotation orders, orientability, mirrors;
  * [[DelaneySymbols.hasRotation]]), and mark which the grower REACHES vs MISSES (matching in the shared
  * canonical-key space). The missed cells' symmetry vs the reached ones' is the answer: a different rotation
  * order/centre type (⇒ seed gap), a mirror / non-orientable feature (⇒ glide/reflection group the rotation
  * grower can't seed), etc.
  *
  * Run: `…GrowerGapDiagnosisProbe [oracleMaxSize] [growerMaxFaces] [growerPerSetMin]` (default 24 / 96 / 4)
  */
object GrowerGapDiagnosisProbe:
  private def ts(s: String): Set[VertexSignature]    =
    s.split(',').toList.map(t => normalize(t.split('.').map(_.toInt).toList)).toSet
  private def label(t: Set[VertexSignature]): String = t.map(_.mkString(".")).toList.sorted.mkString("; ")

  private val deficits = List(
    ts("3.3.6.6,3.4.4.6,3.6.3.6"),
    ts("3.3.6.6,3.6.3.6,6.6.6"),
    ts("3.3.3.3.3.3,3.3.3.3.6,3.3.6.6")
  )

  def main(args: Array[String]): Unit =
    val oracleSize = args.headOption.map(_.toInt).getOrElse(24)
    val maxFaces   = args.lift(1).map(_.toInt).getOrElse(96)
    val perSetMs   = args.lift(2).map(_.toLong).getOrElse(4L) * 60000L

    println(
      s"GrowerGapDiagnosisProbe: oracle maxSize=$oracleSize, grower maxFaces=$maxFaces, ${perSetMs / 60000}min/set"
    )
    println("  enumerating the oracle (all n=3) ...")
    val oracle = DelaneySymbols
      .enumerateSymbolsParallel(3, oracleSize, parallelism = 12)
      .groupBy(t => DelaneySymbols.canonicalKey(t._3))
      .values
      .map(_.head)
      .toList // dedup by canonical key
    println(s"  oracle distinct n≤3 tilings: ${oracle.count(_._1 <= 3)} (n=3: ${oracle.count(_._1 == 3)})")

    for t <- deficits do
      val cells    = oracle.filter(c => c._2.toSet == t)
      // EXACT-type-set keys only (the constrained grower also records sub-tilings n<3 whose types ⊂ t)
      val reached  = KrotenheerdtTorusMapSearch
        .symmetryRotationReferenceParallel(
          3,
          maxFaces,
          parallelism = 12,
          maxMillis = perSetMs,
          targetTypes = t
        )
        .filter(_._2._1 == t)
        .keySet
      val cellKeys = cells.map(c => DelaneySymbols.canonicalKey(c._3)).toSet
      val spurious = reached -- cellKeys // exact-type-set grower keys NOT in the oracle (should be ∅)
      val refMult  = UnionDriver.multiplicity(3, t)

      println(
        s"\n=== ${label(t)} : ORACLE=${cells.size}  REFERENCE=$refMult" +
          (if cells.size != refMult then "  ⚠ ORACLE≠REFERENCE (likely TilingReference transcription error)"
           else "") +
          s"  grower reached ${(reached intersect cellKeys).size} ==="
      )
      cells.sortBy(c => DelaneySymbols.canonicalKey(c._3)).foreach: c =>
        val key  = DelaneySymbols.canonicalKey(c._3)
        val mark = if reached.contains(key) then "  reached" else "MISSED  <—"
        println(
          f"  [$mark] ${DelaneySymbols.orbifoldSignature(c._3)}  hasRot=${DelaneySymbols.hasRotation(c._3)}"
        )
      if spurious.nonEmpty then
        println(
          s"  ⚠ ${spurious.size} grower key(s) for this type-set NOT in the oracle (would be a grower BUG)"
        )
    println("\n[done]")
