package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Phase-3 UNION driver. The symmetry grower reaches the rotational majority (incl. the dodecagon/4.6.12
  * large-cell tilings); the bounded-V dart assembler reaches the small-cell residual — crucially the
  * ROTATION-FREE (p1/pg/pm/cm) tilings the grower structurally cannot reach (measured: 2 of the 20 n=2
  * tilings are rotation-free small cells, ADR-0032). Both engines now emit the SAME `DelaneySymbols` D-symbol
  * key (grower via `torusMapClassify`, bounded-V via `classifyClosedMap`), so the union is a clean key-set
  * union; both are SOUND, so by the project's validation principle a deduped union whose size equals
  * `TilingReference.counts(n)` is the EXACT set.
  */
object UnionDriver:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  // compact Wikipedia notation → vertex signature: "3^2.4.3.4", "3.4^2.6", "3.12^2", "6^3", "4^4", …
  private def parseVertex(v: String): VertexSignature      =
    normalize(v.trim.split('.').toList.flatMap { tok =>
      tok.split('^') match
        case Array(b)    => List(b.toInt)
        case Array(b, e) => List.fill(e.toInt)(b.toInt)
        case _           => Nil
    })
  private def parseTiling(t: String): Set[VertexSignature] = t.split(';').map(parseVertex).toSet

  /** Distinct candidate vertex-type sets for level `n`, from the ground-truth reference (the buckets
    * bounded-V runs over). n=1: the 11 Archimedean singletons; n=2: the distinct 2-type sets; n≥3: the
    * Wikipedia rows.
    */
  def candidateTypeSets(n: Int): List[Set[VertexSignature]] = n match
    case 1 => TilingReference.n1.toList.map(Set(_))
    case 2 => TilingReference.n2.distinct
    case k => TilingReference.rawWikipediaN3to5.getOrElse(k, Nil).map(parseTiling).distinct

  /** Bounded-V D-symbol keys: assemble each candidate type-set up to `maxV` and collect the canonical keys.
    */
  def boundedVKeys(typeSets: List[Set[VertexSignature]], maxV: Int, budget: Long = 20_000_000L): Set[String] =
    typeSets.flatMap(ts => BucketAssembly.enumerateBucket(ts, maxV, budget).tilings.map(_.key)).toSet

  /** GROUND-TRUTH rotation-symmetry table for level `n`: per distinct tiling (D-symbol key), its vertex-type
    * set and its [[KrotenheerdtTorusMapSearch.rotationCenters]] — obtained engine-independently by assembling
    * each candidate type-set with bounded-V, exposing the torus op, and realizing it geometrically
    * (`realizeCell`). Bounded-V reaches the n=2 cells (dodecagon cells are large-V but cheap-tree), so this
    * covers all the multiplicity siblings the growers individually miss/capture.
    */
  /** The reference multiplicity of a type-set (how many distinct tilings share it). */
  def multiplicity(n: Int, ts: Set[VertexSignature]): Int = n match
    case 1 => 1
    case 2 => TilingReference.n2.count(_ == ts)
    case k => TilingReference.rawWikipediaN3to5.getOrElse(k, Nil).map(parseTiling).count(_ == ts)

  def rotationTable(
      n: Int,
      maxV: Int,
      budget: Long = 200_000_000L
  ): Map[String, (Set[VertexSignature], Set[(String, Int)])] =
    candidateTypeSets(n).flatMap { ts =>
      // stop each bucket once its known multiplicity is found ⇒ cheap-tree large-V cells (dodecagons) can go
      // high while square-rich cells halt at their small V before the expensive layers.
      val r = BucketAssembly.enumerateBucket(ts, maxV, budget, targetCount = multiplicity(n, ts))
      r.keys.toList.flatMap { key =>
        KrotenheerdtTorusMapSearch
          .realizeCell(r.ops(key))
          .map((faces, pv, pw) => key -> (ts, KrotenheerdtTorusMapSearch.rotationCenters(faces, pv, pw)))
      }
    }.toMap

  /** Symmetry-grower D-symbol keys, grouped by uniformity `n`. */
  def growerKeysByN(
      maxN: Int,
      maxFaces: Int,
      parallelism: Int = math.max(1, Runtime.getRuntime.availableProcessors - 1),
      maxMillis: Long = Long.MaxValue,
      log: String => Unit = _ => ()
  ): Map[Int, Set[String]] =
    KrotenheerdtTorusMapSearch
      .enumerateAllSeedsParallel(maxN, maxFaces, parallelism = parallelism, log = log, maxMillis = maxMillis)
      .tilings
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._3).toSet)
      .toMap
