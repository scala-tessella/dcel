package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingLattice.{translationLattice, validatedPeriods}
import io.github.scala_tessella.dcel.TilingCertifier.*
import io.github.scala_tessella.dcel.conversion.TilingSVG.fromMetadata

/** Throwaway diagnostic: load a dumped reject patch and trace the certification decisions. */
object KrotDebug:
  def main(args: Array[String]): Unit =
    val xml   = java.nio.file.Files.readString(java.nio.file.Paths.get(args(0)))
    val n     = args.lift(1).map(_.toInt).getOrElse(1)
    val patch = fromMetadata(xml).toOption.get
    println(s"vertices=${patch.vertices.size} innerTypes=${innerVertexTypes(patch).map(_.mkString("."))}")

    val strict = patch.translationLattice(maxDefectFraction = 0.0)
    println(s"strict basis: $strict det=${strict.map((v, w) => v.cross(w).abs)}")

    locally:
      val boundaryIds = patch.boundaryVerticesUnsafe.map(_.id).toSet
      val typeOf      = patch.innerVertices.map(v => v.id -> vertexTypeOf(patch, v)).toMap
      val deep        = patch.innerVertices.filterNot(_.bfsVertices(5).exists(u => boundaryIds.contains(u.id)))
      val monoBall    = deep.filter(v => v.bfsVertices(5).flatMap(u => typeOf.get(u.id)).sizeIs < n)
      println(s"ball check r=5: deep=${deep.size} monoBall=${monoBall.size} " +
        s"sample=${monoBall.take(3).map(v => (v.id.value, typeOf(v.id).mkString(".")))}")
      println(s"tooManyWitnessedOrbits(n=$n): ${tooManyWitnessedOrbits(patch, n)}")
      import io.github.scala_tessella.dcel.conversion.TilingSVG.toScalableVectorGraphics
      java.nio.file.Files.writeString(
        java.nio.file.Paths.get("/tmp/krot-debug.svg"),
        patch.toScalableVectorGraphics()
      ): Unit
      println("svg written to /tmp/krot-debug.svg")

    patch.validatedPeriods() match
      case None                    => println("no validated periods")
      case Some((anchor, vectors)) =>
        println(s"anchor=$anchor candidates=${vectors.size}")
        vectors.take(12).foreach((t, f) => println(f"  cand $t norm2=${t.dot(t)} mismatch=$f%.4f"))
        traceBases(patch, anchor, vectors)
        selectBasis(patch, anchor, vectors) match
          case Left(r)              => println(s"selectBasis reject: $r")
          case Right((basis, cell)) =>
            val (v, w)      = basis
            println(s"selected basis: $basis det=${v.cross(w).abs}")
            println(
              s"block ${cell.block.cellsWide}x${cell.block.cellsHigh} origin=${cell.block.corners.head}"
            )
            val innerIds    = patch.innerVertices.map(_.id).toSet
            val vertexCells = patch.vertices
              .flatMap(u => cell.cellOf(u.coords).map(ij => (ij, u)))
              .groupMap(_._1)(_._2)
            val boundaryIds = patch.boundaryVerticesUnsafe.map(_.id).toSet
            val witness     = vertexCells.toList
              .filter((_, vs) => vs.nonEmpty && vs.forall(u => innerIds.contains(u.id)))
              .maxByOption((_, vs) => vs.map(u => depthToBoundary(u, boundaryIds)).min)
            witness match
              case None             =>
                println("no witness cell")
              case Some((ij, reps)) =>
                println(s"witness $ij reps=${reps.size} types=${reps.map(vertexTypeOf(patch, _)).toSet}")
                val safe                                  = reps.map(depthToBoundary(_, boundaryIds)).min - 1
                println(s"safeDepth=$safe")
                var classes: List[List[structure.Vertex]] = List(reps)
                for d <- 0 to safe do
                  classes = classes.flatMap: group =>
                    if group.sizeIs == 1 then List(group)
                    else
                      val locals = group.map(u => u -> patch.getDcelAtVertex(u.id, d).toOption.get)
                      io.github.scala_tessella.dcel.TilingEquivalency.groupByBoundaryEquivalency(locals)
                  println(s"depth $d -> ${classes.size} classes: ${classes.map(_.map(_.id.value))}")
                println(s"certify n=$n: ${certify(patch, n).left.map(_.toString).map(_.torusKey.take(60))}")

  /** Replicates [[TilingCertifier.selectBasis]]'s enumeration, reporting per-basis verdicts. */
  private def traceBases(
      patch: Tiling,
      anchor: geometry.BigPoint,
      vectors: List[(geometry.BigPoint, Double)]
  ): Unit =
    import io.github.scala_tessella.dcel.TilingLattice.{floorToInt, gaussReduced, maximalRectangle}
    import io.github.scala_tessella.dcel.geometry.BigPoint
    val eps                        = BigDecimal("1e-9")
    def key9(p: BigPoint)          =
      (p.x.setScale(9, BigDecimal.RoundingMode.HALF_UP), p.y.setScale(9, BigDecimal.RoundingMode.HALF_UP))
    def canonicalSign(t: BigPoint) =
      val (x, y) = key9(t)
      if y > 0 || (y == 0 && x > 0) then t else BigPoint.origin - t
    val pool                       =
      vectors
        .map((t, f) => (canonicalSign(t), f))
        .sortBy((t, f) => (f, t.dot(t)))
        .distinctBy((t, _) => key9(t))
        .take(32)
        .map(_._1)
    val bases                      =
      (for
        i <- pool.indices
        j <- (i + 1) until pool.size
        if pool(i).cross(pool(j)).abs > eps
      yield gaussReduced(pool(i), pool(j)))
        .map((a, b) => (canonicalSign(a), canonicalSign(b)))
        .distinctBy((a, b) => (key9(a), key9(b)))
        .sortBy((a, b) => (a.cross(b).abs, a.dot(a) + b.dot(b)))
        .toList
    val faces                      =
      patch.innerFaces.map: f =>
        (f.halfEdgesUnsafe.size, f.getVerticesUnsafe.map(_.coords).centroid, f.areaUnsafe)
    val patchFaceSizes             = faces.map(_._1).toSet
    println(s"pool=${pool.size} bases=${bases.size}, tracing first 25 by det:")
    bases.take(25).foreach: (v, w) =>
      val det                                = v.cross(w)
      val covol                              = det.abs
      def cellIndex(p: BigPoint): (Int, Int) =
        val d = p - anchor
        (floorToInt((d.x * w.y - w.x * d.y) / det), floorToInt((v.x * d.y - d.x * v.y) / det))
      val byCell                             = faces.groupBy((_, c, _) => cellIndex(c))
      val occupied                           =
        byCell.filter((_, fs) => (fs.map(_._3).sum - covol).abs < BigDecimal(1.0e-6)).keySet
      val rect                               = maximalRectangle(occupied)
      val verdict                            = rect match
        case None                                       => "no occupied rect"
        case Some((i0, j0, cw, ch)) if cw < 2 || ch < 2 => s"block ${cw}x$ch too small"
        case Some((i0, j0, cw, ch))                     =>
          val origin    = anchor + TilingLattice.scaled(v, BigDecimal(i0)) +
            TilingLattice.scaled(w, BigDecimal(j0))
          val block     = TilingLattice.ParallelogonBlock(
            List(origin),
            cw,
            ch,
            covol * cw * ch
          )
          val cell      = TilingCertifier.CellGeometry((v, w), block)
          val faceCells =
            faces
              .flatMap((sides, c, _) => cell.cellOf(c).map(ij => (ij, (sides, cell.reduced(c)))))
              .groupMap(_._1)(_._2)
              .view.mapValues(_.toSet).toMap
          val allCells  = (for i <- 0 until cw; j <- 0 until ch yield (i, j)).toSet
          if faceCells.keySet != allCells then s"block ${cw}x$ch: cells missing content"
          else if faceCells.values.toSet.sizeIs != 1 then
            val contents = faceCells.values.toSet.toList
            s"block ${cw}x$ch: ${contents.size} distinct contents, sizes=${contents.map(_.size).distinct}; " +
              s"sample diff=${contents.take(2) match
                  case List(a, b) => (a -- b).take(2).toString + " vs " + (b -- a).take(2).toString
                  case _          => ""
                }"
          else if !patchFaceSizes.subsetOf(faceCells.values.head.map(_._1)) then
            s"block ${cw}x$ch: coverage miss (cell sizes ${faceCells.values.head.map(_._1)} vs patch $patchFaceSizes)"
          else s"block ${cw}x$ch: PASS"
      println(f"  basis det=$covol%-22s norm2=(${v.dot(v)}%-20s,${w.dot(w)}%-20s) -> $verdict")
