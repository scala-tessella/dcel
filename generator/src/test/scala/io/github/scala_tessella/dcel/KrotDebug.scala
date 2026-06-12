package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingLattice.{translationLattice, validatedPeriods}
import io.github.scala_tessella.dcel.TilingCertifier.*
import io.github.scala_tessella.dcel.conversion.TilingSVG.fromMetadata

/** Throwaway diagnostic: load a dumped reject patch and trace the certification decisions. */
object KrotDebug:
  def main(args: Array[String]): Unit =
    val xml   = java.nio.file.Files.readString(java.nio.file.Paths.get(args(0)))
    val patch = fromMetadata(xml).toOption.get
    println(s"vertices=${patch.vertices.size} innerTypes=${innerVertexTypes(patch).map(_.mkString("."))}")

    val strict = patch.translationLattice(maxDefectFraction = 0.0)
    println(s"strict basis: $strict det=${strict.map((v, w) => v.cross(w).abs)}")

    patch.validatedPeriods() match
      case None                    => println("no validated periods")
      case Some((anchor, vectors)) =>
        println(s"anchor=$anchor candidates=${vectors.size}")
        vectors.take(12).foreach(t => println(f"  cand $t norm2=${t.dot(t)}"))
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
                println(s"certify n=1: ${certify(patch, 1).left.map(_.toString).map(_.torusKey.take(60))}")
