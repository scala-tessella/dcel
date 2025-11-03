package io.github.scala_tessella.dcel

import io.github.scala_tessella.ring_seq.RingSeq.*

object TilingSymmetry:

  extension (tiling: TilingDCEL)

    /** Boundary angles rotational symmetry */
    def boundaryRotationalSymmetry: Int =
      tiling.boundaryEdges.map(_.angle.get).rotationalSymmetry

    /** Boundary angles reflectional symmetry */
    def boundaryReflectionalSymmetry: Int =
      tiling.boundaryEdges.map(_.angle.get).symmetry
