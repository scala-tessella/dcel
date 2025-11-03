package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingUniformity.vertexIdClasses
import io.github.scala_tessella.ring_seq.RingSeq.*

object TilingSymmetry:

  extension (tiling: TilingDCEL)

    /** Boundary angles rotational symmetry */
    def boundaryRotationalSymmetry: Int =
      tiling.boundaryEdges.map(_.angle.get).rotationalSymmetry

    /** Boundary angles reflectional symmetry */
    def boundaryReflectionalSymmetry: Int =
      tiling.boundaryEdges.map(_.angle.get).symmetry

    /** Boundary vertices mapped to DCELs equivalency classes */
    private def boundaryVertexClasses: Vector[Int] =
      val boundaryVertexIds = tiling.boundaryVertices.map(_.id)
      val centeredTilings = boundaryVertexIds.toList.map(vertexId => (vertexId, tiling.getDcelAtVertex(vertexId).toOption.get))
      val vertexIdClassAssociation = vertexIdClasses(centeredTilings).zipWithIndex.flatMap {
        case (classes, i) => classes.map(classId => (classId, i))
      }.toMap
      boundaryVertexIds.map(vertexIdClassAssociation)

    /** Boundary DCELs rotational symmetry */
    def boundaryVerticesRotationalSymmetry: Int =
      boundaryVertexClasses.rotationalSymmetry

    /** Boundary DCELs reflectional symmetry */
    def boundaryVerticesReflectionalSymmetry: Int =
      boundaryVertexClasses.symmetry
