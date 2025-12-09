package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.{BigLineSegment, BigPoint}
import io.github.scala_tessella.dcel.structure.VertexId
import io.github.scala_tessella.ring_seq.SymmetryOps.{AxisLocation, Edge as SymEdge, Vertex as SymVertex}

object TilingSymmetry:

  /** A location on the boundary where a symmetry axis can pass through.
    *   - Vertex: The axis passes directly through the element at this index.
    *   - Edge: The axis passes between the elements at these indices.
    */
  sealed trait BoundaryLocation
  case class BoundaryVertex(i: VertexId)            extends BoundaryLocation
  case class BoundaryEdge(i: VertexId, j: VertexId) extends BoundaryLocation

  extension (tiling: TilingDCEL)

    /** Calculates the rotational symmetry of the TilingDCEL. It checks which rotational symmetries of the
      * boundary are also preserved by the internal structure.
      */
    def rotationalSymmetryOrder: Int =
      val edges = tiling.boundaryEdges
      if edges.isEmpty then return 1

      // 1. Get upper bound from the boundary polygon
      val boundarySym = tiling.boundarySimplePolygon.rotationalSymmetryOrder
      val step        = edges.size / boundarySym

      // 2. Check each candidate shift
      // We iterate 0 to boundarySymm-1.
      // i=0 is Identity (shift 0), always true.
      (0 until boundarySym).count: i =>
        edges.head.isStructurallyEquivalentTo(edges(i * step))

    private def fromAxisToBoundary(boundaryVertexIds: Vector[VertexId]): AxisLocation => BoundaryLocation =
      case SymVertex(i)  => BoundaryVertex(boundaryVertexIds(i))
      case SymEdge(i, j) => BoundaryEdge(boundaryVertexIds(i), boundaryVertexIds(j))

    def rotationalVertexIds: List[BoundaryLocation] =
      val symmetryOrder    = rotationalSymmetryOrder
      val boundaryVertices = tiling.boundaryVertices

      def coordsOf(axisLocation: AxisLocation): BigPoint =
        axisLocation match
          case SymVertex(i)  => boundaryVertices(i).coords
          case SymEdge(i, j) =>
            BigLineSegment(boundaryVertices(i).coords, boundaryVertices(j).coords).midPoint

      val boundaryVertexIds   = boundaryVertices.map(_.id)
      val boundaryAngles      = tiling.boundarySimplePolygon.toAngles
      val segmentSize         = boundaryAngles.size / symmetryOrder
      val symVertices         = (0 until segmentSize).map(i => SymVertex(i)).toList
      val symEdges            = (0 until segmentSize - 1).map(i => SymEdge(i, i + 1)).toList
      val center              = boundaryVertices.map(_.coords).toList.centroid
      val first: AxisLocation =
        (symVertices ++ symEdges).maxBy: location =>
          BigLineSegment(coordsOf(location), center).length
      (0 until symmetryOrder).toList
        .map: i =>
          val step = i * segmentSize
          first match
            case SymVertex(i)  => SymVertex(i + step)
            case SymEdge(i, j) => SymEdge(i + step, j + step)
        .map:
          fromAxisToBoundary(boundaryVertexIds)

    def reflectionalVertexIds: List[(BoundaryLocation, BoundaryLocation)] =
      val edges             = tiling.boundaryEdges.toVector
      if edges.isEmpty then return Nil
      val boundaryVertexIds = tiling.boundaryVertices.map(_.id)
      val axes              = tiling.boundarySimplePolygon.reflectionalIndexPairs
      axes
        .filter: (loc1, _) =>
          val (startA, startB) = loc1 match
            case SymEdge(i, _) => (edges(i), edges(i))
            case SymVertex(i)  => (edges(i), edges(i).prev.get)
          startA.isReflectionallyEquivalentTo(startB)
        .map: (loc1, loc2) =>
          (fromAxisToBoundary(boundaryVertexIds)(loc1), fromAxisToBoundary(boundaryVertexIds)(loc2))

    def reflectionalSymmetryOrder: Int =
      reflectionalVertexIds.size
