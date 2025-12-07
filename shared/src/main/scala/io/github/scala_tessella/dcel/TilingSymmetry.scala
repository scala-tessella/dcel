package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.structure.{HalfEdge, VertexId}
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

    /** Checks if the tiling structure starting at edge `a` is isomorphic to the structure starting at edge
      * `b`.
      *
      * Performs a synchronized traversal (BFS) of both structures.
      */
    def areStructurallyEquivalent(startA: HalfEdge, startB: HalfEdge): Boolean =
      startA.traverseAndCompare(
        startB,
        compareAngles = (a, b) => a.angle == b.angle,
        getNeighbors = (a, b) =>
          List(
            (a.next, b.next), // Orientation preserved
            (a.twin, b.twin)
          )
      )

    /** Checks if the tiling structure starting at edge `a` is reflectionally equivalent to the structure
      * starting at edge `b`.
      *
      * Performs a synchronized traversal (BFS) of both structures, comparing `a` with `b`'s reflection. Since
      * reflection reverses orientation:
      *   - `a.next` matches `b.prev`
      *   - `a.prev` matches `b.next`
      *   - `a.twin` matches `b.twin`
      */
    def areReflectionallyEquivalent(startA: HalfEdge, startB: HalfEdge): Boolean =
      startA.traverseAndCompare(
        startB,
        // For reflection, b is traversed backwards. The "origin" angle of b (backwards)
        // corresponds to the angle at b's destination in the graph, which is b.next.angle.
        compareAngles = (a, b) => a.angle == b.next.flatMap(_.angle),
        getNeighbors = (a, b) =>
          List(
            (a.next, b.prev), // Orientation reversed: next maps to prev
            (a.twin, b.twin)
          )
      )

    /** Calculates the true rotational symmetry of the TilingDCEL. It checks which rotational symmetries of
      * the boundary are also preserved by the internal structure.
      */
    def rotationalSymm: Int =
      val edges = tiling.boundaryEdges
      if edges.isEmpty then return 1

      // 1. Get upper bound from boundary polygon
      val boundarySymm = tiling.boundarySimplePolygon.rotationalSymm
      val step         = edges.size / boundarySymm

      // 2. Check each candidate shift
      // We iterate 0 to boundarySymm-1.
      // i=0 is Identity (shift 0), always true.
      (0 until boundarySymm).count: i =>
        areStructurallyEquivalent(edges.head, edges(i * step))

    def rotationalVertexIds: List[VertexId] =
      val symmetryOrder     = rotationalSymm
      val boundaryVertexIds = tiling.boundaryVertices.map(_.id)
      val boundaryAngles    = tiling.boundarySimplePolygon.toAngles
      val segmentSize       = boundaryAngles.size / symmetryOrder
      val first             = (0 until segmentSize).maxBy(boundaryAngles(_).toRational)
      (0 until symmetryOrder).toList.map(first + _ * segmentSize).map(boundaryVertexIds)

    def reflectionalVertexIds: List[(BoundaryLocation, BoundaryLocation)] =
      val edges             = tiling.boundaryEdges.toVector
      if edges.isEmpty then return Nil
      val boundaryVertexIds = tiling.boundaryVertices.map(_.id)

      val fromAxisToBoundary: AxisLocation => BoundaryLocation = {
        case SymVertex(i)  => BoundaryVertex(boundaryVertexIds(i))
        case SymEdge(i, j) => BoundaryEdge(boundaryVertexIds(i), boundaryVertexIds(j))
      }

      val axes = tiling.boundarySimplePolygon.reflectionalIndexPairs
      axes
        .filter { (loc1, _) =>
          val (startA, startB) = loc1 match
            case SymEdge(i, _) => (edges(i), edges(i))
            case SymVertex(i)  => (edges(i), edges(i).prev.get)
          areReflectionallyEquivalent(startA, startB)
        }
        .map { (loc1, loc2) =>

          (fromAxisToBoundary(loc1), fromAxisToBoundary(loc2))
        }

    def reflectionalSymm: Int =
      reflectionalVertexIds.size
