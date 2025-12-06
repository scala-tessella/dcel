package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.structure.{HalfEdge, VertexId}
import io.github.scala_tessella.ring_seq.SymmetryOps.{AxisLocation, Edge as SymEdge, Vertex as SymVertex}

import scala.collection.mutable

object TilingSymmetry:

  type BoundaryLocation = VertexId | (VertexId, VertexId)

  extension (tiling: TilingDCEL)

    /** Checks if the tiling structure starting at edge `a` is isomorphic to the structure starting at edge
      * `b`.
      *
      * Performs a synchronized traversal (BFS) of both structures.
      */
    def areStructurallyEquivalent(startA: HalfEdge, startB: HalfEdge): Boolean =
      val queue    = mutable.Queue[(HalfEdge, HalfEdge)]((startA, startB))
      val visitedA = mutable.Map[HalfEdge, HalfEdge]() // Tracks mapping a -> b

      visitedA.put(startA, startB): Unit

      while queue.nonEmpty do
        val (a, b) = queue.dequeue()

        // 1. Compare Local Geometry (Angles)
        if a.angle != b.angle then return false

        // 2. Compare Topology (Boundary vs Inner)
        val aIsBoundary = tiling.isBoundaryEdge(a)
        val bIsBoundary = tiling.isBoundaryEdge(b)
        if aIsBoundary != bIsBoundary then return false

        // 3. Compare Incident Face properties (if inner)
        if !aIsBoundary then
          // Ideally check if face sizes match, but simpler:
          // The traversal of 'next' edges will implicitly verify face structure.
          if a.incidentFace.map(_.id) == tiling.outerFace.id then return false // Safety check

        // 4. Queue Neighbors (Next, Prev, Twin)
        // We define neighbors relative to the edge direction
        val neighbors = List(
          (a.next, b.next),
          (a.twin, b.twin)
        )

        val neighborsIterator = neighbors.iterator
        while neighborsIterator.hasNext do
          val (optNa, optNb) = neighborsIterator.next()
          (optNa, optNb) match
            case (Some(na), Some(nb)) =>
              if visitedA.contains(na) then
                // If we've seen 'na' before, it MUST map to 'nb'
                if visitedA(na) != nb then return false
              else
                // New correspondence found
                visitedA.put(na, nb): Unit
                queue.enqueue((na, nb))
            case (None, None)         => ()           // Both missing (e.g. incomplete definitions), acceptable consistency
            case _                    => return false // Structural mismatch (one has edge, other doesn't)

      true

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
      val queue    = mutable.Queue[(HalfEdge, HalfEdge)]((startA, startB))
      val visitedA = mutable.Map[HalfEdge, HalfEdge]()

      visitedA.put(startA, startB): Unit

      while queue.nonEmpty do
        val (a, b) = queue.dequeue()

        // 1. Compare Local Geometry (Angles)
        // For reflection, we compare a's origin angle with b's destination angle (b.next's origin)
        // because b is being traversed backwards relative to the reflection.
        if a.angle != b.next.flatMap(_.angle) then return false

        // 2. Compare Topology
        val aIsBoundary = tiling.isBoundaryEdge(a)
        val bIsBoundary = tiling.isBoundaryEdge(b)
        if aIsBoundary != bIsBoundary then return false

        // 3. Compare Incident Face properties (if inner)
        if !aIsBoundary then
          if a.incidentFace.map(_.id) == tiling.outerFace.id then return false

        // 4. Queue Neighbors
        val neighbors = List(
          (a.next, b.prev),
          (a.twin, b.twin)
        )

        val neighborsIterator = neighbors.iterator
        while neighborsIterator.hasNext do
          val (optNa, optNb) = neighborsIterator.next()
          (optNa, optNb) match
            case (Some(na), Some(nb)) =>
              if visitedA.contains(na) then
                if visitedA(na) != nb then return false
              else
                visitedA.put(na, nb): Unit
                queue.enqueue((na, nb))
            case (None, None)         => ()
            case _                    => return false

      true

    /** Calculates the true rotational symmetry of the TilingDCEL. It checks which rotational symmetries of
      * the boundary are also preserved by the internal structure.
      */
    def rotationalSymm: Int =
      val edges = tiling.boundaryEdges
      if edges.isEmpty then return 1

      // 1. Get upper bound from boundary polygon
      val boundarySymm = tiling.boundarySimplePolygon.rotationalSymm
      val totalEdges   = edges.size
      val step         = totalEdges / boundarySymm

      // 2. Check each candidate shift
      // We iterate 0 to boundarySymm-1.
      // i=0 is Identity (shift 0), always true.
      (0 until boundarySymm).count: i =>
        val shiftIndex = i * step
        areStructurallyEquivalent(edges.head, edges(shiftIndex))

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
        case SymVertex(i)  => boundaryVertexIds(i)
        case SymEdge(i, j) => (boundaryVertexIds(i), boundaryVertexIds(j))
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
