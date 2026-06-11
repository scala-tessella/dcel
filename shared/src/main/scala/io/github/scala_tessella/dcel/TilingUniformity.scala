package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingEquivalency.groupByBoundaryEquivalency
import io.github.scala_tessella.dcel.Tree.*
import io.github.scala_tessella.dcel.Utils.associate
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, HalfEdgeId, Vertex, VertexId}
import io.github.scala_tessella.ring_seq.RingSeq.bracelet

import scala.Ordering.Implicits.*
import scala.util.control.TailCalls.{TailRec, done, tailcall}

/** Uniformity and gonality analysis for tilings, exposed as extension methods on [[TilingDCEL]].
  *
  * The headline operation is the **uniformity tree** ([[TilingDCEL.uniformityTree]] /
  * `uniformityTreeUncompressed` here): a hierarchical grouping of the tiling's inner vertices into
  * equivalence classes based on the local DCEL structure around each vertex, refined at increasing
  * vertex-distance. Two vertices end up in the same leaf iff their neighbourhoods are boundary-equivalent at
  * every distance examined.
  *
  * Related views:
  *   - [[scanUniformityTree]] — a sequence of trees showing the progressive refinement at each depth (only
  *     depths that actually split a class are kept), useful for animation.
  *   - [[gonalitySampleInnerVertexIds]] — one representative vertex per gonality class.
  *   - [[TilingDCEL.gonalityTrees]] — the gonality slice of the uniformity tree.
  */
object TilingUniformity:

  extension (tiling: TilingDCEL)

    @inline
    private def getDcelAtVertexKnown(vertexId: VertexId, distance: Int): TilingDCEL =
      tiling.getDcelAtVertex(vertexId, distance) match
        case Right(localTiling) => localTiling
        case Left(error)        =>
          // In this path vertex ids are sourced from this tiling, so missing vertices indicate corruption.
          throw new IllegalStateException(
            s"Could not build local DCEL for ${vertexId.toPrefixedString} at distance $distance: ${error.message}"
          )

    /** Retrieves a reduced TilingDCEL structure around a vertex containing only the polygons reached within
      * the given vertex-distance. Distance is clamped to >= 0.
      *
      * @param vertexId
      *   The ID of the vertex around which the reduced TilingDCEL is generated.
      * @param distance
      *   The vertex-radius to include (0 = only polygons incident to the center; 1 = also polygons around the
      *   neighbors of the center; etc.).
      * @return
      *   An `Either` containing the structure of the reduced TilingDCEL if the operation succeeds or a
      *   `NotFoundError` if the specified vertex is not found.
      */
    private[dcel] def getStructureAtVertex(
        vertexId: VertexId,
        distance: Int = 0
    ): Either[NotFoundError, (List[Vertex], List[HalfEdge], Face, List[Face])] =
      val d = math.max(0, distance)

      for
        center <- tiling.findVertex(vertexId)
      yield
        // 1) Collect vertex set in the radius
        val inRadius: Set[Vertex] = center.bfsVertices(d)

        // 2) Collect all inner faces that are incident to at least one vertex in the set, in
        //    stable id order so the build below is a pure function of its input, not of hash
        //    iteration order (cf. ADR-0013)
        val selectedInnerFaces: List[Face] =
          inRadius
            .flatMap: vertex =>
              vertex.incidentEdgesUnsafe
                .flatMap: halfEdge =>
                  halfEdge.incidentFace
                .filter: face =>
                  face != tiling.outerFace
            .toList
            .sortBy(_.id.value)

        // 3) Build a local DCEL from those faces, cloning only the necessary vertices/edges/faces.
        //    Insertion-ordered maps carry the deterministic face order through every pass.
        val vMap  = scala.collection.mutable.LinkedHashMap[VertexId, Vertex]()
        val heMap = scala.collection.mutable.LinkedHashMap[HalfEdgeId, HalfEdge]()
        val fMap  = scala.collection.mutable.LinkedHashMap[FaceId, Face]()

        def cloneVertex(v: Vertex): Vertex =
          vMap.getOrElseUpdate(v.id, Vertex(v.id, v.coords, leaving = None))

        // First pass: clone faces and half-edges on their outer cycles
        selectedInnerFaces.foreach: face =>
          val nf = Face(face.id, outerComponent = None, innerComponents = Nil)
          fMap.put(face.id, nf): Unit

          face.outerComponent.foreach: faceStart =>
            val cycle = faceStart.faceTraversalUnsafe[HalfEdge]()
            cycle.foreach: halfEdge =>
              val key = halfEdge.idUnsafe
              if !heMap.contains(key) then
                val o  = cloneVertex(halfEdge.origin)
                val nh = HalfEdge(
                  origin = o,
                  twin = None,
                  next = None,
                  prev = None,
                  incidentFace = None,
                  angle = halfEdge.angle
                )
                heMap.put(key, nh): Unit
                if o.leaving.isEmpty then o.leaving = Some(nh)

        // Second pass: set next/prev/incidentFace for the inner faces
        selectedInnerFaces.foreach: face =>
          val nf                         = fMap(face.id)
          var firstNew: Option[HalfEdge] = None
          var prevNew: Option[HalfEdge]  = None
          face.outerComponent.foreach: faceStart =>
            val cycle = faceStart.faceTraversalUnsafe[HalfEdge]()
            cycle.foreach: halfEdge =>
              val nh = heMap(halfEdge.idUnsafe)
              nh.incidentFace = Some(nf)
              if firstNew.isEmpty then firstNew = Some(nh)
              prevNew.foreach: previous =>
                previous.next = Some(nh)
                nh.prev = Some(previous)
              prevNew = Some(nh)
          for
            first <- firstNew
            prev  <- prevNew
          do
            prev.next = Some(first)
            first.prev = Some(prev)
            nf.outerComponent = Some(first)

        // Third pass: wire twins for inner-inner edges
        selectedInnerFaces.foreach: face =>
          face.outerComponent.foreach: faceStart =>
            val cycle = faceStart.faceTraversalUnsafe[HalfEdge]()
            cycle.foreach: halfEdge =>
              val nh = heMap(halfEdge.idUnsafe)
              halfEdge.twin.foreach: twin =>
                val tk = twin.idUnsafe
                if heMap.contains(tk) then
                  val nt = heMap(tk)
                  nh.twin = Some(nt)
                  nt.twin = Some(nh)

        // 4) Build boundary half-edges where twins are missing (outer boundary of the local DCEL)
        val localOuter    = Face.outer
        val boundaryStubs = scala.collection.mutable.ArrayBuffer[HalfEdge]()
        heMap.values.foreach: halfEdge =>
          if halfEdge.twin.isEmpty then
            halfEdge.next.foreach: nextHalfEdge =>
              val b = HalfEdge(
                origin = nextHalfEdge.origin,
                twin = Some(halfEdge),
                next = None,
                prev = None,
                incidentFace = Some(localOuter),
                angle = halfEdge.angle.map(_.conjugate)
              )
              halfEdge.twin = Some(b)
              boundaryStubs += b

        val stubByKey =
          boundaryStubs
            .map: halfEdge =>
              halfEdge.idUnsafe -> halfEdge
            .toMap

        def nextBoundaryOf(b: HalfEdge): Option[HalfEdge] =
          for
            twin      <- b.twin
            innerPrev <- twin.prev
            nextEdge  <- stubByKey.get((innerPrev.destinationUnsafe.id, innerPrev.origin.id))
          yield nextEdge

        val visitedPairs   = scala.collection.mutable.HashSet[HalfEdgeId]()
        val boundaryCycles = scala.collection.mutable.ArrayBuffer[HalfEdge]()
        boundaryStubs.foreach: start =>
          val startKey = start.idUnsafe
          if !visitedPairs.contains(startKey) then
            var cur   = start
            val first = start
            var ok    = true
            while ok && !visitedPairs.contains(cur.idUnsafe) do
              visitedPairs += cur.idUnsafe
              nextBoundaryOf(cur) match
                case Some(n) =>
                  cur.next = Some(n)
                  n.prev = Some(cur)
                  cur = n
                case None    =>
                  ok = false
            if ok && (cur eq first) then boundaryCycles += first

        if boundaryCycles.isEmpty && boundaryStubs.nonEmpty then
          val ordered = boundaryStubs.toList.orderBoundary
          if ordered.nonEmpty then
            ordered.linkInCycle()
            boundaryCycles += ordered.head

        if boundaryCycles.nonEmpty then
          localOuter.outerComponent = Some(boundaryCycles.head)

        // Deterministic order for free: the maps are insertion-ordered and were filled in
        // sorted-face order.
        val newVertices = vMap.values.toList
        val newInner    = fMap.values.toList
        val newHalf     = (heMap.values ++ boundaryStubs).toList

        // Fix boundary angles from inner incident angles
        localOuter.outerComponent.foreach: start =>
          val boundaryEdges = start.faceTraversalUnsafe[HalfEdge]()
          newHalf.setOuterEdgeAngles(boundaryEdges, localOuter)

        (
          newVertices,
          newHalf,
          localOuter,
          newInner
        )

    /** Constructs an uncompressed uniformity tree for the given tiling, where each node in the tree groups
      * vertices by their equivalency, that is being at the center of equivalent DCEL structure, expanding at
      * distance. The algorithm uses tail recursion for efficient processing and optionally limits the
      * recursion depth based on a maximum distance.
      *
      * @param maxDistance
      *   An optional maximum distance for recursion depth. If provided, nodes at a depth greater than this
      *   value will not be further processed.
      *
      * @return
      *   A Tree data structure, where each node contains a list of vertex IDs that belong to the same
      *   equivalence class. The result begins at the root with all inner vertices and progressively divides
      *   them through recursion.
      */
    def uniformityTreeUncompressed(maxDistance: Option[Int] = None): Tree[List[VertexId]] =
      val boundaryVertexIds =
        tiling.boundaryVerticesUnsafe
          .map:
            _.id
          .toSet

      // Tail-recursive helper using TailCalls
      def deepMap(key: List[Int], vertexIds: List[VertexId]): TailRec[Tree[List[VertexId]]] =
        val distance           = key.length
        val centeredTilingsMap =
          vertexIds
            .associate: vertexId =>
              getDcelAtVertexKnown(vertexId, distance)

        val classes = groupByBoundaryEquivalency(centeredTilingsMap.toList)

        val partitioned =
          classes.map: classIds =>
            classIds.partition: vertexId =>
              val localTiling      = centeredTilingsMap(vertexId)
              val localBoundaryIds =
                localTiling.boundaryVerticesUnsafe
                  .map:
                    _.id
                  .toSet
              boundaryVertexIds.intersect(localBoundaryIds).isEmpty

        // Process children with tail recursion
        def iterate(
            remaining: List[((List[VertexId], List[VertexId]), Int)],
            accumulated: List[Tree[List[VertexId]]]
        ): TailRec[List[Tree[List[VertexId]]]] =
          if maxDistance.exists:
              _ < distance
          then
            done:
              accumulated.reverse
          else
            remaining match
              case Nil                             =>
                done:
                  accumulated.reverse
              case ((inner, stuck), index) :: tail =>
                if inner.nonEmpty then
                  val childKey = key :+ index
                  tailcall:
                    deepMap(childKey, inner)
                  .flatMap: childTree =>
                    val updatedChild =
                      childTree match
                        case Leaf(_)                  => Leaf(stuck)
                        case Branch(_, grandchildren) => Branch(stuck, grandchildren)
                    iterate(tail, updatedChild :: accumulated)
                else
                  iterate(tail, Leaf(stuck) :: accumulated)

        tailcall(iterate(partitioned.zipWithIndex, Nil)).map: children =>
          Branch(Nil, children)

      // Start from all inner vertices at the root
      deepMap(Nil, tiling.innerVertices.map(_.id)).result

    /** One representative inner-vertex id per gonality class (i.e. per equivalence class at distance 0). The
      * list size equals the gonality of the tiling.
      */
    def gonalitySampleInnerVertexIds: List[VertexId] =
      uniformityTreeUncompressed(Option(0))
        .compress:
          _ ::: _
        .flattenLeaves
        .map: vertexIds =>
          vertexIds.head

    private[dcel] def regularPolygonsUnsafeFrom(vertexId: VertexId): List[RegularPolygon] =
      val angles =
        tiling.getAnglesAtVertex(vertexId) match
          case Right(values) => values
          case Left(error)   =>
            throw new IllegalStateException(
              s"Could not compute angles at ${vertexId.toPrefixedString}: ${error.message}"
            )
      angles.bracelet
        .map: angleDegree =>
          RegularPolygon.fromInteriorAngle(angleDegree)

    private[dcel] def gonalityUnsafe: List[List[RegularPolygon]] =
      gonalitySampleInnerVertexIds
        .map: vertexId =>
          regularPolygonsUnsafeFrom(vertexId)
        .sorted

    /** Scans the uniformity tree of a given tiling structure and generates a sequence of trees representing
      * vertex partitions grouped by equivalency at increasing depths.
      *
      * The method constructs the full uniformity tree and progressively computes and collects trees up to
      * each depth level using memoization for optimization.
      *
      * @return
      *   A list of trees, where each tree corresponds to the vertex equivalency classes at increasing depths.
      *   The final tree represents the full uniformity tree for the given tiling structure.
      */
    def scanUniformityTree: List[Tree[List[VertexId]]] =
      // Optimization: compute the full tree first to find the max required depth
      val fullTree = uniformityTreeUncompressed(None)
      val maxDepth = fullTree.depth

      (0 to maxDepth).toList.map: d =>
        uniformityTreeUncompressed(Some(d)).compress:
          _ ::: _
      .distinctBy:
        _.flattenLeaves // Only keep trees that actually changed the partitioning
