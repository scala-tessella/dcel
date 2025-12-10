package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingEquivalency.groupByBoundaryEquivalency
import io.github.scala_tessella.dcel.Tree.*
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}

import scala.util.control.TailCalls.{TailRec, done, tailcall}

object TilingUniformity:

  extension (tiling: TilingDCEL)

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

      // BFS on vertices to get all vertices within distance d from the center
      def bfsVertices(center: Vertex): Set[Vertex] =
        val visited = scala.collection.mutable.LinkedHashSet.empty[Vertex]
        val queue   = scala.collection.mutable.Queue.empty[(Vertex, Int)]
        visited += center
        queue.enqueue((center, 0))
        while queue.nonEmpty do
          val (vertex, dist) = queue.dequeue()
          if dist < d then
            vertex.incidentEdgesUnsafe.foreach: halfEdge =>
              halfEdge.destination.foreach: destinationVertex =>
                if !visited.contains(destinationVertex) then
                  visited += destinationVertex
                  queue.enqueue((destinationVertex, dist + 1))
        visited.toSet

      for
        center <- tiling.findVertex(vertexId)
      yield
        // 1) Collect vertex set in the radius
        val inRadius: Set[Vertex] = bfsVertices(center)

        // 2) Collect all inner faces that are incident to at least one vertex in the set
        var selectedInnerFaces: Set[Face] =
          tiling.innerFaces
            .filter: face =>
              face.getVerticesUnsafe.exists: vertex =>
                inRadius.contains(vertex)
            .toSet

        // 2b) Iteratively find and add "hole" faces
        // A hole face has all its vertices already in our vertex set but wasn't selected
        var changed = true
        while changed do
          val currentVertices =
            selectedInnerFaces
              .flatMap: face =>
                face.getVerticesUnsafe
          val holeFaces       = tiling.innerFaces.filter: face =>
            !selectedInnerFaces.contains(face) &&
              face.getVerticesUnsafe.forall: vertex =>
                currentVertices.contains(vertex)
          if holeFaces.nonEmpty then
            selectedInnerFaces ++= holeFaces
          else
            changed = false

        // 3) Build a local DCEL from those faces, cloning only the necessary vertices/edges/faces
        val vMap  = scala.collection.mutable.HashMap[VertexId, Vertex]()
        val heMap = scala.collection.mutable.HashMap[(VertexId, VertexId), HalfEdge]()
        val fMap  = scala.collection.mutable.HashMap[FaceId, Face]()

        def cloneVertex(v: Vertex): Vertex =
          vMap.getOrElseUpdate(v.id, Vertex(v.id, v.coords, leaving = None))

        // First pass: clone faces and half-edges on their outer cycles
        selectedInnerFaces.foreach: face =>
          val nf = Face(face.id, outerComponent = None, innerComponents = Nil)
          fMap.put(face.id, nf): Unit

          val cycle = face.outerComponent.get.faceTraversalUnsafe[HalfEdge]()
          cycle.foreach: he =>
            val key = he.key.get
            if !heMap.contains(key) then
              val o  = cloneVertex(he.origin)
              val nh = HalfEdge(
                origin = o,
                twin = None,
                next = None,
                prev = None,
                incidentFace = None,
                angle = he.angle
              )
              heMap.put(key, nh): Unit
              if o.leaving.isEmpty then o.leaving = Some(nh)

        // Second pass: set next/prev/incidentFace for the inner faces
        selectedInnerFaces.foreach: face =>
          val nf                 = fMap(face.id)
          val cycle              = face.outerComponent.get.faceTraversalUnsafe[HalfEdge]()
          var firstNew: HalfEdge = null
          var prevNew: HalfEdge  = null
          cycle.foreach: he =>
            val nh = heMap(he.key.get)
            nh.incidentFace = Some(nf)
            if firstNew eq null then firstNew = nh
            if prevNew != null then
              prevNew.next = Some(nh)
              nh.prev = Some(prevNew)
            prevNew = nh
          prevNew.next = Some(firstNew)
          firstNew.prev = Some(prevNew)
          nf.outerComponent = Some(firstNew)

        // Third pass: wire twins for inner-inner edges
        selectedInnerFaces.foreach: face =>
          val cycle = face.outerComponent.get.faceTraversalUnsafe[HalfEdge]()
          cycle.foreach: he =>
            val nh = heMap(he.key.get)
            he.twin.foreach: t =>
              val tk = t.key.get
              if heMap.contains(tk) then
                val nt = heMap(tk)
                nh.twin = Some(nt)
                nt.twin = Some(nh)

        // 4) Build boundary half-edges where twins are missing (outer boundary of the local DCEL)
        val localOuter    = Face.outer
        val boundaryStubs = scala.collection.mutable.ArrayBuffer[HalfEdge]()
        heMap.values.foreach: nh =>
          if nh.twin.isEmpty then
            val b = HalfEdge(
              origin = nh.next.get.origin,
              twin = Some(nh),
              next = None,
              prev = None,
              incidentFace = Some(localOuter),
              angle = nh.angle.map(_.conjugate)
            )
            nh.twin = Some(b)
            boundaryStubs += b

        val keyOf: HalfEdge => (VertexId, VertexId) = he => (he.origin.id, he.destination.get.id)
        val stubByKey                               =
          boundaryStubs
            .map: halfEdge =>
              keyOf(halfEdge) -> halfEdge
            .toMap

        def nextBoundaryOf(b: HalfEdge): Option[HalfEdge] =
          val innerPrev = b.twin.get.prev.get
          val wantedKey = (innerPrev.destination.get.id, innerPrev.origin.id)
          stubByKey.get(wantedKey)

        val visitedPairs   = scala.collection.mutable.HashSet[(VertexId, VertexId)]()
        val boundaryCycles = scala.collection.mutable.ArrayBuffer[HalfEdge]()
        boundaryStubs.foreach: start =>
          val startKey = keyOf(start)
          if !visitedPairs.contains(startKey) then
            var cur   = start
            val first = start
            var ok    = true
            while ok && !visitedPairs.contains(keyOf(cur)) do
              visitedPairs += keyOf(cur)
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

        val newVertices = vMap.values.toList
        val newInner    = fMap.values.toList
        val newHalf     = (heMap.values ++ boundaryStubs).toList

        // Fix boundary angles from inner incident angles
        localOuter.outerComponent.foreach: start =>
          val loop = start.faceTraversalUnsafe[HalfEdge]()
          loop.foreach: be =>
            val v           = be.origin
            val incidentAtV = newHalf.filter(_.origin eq v)
            val innerSum    = incidentAtV
              .filterNot(_.hasIncidentFace(localOuter))
              .flatMap(_.angle)
              .sumExact
            be.angle = Some(innerSum.conjugate)

        (
          newVertices,
          newHalf,
          localOuter,
          newInner
        )

    /** Calculates the uniformity of the tiling, each leaf a different class of vertices. */
    def uniformityTreeUncompressed(maxDistance: Option[Int] = None): Tree[List[VertexId]] =
      val boundaryVertexIds = tiling.boundaryVertices.map(_.id)

      // Tail-recursive helper using TailCalls
      def deepMap(key: List[Int], vertexIds: List[VertexId]): TailRec[Tree[List[VertexId]]] =
        val distance        = key.length
        val centeredTilings = vertexIds.map: id =>
          id -> tiling.getDcelAtVertex(id, distance).toOption.get
        val classes         = groupByBoundaryEquivalency(centeredTilings)
        val boundaryInfoMap =
          centeredTilings
            .map: (vertexId, tiling) =>
              vertexId -> tiling.boundaryVertices.map(_.id).toSet
            .toMap
        val partitioned     =
          classes.map:
            _.partition: vertexId =>
              val localBoundaryVertexIds = boundaryInfoMap(vertexId)
              boundaryVertexIds.toSet.intersect(localBoundaryVertexIds).isEmpty

        // Process children with tail recursion
        def iterate(
            remaining: List[((List[VertexId], List[VertexId]), Int)],
            accumulated: List[Tree[List[VertexId]]]
        ): TailRec[List[Tree[List[VertexId]]]] =
          if maxDistance.exists:
              _ < distance
          then
            done(accumulated.reverse)
          else
            remaining match
              case Nil                             => done(accumulated.reverse)
              case ((inner, stuck), index) :: tail =>
                if inner.nonEmpty then
                  val childKey = key :+ index
                  tailcall(deepMap(childKey, inner)).flatMap: childTree =>
                    val updatedChild = childTree match
                      case Leaf(_)                  => Leaf(stuck)
                      case Branch(_, grandchildren) => Branch(stuck, grandchildren)
                    iterate(tail, updatedChild :: accumulated)
                else
                  iterate(tail, Leaf(stuck) :: accumulated)

        tailcall(iterate(partitioned.zipWithIndex, Nil)).map: children =>
          Branch(Nil, children)

      // Start from all inner vertices at the root
      deepMap(Nil, tiling.innerVertices.map(_.id)).result

    def scanUniformityTree: List[Tree[List[VertexId]]] =
      val boundaryVertexIds = tiling.boundaryVertices.map(_.id)

      // Cache to store computed subtrees for each (key, maxDepth) pair
      val cache = scala.collection.mutable.HashMap[(List[Int], Option[Int]), Tree[List[VertexId]]]()

      // Build tree with memoization
      def deepMap(
          key: List[Int],
          vertexIds: List[VertexId],
          maxDistance: Option[Int]
      ): TailRec[Tree[List[VertexId]]] =
        val cacheKey = (key, maxDistance)
        cache.get(cacheKey) match
          case Some(cached) => done(cached)
          case None         =>
            val distance        = key.length
            val centeredTilings =
              vertexIds.map: id =>
                id -> tiling.getDcelAtVertex(id, distance).toOption.get
            val classes         = groupByBoundaryEquivalency(centeredTilings)
            val boundaryInfoMap =
              centeredTilings
                .map: (vertexId, tiling) =>
                  vertexId -> tiling.boundaryVertices.map(_.id).toSet
                .toMap
            val partitioned     =
              classes.map:
                _.partition: vertexId =>
                  val localBoundaryVertexIds = boundaryInfoMap(vertexId)
                  boundaryVertexIds.toSet.intersect(localBoundaryVertexIds).isEmpty

            def iterate(
                remaining: List[((List[VertexId], List[VertexId]), Int)],
                accumulated: List[Tree[List[VertexId]]]
            ): TailRec[List[Tree[List[VertexId]]]] =
              if maxDistance.exists(_ < distance) then
                done(accumulated.reverse)
              else
                remaining match
                  case Nil                             => done(accumulated.reverse)
                  case ((inner, stuck), index) :: tail =>
                    if inner.nonEmpty then
                      val childKey = key :+ index
                      tailcall(deepMap(childKey, inner, maxDistance)).flatMap: childTree =>
                        val updatedChild = childTree match
                          case Leaf(_)                  => Leaf(stuck)
                          case Branch(_, grandchildren) => Branch(stuck, grandchildren)
                        iterate(tail, updatedChild :: accumulated)
                    else
                      iterate(tail, Leaf(stuck) :: accumulated)

            tailcall(iterate(partitioned.zipWithIndex, Nil)).map: children =>
              val result = Branch(Nil, children)
              cache.put(cacheKey, result): Unit
              result

      // First, compute the full tree without limits
      val fullTree       =
        deepMap(
          Nil,
          tiling.innerVertices.map(vertex => vertex.id),
          None
        ).result
      val fullCompressed =
        fullTree.compress(_ ::: _)

      // Now collect trees at each depth, reusing cached computations
      val results  = scala.collection.mutable.ListBuffer.empty[Tree[List[VertexId]]]
      var depth    = 0
      var continue = true

      while continue do
        val treeAtDepth =
          deepMap(
            Nil,
            tiling.innerVertices.map(vertex => vertex.id),
            Some(depth)
          ).result
        val compressed  = treeAtDepth.compress(_ ::: _)
        results += compressed

        if compressed == fullCompressed then
          continue = false
        else
          depth += 1

      results.toList
