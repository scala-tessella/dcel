package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}

/** Vertex-level merge of two [[TilingDCEL]] instances: identifies coincident vertices, unifies them, rewires
  * half-edges and faces, and recomputes the outer boundary. The result is a single tiling containing the
  * union of both inputs' inner faces.
  *
  * Called by the "grow by translation" paths in [[TilingAddition]] (`rawDouble`, `maybeFilled`).
  */
private[dcel] object TilingMerge:

  /** A pair of old-and-new [[Vertex]] instances used while reconciling boundary vertices between two tilings
    * being merged.
    */
  type OldNewVertexPair = (oldVertex: Vertex, newVertex: Vertex)

  def mergeTilings(base: TilingDCEL, other: TilingDCEL): TilingDCEL =
    // -----------------------------------------------------------------------
    // 1. Identify coincident vertices between base and other
    // -----------------------------------------------------------------------
    import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY

    import scala.collection.mutable

    val sharedVertexPairs: List[(Vertex, Vertex)] =
      base.vertices.sameCoords(other.vertices, accuracy = ACCURACY)

    // Map: "secondary" (other) vertex id -> "primary" (base) vertex id
    val substitutionMap: Map[VertexId, VertexId] =
      sharedVertexPairs
        .map: (orig, copied) =>
          copied.id -> orig.id
        .toMap

    def repOf(id: VertexId): VertexId =
      substitutionMap.getOrElse(id, id)

    val allOldVertices: List[Vertex] =
      base.vertices ++ other.vertices

    // Representative id -> merged Vertex
    val repToVertex = mutable.HashMap.empty[VertexId, Vertex]

    allOldVertices.foreach: vertex =>
      val r = repOf(vertex.id)
      if !repToVertex.contains(r) then
        // Use coordinates of the first encountered vertex in that equivalence class
        repToVertex(r) = Vertex(r, vertex.coords)

    // Every old vertex id maps to its merged vertex
    val newVertexOfId: Map[VertexId, Vertex] =
      allOldVertices
        .map: vertex =>
          vertex.id -> repToVertex(repOf(vertex.id))
        .toMap

    val newVertices: List[Vertex] =
      repToVertex.values.toList

    // -----------------------------------------------------------------------
    // 2. Clone half-edges and faces for the union of the two tilings
    // -----------------------------------------------------------------------
    val allOldEdges: List[HalfEdge] =
      base.halfEdges ++ other.halfEdges

    val allOldFaces: List[Face] =
      // include both outers and inners, but we will rebuild the single outer face later
      (base.outerFace :: base.innerFaces) ++ (other.outerFace :: other.innerFaces)

    val edgeMap = mutable.HashMap.empty[HalfEdge, HalfEdge]
    // 2.a – create new edges with merged origins and copy angles only
    allOldEdges.foreach: halfEdge =>
      val newOrigin = newVertexOfId(halfEdge.origin.id)
      val ne        = HalfEdge(newOrigin)
      ne.angle = halfEdge.angle
      edgeMap(halfEdge) = ne

    // 2.b – create inner faces only (discard old outers, we will recompute boundary).
    //       Inner faces from the two tilings that occupy the same set of merged vertices are unified into a
    //       single Face object. Without this, a fully-overlapping copy face is doubled: its edges survive
    //       seam-collapse but reference a distinct, duplicate Face, so the polygon's boundary ends up split
    //       across two face objects and topology validation rejects it. The merged-vertex-id set is an exact
    //       key (it reuses `repOf`, the same coincidence relation used to unify vertices) — no rounding.
    val faceMap              = mutable.HashMap.empty[Face, Face]
    val faceByMergedVertices = mutable.HashMap.empty[Set[VertexId], Face]
    allOldFaces.foreach: face =>
      if face.id != FaceId.outerId then
        val mergedVertexKey: Set[VertexId] =
          face.getVerticesUnsafe
            .map: vertex =>
              repOf(vertex.id)
            .toSet
        val canonical                      =
          faceByMergedVertices.getOrElseUpdate(mergedVertexKey, Face(face.id))
        faceMap(face) = canonical

    // 2.c – wire next / prev / incidentFace from old to new via maps
    allOldEdges.foreach:
      oldEdge =>
        val newEdge = edgeMap(oldEdge)
        oldEdge.next.foreach: on =>
          newEdge.next = Some(edgeMap(on))
        oldEdge.prev.foreach: op =>
          newEdge.prev = Some(edgeMap(op))
        oldEdge.incidentFace.foreach:
          oldFace =>
            if oldFace.id != FaceId.outerId then
              // inner face
              newEdge.incidentFace = Some(faceMap(oldFace))
            // else: old outer face, leave as boundary (incidentFace = None)

    // 2.d – set outerComponent / innerComponents on inner faces only
    allOldFaces.foreach: face =>
      if face.id != FaceId.outerId then
        val newFace = faceMap(face)
        face.outerComponent.foreach: startOld =>
          newFace.outerComponent = Some(edgeMap(startOld))
        newFace.innerComponents =
          face.innerComponents.map: maybeHalfEdge =>
            maybeHalfEdge.map: halfEdge =>
              edgeMap(halfEdge)

    // 2.e – (re)wire twins purely by endpoints in the merged graph
    val dirBuckets =
      mutable.HashMap.empty[(VertexId, VertexId), mutable.ArrayBuffer[HalfEdge]]

    edgeMap.values.foreach: e =>
      e.next.foreach: n =>
        val key = (e.origin.id, n.origin.id)
        val buf = dirBuckets.getOrElseUpdate(key, mutable.ArrayBuffer.empty[HalfEdge])
        buf += e

    dirBuckets.foreach:
      case ((o, d), buf) =>
        val oppKey = (d, o)
        dirBuckets.get(oppKey).foreach: oppBuf =>
          val count = math.min(buf.size, oppBuf.size)
          var i     = 0
          while i < count do
            val e1 = buf(i)
            val e2 = oppBuf(i)
            if e1.twin.isEmpty && e2.twin.isEmpty then
              e1.twinWith(e2)
            i += 1

    // -----------------------------------------------------------------------
    // 3. Collapse duplicated seam edges (same origin/destination)
    // -----------------------------------------------------------------------
    val allNewEdgesInitial: List[HalfEdge] =
      edgeMap.values.toList

    val toRemove = mutable.Set.empty[HalfEdge]

    // Group by undirected key (min(originId, destId), max(originId, destId))
    val byUndirected: Map[(VertexId, VertexId), List[HalfEdge]] =
      allNewEdgesInitial
        .map: halfEdge =>
          val oId = halfEdge.origin.id
          val dId = halfEdge.destinationUnsafe.id
          if oId.value <= dId.value then ((oId, dId), halfEdge) else ((dId, oId), halfEdge)
        .groupMap((vertexIdPair, _) => vertexIdPair): (_, halfEdge) =>
          halfEdge

    byUndirected.values.foreach: edges =>
      // Partition by direction (origin,dest)
      val byDir = edges.groupBy: e =>
        (e.origin.id, e.destinationUnsafe.id)
      if byDir.size >= 2 then
        // One representative per direction, prefer edges with an incident inner face
        val mainPerDir: Map[(VertexId, VertexId), HalfEdge] =
          byDir.view
            .mapValues: sameDirEdges =>
              sameDirEdges
                .find:
                  _.incidentFace.isDefined
                .getOrElse(sameDirEdges.head)
            .toMap

        // Wire the two main directions as twins
        val mains = mainPerDir.values.toList
        if mains.size == 2 then
          val a = mains.head
          val b = mains(1)
          a.twinWith(b)

        // All other edges in each direction are redundant; rewire their neighbours to the main
        byDir.foreach:
          case ((origId, destId), sameDirEdges) =>
            if sameDirEdges.size > 1 then
              val main = mainPerDir((origId, destId))
              sameDirEdges.foreach: redundant =>
                if redundant ne main then
                  // Redirect prev / next around redundant edge to main
                  redundant.prev.foreach: p =>
                    if p.next.contains(redundant) then p.next = Some(main)
                  redundant.next.foreach: n =>
                    if n.prev.contains(redundant) then n.prev = Some(main)
                  toRemove += redundant

    val allNewEdgesNoDuplicates: List[HalfEdge] =
      allNewEdgesInitial.filterNot(toRemove.contains)

    // -----------------------------------------------------------------------
    // 4. Rebuild outer face and assign boundary edges
    // -----------------------------------------------------------------------
    // Boundary edges are those not incident to any inner face
    val boundaryEdges =
      allNewEdgesNoDuplicates.filter:
        _.incidentFace.isEmpty

    val newOuterFace = Face(FaceId.outerId)

    if boundaryEdges.nonEmpty then
      val ordered = boundaryEdges.orderBoundary
      // Link boundary cycle and assign outer face
      ordered.linkInCycle()
      ordered.foreach: e =>
        e.incidentFace = Some(newOuterFace)
      newOuterFace.outerComponent = ordered.headOption
      // Recompute boundary angles from incident inner angles, as in TilingBuilder.setOuterAngles
      ordered.foreach: outerEdge =>
        val vertex         = outerEdge.origin
        val incidentAtV    = allNewEdgesNoDuplicates.filter:
          _.origin eq vertex
        val innerAnglesSum = incidentAtV.interiorAnglesSum(newOuterFace)
        outerEdge.angle = Some(innerAnglesSum.conjugate)

    // -----------------------------------------------------------------------
    // 5. Ensure each merged vertex has a leaving edge
    // -----------------------------------------------------------------------
    val allNewEdges = allNewEdgesNoDuplicates

    newVertices.foreach: vertex =>
      // Prefer a boundary edge as leaving if available, else any incident edge
      val boundaryLeaving = boundaryEdges.find:
        _.origin eq vertex
      val anyLeaving      = allNewEdges.find:
        _.origin eq vertex
      vertex.leaving = boundaryLeaving.orElse(anyLeaving)

    // -----------------------------------------------------------------------
    // 6. Build merged faces lists and validate
    // -----------------------------------------------------------------------
    // Keep only faces still referenced by a surviving inner edge: a unified, fully-overlapping face whose
    // duplicate edges were collapsed away drops out here, leaving exactly one face per occupied region.
    val newInnerFaces: List[Face] =
      allNewEdges
        .flatMap: edge =>
          edge.incidentFace
        .filterNot: face =>
          face.isOuter
        .distinct

    // Seam-collapse may have removed the very edge a surviving face pointed to as its outer component
    // (the kept representative can come from either tiling). Re-anchor it to a surviving incident edge.
    val survivingEdges: Set[HalfEdge] =
      allNewEdges.toSet
    newInnerFaces.foreach: face =>
      if !face.outerComponent.exists(survivingEdges.contains) then
        allNewEdges
          .find: edge =>
            edge.incidentFace.contains(face)
          .foreach: edge =>
            face.outerComponent = Some(edge)

    TilingDCEL(newVertices, allNewEdges, newInnerFaces, newOuterFace)
