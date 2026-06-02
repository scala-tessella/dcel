package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}

/** Vertex-level merge of two [[TilingDCEL]] instances: identifies coincident vertices, unifies them, rewires
  * half-edges and faces, and recomputes the boundary. The result is a single tiling containing the union of
  * both inputs' inner faces, plus any region the weld newly **encloses** — such a region is materialised as a
  * new inner face rather than left as a hole, per the model's "holes are just inner polygons" invariant
  * (ADR-0012).
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

    // Stable order (by id) so the merged tiling is a pure function of its inputs, not of hash iteration order.
    val newVertices: List[Vertex] =
      repToVertex.values.toList.sortBy(_.id.value)

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

    // 2.e – (re)wire twins purely by endpoints in the merged graph. Buckets are filled in `allOldEdges` order
    //        (base edges before copy edges) so the index-pairing below is deterministic: coincident edges from
    //        the same operand pair with each other, and seam-collapse (step 3) then merges across operands.
    val dirBuckets =
      mutable.HashMap.empty[(VertexId, VertexId), mutable.ArrayBuffer[HalfEdge]]

    allOldEdges.foreach: oldEdge =>
      val e = edgeMap(oldEdge)
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
    // Stable order (base edges before copy edges); everything downstream — seam-collapse grouping, faceless-edge
    // ordering, boundary tracing — inherits it, so the merge result no longer depends on hash iteration order.
    val allNewEdgesInitial: List[HalfEdge] =
      allOldEdges.map(edgeMap)

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
    // 4. Decompose the faceless edges into boundary cycles and classify them
    // -----------------------------------------------------------------------
    // After seam-collapse, every half-edge with no incident inner face lies on *some* rim: either the tiling's
    // outer silhouette, or a region the merge has just enclosed (e.g. the rhombus gap a reflected pentagon
    // cluster closes). Those faceless edges form one cycle per rim. We trace each cycle (the next-rim edge is
    // chosen geometrically at pinch vertices, see `nextBoundary`) and classify it by signed area: the clockwise
    // cycle(s) are the true outer boundary; every counterclockwise
    // cycle bounds an enclosed empty region that must become a new inner face (ADR-0012).
    import io.github.scala_tessella.dcel.geometry.BigPoint.*
    import io.github.scala_tessella.ring_seq.RingSeq.slidingO
    import scala.annotation.tailrec

    val facelessEdges: List[HalfEdge] =
      allNewEdgesNoDuplicates.filter:
        _.incidentFace.isEmpty

    val facelessByOrigin: Map[Vertex, List[HalfEdge]] =
      facelessEdges.groupBy:
        _.origin

    // Next faceless edge along the same rim. `edge` arrives at `vertex`; its rim continues along a faceless edge
    // *leaving* `vertex`. Almost every boundary vertex has exactly one such edge. The exception is a **pinch**,
    // where two rims (e.g. the outer silhouette and an enclosed region) meet at a single vertex and several
    // faceless edges leave it — there the rim that keeps the region on `edge`'s left is the first one
    // *clockwise* from `edge.twin`. A topological "rotate through the inner fan" rule cannot do this: the
    // region's own (empty) wedge has no inner edges to rotate across, so it would jump onto the wrong rim
    // (ADR-0013).
    val twoPi                                          = 2 * Math.PI
    def nextBoundary(edge: HalfEdge): Option[HalfEdge] =
      edge.twin.flatMap: twin =>
        val vertex = twin.origin // = edge.destination
        facelessByOrigin.getOrElse(vertex, Nil) match
          case Nil           => None
          case single :: Nil => Some(single)
          case candidates    =>
            // Direction of `edge.twin` (from `vertex` back along `edge`), and the smallest clockwise turn from
            // it to each candidate's direction.
            val reverseHeading = vertex.coords.angleTo(edge.origin.coords).toDouble
            Some(candidates.minBy: candidate =>
              val heading       = vertex.coords.angleTo(candidate.destinationUnsafe.coords).toDouble
              val clockwiseTurn = ((reverseHeading - heading) % twoPi + twoPi) % twoPi
              if clockwiseTurn <= 1e-9 then twoPi else clockwiseTurn)

    val visitedBoundary = mutable.Set.empty[HalfEdge]
    val boundaryCycles  = mutable.ListBuffer.empty[List[HalfEdge]]
    facelessEdges.foreach: start =>
      if !visitedBoundary.contains(start) then
        val cycle                       = mutable.ListBuffer.empty[HalfEdge]
        @tailrec
        def trace(edge: HalfEdge): Unit =
          if !visitedBoundary.contains(edge) then
            visitedBoundary += edge
            cycle += edge
            nextBoundary(edge) match
              case Some(next) => trace(next)
              case None       => ()
        trace(start)
        boundaryCycles += cycle.toList

    // Signed double area (shoelace, no abs): positive is counterclockwise (an enclosed region in this y-up
    // frame), negative is clockwise (the outer silhouette).
    def signedDoubleArea(cycle: List[HalfEdge]): BigDecimal =
      if cycle.sizeIs < 3 then BigDecimal(0)
      else
        cycle
          .map:
            _.origin.coords
          .slidingO(2)
          .map:
            (_: @unchecked) match
              case p1 :: p2 :: Nil => p1.cross(p2)
          .sum

    val (holeCycles, outerCycles) =
      boundaryCycles.toList.partition: cycle =>
        signedDoubleArea(cycle) > 0

    // -----------------------------------------------------------------------
    // 5. Materialise each enclosed region as a new inner face
    // -----------------------------------------------------------------------
    // An ordinary corner's interior angle is the conjugate of the angles the surrounding tiles already
    // contribute at that vertex (exact, the same per-vertex rule the outer boundary uses below). At a **pinch**
    // vertex the region also shares the vertex with the outer face (or another region), so "360 − tiles"
    // overshoots. We avoid an inexact `atan2` there: a simple polygon's interior angles sum to `(n − 2)·180`, so
    // with `k` pinch corners we read `k − 1` of them exactly off the geometry and close the last by that angle
    // sum (ADR-0014) — `k = 1` is ADR-0013's single-pinch closure unchanged. Keeping every corner rational is
    // what the angle-based simplicity check in `TilingValidation` needs. Fresh ids sit above every id the merged
    // inner faces already use.
    import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigRadian}

    def conjugateInteriorAngle(edge: HalfEdge, face: Face): AngleDegree =
      allNewEdgesNoDuplicates
        .filter:
          _.origin eq edge.origin
        .interiorAnglesSum(face)
        .conjugate

    def geometricInteriorAngle(edge: HalfEdge): AngleDegree =
      val vertex      = edge.origin
      val incomingEnd = edge.prev.map(_.origin).getOrElse(vertex)
      val incoming    = incomingEnd.coords.angleTo(vertex.coords).toDouble
      val outgoing    = vertex.coords.angleTo(edge.destinationUnsafe.coords).toDouble
      var turn        = outgoing - incoming
      while turn > Math.PI do turn -= twoPi
      while turn <= -Math.PI do turn += twoPi
      AngleDegree(BigRadian(Math.PI - turn))

    // ADR-0014: a pinch corner's interior wedge is one of a small set of canonical tiling angles. Recover it
    // *exactly* from the dot-product cosine of the hole's two (unit) edges at the corner — snapped to the
    // well-separated admissible cosines (squares/triangles/hexagons → {0, ±½, ±1}; octagons/dodecagons add
    // ±√2⁄2, ±√3⁄2) — with the cross-product sign giving the reflex case (the hole cycle is CCW). Returns None
    // for an unrecognised wedge, leaving the inexact `geometricInteriorAngle` fallback.
    val canonicalCosines: List[(Double, Int)] =
      List(
        1.0                 -> 0,
        Math.sqrt(3) / 2    -> 30,
        Math.sqrt(2) / 2    -> 45,
        0.5                 -> 60,
        0.0                 -> 90,
        -0.5                -> 120,
        -(Math.sqrt(2) / 2) -> 135,
        -(Math.sqrt(3) / 2) -> 150,
        -1.0                -> 180
      )
    val snapTolerance                         = 1e-6

    def exactInteriorAngle(edge: HalfEdge): Option[AngleDegree] =
      edge.prev.flatMap: prev =>
        val vertex = edge.origin.coords
        val u      = prev.origin.coords - vertex
        val w      = edge.destinationUnsafe.coords - vertex
        val lenU   = prev.origin.coords.distanceTo(vertex)
        val lenW   = edge.destinationUnsafe.coords.distanceTo(vertex)
        if lenU.signum == 0 || lenW.signum == 0 then None
        else
          val cos    = (u.dot(w) / (lenU * lenW)).toDouble
          val reflex = u.cross(w).signum > 0
          canonicalCosines
            .minByOption((c, _) => Math.abs(c - cos))
            .collect:
              case (c, degrees) if Math.abs(c - cos) <= snapTolerance =>
                AngleDegree(if reflex then 360 - degrees else degrees)

    val firstFreeFaceId: Int =
      (FaceId.outerId.value :: faceMap.values.map(_.id.value).toList).max + 1

    val newHoleFaces: List[Face] =
      holeCycles.zipWithIndex.map: (cycle, index) =>
        val face                     = Face(FaceId(firstFreeFaceId + index))
        cycle.linkInCycle()
        cycle.foreach: edge =>
          edge.incidentFace = Some(face)
        face.outerComponent = cycle.headOption
        val (pinchEdges, plainEdges) =
          cycle.partition: edge =>
            facelessByOrigin.getOrElse(edge.origin, Nil).sizeIs > 1
        plainEdges.foreach: edge =>
          edge.angle = Some(conjugateInteriorAngle(edge, face))
        pinchEdges match
          case Nil => () // fully surrounded: every corner is an exact conjugate above
          case _   =>
            // Read all but one pinch corner exactly off the geometry, then close the last with the polygon's
            // angle sum (ADR-0014). For a single pinch this is `k − 1 = 0` reads and the closure alone
            // (ADR-0013). The closure keeps the hole's angle sum exact even if a read fell back to `atan2`.
            val readEdges   = pinchEdges.init
            readEdges.foreach: edge =>
              edge.angle = Some(exactInteriorAngle(edge).getOrElse(geometricInteriorAngle(edge)))
            val interiorSum = AngleDegree(180 * (cycle.size - 2))
            val others      = (plainEdges ::: readEdges).flatMap(_.angle).sumExact
            pinchEdges.last.angle = Some(interiorSum - others)
        face

    // -----------------------------------------------------------------------
    // 6. Rebuild the outer face from the clockwise boundary cycle(s)
    // -----------------------------------------------------------------------
    val newOuterFace               = Face(FaceId.outerId)
    val outerEdges: List[HalfEdge] =
      outerCycles.flatten

    if outerEdges.nonEmpty then
      outerCycles.foreach:
        _.linkInCycle()
      outerEdges.foreach: edge =>
        edge.incidentFace = Some(newOuterFace)
      newOuterFace.outerComponent = outerCycles.headOption.flatMap:
        _.headOption
      // Recompute boundary angles from incident inner angles, as in TilingBuilder.setOuterAngles
      outerEdges.foreach: outerEdge =>
        val vertex         = outerEdge.origin
        val incidentAtV    = allNewEdgesNoDuplicates.filter:
          _.origin eq vertex
        val innerAnglesSum = incidentAtV.interiorAnglesSum(newOuterFace)
        outerEdge.angle = Some(innerAnglesSum.conjugate)

    // -----------------------------------------------------------------------
    // 7. Ensure each merged vertex has a leaving edge
    // -----------------------------------------------------------------------
    val allNewEdges = allNewEdgesNoDuplicates

    newVertices.foreach: vertex =>
      // Prefer an outer-boundary edge as leaving if available, else any incident edge
      val boundaryLeaving = outerEdges.find:
        _.origin eq vertex
      val anyLeaving      = allNewEdges.find:
        _.origin eq vertex
      vertex.leaving = boundaryLeaving.orElse(anyLeaving)

    // -----------------------------------------------------------------------
    // 8. Build merged faces lists and validate
    // -----------------------------------------------------------------------
    // Keep faces still referenced by a surviving inner edge (a unified, fully-overlapping face whose duplicate
    // edges were collapsed away drops out here, leaving exactly one face per occupied region) and add the
    // freshly-materialised enclosed faces from step 5.
    val newInnerFaces: List[Face] =
      allNewEdges
        .flatMap: edge =>
          edge.incidentFace
        .filterNot: face =>
          face.isOuter
        .:::(newHoleFaces)
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
