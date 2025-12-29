package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}

import scala.annotation.tailrec

object TilingDeletion:

  /** Classification of edges around a face being deleted.
    *
    * @param faceEdges
    *   The half-edges that bound the face to be deleted
    * @param boundaryTwins
    *   Twin edges that are on the outer boundary
    * @param innerTwins
    *   Twin edges that are internal to the tiling
    */
  private case class EdgeClassification(
      faceEdges: List[HalfEdge],
      boundaryTwins: List[HalfEdge],
      innerTwins: List[HalfEdge]
  )

  extension (tiling: TilingDCEL)

    /** Deletes an inner face and updates the boundary and incidence structure accordingly.
      *
      * Preconditions:
      *   - faceId references an existing bounded face of the tiling.
      *   - Removing the face will not partition the tiling into multiple disconnected components except for
      *     the intended boundary change; specifically: a viable interior path among the remaining faces must
      *     exist, and no single-vertex bottlenecks should be introduced.
      *
      * Postconditions on success:
      *   - The specified face is removed from innerFaces.
      *   - If the deleted face shared edges with the outer boundary, a new boundary cycle is formed by
      *     creating or relinking corresponding boundary half-edges; otherwise, vertices belonging only to the
      *     removed face may be deleted if they become isolated.
      *   - DCEL invariants are preserved: twin, next/prev, incidentFace, and vertex-leaving edges are
      *     consistent.
      *   - Returns a new TilingDCEL reflecting the deletion; if it was the only inner face, the result is an
      *     empty tessellation.
      *
      * Failure cases:
      *   - Returns a TilingError if the face does not exist, or if removing it would invalidate connectivity,
      *     or if the operation cannot maintain DCEL consistency.
      */
    private[dcel] def deleteFace(faceId: FaceId): Either[TilingError, TilingDCEL] =
      for
        faceToDelete       <- tiling.findInnerFace(faceId)
        edgeClassification <- classifyFaceEdges(faceToDelete)
        _                  <- validateDeletionWontPartition(faceToDelete, edgeClassification.innerTwins)
        result             <-
          if tiling.innerFaces.length == 1 then Right(TilingDCEL.empty)
          else if edgeClassification.boundaryTwins.nonEmpty then
            Right(performFaceDeletion(faceToDelete, edgeClassification))
          else
            val vertices = faceToDelete.getVerticesUnsafe
            vertices.foldLeft(Right(tiling): Either[TilingError, TilingDCEL]): (either, vertex) =>
              either.flatMap:
                _.deleteVertex(vertex.id)
              match
                case Left(error) if error == NotFoundError("Vertex", vertex.id.value) => either
                case other                                                            => other
      yield result

    private def classifyFaceEdges(face: Face): Either[TilingError, EdgeClassification] =
      val faceEdges                   = face.halfEdgesUnsafe
      val twinEdges                   = faceEdges.map(_.twin.get)
      val (boundaryTwins, innerTwins) = twinEdges.partition(tiling.isBoundaryEdge)
      Right(EdgeClassification(faceEdges, boundaryTwins, innerTwins))

    /** Validates that removing a face won't partition the tiling into disconnected components.
      *
      * @param face
      *   The face to be deleted
      * @param innerTwins
      *   The twin edges of the face that are internal (not on boundary)
      * @return
      *   Either an error message if deletion would partition the tiling, or Unit if safe
      */
    private def validateDeletionWontPartition(
        face: Face,
        innerTwins: List[HalfEdge]
    ): Either[TilingError, Unit] =
      innerTwins.maybePath match
        case None       => Left(
            TopologyError(s"Removing face ${face.id} would partition the tiling in two disconnected halves.")
          )
        case Some(path) =>
          val innerVertices = path.map(_.origin).drop(1)
          if tiling.boundaryVertices.intersect(innerVertices).isEmpty then Right(())
          else
            Left(TopologyError(
              s"Removing face ${face.id} would partition the tiling in two or more parts connected by just a vertex."
            ))

    private def performFaceDeletion(faceToDelete: Face, classification: EdgeClassification): TilingDCEL =
      val EdgeClassification(faceEdges, boundaryTwins, innerTwins) = classification
      val orderedInnerTwins                                        = innerTwins.maybePath.getOrElse(List.empty)

      val newOuterEdges = createNewOuterBoundaryEdges(orderedInnerTwins)
      relinkBoundaryAroundDeletedFace(boundaryTwins, newOuterEdges)
      updateDCELAfterFaceDeletion(faceToDelete, faceEdges, boundaryTwins, newOuterEdges, innerTwins)

    private def createNewOuterBoundaryEdges(orderedInnerTwins: List[HalfEdge]): List[HalfEdge] =
      val newOuterEdges = orderedInnerTwins.reverse.map { innerTwin =>
        val newTwin = HalfEdge(innerTwin.destinationUnsafe, incidentFace = Some(tiling.outerFace))
        newTwin.twinWith(innerTwin)
        newTwin
      }
      newOuterEdges.linkInCycle()
      newOuterEdges

    private def relinkBoundaryAroundDeletedFace(
        boundaryTwins: List[HalfEdge],
        newOuterEdges: List[HalfEdge]
    ): Unit =
      if boundaryTwins.nonEmpty then
        val boundaryTwinsSet  = boundaryTwins.toSet
        val firstBoundaryTwin = boundaryTwins.find(edge => !boundaryTwinsSet.contains(edge.prev.get)).get
        val lastBoundaryTwin  = boundaryTwins.find(edge => !boundaryTwinsSet.contains(edge.next.get)).get
        val beforeGap         = firstBoundaryTwin.prev.get
        val afterGap          = lastBoundaryTwin.next.get

        if newOuterEdges.nonEmpty then
          // Stitch the newOuterEdges chain into the gap.
          HalfEdge.insertBoundarySegment(beforeGap, afterGap, newOuterEdges)
        else
          // No inner neighbors, just close the gap in the boundary.
          beforeGap.linkWith(afterGap)

        tiling.outerFace.outerComponent = Some(beforeGap)

    private def updateDCELAfterFaceDeletion(
        faceToDelete: Face,
        faceEdges: List[HalfEdge],
        boundaryTwins: List[HalfEdge],
        newOuterEdges: List[HalfEdge],
        innerTwins: List[HalfEdge]
    ): TilingDCEL =
      val removedEdges   = faceEdges.toSet ++ boundaryTwins.toSet
      val finalHalfEdges = tiling.halfEdges.filterNot(removedEdges.contains) ++ newOuterEdges

      val verticesInUse =
        finalHalfEdges
          .map: halfEdge =>
            halfEdge.origin
          .toSet
      val finalVertices =
        tiling.vertices.filter: vertex =>
          verticesInUse.contains(vertex)

      updateVertexLeavingPointers(finalVertices, removedEdges, finalHalfEdges)
      recalculateBoundaryAngles(boundaryTwins, innerTwins)

      TilingDCEL(
        vertices = finalVertices,
        halfEdges = finalHalfEdges,
        innerFaces = tiling.innerFaces.filterNot(_ == faceToDelete),
        outerFace = tiling.outerFace
      )

    private def updateVertexLeavingPointers(
        vertices: List[Vertex],
        removedEdges: Set[HalfEdge],
        finalHalfEdges: List[HalfEdge]
    ): Unit =
      vertices.foreach: vertex =>
        if vertex.leaving.exists: halfEdge =>
            removedEdges.contains(halfEdge)
        then
          vertex.leaving = finalHalfEdges.find: halfEdge =>
            halfEdge.origin == vertex

    private def recalculateBoundaryAngles(boundaryTwins: List[HalfEdge], innerTwins: List[HalfEdge]): Unit =
      val fromBoundary          =
        boundaryTwins.map: halfEdge =>
          halfEdge.origin
      val fromInner             =
        innerTwins.flatMap: halfEdge =>
          List(halfEdge.origin, halfEdge.destinationUnsafe)
      val verticesOnNewBoundary =
        (fromBoundary ::: fromInner).distinct
      verticesOnNewBoundary.foreach: vertex =>
        val angleSum = vertex.currentInteriorAngleSumUnsafe(tiling.outerFace)
        vertex.incidentEdgesUnsafe
          .find: halfEdge =>
            tiling.isBoundaryEdge(halfEdge)
          .foreach: halfEdge =>
            halfEdge.angle = Some(angleSum.conjugate)

    private def deleteIncidentFace(halfEdge: HalfEdge): Either[TilingError, TilingDCEL] =
      halfEdge.incidentFace
        .map: face =>
          face.id
        .toRight(ValidationError("Edge has no incident face"))
        .flatMap: faceId =>
          deleteFace(faceId)

    private[dcel] def deleteEdge(
        startVertexId: VertexId,
        endVertexId: VertexId
    ): Either[TilingError, TilingDCEL] =
      for
        (_, _, edge) <- tiling.findVerticesAndEdgeBetween(startVertexId, endVertexId)
        result       <-
          if tiling.isBoundaryEdge(edge.twin.get) then
            // this should never happen if a TilingDCEL is well-formed, but just in case
            deleteIncidentFace(edge)
          else if tiling.isBoundaryEdge(edge) then
            deleteIncidentFace(edge.twin.get)
          else
            performEdgePathDeletion(expandPathToDelete(edge))
      yield result

    private def expandPathToDelete(startEdge: HalfEdge): List[HalfEdge] =
      @tailrec
      def expand(path: List[HalfEdge], forward: Boolean): List[HalfEdge] =
        val (vertex, edge) =
          if forward then (path.last.destinationUnsafe, path.last) else (path.head.origin, path.head)
        if !vertex.isThread then path
        else
          val nextEdge =
            if forward then
              vertex.incidentEdgesUnsafe
                .find: halfEdge =>
                  halfEdge ne edge.twin.get
                .get
            else
              vertex.incidentEdgesUnsafe
                .find: halfEdge =>
                  halfEdge ne edge
                .get.twin.get
          val newPath  = if forward then path :+ nextEdge else nextEdge :: path
          expand(newPath, forward)

      val forwardExpanded = expand(List(startEdge), forward = true)
      expand(forwardExpanded, forward = false)

    private def performEdgePathDeletion(pathToDelete: List[HalfEdge]): Either[TilingError, TilingDCEL] =
      pathToDelete match
        case Nil   => Left(TopologyError("Cannot delete an empty path of edges."))
        case edges =>
          val twinsToDelete       = edges.map(_.twin.get)
          val faceToSurvive       = edges.head.incidentFace.get
          val faceToRemove        = twinsToDelete.head.incidentFace.get
          val edgesOfFaceToRemove = faceToRemove.halfEdgesUnsafe

          // Capture angles at endpoints before modification
          val startV = edges.head.origin
          val endV   = edges.last.destinationUnsafe

          val angleAtStartSurvived    = edges.head.angle
          val twinOfEdgeToPathAtStart = twinsToDelete.head.next.get
          val angleAtStartRemoved     = twinOfEdgeToPathAtStart.angle
          val newAngleAtStart         = for (a1 <- angleAtStartSurvived; a2 <- angleAtStartRemoved) yield a1 + a2

          val edgeFromPathAtEnd  = edges.last.next.get
          val angleAtEndSurvived = edgeFromPathAtEnd.angle
          val angleAtEndRemoved  = twinsToDelete.last.angle
          val newAngleAtEnd      = for (a1 <- angleAtEndSurvived; a2 <- angleAtEndRemoved) yield a1 + a2

          // 1. Relink edges
          val pathStartPrev     = edges.head.prev.get
          val pathEndNext       = edgeFromPathAtEnd
          val twinPathStartPrev = twinsToDelete.last.prev.get
          val twinPathEndNext   = twinOfEdgeToPathAtStart

          pathStartPrev.linkWith(twinPathEndNext)
          twinPathStartPrev.linkWith(pathEndNext)

          // Set new angles
          twinPathEndNext.angle = newAngleAtStart
          pathEndNext.angle = newAngleAtEnd

          // 2. Update incident face pointers for the edges that were part of the removed face's boundary
          edgesOfFaceToRemove.foreach(_.incidentFace = Some(faceToSurvive))

          // 3. Update component edge for the surviving face
          faceToSurvive.outerComponent = Some(pathEndNext)

          // 4. Remove obsolete entities
          val verticesInTheMiddle = pathToDelete.map(_.origin).tail.toSet
          val verticesToRemove    = verticesInTheMiddle -- Set(startV, endV)
          val edgesToRemove       = (pathToDelete ++ twinsToDelete).toSet

          // 5. Update 'leaving' pointers for start and end vertices of the path
          if startV.leaving.exists(edgesToRemove.contains) then
            startV.leaving = Some(twinPathEndNext)
          if endV.leaving.exists(edgesToRemove.contains) then
            endV.leaving = Some(pathEndNext)

          // Finalize DCEL
          val finalVertices   = tiling.vertices.filterNot(verticesToRemove.contains)
          val finalHalfEdges  = tiling.halfEdges.filterNot(edgesToRemove.contains)
          val finalInnerFaces = tiling.innerFaces.filterNot(_ == faceToRemove)

          val adjustedTiling      =
            TilingDCEL(
              vertices = finalVertices,
              halfEdges = finalHalfEdges,
              innerFaces = finalInnerFaces,
              outerFace = tiling.outerFace
            )
          val survivingFacePoints =
            faceToSurvive.halfEdgesUnsafe.map(_.origin.coords)
          if !survivingFacePoints.hasNoAlmostEqualPoints() then
            Left(ValidationError(
              s"The surviving face ${faceToSurvive.id} is not simple (it has vertices that are equal, which is not allowed)."
            ))
          else
            Right(adjustedTiling)

    private def deleteEdgesRecursively(edges: List[HalfEdge]): Either[TilingError, TilingDCEL] =
      val startingEither: Either[TilingError, TilingDCEL] = Right(tiling)
      edges.foldLeft(startingEither): (either, halfEdge) =>
        either.flatMap: tilingDCEL =>
          tilingDCEL.deleteEdge(halfEdge.origin.id, halfEdge.destinationUnsafe.id)

    private[dcel] def deleteVertex(vertexId: VertexId): Either[TilingError, TilingDCEL] =
      for
        vertex       <- tiling.findVertex(vertexId)
        adjacentEdges = tiling.halfEdges.filter(_.origin == vertex)
        result       <-
          val boundaryVertices = tiling.boundaryVertices
          if boundaryVertices.contains(vertex) then
            val boundaryHalfEdges              = tiling.boundaryEdges
            val start: HalfEdge                = boundaryHalfEdges.find(_.origin == vertex).get
            val prev                           = start.prev.get
            val (boundaryEdges, interiorEdges) =
              adjacentEdges.partition: halfEdge =>
                halfEdge.destinationUnsafe == start.destinationUnsafe
                  || halfEdge.destinationUnsafe == prev.origin
            val withoutInteriorEdges           =
              deleteEdgesRecursively(interiorEdges)
            withoutInteriorEdges.flatMap: tilingDCEL =>
              tilingDCEL.deleteEdge(vertexId, boundaryEdges.head.destinationUnsafe.id)
          else
            deleteEdgesRecursively(adjacentEdges.tail)
      yield result
