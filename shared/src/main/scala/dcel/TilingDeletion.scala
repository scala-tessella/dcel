package dcel

import scala.annotation.tailrec

object TilingDeletion:

  /**
   * Classification of edges around a face being deleted.
   *
   * @param faceEdges     The half-edges that bound the face to be deleted
   * @param boundaryTwins Twin edges that are on the outer boundary
   * @param innerTwins    Twin edges that are interior to the tiling
   */
  private case class EdgeClassification(
    faceEdges: List[HalfEdge],
    boundaryTwins: List[HalfEdge],
    innerTwins: List[HalfEdge]
  )

  extension (tiling: TilingDCEL)

    def deleteFace(faceId: String): Either[String, TilingDCEL] =
      for
        faceToDelete <- findInnerFace(faceId)
        edgeClassification <- classifyFaceEdges(faceToDelete)
        _ <- validateDeletionWontPartition(faceToDelete, edgeClassification.innerTwins)
      yield
        if tiling.innerFaces.length == 1 then TilingDCEL.empty
        else if edgeClassification.boundaryTwins.nonEmpty then
          performFaceDeletion(faceToDelete, edgeClassification)
        else
          val vertices = faceToDelete.getVerticesUnsafe
          vertices.foldLeft(Right(tiling): Either[String, TilingDCEL]) {
            (either, vertex) => either.flatMap(_.deleteVertex(vertex.id))
          }.toOption.get

    private def findInnerFace(faceId: String): Either[String, Face] =
      tiling.innerFaces.find(_.id == faceId)
        .toRight(s"Inner face with ID $faceId not found.")

    private def classifyFaceEdges(face: Face): Either[String, EdgeClassification] =
      val faceEdges = face.halfEdgesUnsafe
      val twinEdges = faceEdges.map(_.twin.get)
      val (boundaryTwins, innerTwins) = twinEdges.partition(tiling.isBoundaryEdge)
      Right(EdgeClassification(faceEdges, boundaryTwins, innerTwins))

    /**
     * Validates that removing a face won't partition the tiling into disconnected components.
     *
     * @param face       The face to be deleted
     * @param innerTwins The twin edges of the face that are interior (not on boundary)
     * @return Either an error message if deletion would partition the tiling, or Unit if safe
     */
    private def validateDeletionWontPartition(face: Face, innerTwins: List[HalfEdge]): Either[String, Unit] =
      innerTwins.maybePath match
        case None => Left(s"Removing face ${face.id} would partition the tiling in two disconnected halves.")
        case Some(path) =>
          val innerVertices = path.map(_.origin).drop(1)
          if tiling.boundaryUnsafe.intersect(innerVertices).isEmpty then Right(())
          else Left(s"Removing face ${face.id} would partition the tiling in two or more parts connected by just a vertex.")

    private def performFaceDeletion(faceToDelete: Face, classification: EdgeClassification): TilingDCEL =
      val EdgeClassification(faceEdges, boundaryTwins, innerTwins) = classification
      val orderedInnerTwins = innerTwins.maybePath.getOrElse(List.empty)

      val newOuterEdges = createNewOuterBoundaryEdges(orderedInnerTwins)
      relinkBoundaryAroundDeletedFace(boundaryTwins, newOuterEdges)
      updateDCELAfterFaceDeletion(faceToDelete, faceEdges, boundaryTwins, newOuterEdges, innerTwins)

    private def createNewOuterBoundaryEdges(orderedInnerTwins: List[HalfEdge]): List[HalfEdge] =
      val newOuterEdges = orderedInnerTwins.reverse.map { innerTwin =>
        val newTwin = HalfEdge(innerTwin.destination.get, incidentFace = Some(tiling.outerFace))
        newTwin.twinWith(innerTwin)
        newTwin
      }
      newOuterEdges.linkInCycle()
      newOuterEdges

    private def relinkBoundaryAroundDeletedFace(boundaryTwins: List[HalfEdge], newOuterEdges: List[HalfEdge]): Unit =
      if boundaryTwins.nonEmpty then
        val boundaryTwinsSet = boundaryTwins.toSet
        val firstBoundaryTwin = boundaryTwins.find(edge => !boundaryTwinsSet.contains(edge.prev.get)).get
        val lastBoundaryTwin = boundaryTwins.find(edge => !boundaryTwinsSet.contains(edge.next.get)).get
        val beforeGap = firstBoundaryTwin.prev.get
        val afterGap = lastBoundaryTwin.next.get

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
      val removedEdges = faceEdges.toSet ++ boundaryTwins.toSet
      val finalHalfEdges = tiling.halfEdges.filterNot(removedEdges.contains) ++ newOuterEdges

      val verticesInUse = finalHalfEdges.map(_.origin).toSet
      val finalVertices = tiling.vertices.filter(verticesInUse.contains)

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
      vertices.foreach { vertex =>
        if vertex.leaving.exists(removedEdges.contains) then
          vertex.leaving = finalHalfEdges.find(_.origin == vertex)
      }

    private def recalculateBoundaryAngles(boundaryTwins: List[HalfEdge], innerTwins: List[HalfEdge]): Unit =
      val verticesOnNewBoundary = (boundaryTwins.map(_.origin) ++ innerTwins.flatMap(e => List(e.origin, e.destination.get))).distinct
      verticesOnNewBoundary.foreach { vertex =>
        val angleSum = vertex.currentInteriorAngleSumUnsafe(tiling.outerFace)
        vertex.incidentEdgesUnsafe
          .find(tiling.isBoundaryEdge)
          .foreach(_.angle = Some(angleSum.conjugate))
      }

    def deleteEdge(startVertexId: String, endVertexId: String): Either[String, TilingDCEL] =
      for
        (_, _, edge) <- tiling.findVerticesAndEdgeBetween(startVertexId, endVertexId)
        result <-
          if tiling.isBoundaryEdge(edge.twin.get) then
            // this should never happen if a TilingDCEL is well-formed, but just in case
            edge.incidentFace.map(_.id).toRight("Edge has no incident face").flatMap(deleteFace)
          else if tiling.isBoundaryEdge(edge) then
            edge.twin.get.incidentFace.map(_.id).toRight("Edge has no incident face").flatMap(deleteFace)
          else
            performEdgePathDeletion(expandPathToDelete(edge))
      yield result

    private def expandPathToDelete(startEdge: HalfEdge): List[HalfEdge] =
      @tailrec
      def expand(path: List[HalfEdge], forward: Boolean): List[HalfEdge] =
        val (vertex, edge) = if forward then (path.last.destination.get, path.last) else (path.head.origin, path.head)
        if !vertex.isThread then path
        else
          val nextEdge =
            if forward then
              vertex.incidentEdgesUnsafe.find(_ ne edge.twin.get).get
            else
              vertex.incidentEdgesUnsafe.find(_ ne edge).get.twin.get
          val newPath = if forward then path :+ nextEdge else nextEdge :: path
          expand(newPath, forward)

      val forwardExpanded = expand(List(startEdge), forward = true)
      expand(forwardExpanded, forward = false)

    private def performEdgePathDeletion(pathToDelete: List[HalfEdge]): Either[String, TilingDCEL] =
      pathToDelete match
        case Nil => Left("Cannot delete an empty path of edges.")
        case edges =>
          val twinsToDelete = edges.map(_.twin.get)
          val faceToSurvive = edges.head.incidentFace.get
          val faceToRemove = twinsToDelete.head.incidentFace.get
          val edgesOfFaceToRemove = faceToRemove.halfEdgesUnsafe

          // Capture angles at endpoints before modification
          val startV = edges.head.origin
          val endV = edges.last.destination.get

          val angleAtStartSurvived = edges.head.angle
          val twinOfEdgeToPathAtStart = twinsToDelete.head.next.get
          val angleAtStartRemoved = twinOfEdgeToPathAtStart.angle
          val newAngleAtStart = for (a1 <- angleAtStartSurvived; a2 <- angleAtStartRemoved) yield a1 + a2

          val edgeFromPathAtEnd = edges.last.next.get
          val angleAtEndSurvived = edgeFromPathAtEnd.angle
          val angleAtEndRemoved = twinsToDelete.last.angle
          val newAngleAtEnd = for (a1 <- angleAtEndSurvived; a2 <- angleAtEndRemoved) yield a1 + a2

          // 1. Relink edges
          val pathStartPrev = edges.head.prev.get
          val pathEndNext = edgeFromPathAtEnd
          val twinPathStartPrev = twinsToDelete.last.prev.get
          val twinPathEndNext = twinOfEdgeToPathAtStart

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
          val verticesToRemove = verticesInTheMiddle -- Set(startV, endV)
          val edgesToRemove = (pathToDelete ++ twinsToDelete).toSet

          // 5. Update 'leaving' pointers for start and end vertices of the path
          if startV.leaving.exists(edgesToRemove.contains) then
            startV.leaving = Some(twinPathEndNext)
          if endV.leaving.exists(edgesToRemove.contains) then
            endV.leaving = Some(pathEndNext)

          // Finalize DCEL
          val finalVertices = tiling.vertices.filterNot(verticesToRemove.contains)
          val finalHalfEdges = tiling.halfEdges.filterNot(edgesToRemove.contains)
          val finalInnerFaces = tiling.innerFaces.filterNot(_ == faceToRemove)

          val adjustedTiling =
            TilingDCEL(
              vertices = finalVertices,
              halfEdges = finalHalfEdges,
              innerFaces = finalInnerFaces,
              outerFace = tiling.outerFace
            )
          val survivingFacePoints =
            faceToSurvive.halfEdgesUnsafe.map(_.origin.coords)
          if !survivingFacePoints.hasNoAlmostEqualPoints() then
            Left(s"The surviving face ${faceToSurvive.id} is not simple (it has vertices that are equal, which is not allowed).")
          else
            Right(adjustedTiling)

    def deleteVertex(vertexId: String): Either[String, TilingDCEL] =
      for
        vertex <- tiling.findVertex(vertexId).toRight(s"Vertex with ID $vertexId not found.")
        adjacentEdges = tiling.halfEdges.filter(_.origin == vertex)
        result <-
          val boundaryVertices = tiling.boundaryUnsafe
          if boundaryVertices.contains(vertex) then
            val boundaryHalfEdges = tiling.getBoundaryEdgesUnsafe
            val start: HalfEdge = boundaryHalfEdges.find(_.origin == vertex).get
            val prev = start.prev.get
            val (boundaryEdges, interiorEdges) = adjacentEdges.partition(edge =>
              edge.destination.get == start.destination.get || edge.destination.get == prev.origin
            )
            val withoutInteriorEdges = interiorEdges.foldLeft(Right(tiling): Either[String, TilingDCEL]) {
              (either, edge) => either.flatMap(_.deleteEdge(edge.origin.id, edge.destination.get.id))
            }
            withoutInteriorEdges.flatMap(_.deleteEdge(vertexId, boundaryEdges.head.destination.get.id))
          else
            adjacentEdges.tail.foldLeft(Right(tiling): Either[String, TilingDCEL]) {
              (either, edge) => either.flatMap(_.deleteEdge(edge.origin.id, edge.destination.get.id))
            }
      yield
        result
