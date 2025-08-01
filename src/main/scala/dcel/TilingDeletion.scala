package io.github.scala_tessella
package dcel

import scala.annotation.tailrec

object TilingDeletion:

  private case class EdgeClassification(
    faceEdges: List[HalfEdge],
    boundaryTwins: List[HalfEdge],
    innerTwins: List[HalfEdge]
  )

  extension (tilingDCEL: TilingDCEL)

    def deletePolygon(faceId: String): Either[String, TilingDCEL] =
      for
        faceToDelete <- findInnerFace(faceId)
        edgeClassification <- classifyFaceEdges(faceToDelete)
        _ <- validateFaceDeletion(faceToDelete, edgeClassification)
      yield
        if tilingDCEL.innerFaces.length == 1 then TilingBuilder.empty
        else performFaceDeletion(faceToDelete, edgeClassification)
    
    private def findInnerFace(faceId: String): Either[String, Face] =
      tilingDCEL.innerFaces.find(_.id == faceId)
        .toRight(s"Inner face with ID $faceId not found.")
    
    private def classifyFaceEdges(face: Face): Either[String, EdgeClassification] =
      val faceEdges = face.halfEdgesSafe
      val twinEdges = faceEdges.map(_.twin.get)
      val (boundaryTwins, innerTwins) = twinEdges.partition(_.incidentFace.contains(tilingDCEL.outerFace))
      Right(EdgeClassification(faceEdges, boundaryTwins, innerTwins))
    
    private def validateFaceDeletion(face: Face, classification: EdgeClassification): Either[String, Unit] =
      for
        _ <- validateFaceIsBoundaryAdjacent(face, classification.boundaryTwins)
        _ <- validateDeletionWontPartition(face, classification.innerTwins)
      yield ()
    
    private def validateFaceIsBoundaryAdjacent(face: Face, boundaryTwins: List[HalfEdge]): Either[String, Unit] =
      if boundaryTwins.isEmpty then
        Left(s"Face ${face.id} is not adjacent to the outer boundary.")
      else
        Right(())
    
    private def validateDeletionWontPartition(face: Face, innerTwins: List[HalfEdge]): Either[String, Unit] =
      val neighborInnerFaces = innerTwins.map(_.incidentFace.get).distinct
      if neighborInnerFaces.length <= 1 then
        Right(())
      else
        // Check if the boundary vertices shared by the face form a connected path
        checkBoundaryVertexConnectivity(innerTwins) match
          case Some(_) => Right(())
          case None => Left(s"Removing face ${face.id} would partition the tiling.")
    
    /**
     * Checks if the boundary vertices that the deleted face shares with neighbor inner faces
     * form a connected path. If they do, removing the face won't partition the tiling.
     *
     * This is much more efficient than doing BFS on all faces, as we only need to check
     * the local connectivity around the face being deleted.
     */
    private def checkBoundaryVertexConnectivity(innerTwins: List[HalfEdge]): Option[Unit] =
      if innerTwins.isEmpty then return Some(())
    
      // Get vertices where the deleted face connects to other inner faces
      val sharedVertices = innerTwins.flatMap(edge => List(edge.origin, edge.twin.get.origin)).distinct
    
      if sharedVertices.length <= 1 then
        // If there's only one or no shared vertex, deletion won't partition
        return Some(())
    
      // Build adjacency map for these vertices through the boundary of the outer face
      val boundaryEdges = tilingDCEL.getBoundaryEdges
      val boundaryVertexAdjacency = Vertex.buildBoundaryVertexAdjacency(boundaryEdges.toOption.get, sharedVertices.toSet)
    
      // Check if all shared vertices are connected through the boundary path
      Vertex.checkConnectivity(sharedVertices.head, sharedVertices.toSet, boundaryVertexAdjacency)

    private def performFaceDeletion(faceToDelete: Face, classification: EdgeClassification): TilingDCEL =
      val EdgeClassification(faceEdges, boundaryTwins, innerTwins) = classification

      // Create new twin half-edges for the inner boundary edges, which will form a new segment of the outer boundary.
      val newOuterEdges = innerTwins.map { innerTwin =>
        val newTwin = HalfEdge(innerTwin.destination.get, incidentFace = Some(tilingDCEL.outerFace))
        newTwin.twinWith(innerTwin)
        newTwin
      }

      // Link the new outer edges together to form a chain.
      newOuterEdges.linkInCycle()

      // Re-link the main outer boundary around the deleted face.
      val boundaryTwinsSet = boundaryTwins.toSet
      if boundaryTwins.nonEmpty then
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

        tilingDCEL.outerFace.outerComponent = Some(beforeGap)

      // Update the DCEL components.
      val removedEdges = faceEdges.toSet ++ boundaryTwins.toSet
      val finalHalfEdges = tilingDCEL.halfEdges.filterNot(removedEdges.contains) ++ newOuterEdges

      val verticesInUse = finalHalfEdges.map(_.origin).toSet
      val finalVertices = tilingDCEL.vertices.filter(verticesInUse.contains)

      // Update `leaving` pointers for affected vertices.
      finalVertices.foreach { vertex =>
        if vertex.leaving.exists(removedEdges.contains) then
          vertex.leaving = finalHalfEdges.find(_.origin == vertex)
      }

      val finalInnerFaces = tilingDCEL.innerFaces.filterNot(_ == faceToDelete)

      // Recalculate angles on the new boundary.
      val verticesOnNewBoundary = (boundaryTwins.map(_.origin) ++ innerTwins.flatMap(e => List(e.origin, e.destination.get))).distinct
      verticesOnNewBoundary.foreach { vertex =>
        val angleSum = vertex.getCurrentInteriorAngleSum(tilingDCEL.outerFace)
        vertex.incidentEdges
          .find(_.incidentFace.contains(tilingDCEL.outerFace))
          .foreach(_.angle = Some(angleSum.conjugate))
      }

      TilingDCEL(
        vertices = finalVertices,
        halfEdges = finalHalfEdges,
        innerFaces = finalInnerFaces,
        outerFace = tilingDCEL.outerFace
      )

    def deleteEdge(vertexId1: String, vertexId2: String): Either[String, TilingDCEL] =
      for
        v1 <- tilingDCEL.findVertex(vertexId1).toRight(s"Vertex with ID $vertexId1 not found.")
        v2 <- tilingDCEL.findVertex(vertexId2).toRight(s"Vertex with ID $vertexId2 not found.")
        edge <- tilingDCEL.findEdgeBetween(v1, v2).toRight(s"Edge between vertices $vertexId1 and $vertexId2 not found.")
        result <-
          if edge.twin.exists(_.incidentFace.contains(tilingDCEL.outerFace)) then
            edge.incidentFace.map(_.id).toRight("Edge has no incident face").flatMap(deletePolygon)
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
              vertex.incidentEdges.find(_ ne edge.twin.get).get
            else
              vertex.incidentEdges.find(_ ne edge).get.twin.get
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

          // 1. Relink edges around the path to be deleted
          val pathStartPrev = edges.head.prev.get
          val pathEndNext = edges.last.next.get
          pathStartPrev.linkWith(pathEndNext)

          val twinPathStartPrev = twinsToDelete.last.prev.get
          val twinPathEndNext = twinsToDelete.head.next.get
          twinPathStartPrev.linkWith(twinPathEndNext)

          // 2. Update incident face pointers for the merged face
          @tailrec
          def updateFaceReferences(current: HalfEdge): Unit =
            current.incidentFace = Some(faceToSurvive)
            val next = current.next.get
            if next ne twinPathEndNext then updateFaceReferences(next)

          updateFaceReferences(twinPathEndNext)

          // 3. Update component edge for the surviving face
          faceToSurvive.outerComponent = Some(pathEndNext)

          // 4. Update 'leaving' pointers for start and end vertices of the path
          val startV = edges.head.origin
          val endV = edges.last.destination.get
          val edgesToRemove = (edges ++ twinsToDelete).toSet

          if startV.leaving.exists(edgesToRemove.contains) then
            startV.leaving = Some(twinPathEndNext)
          if endV.leaving.exists(edgesToRemove.contains) then
            endV.leaving = Some(twinPathStartPrev.twin.get)

          // 5. Create new collections for the updated TilingDCEL
          val verticesToRemove = edges.tail.map(_.origin)
          val newInnerFaces = tilingDCEL.innerFaces.filterNot(_.id == faceToRemove.id)

          Right(TilingDCEL(
            vertices = tilingDCEL.vertices.filterNot(verticesToRemove.contains),
            halfEdges = tilingDCEL.halfEdges.filterNot(edgesToRemove.contains),
            innerFaces = newInnerFaces,
            outerFace = tilingDCEL.outerFace
          ))
