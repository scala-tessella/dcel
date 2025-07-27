package io.github.scala_tessella
package dcel

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
        newTwin.twin = Some(innerTwin)
        innerTwin.twin = Some(newTwin)
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