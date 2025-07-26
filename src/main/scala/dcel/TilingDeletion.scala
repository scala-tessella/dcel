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
        else ??? //performFaceDeletion(faceToDelete, edgeClassification)
    
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
