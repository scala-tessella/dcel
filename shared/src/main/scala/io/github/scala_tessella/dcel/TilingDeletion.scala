package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingBuilder.*
import io.github.scala_tessella.dcel.Utils.sequence
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
            performFaceDeletion(faceToDelete, edgeClassification)
          else
            val vertices = faceToDelete.getVerticesUnsafe
            vertices.foldLeft(Right(tiling): Either[TilingError, TilingDCEL]): (either, vertex) =>
              either.flatMap:
                _.deleteVertex(vertex.id)
              match
                case Left(error) if error == NotFoundError("Vertex", vertex.id.toPrefixedString) => either
                case other                                                                       => other
      yield result

    private def classifyFaceEdges(face: Face): Either[TilingError, EdgeClassification] =
      val faceEdges = face.halfEdgesUnsafe
      for
        twinEdges <- faceEdges
                       .map: edge =>
                         edge.twin.toRight(TopologyError(s"Edge ${edge.idUnsafe} has no twin."))
                       .sequence
      yield
        val (boundaryTwins, innerTwins) = twinEdges.partition(tiling.isBoundaryEdge)
        EdgeClassification(faceEdges, boundaryTwins, innerTwins)

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
      innerTwins.maybePathUnsafe match
        case None       => Left(
            TopologyError(s"Removing face ${face.id} would partition the tiling in two disconnected halves.")
          )
        case Some(path) =>
          val innerVertices = path.map(_.origin).drop(1)
          if tiling.boundaryVerticesUnsafe.intersect(innerVertices).isEmpty then Right(())
          else
            Left(TopologyError(
              s"Removing face ${face.id} would partition the tiling in two or more parts connected by just a vertex."
            ))

    private def performFaceDeletion(
        faceToDelete: Face,
        classification: EdgeClassification
    ): Either[TilingError, TilingDCEL] =
      val EdgeClassification(faceEdges, boundaryTwins, innerTwins) = classification
      for
        orderedInnerTwins <- innerTwins.maybePathUnsafe.toRight(
                               TopologyError(
                                 s"Cannot order inner twin edges while deleting face ${faceToDelete.id}."
                               )
                             )
        newOuterEdges      = createNewOuterBoundaryEdges(orderedInnerTwins)
        _                 <- relinkBoundaryAroundDeletedFace(boundaryTwins, newOuterEdges)
      yield updateDCELAfterFaceDeletion(faceToDelete, faceEdges, boundaryTwins, newOuterEdges, innerTwins)

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
    ): Either[TilingError, Unit] =
      if boundaryTwins.isEmpty then Right(())
      else
        val boundaryTwinsSet = boundaryTwins.toSet
        for
          firstBoundaryTwin <-
            boundaryTwins
              .find: edge =>
                edge.prev.exists(prev => !boundaryTwinsSet.contains(prev))
              .toRight(TopologyError("Could not locate first boundary twin while deleting face."))
          lastBoundaryTwin  <-
            boundaryTwins
              .find: edge =>
                edge.next.exists(next => !boundaryTwinsSet.contains(next))
              .toRight(TopologyError("Could not locate last boundary twin while deleting face."))
          beforeGap         <- firstBoundaryTwin.prev.toRight(
                                 TopologyError("Boundary twin has no previous edge while deleting face.")
                               )
          afterGap          <- lastBoundaryTwin.next.toRight(
                                 TopologyError("Boundary twin has no next edge while deleting face.")
                               )
        yield
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
      recalculateBoundaryAngles(boundaryTwins, innerTwins, finalHalfEdges)

      finalizeDeletion(
        finalVertices,
        finalHalfEdges,
        tiling.innerFaces.filterNot(_ == faceToDelete)
      )

    private def finalizeDeletion(
        finalVertices: List[Vertex],
        finalHalfEdges: List[HalfEdge],
        finalInnerFaces: List[Face]
    ): TilingDCEL =
      TilingDCEL(
        vertices = finalVertices,
        halfEdges = finalHalfEdges,
        innerFaces = finalInnerFaces,
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

    private def recalculateBoundaryAngles(
        boundaryTwins: List[HalfEdge],
        innerTwins: List[HalfEdge],
        allHalfEdges: List[HalfEdge]
    ): Unit =
      val fromBoundary          =
        boundaryTwins.map: halfEdge =>
          halfEdge.origin
      val fromInner             =
        innerTwins.flatMap: halfEdge =>
          List(halfEdge.origin, halfEdge.destinationUnsafe)
      val verticesOnNewBoundary =
        (fromBoundary ::: fromInner).distinct
      val boundaryEdges         =
        verticesOnNewBoundary.flatMap: vertex =>
          vertex.incidentEdgesUnsafe.find: halfEdge =>
            tiling.isBoundaryEdge(halfEdge)
      allHalfEdges.setOuterEdgeAngles(boundaryEdges.distinct, tiling.outerFace)

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
        twinEdge     <- edge.twin.toRight(ValidationError("Edge has no twin"))
        result       <-
          if tiling.isBoundaryEdge(twinEdge) then
            // this should never happen if a TilingDCEL is well-formed, but just in case
            deleteIncidentFace(edge)
          else if tiling.isBoundaryEdge(edge) then
            deleteIncidentFace(twinEdge)
          else
            expandPathToDelete(edge).flatMap(performEdgePathDeletion)
      yield result

    private def expandPathToDelete(startEdge: HalfEdge): Either[TilingError, List[HalfEdge]] =
      @tailrec
      def expand(path: List[HalfEdge], forward: Boolean): Either[TilingError, List[HalfEdge]] =
        val edge          =
          if forward then path.last else path.head
        val vertexOrError =
          if forward then
            edge.destination.toRight(
              TopologyError(s"Edge ${edge.idUnsafe} has no destination while deleting.")
            )
          else
            Right(path.head.origin)
        vertexOrError match
          case Left(error)   => Left(error)
          case Right(vertex) =>
            if !vertex.isThread then Right(path)
            else
              val nextEdgeEither: Either[TilingError, HalfEdge] =
                if forward then
                  for
                    twin <-
                      edge.twin.toRight(TopologyError(s"Edge ${edge.idUnsafe} has no twin while deleting."))
                    next <- vertex.incidentEdgesUnsafe
                              .find: halfEdge =>
                                halfEdge ne twin
                              .toRight(TopologyError(
                                s"No next edge found from vertex ${vertex.id} while deleting."
                              ))
                  yield next
                else
                  for
                    previous <-
                      vertex.incidentEdgesUnsafe
                        .find: halfEdge =>
                          halfEdge ne edge
                        .toRight(
                          TopologyError(s"No previous edge found from vertex ${vertex.id} while deleting.")
                        )
                    twin     <- previous.twin.toRight(
                                  TopologyError(s"Edge ${previous.idUnsafe} has no twin while deleting.")
                                )
                  yield twin
              nextEdgeEither match
                case Left(error)     => Left(error)
                case Right(nextEdge) =>
                  val newPath = if forward then path :+ nextEdge else nextEdge :: path
                  expand(newPath, forward)
      for
        forwardExpanded <- expand(List(startEdge), forward = true)
        fullPath        <- expand(forwardExpanded, forward = false)
      yield fullPath

    private def performEdgePathDeletion(pathToDelete: List[HalfEdge]): Either[TilingError, TilingDCEL] =
      pathToDelete match
        case Nil   => Left(TopologyError("Cannot delete an empty path of edges."))
        case edges =>
          for
            twinsToDelete           <- edges
                                         .map: edge =>
                                           edge.twin.toRight(TopologyError(s"Edge ${edge.idUnsafe} has no twin."))
                                         .sequence
            faceToSurvive           <- edges.head.incidentFace.toRight(
                                         ValidationError("Edge has no incident face")
                                       )
            faceToRemove            <- twinsToDelete.head.incidentFace.toRight(
                                         ValidationError("Twin edge has no incident face")
                                       )
            twinOfEdgeToPathAtStart <- twinsToDelete.head.next.toRight(
                                         TopologyError("Twin path start edge has no next edge.")
                                       )
            edgeFromPathAtEnd       <- edges.last.next.toRight(
                                         TopologyError("Path end edge has no next edge.")
                                       )
            pathStartPrev           <- edges.head.prev.toRight(
                                         TopologyError("Path start edge has no previous edge.")
                                       )
            twinPathStartPrev       <- twinsToDelete.last.prev.toRight(
                                         TopologyError("Twin path start edge has no previous edge.")
                                       )
            endV                    <- edges.last.destination.toRight(
                                         TopologyError(s"Edge ${edges.last.idUnsafe} has no destination.")
                                       )
            adjustedTiling          <-
              val edgesOfFaceToRemove = faceToRemove.halfEdgesUnsafe

              // Capture angles at endpoints before modification
              val startV = edges.head.origin

              val angleAtStartSurvived = edges.head.angle
              val angleAtStartRemoved  = twinOfEdgeToPathAtStart.angle
              val newAngleAtStart      = for a1 <- angleAtStartSurvived; a2 <- angleAtStartRemoved yield a1 + a2

              val angleAtEndSurvived = edgeFromPathAtEnd.angle
              val angleAtEndRemoved  = twinsToDelete.last.angle
              val newAngleAtEnd      = for a1 <- angleAtEndSurvived; a2 <- angleAtEndRemoved yield a1 + a2

              // 1. Relink edges
              val pathEndNext     = edgeFromPathAtEnd
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

              val computed            = finalizeDeletion(finalVertices, finalHalfEdges, finalInnerFaces)
              val survivingFacePoints =
                faceToSurvive.halfEdgesUnsafe.map(_.origin.coords)
              if !survivingFacePoints.hasNoAlmostEqualPoints() then
                Left(ValidationError(
                  s"The surviving face ${faceToSurvive.id} is not simple (it has vertices that are equal, which is not allowed)."
                ))
              else
                Right(computed)
          yield adjustedTiling

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
          val boundaryVertices = tiling.boundaryVerticesUnsafe
          if boundaryVertices.contains(vertex) then
            val boundaryHalfEdges = tiling.boundaryEdgesUnsafe
            for
              start                         <- boundaryHalfEdges
                                                 .find(_.origin == vertex)
                                                 .toRight(
                                                   NotFoundError("Boundary edge", s"starting from ${vertexId.toPrefixedString}")
                                                 )
              prev                          <- start.prev.toRight(
                                                 TopologyError(
                                                   s"Boundary edge ${start.idUnsafe} has no previous edge while deleting vertex."
                                                 )
                                               )
              (boundaryEdges, interiorEdges) = adjacentEdges.partition: halfEdge =>
                                                 halfEdge.destinationUnsafe == start.destinationUnsafe
                                                   || halfEdge.destinationUnsafe == prev.origin
              boundaryEdge                  <- boundaryEdges.headOption.toRight(
                                                 TopologyError(
                                                   s"No boundary edge found to remove for vertex ${vertexId.toPrefixedString}."
                                                 )
                                               )
              withoutInteriorEdges          <- deleteEdgesRecursively(interiorEdges)
              deletedBoundary               <- withoutInteriorEdges.deleteEdge(
                                                 vertexId,
                                                 boundaryEdge.destinationUnsafe.id
                                               )
            yield deletedBoundary
          else
            deleteEdgesRecursively(adjacentEdges.tail)
      yield result
