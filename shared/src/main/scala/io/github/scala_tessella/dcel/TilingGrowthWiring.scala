package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingBoundaryIntersection.{
  BoundaryAngles,
  BoundaryState,
  SharedEdgesResult,
  boundaryAngleForVertex,
  findSharedEdges
}
import io.github.scala_tessella.dcel.TilingMerge.OldNewVertexPair
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}

/** Low-level DCEL surgery behind [[TilingAddition]]'s growth pipeline: computing the vertices and twin
  * half-edge pairs a new polygon contributes, detecting hole closures, and rewiring the existing structures
  * (shared edges, boundary links, leaving edges) around the new face.
  */
private[dcel] object TilingGrowthWiring:

  private def createVertices(points: List[BigPoint], startingIndex: Int): List[Vertex] =
    points.zipWithIndex.map: (point, index) =>
      Vertex(VertexId(startingIndex + index), point)

  private def createEdgePairs(
      vertices: List[Vertex],
      outerFace: Face,
      newFace: Face,
      startVertexAngle: AngleDegree,
      polygonAngles: List[AngleDegree]
  ): List[(HalfEdge, HalfEdge)] =
    val edgeVertices =
      vertices.sliding(2)
        .collect:
          case origin :: destination :: Nil => (origin, destination)
        .toList

    val subsequentBoundaryAngles =
      polygonAngles.init.map: angle =>
        angle.conjugate
    val boundaryAngles           = startVertexAngle +: subsequentBoundaryAngles

    edgeVertices.lazyZip(boundaryAngles).lazyZip(polygonAngles).map:
      case ((origin, destination), boundaryAngle, innerAngle) =>
        HalfEdge.createTwinHalfEdges(
          origin,
          destination,
          outerFace,
          newFace,
          boundaryAngle,
          innerAngle
        )

  private[dcel] def additionalVertices(
      startVertex: Vertex,
      endVertex: Vertex,
      edgeToBuildOn: HalfEdge,
      angles: List[AngleDegree],
      points: List[BigPoint],
      vertexIndex: Int,
      outer: Face
  ): Either[TilingError, (List[Vertex], SharedEdgesResult, BoundaryAngles)] =
    given outerFace: Face = outer

    // Calculate boundary angles
    val boundaryAngles = BoundaryAngles(
      start = boundaryAngleForVertex(startVertex, outerFace, angles.head),
      end = boundaryAngleForVertex(endVertex, outerFace, angles(1)),
      newVertices = angles.drop(2)
    )

    for
      edgesResult <- findSharedEdges(edgeToBuildOn, boundaryAngles)

      // Create new components
      vertexPoints        = points.drop(2).reverse
      revisedVertexPoints = vertexPoints.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)
      newVertices         = createVertices(revisedVertexPoints, vertexIndex)
    yield (newVertices, edgesResult, boundaryAngles)

  /** Finds a couple of vertices from the existing and the additional boundary sharing the same coords and
    * thus marking a hole
    *
    * @param startVertex
    *   the boundary vertex from where the new vertices are added
    * @param boundaryEdges
    *   the edges forming the tiling's boundary
    * @param newVertices
    *   the added vertices
    */
  private[dcel] def findHoleClosure(
      startVertex: Vertex,
      boundaryEdges: List[HalfEdge],
      newVertices: List[Vertex]
  ): Option[OldNewVertexPair] =
    val boundaryVertices =
      boundaryEdges.map: halfEdge =>
        halfEdge.origin

    // Find pairs of vertices (one from boundary, one new) that share coordinates
    val sharedVertices: List[OldNewVertexPair] =
      newVertices.sameCoords(boundaryVertices)
        .map: vertexPair =>
          vertexPair.swap

    if sharedVertices.isEmpty then
      return None

    val oldVertexIndices = sharedVertices.map: pair =>
      boundaryVertices.indexOf(pair.oldVertex)

    val forwardContiguousCount =
      oldVertexIndices.zip(oldVertexIndices.tail)
        .takeWhile: (a, b) =>
          a + 1 == b
        .length
    val endOfFirstBlock        = sharedVertices(forwardContiguousCount)

    // Find the first vertex from the final contiguous block of shared vertices
    val backwardContiguousCount =
      oldVertexIndices.reverse.zip(oldVertexIndices.reverse.tail)
        .takeWhile: (a, b) =>
          a - 1 == b
        .length
    val startOfLastBlock        = sharedVertices(sharedVertices.length - 1 - backwardContiguousCount)

    // Determine which closure point results in a smaller path on the boundary
    def shortestBoundaryPathLength(to: Vertex): Int =
      val pathLength = boundaryEdges.getPathUnsafe(from = startVertex, to = to).length
      math.min(pathLength, boundaryEdges.length - pathLength)

    val forwardPathLength  = shortestBoundaryPathLength(endOfFirstBlock.oldVertex)
    val backwardPathLength = shortestBoundaryPathLength(startOfLastBlock.oldVertex)

    if forwardPathLength < backwardPathLength then
      Some(endOfFirstBlock)
    else
      Some(startOfLastBlock)

  private[dcel] def additionalElements(
      edgeToBuildOn: HalfEdge,
      angles: List[AngleDegree],
      newFaceId: FaceId,
      newVertices: List[Vertex],
      edgesResult: SharedEdgesResult,
      boundaryAngles: BoundaryAngles
  ): (List[Vertex], List[HalfEdge], Face) =
    val innerFace = edgeToBuildOn.incidentFace.get

    given outerFace: Face = innerFace

    val newFace = Face(newFaceId)

    // Different start and end vertex
    val revisedStartVertex = edgesResult.startEdge.destinationUnsafe
    val revisedEndVertex   = edgesResult.endEdge.origin

    // Different boundary angles
    val revisedBoundaryAngles = BoundaryAngles(
      start = edgesResult.startCheck,
      end = edgesResult.endCheck,
      newVertices =
        boundaryAngles.newVertices.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)
    )

    // Different boundary
    val completeBoundary = BoundaryState(Some(edgesResult.startEdge), Some(edgesResult.endEdge))

    val allVertices = revisedStartVertex :: newVertices ::: revisedEndVertex :: Nil

    val revisedAngles = angles.reverse.drop(edgesResult.startCounter).dropRight(edgesResult.endCounter)

    val edgePairs                         =
      createEdgePairs(allVertices, outerFace, newFace, revisedBoundaryAngles.start, revisedAngles)
    val (newBoundaryEdges, newInnerEdges) = edgePairs.unzip

    val sharedAngles = angles.takeRight(edgesResult.startCounter) ++ angles.take(edgesResult.endCounter + 1)
    // Update existing structures
    updateExistingStructures(
      edgesResult.sharedEdges,
      newFace,
      sharedAngles,
      newBoundaryEdges,
      completeBoundary,
      revisedBoundaryAngles
    )

    // Link new face edges
    linkNewFaceEdges(edgeToBuildOn, edgesResult.sharedEdges, newInnerEdges.reverse, newFace)

    // Connect to boundary
    connectNewBoundaryEdges(newBoundaryEdges, completeBoundary, outerFace, edgesResult.sharedEdges)

    // Update vertex leaving edges
    updateVertexLeavingEdges(revisedStartVertex :: newVertices, newBoundaryEdges)

    (newVertices, newBoundaryEdges ::: newInnerEdges, newFace)

  private def updateExistingStructures(
      sharedEdges: List[HalfEdge],
      newFace: Face,
      polyAngles: List[AngleDegree],
      newBoundaryEdges: List[HalfEdge],
      originalBoundary: BoundaryState,
      boundaryAngles: BoundaryAngles
  ): Unit =
    // Update shared edges
    sharedEdges.foreach(_.incidentFace = Some(newFace))
    val sharedEdgesFirstAngle = newBoundaryEdges.head.angle
    sharedEdges.zipWithIndex.foreach: (edge, index) =>
      edge.angle = Some(polyAngles(index))

    // Update the existing boundary edge from end vertex
    val endVertexId =
      sharedEdges.last.destinationUnsafe.id
    originalBoundary.next.foreach: nextEdge =>
      if nextEdge.origin.id == endVertexId then
        nextEdge.angle = Some(boundaryAngles.end)

    // Update boundary in the special shared edges case
    if sharedEdges.length > 1 && newBoundaryEdges.length == 1 then
      newBoundaryEdges.head.angle = sharedEdgesFirstAngle

  private def linkNewFaceEdges(
      edgeToBuildOn: HalfEdge,
      sharedEdges: List[HalfEdge],
      reversedInnerEdges: List[HalfEdge],
      newFace: Face
  ): Unit =
    val allInnerEdges = sharedEdges ::: reversedInnerEdges
    allInnerEdges.linkInCycle()
    newFace.outerComponent = Some(edgeToBuildOn)

  private def connectNewBoundaryEdges(
      newBoundaryEdges: List[HalfEdge],
      originalBoundary: BoundaryState,
      outerFace: Face,
      sharedEdges: List[HalfEdge]
  ): Unit =
    HalfEdge.insertBoundarySegment(
      originalBoundary.prev.get,
      originalBoundary.next.get,
      newBoundaryEdges
    )

    // Update the outer face component if necessary
    if outerFace.outerComponent.exists(e => sharedEdges.contains(e)) then
      outerFace.outerComponent = newBoundaryEdges.headOption

  private def updateVertexLeavingEdges(
      verticesWithNewEdges: List[Vertex],
      newBoundaryEdges: List[HalfEdge]
  ): Unit =
    verticesWithNewEdges.zip(newBoundaryEdges).foreach: (vertex, edge) =>
      vertex.leaving = Some(edge)
