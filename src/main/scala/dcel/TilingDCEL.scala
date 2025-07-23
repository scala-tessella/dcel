package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, BigBox, BigLineSegment, BigPoint}
import Polygon.RegularPolygon
import spire.implicits.*

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Represents the entire tiling structure as a container for its components.
 *
 * @param vertices   List of all vertices in the tiling.
 * @param halfEdges  List of all half-edges in the tiling.
 * @param innerFaces List of the tiling's interior faces.
 * @param outerFace  The single, unbounded outer face of the tiling.
 */
case class TilingDCEL(
  vertices: List[Vertex],
  halfEdges: List[HalfEdge],
  innerFaces: List[Face],
  outerFace: Face
):

  /** @return a list of all faces, both inner and outer */
  def faces: List[Face] =
    outerFace :: innerFaces

  /**
   * Finds the outer boundary of the tiling.
   *
   * The traversal follows the half-edges of the outer face, which are linked in
   * a clockwise order around the perimeter.
   *
   * @return A Vector of Vertices forming the perimeter, in clockwise order.
   *         Returns an empty Vector if the outer face has no boundary component.
   */
  def boundary: Vector[Vertex] =
    outerFace.outerComponent.map { startEdge =>
      val builder = Vector.newBuilder[Vertex]

      @tailrec
      def collectVertices(edge: HalfEdge): Unit =
        builder += edge.origin
        edge.next match
          case Some(nextEdge) if nextEdge ne startEdge => collectVertices(nextEdge)
          case _ => // Traversal complete or malformed loop

      collectVertices(startEdge)
      builder.result()
    }.getOrElse(Vector.empty)

  def boundarySafe: Vector[Vertex] =
    outerFace.outerComponent match
      case None => Vector.empty
      case Some(startEdge) =>
        val builder = Vector.newBuilder[Vertex]
        val visited = scala.collection.mutable.Set.empty[HalfEdge]

        @tailrec
        def collectVertices(edge: HalfEdge): Unit =
          if visited.contains(edge) then
            if edge eq startEdge then
              () // completed loop: that's fine
            else
              throw new Error("Malformed loop detected") // repeated non-start: broken!
          else
            builder += edge.origin
            visited += edge
            edge.next match
              case Some(nextEdge) => collectVertices(nextEdge)
              case None => throw new Error("Open chain")

        collectVertices(startEdge)
        builder.result()

  /**
   * Helper method to get all half-edges forming the outer boundary loop.
   */
  private def getBoundaryEdges: List[HalfEdge] =
    outerFace.outerComponent.map { start =>
      @tailrec
      def loop(current: HalfEdge, acc: List[HalfEdge], visited: Set[HalfEdge]): List[HalfEdge] =
        if visited(current) then
          throw new IllegalStateException("Malformed boundary: an edge was visited twice.")

        val next = current.next.getOrElse(throw new IllegalStateException("Boundary loop is not closed."))

        if next eq start then
          (current :: acc).reverse
        else
          loop(next, current :: acc, visited + current)

      loop(start, Nil, Set.empty)
    }.getOrElse(Nil)

  /**
   * Finds a boundary half-edge that originates at the vertex with the given ID.
   *
   * @param vertexId The ID of the origin vertex.
   * @return An Option containing the HalfEdge if found, otherwise None.
   */
  private def findBoundaryEdge(vertexId: String): Option[HalfEdge] =
    getBoundaryEdges.find(_.origin.id == vertexId)

  /**
   * Calculates the coordinates of the new vertices for a regular polygon built upon an existing edge.
   *
   * @param v_start The first vertex of the base edge.
   * @param v_end   The second vertex of the base edge.
   * @param sides   The number of sides for the new regular polygon.
   * @return A list of (Double, Double) tuples for the new vertex coordinates.
   */
  private def calculateNewVertices(v_start: Vertex, v_end: Vertex, sides: Int): List[BigPoint] =
    val edgeVector = (v_end.coords.x - v_start.coords.x, v_end.coords.y - v_start.coords.y)
    val turnAngle = calculateTurnAngle(sides)
    generatePolygonVertices(v_end.coords, edgeVector, turnAngle, sides)

  private def calculateTurnAngle(sides: Int): AngleDegree =
    AngleDegree(180) - RegularPolygon(sides).alphaDegree

  private def generatePolygonVertices(startPoint: BigPoint, edgeVector: (BigDecimal, BigDecimal), turnAngle: AngleDegree, sides: Int): List[BigPoint] =
    var heading: BigDecimal = spire.math.atan2(edgeVector._2, edgeVector._1)
    var currentPoint = startPoint
    val newPoints = List.newBuilder[BigPoint]
    val turnAngleRadians = turnAngle.toBigRadian.toBigDecimal
    for (_ <- 1 until sides - 1)
      heading = heading + turnAngleRadians
      val nextPoint = BigPoint(
        currentPoint.x + spire.math.cos(heading),
        currentPoint.y + spire.math.sin(heading)
      )
      newPoints += nextPoint
      currentPoint = nextPoint

    newPoints.result()

  /**
   * Enhanced intersection check that identifies which existing vertices should be reused.
   *
   * @param hasProperIntersection     True if there are edge crossings outside of vertices  
   * @param hasVertexOnlyIntersection True if intersection exists only through shared vertex coordinates
   * @param vertexMatches             Map from new vertex indices to existing vertices that should be reused
   */
  private case class IntersectionCheck(
    hasProperIntersection: Boolean,
    hasVertexOnlyIntersection: Boolean,
    vertexMatches: Map[Int, Vertex] = Map.empty
  )

  /**
   * Enhanced polygon intersection check that identifies vertex matches for reuse.
   *
   * @param baseEdge        The base edge of the existing boundary
   * @param v_start         The starting vertex of the base edge
   * @param v_end           The ending vertex of the base edge
   * @param newVertexCoords The coordinates of the new vertices to be added
   * @return IntersectionCheck with vertex matching information
   */
  private def checkPolygonIntersection(
    baseEdge: HalfEdge,
    v_start: Vertex,
    v_end: Vertex,
    newVertexCoords: List[BigPoint]
  ): IntersectionCheck =
    val newEdgesPoints = v_end.coords +: newVertexCoords :+ v_start.coords
    val newEdgeSegments = newEdgesPoints.sliding(2).collect { case Seq(p1, p2) => BigLineSegment(p1, p2) }.toList

    val newPolygonBBox = BigBox.fromPoints(newEdgesPoints)
    val searchBBox = newPolygonBBox.expand(BigDecimal(1.0))

    val boundaryEdgesToCheck = getBoundaryEdges.filterNot { edge =>
      edge == baseEdge || baseEdge.next.contains(edge) || baseEdge.prev.contains(edge)
    }

    val nearbyBoundarySegments = for {
      edge <- boundaryEdgesToCheck
      twin <- edge.twin
      segment = BigLineSegment(edge.origin.coords, twin.origin.coords)
      if searchBBox.intersects(BigBox.fromSegment(segment))
    } yield segment

    var hasProperIntersection = false
    var hasVertexOnlyIntersection = false

    // Check each new edge segment against nearby boundary segments
    for
      newSeg <- newEdgeSegments
      boundarySeg <- nearbyBoundarySegments
      if newSeg.intersects(boundarySeg)
    do
      // Check if this is a proper intersection (crossing) or just vertex sharing
      if newSeg.properlyIntersects(boundarySeg) then
        hasProperIntersection = true
      else
        hasVertexOnlyIntersection = true

    // Find existing boundary vertices that match new vertex coordinates
    val existingBoundaryVertices = getBoundaryEdges.map(_.origin)
    val vertexMatches = mutable.Map.empty[Int, Vertex]

    for
      (newPoint, index) <- newVertexCoords.zipWithIndex
      existingVertex <- existingBoundaryVertices
      if existingVertex.coords.almostEquals(newPoint)
    do
      vertexMatches(index) = existingVertex
      hasVertexOnlyIntersection = true

    IntersectionCheck(hasProperIntersection, hasVertexOnlyIntersection, vertexMatches.toMap)

  /**
   * Gets the next available vertex ID by finding the maximum existing ID number.
   */
  private def getNextVertexId: Int =
    val existingIds = vertices.map(_.id).collect {
      case id if id.startsWith("V") && id.drop(1).forall(_.isDigit) => id.drop(1).toInt
    }
    if existingIds.nonEmpty then existingIds.max + 1 else 0

  /**
   * Creates new vertices for a regular polygon, reusing existing vertices where matches are found.
   *
   * @param newVertexCoords The coordinates for new vertices
   * @param vertexMatches   Map of indices to existing vertices that should be reused
   * @return List of vertices (mix of new and existing)
   */
  private def createNewVerticesWithReuse(
    newVertexCoords: List[BigPoint],
    vertexMatches: Map[Int, Vertex]
  ): List[Vertex] =
    val nextVertexId = getNextVertexId
    var newVertexCounter = 0

    newVertexCoords.zipWithIndex.map { case (bigPoint, i) =>
      vertexMatches.get(i) match
        case Some(existingVertex) => existingVertex
        case None =>
          val vertex = Vertex(s"V${nextVertexId + newVertexCounter}", bigPoint)
          newVertexCounter += 1
          vertex
    }

  /**
   * Enhanced edge creation that handles vertex reuse correctly.
   */
  private def createHalfEdgePairsWithReuse(
    polyVertices: List[Vertex],
    newFace: Face,
    vertexMatches: Map[Int, Vertex]
  ): (List[HalfEdge], List[HalfEdge]) =
    val newInnerEdges = mutable.ListBuffer.empty[HalfEdge]
    val newOuterEdges = mutable.ListBuffer.empty[HalfEdge]
    val sides = polyVertices.length

    // Create sides-1 new pairs of half-edges, but skip edges that would duplicate existing boundary edges
    for (i <- 1 until sides)
      val p1 = polyVertices(i)
      val p2 = polyVertices((i + 1) % sides)

      // Check if this edge already exists as a boundary edge
      val existingBoundaryEdge = getBoundaryEdges.find { edge =>
        edge.origin == p1 && edge.twin.exists(_.origin == p2)
      }

      existingBoundaryEdge match
        case Some(existing) =>
          // Reuse the existing boundary edge for the inner face
          existing.incidentFace = Some(newFace)
          newInnerEdges.addOne(existing)
        // The twin becomes part of a merged boundary segment

        case None =>
          // Create new edge pair as before
          val inner = HalfEdge(p1, incidentFace = Some(newFace))
          val outer = HalfEdge(p2, incidentFace = Some(outerFace))
          inner.twin = Some(outer)
          outer.twin = Some(inner)
          newInnerEdges.addOne(inner)
          newOuterEdges.addOne(outer)

    (newInnerEdges.toList, newOuterEdges.toList)

  /**
   * Enhanced stitching that handles boundary merging when vertices are shared.
   */
  private def stitchPolygonEdgesWithReuse(
    baseEdge: HalfEdge,
    newInnerEdges: List[HalfEdge],
    newOuterEdges: List[HalfEdge],
    polyVertices: List[Vertex],
    newFace: Face,
    vertexMatches: Map[Int, Vertex]
  ): Unit =
    // Identify boundary segments that need to be merged
    val boundaryEdges = getBoundaryEdges
    val sharedVertices = vertexMatches.values.toSet

    // Find boundary segments between shared vertices
    val segmentsToMerge = findBoundarySegmentsBetweenSharedVertices(boundaryEdges, sharedVertices)

    // Get the edges that will be connected to the new outer boundary
    val oldPrev = baseEdge.prev.get
    val oldNext = baseEdge.next.get
    baseEdge.incidentFace = Some(newFace)

    // Link the inner loop for the new face
    val allInnerEdges = baseEdge +: newInnerEdges
    val sides = allInnerEdges.length
    for (i <- 0 until sides)
      val current = allInnerEdges(i)
      val next = allInnerEdges((i + 1) % sides)
      current.next = Some(next)
      next.prev = Some(current)
      if i >= 1 && !vertexMatches.contains(i - 1) then // Only update leaving edge for new vertices
        polyVertices(i).leaving = Some(current)
    newFace.outerComponent = Some(baseEdge)

    // Handle boundary reconstruction with merged segments
    if segmentsToMerge.nonEmpty then
      mergeBoundarySegments(segmentsToMerge, newOuterEdges, oldPrev, oldNext)
    else
      // Standard boundary linking
      val outerChain = newOuterEdges.reverse
      oldPrev.next = Some(outerChain.head)
      outerChain.head.prev = Some(oldPrev)
      outerChain.last.next = Some(oldNext)
      oldNext.prev = Some(outerChain.last)

      for (i <- 0 until outerChain.size - 1)
        outerChain(i).next = Some(outerChain(i + 1))
        outerChain(i + 1).prev = Some(outerChain(i))

  /**
   * Finds boundary segments that lie between shared vertices and should be merged.
   */
  private def findBoundarySegmentsBetweenSharedVertices(
    boundaryEdges: List[HalfEdge],
    sharedVertices: Set[Vertex]
  ): List[List[HalfEdge]] =
    val segments = mutable.ListBuffer.empty[List[HalfEdge]]
    var currentSegment = mutable.ListBuffer.empty[HalfEdge]
    var inSegment = false

    for (edge <- boundaryEdges)
      if sharedVertices.contains(edge.origin) then
        if inSegment && currentSegment.nonEmpty then
          segments.addOne(currentSegment.toList)
          currentSegment.clear()
        inSegment = !inSegment
      else if inSegment then
        currentSegment.addOne(edge)

    // Handle wrap-around case
    if inSegment && currentSegment.nonEmpty then
      segments.addOne(currentSegment.toList)

    segments.toList

  /**
   * Merges boundary segments when vertices are shared between polygons.
   */
  private def mergeBoundarySegments(
    segmentsToMerge: List[List[HalfEdge]],
    newOuterEdges: List[HalfEdge],
    oldPrev: HalfEdge,
    oldNext: HalfEdge
  ): Unit =
    // This is a complex operation that needs to:
    // 1. Remove the segments that are now "inside" the merged polygon
    // 2. Connect the remaining boundary properly
    // 3. Update the outer face component if needed

    val remainingBoundaryEdges = mutable.ListBuffer.empty[HalfEdge]
    val segmentEdges = segmentsToMerge.flatten.toSet

    // Keep edges that are not being merged
    getBoundaryEdges.foreach { edge =>
      if !segmentEdges.contains(edge) then
        remainingBoundaryEdges.addOne(edge)
    }

    // Insert new outer edges where appropriate
    val outerChain = newOuterEdges.reverse

    // Link everything together (simplified version - needs more sophisticated logic)
    if remainingBoundaryEdges.nonEmpty && outerChain.nonEmpty then
      // Find connection points and link appropriately
      oldPrev.next = Some(outerChain.head)
      outerChain.head.prev = Some(oldPrev)
      outerChain.last.next = Some(oldNext)
      oldNext.prev = Some(outerChain.last)

  /**
   * Enhanced polygon building with vertex reuse capability.
   */
  private def buildPolygonOnEdge(
                                  baseEdge: HalfEdge,
                                  twin: HalfEdge,
                                  sides: Int,
                                  allowVertexOnlyIntersection: Boolean = false
                                ): Either[String, TilingDCEL] =
    val v_start = baseEdge.origin
    val v_end = twin.origin // Destination of baseEdge

    // 1. Calculate new vertex positions
    val newVertexCoords = calculateNewVertices(v_start, v_end, sides)

    // 2. Enhanced intersection check with vertex matching
    val intersectionCheck = checkPolygonIntersection(baseEdge, v_start, v_end, newVertexCoords)

    if intersectionCheck.hasProperIntersection then
      Left(s"The new $sides-sided polygon would intersect with existing boundary edges.")
    else if intersectionCheck.hasVertexOnlyIntersection && !allowVertexOnlyIntersection then
      Left(s"The new $sides-sided polygon would intersect with existing vertices. Set allowVertexOnlyIntersection=true to allow this.")
    else
      // 3. Create vertices with reuse
      val newVertices = createNewVerticesWithReuse(newVertexCoords, intersectionCheck.vertexMatches)
      val actuallyNewVertices = newVertices.filterNot(intersectionCheck.vertexMatches.values.toSet.contains)

      val newFace = Face(s"F_Poly_${innerFaces.size}")
      val polyVertices = v_start :: v_end :: newVertices
      val (newInnerEdges, newOuterEdges) = createHalfEdgePairsWithReuse(polyVertices, newFace, intersectionCheck.vertexMatches)

      // 4. Enhanced stitching with vertex reuse
      stitchPolygonEdgesWithReuse(baseEdge, newInnerEdges, newOuterEdges, polyVertices, newFace, intersectionCheck.vertexMatches)

      Right(this.copy(
        vertices = this.vertices ++ actuallyNewVertices,
        halfEdges = this.halfEdges ++ newInnerEdges ++ newOuterEdges,
        innerFaces = this.innerFaces :+ newFace
      ))

  /**
   * Adds a new regular polygon to a specified boundary edge of the tiling.
   *
   * This method checks for self-intersections with the boundary.
   *
   * @param sides The number of sides of the regular polygon to add.
   * @param onEdgeStartingWithVertexId The ID of the vertex where the boundary edge starts.
   * @param allowVertexOnlyIntersection If true, allows the polygon to be added even if new vertices
   *                                   share coordinates with existing ones, as long as there are no
   *                                   proper edge crossings outside of vertices.
   * @return Either an error message or a new TilingDCEL with the polygon added.
   */
  def maybeAddRegularPolygon(
    sides: Int,
    onEdgeStartingWithVertexId: String,
    allowVertexOnlyIntersection: Boolean = false
  ): Either[String, TilingDCEL] =
    for
      _ <- TilingBuilder.validateSides(sides, "regular")
      baseEdge <- findBoundaryEdge(onEdgeStartingWithVertexId)
        .toRight(s"No boundary edge found starting with vertex ID $onEdgeStartingWithVertexId")
      twin <- baseEdge.twin.toRight("Boundary edge has no twin, which should not happen.")
      result <- buildPolygonOnEdge(baseEdge, twin, sides, allowVertexOnlyIntersection)
    yield result

  /**
   * Deletes an inner polygon (face) from the tiling.
   *
   * The method includes checks to ensure that the face is removable without compromising the tiling's integrity.
   * Specifically, it verifies that:
   * 1. The face exists.
   * 2. The face is adjacent to the outer boundary of the tiling.
   * 3. Removing the face will not partition the tiling into disconnected components.
   *
   * @param faceId The identifier of the face to be removed.
   * @return An `Either` containing a new `TilingDCEL` instance if the deletion is successful,
   *         or a `String` with an error message if the deletion is not possible.
   */
  def maybeDeletePolygon(faceId: String): Either[String, TilingDCEL] =
    for
      faceToDelete <- findInnerFace(faceId)
      edgeClassification <- classifyFaceEdges(faceToDelete)
      _ <- validateFaceDeletion(faceToDelete, edgeClassification)
    yield
      if innerFaces.length == 1 then TilingBuilder.empty
      else performFaceDeletion(faceToDelete, edgeClassification)

  private case class EdgeClassification(
                                         faceEdges: List[HalfEdge],
                                         boundaryTwins: List[HalfEdge],
                                         innerTwins: List[HalfEdge]
                                       )

  private def findInnerFace(faceId: String): Either[String, Face] =
    innerFaces.find(_.id == faceId)
      .toRight(s"Inner face with ID $faceId not found.")

  private def classifyFaceEdges(face: Face): Either[String, EdgeClassification] =
    val faceEdges = face.halfEdges
    val twinEdges = faceEdges.map(_.twin.get)
    val (boundaryTwins, innerTwins) = twinEdges.partition(_.incidentFace.contains(outerFace))
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
    val boundaryEdges = getBoundaryEdges
    val boundaryVertexAdjacency = Vertex.buildBoundaryVertexAdjacency(boundaryEdges, sharedVertices.toSet)

    // Check if all shared vertices are connected through the boundary path
    Vertex.checkConnectivity(sharedVertices.head, sharedVertices.toSet, boundaryVertexAdjacency)

  private def performFaceDeletion(faceToDelete: Face, classification: EdgeClassification): TilingDCEL =
    // Update edge incident faces
    classification.innerTwins.foreach(_.incidentFace = Some(outerFace))

    // Reconstruct boundary
    reconstructBoundary(classification)

    // Determine vertices and edges to keep
    val remainingElements = calculateRemainingElements(faceToDelete, classification)

    // Update vertex leaving edges
    updateVertexLeavingEdges(remainingElements.verticesToRemove, classification.faceEdges, classification.boundaryTwins)

    // Create new tiling
    TilingDCEL(
      vertices = remainingElements.vertices,
      halfEdges = remainingElements.halfEdges,
      innerFaces = remainingElements.innerFaces,
      outerFace = outerFace
    )

  private def reconstructBoundary(classification: EdgeClassification): Unit =
    outerFace.outerComponent.foreach { _ =>
      val boundaryEdges = getBoundaryEdges
      val twinIndices = classification.boundaryTwins.map(boundaryEdges.indexOf).sorted

      twinIndices.headOption.foreach { firstTwinIndex =>
        val lastTwinIndex = twinIndices.last
        val firstTwin = boundaryEdges(firstTwinIndex)
        val lastTwin = boundaryEdges(lastTwinIndex)
        val prevEdge = firstTwin.prev.get
        val nextEdge = lastTwin.next.get

        val newSegment = classification.innerTwins.sortBy(twin =>
          classification.faceEdges.indexOf(twin.twin.get))

        if newSegment.isEmpty then
          HalfEdge.linkEdges(prevEdge, nextEdge)
        else
          HalfEdge.insertBoundarySegment(prevEdge, nextEdge, newSegment)
          updateOuterFaceComponent(classification.boundaryTwins, newSegment, nextEdge)
      }
    }

  private def updateOuterFaceComponent(boundaryTwins: List[HalfEdge], newSegment: List[HalfEdge], nextEdge: HalfEdge): Unit =
    if boundaryTwins.contains(outerFace.outerComponent.get) then
      outerFace.outerComponent = Some(
        if newSegment.nonEmpty then newSegment.head else nextEdge
      )

  private case class RemainingElements(
    vertices: List[Vertex],
    halfEdges: List[HalfEdge],
    innerFaces: List[Face],
    verticesToRemove: List[Vertex]
  )

  private def calculateRemainingElements(faceToDelete: Face, classification: EdgeClassification): RemainingElements =
    val remainingInnerFaces = innerFaces.filterNot(_ == faceToDelete)
    val verticesUsedByRemainingFaces = remainingInnerFaces.flatMap(_.halfEdges.map(_.origin)).toSet
    val verticesOfInnerTwins = classification.innerTwins.flatMap(edge =>
      List(edge.origin, edge.twin.get.origin)).toSet

    val verticesToKeep = verticesUsedByRemainingFaces ++ verticesOfInnerTwins
    val verticesToRemove = vertices.filterNot(verticesToKeep.contains)
    val edgesToRemove = classification.faceEdges ++ classification.boundaryTwins

    val newHalfEdges = halfEdges
      .filterNot(edgesToRemove.contains)
      .filterNot(e => verticesToRemove.contains(e.origin) ||
        verticesToRemove.contains(e.twin.get.origin))

    RemainingElements(
      vertices = vertices.filterNot(verticesToRemove.contains),
      halfEdges = newHalfEdges,
      innerFaces = remainingInnerFaces,
      verticesToRemove = verticesToRemove
    )

  private def updateVertexLeavingEdges(
    verticesToRemove: List[Vertex],
    faceEdges: List[HalfEdge],
    boundaryTwins: List[HalfEdge]
  ): Unit =
    vertices.foreach { vertex =>
      if !verticesToRemove.contains(vertex) then
        val validLeavingEdge = halfEdges.find { edge =>
          edge.origin == vertex &&
            !faceEdges.contains(edge) &&
            !boundaryTwins.contains(edge) &&
            !verticesToRemove.contains(edge.twin.get.origin)
        }
        vertex.leaving = validLeavingEdge
    }

  /**
   * Generates an SVG representation of the tiling.
   *
   * @param width       The desired width of the SVG canvas.
   * @param height      The desired height of the SVG canvas.
   * @param strokeWidth The width of the edge lines.
   * @param padding     The padding around the tiling within the SVG viewBox.
   * @param scale       The factor by which to scale the tiling coordinates.
   * @return A String containing the SVG markup.
   */
  def toSVG(
    width: Int = 800,
    height: Int = 600,
    strokeWidth: Double = 1.0,
    padding: Double = 20.0,
    scale: Double = 50.0
  ): String =
    if vertices.isEmpty then return s"""<svg width="$width" height="$height"></svg>"""

    // Calculate the bounding box of the SCALED vertices to set the viewBox
    val minX = vertices.map(_.coords.x).min * scale
    val maxX = vertices.map(_.coords.x).max * scale
    val minY = vertices.map(_.coords.y).min * scale
    val maxY = vertices.map(_.coords.y).max * scale

    val viewBoxMinX = minX - padding
    val viewBoxMinY = minY - padding
    val viewBoxWidth = (maxX - minX) + 2 * padding
    val viewBoxHeight = (maxY - minY) + 2 * padding

    // Use a mutable set to ensure each edge is drawn only once
    val drawnEdges = mutable.Set.empty[HalfEdge]
    val edgeLines = halfEdges.map { edge =>
      if drawnEdges.contains(edge) || edge.twin.isEmpty then None
      else
        val twinEdge = edge.twin.get
        val p1 = edge.origin
        val p2 = twinEdge.origin
        drawnEdges ++= List(edge, twinEdge) // Mark both halves as drawn
        // Y-coordinates are negated to be flipped back by the group transform.
        Some(s"""      <line x1="${p1.coords.x * scale}" y1="${-p1.coords.y * scale}" x2="${p2.coords.x * scale}" y2="${-p2.coords.y * scale}" />""")
    }.filter(_.isDefined).map(_.get).mkString("\n")

    val vertexCircles = vertices.map { v =>
      s"""      <circle cx="${v.coords.x * scale}" cy="${-v.coords.y * scale}" r="${strokeWidth * 2}" />"""
    }.mkString("\n")

    val vertexLabels = vertices.map { v =>
      s"""      <text x="${v.coords.x * scale + strokeWidth * 2.5}" y="${-v.coords.y * scale - strokeWidth * 2.5}">${v.id}</text>"""
    }.mkString("\n")

    s"""<svg width="$width" height="$height" viewBox="$viewBoxMinX $viewBoxMinY $viewBoxWidth $viewBoxHeight" xmlns="http://www.w3.org/2000/svg">
       |  <g transform="scale(1, 1)">
       |    <!-- Edges -->
       |    <g stroke="black" stroke-width="$strokeWidth">
       |$edgeLines
       |    </g>
       |    <!-- Vertices -->
       |    <g fill="red">
       |$vertexCircles
       |    </g>
       |    <!-- Vertex Labels -->
       |    <g font-size="${(strokeWidth * 8).toInt}" fill="darkblue">
       |$vertexLabels
       |    </g>
       |  </g>
       |</svg>
       |""".stripMargin