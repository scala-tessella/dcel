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

  private def polygonWouldIntersectBoundary(
    baseEdge: HalfEdge,
    v_start: Vertex,
    v_end: Vertex,
    newVertexCoords: List[BigPoint]
  ): Boolean =
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

    newEdgeSegments.exists { newSeg =>
      nearbyBoundarySegments.exists(boundarySeg => newSeg.intersects(boundarySeg))
    }

  /**
   * Creates new vertices for a regular polygon based on calculated coordinates.
   */
  private def createNewVertices(newVertexCoords: List[BigPoint]): List[Vertex] =
    val nextVertexId = getNextVertexId
    newVertexCoords.zipWithIndex.map { case (bigPoint, i) =>
      Vertex(s"V${nextVertexId + i}", bigPoint)
    }

  /**
   * Gets the next available vertex ID number.
   */
  private def getNextVertexId: Int =
    vertices
      .map(_.id)
      .collect { case id if id.startsWith("V") && id.drop(1).forall(_.isDigit) => id.drop(1).toInt }
      .maxOption
      .getOrElse(-1) + 1

  /**
   * Creates pairs of half-edges for the new polygon edges.
   */
  private def createHalfEdgePairs(polyVertices: List[Vertex], newFace: Face): (List[HalfEdge], List[HalfEdge]) =
    val newInnerEdges = mutable.ListBuffer.empty[HalfEdge]
    val newOuterEdges = mutable.ListBuffer.empty[HalfEdge]
    val sides = polyVertices.length

    // Create sides-1 new pairs of half-edges
    for (i <- 1 until sides)
      val p1 = polyVertices(i)
      val p2 = polyVertices((i + 1) % sides)
      val inner = HalfEdge(p1, incidentFace = Some(newFace))
      val outer = HalfEdge(p2, incidentFace = Some(outerFace))
      inner.twin = Some(outer)
      outer.twin = Some(inner)
      newInnerEdges.addOne(inner)
      newOuterEdges.addOne(outer)

    (newInnerEdges.toList, newOuterEdges.toList)

  /**
   * Links the edges of the new polygon into the DCEL structure.
   */
  private def stitchPolygonEdges(
    baseEdge: HalfEdge,
    newInnerEdges: List[HalfEdge],
    newOuterEdges: List[HalfEdge],
    polyVertices: List[Vertex],
    newFace: Face
  ): Unit =
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
      if i >= 1 then
        polyVertices(i).leaving = Some(current)
    newFace.outerComponent = Some(baseEdge)

    // Link the new outer boundary edges
    val outerChain = newOuterEdges.reverse
    oldPrev.next = Some(outerChain.head)
    outerChain.head.prev = Some(oldPrev)
    outerChain.last.next = Some(oldNext)
    oldNext.prev = Some(outerChain.last)

    for (i <- 0 until outerChain.size - 1)
      outerChain(i).next = Some(outerChain(i + 1))
      outerChain(i + 1).prev = Some(outerChain(i))

  /**
   * Builds and integrates a new polygon on the given base edge.
   */
  private def buildPolygonOnEdge(baseEdge: HalfEdge, twin: HalfEdge, sides: Int): Either[String, TilingDCEL] =
    val v_start = baseEdge.origin
    val v_end = twin.origin // Destination of baseEdge

    // 1. Calculate new vertex positions
    val newVertexCoords = calculateNewVertices(v_start, v_end, sides)

    // 2. Check for boundary intersections
    if polygonWouldIntersectBoundary(baseEdge, v_start, v_end, newVertexCoords) then
      Left(s"The new $sides-sided polygon would intersect with existing boundary edges.")
    else
      // 3. Create the new face and half-edges
      val newVertices = createNewVertices(newVertexCoords)
      val newFace = Face(s"F_Poly_${innerFaces.size}")
      val polyVertices = v_start :: v_end :: newVertices
      val (newInnerEdges, newOuterEdges) = createHalfEdgePairs(polyVertices, newFace)

      // 4. Stitch the new elements into the DCEL graph
      stitchPolygonEdges(baseEdge, newInnerEdges, newOuterEdges, polyVertices, newFace)

      Right(this.copy(
        vertices = this.vertices ++ newVertices,
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
   * @return Either an error message or a new TilingDCEL with the polygon added.
   */
  def maybeAddRegularPolygon(sides: Int, onEdgeStartingWithVertexId: String): Either[String, TilingDCEL] =
    for
      _ <- TilingBuilder.validateSides(sides, "regular")
      baseEdge <- findBoundaryEdge(onEdgeStartingWithVertexId)
        .toRight(s"No boundary edge found starting with vertex ID $onEdgeStartingWithVertexId")
      twin <- baseEdge.twin.toRight("Boundary edge has no twin, which should not happen.")
      result <- buildPolygonOnEdge(baseEdge, twin, sides)
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