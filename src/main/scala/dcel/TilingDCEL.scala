package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{BigBox, BigLineSegment, BigPoint}
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
      @scala.annotation.tailrec
      def loop(current: HalfEdge, acc: List[HalfEdge]): List[HalfEdge] =
        val next = current.next.getOrElse(throw new IllegalStateException("Boundary loop is not closed."))
        // Prepend the current edge and continue with the next, until we are back at the start.
        if next eq start then (current :: acc).reverse
        else loop(next, current :: acc)
      // Start the traversal from the beginning.
      loop(start, Nil)
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
    val interiorAngle = RegularPolygon(sides).alphaDegree.toRational.toDouble
    val turnAngle = 180.0 - interiorAngle

    val dx = v_end.coords.x - v_start.coords.x
    val dy = v_end.coords.y - v_start.coords.y
    var heading = spire.math.atan2(dy, dx)
    var currentPoint = v_end.coords
    val newPoints = List.newBuilder[BigPoint]

    // We need to add (sides - 2) new vertices.
    for (_ <- 1 until sides - 1)
      heading += spire.math.toRadians(turnAngle)
      val nextPoint = BigPoint(currentPoint.x + spire.math.cos(heading), currentPoint.y + spire.math.sin(heading))
      newPoints += nextPoint
      currentPoint = nextPoint
    newPoints.result()

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
      result <- findBoundaryEdge(onEdgeStartingWithVertexId) match
        case None =>
          Left(s"No boundary edge found starting with vertex ID $onEdgeStartingWithVertexId")
        case Some(baseEdge) =>
          val v_start = baseEdge.origin
          val v_end = baseEdge.twin.get.origin // Destination of baseEdge

          // 1. Calculate new vertex positions
          val newVertexCoords = calculateNewVertices(v_start, v_end, sides)

          // 2. Check for boundary intersections before modifying the DCEL
          val newEdgesPoints = v_end.coords +: newVertexCoords :+ v_start.coords
          val newEdgeSegments = newEdgesPoints.sliding(2).collect { case Seq(p1, p2) => BigLineSegment(p1, p2) }.toList

          // Create a bounding box for the new polygon and expand it by 1 unit.
          val newPolygonBBox = BigBox.fromPoints(newEdgesPoints)
          val searchBBox = newPolygonBBox.expand(BigDecimal(1.0))

          // Filter boundary edges to check only those within the search area.
          val boundaryEdgesToCheck = getBoundaryEdges.filterNot { edge =>
            edge == baseEdge || edge == baseEdge.next.get || edge == baseEdge.prev.get
          }
          val nearbyBoundarySegments = boundaryEdgesToCheck.collect {
            case edge if {
              val segment = BigLineSegment(edge.origin.coords, edge.twin.get.origin.coords)
              val edgeBBox = BigBox.fromSegment(segment)
              searchBBox.intersects(edgeBBox)
            } => BigLineSegment(edge.origin.coords, edge.twin.get.origin.coords)
          }

          val intersects = newEdgeSegments.exists { newSeg =>
            nearbyBoundarySegments.exists(boundarySeg => BigLineSegment.doIntersect(newSeg, boundarySeg))
          }

          if intersects then
            Left("The new polygon would cross a boundary edge.")
          else
            // 3. Create the new face and half-edges
            val maxVertexNum = this.vertices.map(_.id.filter(_.isDigit).toInt).maxOption.getOrElse(-1)
            val newVertices = newVertexCoords.zipWithIndex.map { case (bigPoint, i) =>
              Vertex(s"V${maxVertexNum + 1 + i}", bigPoint)
            }

            val newFace = Face(s"F_Poly_${innerFaces.size}")
            val polyVertices = List(v_start, v_end) ++ newVertices
            val newInnerEdges = mutable.ListBuffer.empty[HalfEdge]
            val newOuterEdges = mutable.ListBuffer.empty[HalfEdge]

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

            // 4. Stitch the new elements into the DCEL graph
            val oldPrev = baseEdge.prev.get
            val oldNext = baseEdge.next.get
            baseEdge.incidentFace = Some(newFace)

            // Link the inner loop for the new face
            val allInnerEdges = baseEdge +: newInnerEdges.toList
            for (i <- 0 until sides)
              val current = allInnerEdges(i)
              val next = allInnerEdges((i + 1) % sides)
              current.next = Some(next)
              next.prev = Some(current)
              if i >= 1 then
                polyVertices(i).leaving = Some(current)
            newFace.outerComponent = Some(baseEdge)

            // Link the new outer boundary edges
            val outerChain = newOuterEdges.reverse.toList
            oldPrev.next = Some(outerChain.head)
            outerChain.head.prev = Some(oldPrev)
            outerChain.last.next = Some(oldNext)
            oldNext.prev = Some(outerChain.last)

            for (i <- 0 until outerChain.size - 1)
              outerChain(i).next = Some(outerChain(i + 1))
              outerChain(i + 1).prev = Some(outerChain(i))

            Right(this.copy(
              vertices = this.vertices ++ newVertices,
              halfEdges = this.halfEdges ++ newInnerEdges ++ newOuterEdges,
              innerFaces = this.innerFaces :+ newFace
            ))
    yield
      result

  /**
   * Deletes a polygon from the tiling
   *
   * @param faceId The identifier of the face to be removed.
   * @return An `Either` containing the modified `TilingDCEL` on success, or an error message on failure.
   */
  def maybeDeletePolygon(faceId: String): Either[String, TilingDCEL] =
    // Find the face to delete
    innerFaces.find(_.id == faceId) match
      case None =>
        Left(s"Face with id '$faceId' not found")
      case Some(faceToDelete) =>
        // Special case: if this is the only polygon, return an empty tiling
        if innerFaces.length == 1 then
          Right(TilingBuilder.empty)
        else
          // For now, we only handle the case of deleting the single polygon
          // More complex deletions would require careful handling of shared edges
          Left("Deletion of polygons from multi-polygon tilings is not yet implemented")

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