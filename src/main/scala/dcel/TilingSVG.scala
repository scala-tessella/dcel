package io.github.scala_tessella
package dcel

import spire.implicits.*

import scala.collection.mutable

object TilingSVG:

  /**
   * Formats a decimal number to a maximum of 6 decimal places, removing trailing zeros.
   */
  private def formatCoordinate(value: BigDecimal): String =
    val formatted = f"${value.toDouble}%.6f"
    if formatted.contains('.') then
      formatted.replaceAll("0+$", "").replaceAll("\\.$", "")
    else
      formatted

  case class Point2D(x: BigDecimal, y: BigDecimal):
    def scaled(scale: Double): Point2D = Point2D(x * scale, y * scale)
    def flippedY: Point2D = Point2D(x, -y)
    def toSvgCoords(scale: Double): (String, String) =
      val scaledPoint = this.scaled(scale).flippedY
      (formatCoordinate(scaledPoint.x), formatCoordinate(scaledPoint.y))

  object Point2D:
    def from(vertex: Vertex): Point2D = Point2D(vertex.coords.x, vertex.coords.y)

  case class Arrow(tipX: String, tipY: String, baseX1: String, baseY1: String, baseX2: String, baseY2: String):
    def toSvgPolygon: String = s"""      <polygon points="$tipX,$tipY $baseX1,$baseY1 $baseX2,$baseY2" />"""

  private def createArrow(origin: Point2D, destination: Point2D, scale: Double, arrowSize: Double): Option[Arrow] =
    val midPoint = Point2D((origin.x + destination.x) / 2, (origin.y + destination.y) / 2)
    val direction = Point2D(destination.x - origin.x, destination.y - origin.y).scaled(scale)
    val length = spire.math.sqrt(direction.x * direction.x + direction.y * direction.y)

    if length > 0 then
      val unit = Point2D(direction.x / length, direction.y / length)
      val perp = Point2D(-unit.y, unit.x)

      val tip = Point2D(midPoint.x * scale + unit.x * arrowSize, -midPoint.y * scale + unit.y * arrowSize)
      val base1 = Point2D(midPoint.x * scale + perp.x * arrowSize * 0.4, -midPoint.y * scale + perp.y * arrowSize * 0.4)
      val base2 = Point2D(midPoint.x * scale - perp.x * arrowSize * 0.4, -midPoint.y * scale - perp.y * arrowSize * 0.4)

      Some(Arrow(
        formatCoordinate(tip.x), formatCoordinate(tip.y),
        formatCoordinate(base1.x), formatCoordinate(base1.y),
        formatCoordinate(base2.x), formatCoordinate(base2.y)
      ))
    else None

  private def calculateCentroid(vertices: List[Vertex]): Point2D =
    if vertices.nonEmpty then
      val sumX = vertices.map(_.coords.x).sum
      val sumY = vertices.map(_.coords.y).sum
      Point2D(sumX / vertices.length, sumY / vertices.length)
    else
      Point2D(BigDecimal(0), BigDecimal(0))

  private def calculateDirection(from: Point2D, to: Point2D, fallback: Point2D = Point2D(BigDecimal(0.1), BigDecimal(0.1))): Point2D =
    val direction = Point2D(to.x - from.x, to.y - from.y)
    val length = spire.math.sqrt(direction.x * direction.x + direction.y * direction.y)

    if length > 0 then
      Point2D(direction.x / length, direction.y / length)
    else
      fallback

  private def createAngleBisectorDirection(halfEdge: HalfEdge, inward: Boolean = true): Point2D =
    val prevEdge = halfEdge.prev.get
    val nextEdge = halfEdge.next.get
    val origin = Point2D.from(halfEdge.origin)

    val prevDest = Point2D.from(prevEdge.origin)
    val nextDest = Point2D.from(nextEdge.twin.get.origin)

    val toPrev = calculateDirection(origin, prevDest)
    val toNext = calculateDirection(origin, nextDest)

    val bisector = Point2D(toPrev.x + toNext.x, toPrev.y + toNext.y)
    val direction = calculateDirection(Point2D(BigDecimal(0), BigDecimal(0)), bisector)

    if inward then direction else Point2D(-direction.x, -direction.y)

  private def createAngleLabel(halfEdge: HalfEdge, direction: Point2D, scale: Double, strokeWidth: Double, color: String): String =
    val origin = Point2D.from(halfEdge.origin)
    val angleText = f"${halfEdge.angle.toRational.toDouble}%.0f°"
    val labelDistance = strokeWidth * 8

    val labelX = formatCoordinate(origin.x * scale + direction.x * labelDistance)
    val labelY = formatCoordinate(-origin.y * scale - direction.y * labelDistance)

    s"""      <text x="$labelX" y="$labelY" font-size="${(strokeWidth * 5).toInt}" fill="$color" text-anchor="middle" dominant-baseline="middle">$angleText</text>"""

  private def createSvgSection(title: String, content: String, attributes: String = ""): String =
    if content.nonEmpty then
      s"""    <!-- $title -->
         |    <g$attributes>
         |$content
         |    </g>""".stripMargin
    else ""

  extension (tilingDCEL: TilingDCEL)

    /**
     * Generates an SVG representation of the tiling.
     * The width, height, and viewBox are automatically calculated to fit the tiling at the given scale.
     */
    def toScalableVectorGraphics(
      strokeWidth: Double = 1.0,
      padding: Double = 20.0,
      scale: Double = 50.0
    ): String =
      if tilingDCEL.vertices.isEmpty then return """<svg width="0" height="0"></svg>"""
  
      // Calculate bounding box
      val vertices = tilingDCEL.vertices.map(Point2D.from)
      val scaledVertices = vertices.map(_.scaled(scale).flippedY)
  
      val minX = scaledVertices.map(_.x).min
      val maxX = scaledVertices.map(_.x).max
      val minY = scaledVertices.map(_.y).min
      val maxY = scaledVertices.map(_.y).max
  
      val viewBox = (minX - padding, minY - padding, (maxX - minX) + 2 * padding, (maxY - minY) + 2 * padding)
      val (width, height) = (viewBox._3.toInt, viewBox._4.toInt)
  
      // Generate edge lines
      val drawnEdges = mutable.Set.empty[HalfEdge]
      val edgeLines = tilingDCEL.halfEdges.flatMap { edge =>
        if drawnEdges.contains(edge) || edge.twin.isEmpty then None
        else
          val twin = edge.twin.get
          drawnEdges ++= List(edge, twin)
  
          val (x1, y1) = Point2D.from(edge.origin).toSvgCoords(scale)
          val (x2, y2) = Point2D.from(twin.origin).toSvgCoords(scale)
  
          Some(s"""      <line x1="$x1" y1="$y1" x2="$x2" y2="$y2" />""")
      }.mkString("\n")
  
      // Generate arrows for half-edges
      def createHalfEdgeArrows(halfEdges: List[HalfEdge]): String =
        halfEdges.flatMap { halfEdge =>
          val origin = Point2D.from(halfEdge.origin)
          val destination = Point2D.from(halfEdge.twin.get.origin)
          createArrow(origin, destination, scale, strokeWidth * 3).map(_.toSvgPolygon)
        }.mkString("\n")
  
      val innerFaceArrows = createHalfEdgeArrows(tilingDCEL.innerFaces.flatMap(_.halfEdges))
      val outerFaceArrows = createHalfEdgeArrows(tilingDCEL.getBoundaryEdges)
  
      // Generate angle labels
      val innerAngleLabels = tilingDCEL.innerFaces.flatMap { face =>
        val centroid = calculateCentroid(face.getVertices)
        face.halfEdges.map { halfEdge =>
          val origin = Point2D.from(halfEdge.origin)
          val direction = calculateDirection(origin, centroid, createAngleBisectorDirection(halfEdge, inward = true))
          createAngleLabel(halfEdge, direction, scale, strokeWidth, "purple")
        }
      }.mkString("\n")
  
      val outerAngleLabels = tilingDCEL.getBoundaryEdges.map { halfEdge =>
        val centroid = if tilingDCEL.innerFaces.nonEmpty then
          calculateCentroid(tilingDCEL.innerFaces.head.getVertices)
        else Point2D(BigDecimal(0), BigDecimal(0))
  
        val origin = Point2D.from(halfEdge.origin)
        val inwardDirection = calculateDirection(origin, centroid, createAngleBisectorDirection(halfEdge, inward = true))
        val outwardDirection = Point2D(-inwardDirection.x, -inwardDirection.y)
  
        createAngleLabel(halfEdge, outwardDirection, scale, strokeWidth, "orange")
      }.mkString("\n")
  
      // Generate boundary polygon and arrows
      val (boundaryPolygon, boundaryArrows) = tilingDCEL.boundary match
        case vertices if vertices.nonEmpty =>
          val points = vertices.map { v =>
            val (x, y) = Point2D.from(v).toSvgCoords(scale)
            s"$x,$y"
          }.mkString(" ")
  
          val arrows = vertices.zipWithIndex.flatMap { case (v1, i) =>
            val v2 = vertices((i + 1) % vertices.length)
            createArrow(Point2D.from(v1), Point2D.from(v2), scale, strokeWidth * 4).map(_.toSvgPolygon)
          }.mkString("\n")
  
          (Some(s"""      <polygon points="$points" />"""), arrows)
        case _ => (None, "")
  
      // Generate vertex elements
      val vertexCircles = tilingDCEL.vertices.map { v =>
        val (cx, cy) = Point2D.from(v).toSvgCoords(scale)
        s"""      <circle cx="$cx" cy="$cy" r="${strokeWidth * 2}" />"""
      }.mkString("\n")
  
      val vertexLabels = tilingDCEL.vertices.map { v =>
        val point = Point2D.from(v).scaled(scale).flippedY
        val x = formatCoordinate(point.x + strokeWidth * 2.5)
        val y = formatCoordinate(point.y - strokeWidth * 2.5)
        s"""      <text x="$x" y="$y">${v.id}</text>"""
      }.mkString("\n")
  
      val faceLabels = tilingDCEL.innerFaces.flatMap { face =>
        val vertices = face.getVertices
        if vertices.nonEmpty then
          val (x, y) = calculateCentroid(vertices).toSvgCoords(scale)
          Some(s"""      <text x="$x" y="$y" text-anchor="middle" dominant-baseline="middle">${face.id}</text>""")
        else None
      }.mkString("\n")
  
      // Build sections
      val boundarySection = boundaryPolygon.fold("") { polygon =>
        createSvgSection("Boundary Highlight", polygon, s""" stroke="red" stroke-width="${strokeWidth * 3}" fill="none"""") +
          "\n" + createSvgSection("Boundary Direction Arrows", boundaryArrows, s""" fill="red" stroke="red" stroke-width="${strokeWidth * 0.5}"""")
      }
  
      val sections = List(
        createSvgSection("Edges", edgeLines, s""" stroke="black" stroke-width="$strokeWidth""""),
        boundarySection,
        createSvgSection("Inner Face Half-Edge Direction Arrows", innerFaceArrows, s""" fill="blue" stroke="blue" stroke-width="${strokeWidth * 0.5}""""),
        createSvgSection("Outer Face Half-Edge Direction Arrows", outerFaceArrows, s""" fill="black" stroke="black" stroke-width="${strokeWidth * 0.5}""""),
        createSvgSection("Vertices", vertexCircles, """ fill="red""""),
        createSvgSection("Vertex Labels", vertexLabels, s""" font-size="${(strokeWidth * 8).toInt}" fill="darkblue""""),
        createSvgSection("Face Labels", faceLabels, s""" font-size="${(strokeWidth * 6).toInt}" fill="green""""),
        createSvgSection("Angle Labels", innerAngleLabels + "\n" + outerAngleLabels)
      ).filter(_.nonEmpty).mkString("\n")
  
      val formattedViewBox = s"${formatCoordinate(viewBox._1)} ${formatCoordinate(viewBox._2)} ${formatCoordinate(viewBox._3)} ${formatCoordinate(viewBox._4)}"
  
      s"""<svg width="$width" height="$height" viewBox="$formattedViewBox" xmlns="http://www.w3.org/2000/svg">
         |  <g>
         |$sections
         |  </g>
         |</svg>""".stripMargin