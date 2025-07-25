package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{BigLineSegment, BigPoint, BigRadian}

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

  extension (bigPoint: BigPoint)

    def toSvgCoords(scale: Double): (String, String) =
      val scaledPoint: BigPoint = bigPoint.scaled(scale).flippedY
      (formatCoordinate(scaledPoint.x), formatCoordinate(scaledPoint.y))

  def toBigPointFromVertex(vertex: Vertex): BigPoint = BigPoint(vertex.coords.x, vertex.coords.y)

  case class Arrow(tipX: String, tipY: String, baseX1: String, baseY1: String, baseX2: String, baseY2: String):
    def toSvgPolygon: String = s"""      <polygon points="$tipX,$tipY $baseX1,$baseY1 $baseX2,$baseY2" />"""

  private def createArrow(origin: BigPoint, destination: BigPoint, scale: Double, arrowSize: Double): Option[Arrow] =
    val distance = origin.distanceTo(destination)

    if distance > BigDecimal(BigDecimalGeometry.ACCURACY) then
      // Use the existing midPoint method from BigLineSegment
      val segment = BigLineSegment(origin, destination)
      val midPoint = segment.midPoint

      // Get the direction angle using existing methods
      val directionAngle = origin.angleTo(destination)

      // Create unit vector in the direction using fromPolar
      val unitDirection = BigPoint.fromPolar(BigDecimal(1.0), directionAngle)

      // Create perpendicular vector (90 degrees counterclockwise)
      val perpAngle = directionAngle + BigRadian.TAU_4
      val unitPerpendicular = BigPoint.fromPolar(BigDecimal(1.0), perpAngle)

      // Calculate arrow points using the scaled midpoint and unit vectors
      val scaledMidPoint = midPoint.scaled(scale).flippedY
      val arrowSizeBD = BigDecimal(arrowSize)

      val tip = scaledMidPoint.plus(
        BigPoint(unitDirection.x * arrowSizeBD, unitDirection.y * arrowSizeBD)
      )

      val baseOffset = arrowSizeBD * BigDecimal(0.4)
      val base1 = scaledMidPoint.plus(
        BigPoint(unitPerpendicular.x * baseOffset, unitPerpendicular.y * baseOffset)
      )

      val base2 = scaledMidPoint.plus(
        BigPoint(unitPerpendicular.x * (-baseOffset), unitPerpendicular.y * (-baseOffset))
      )

      Some(Arrow(
        formatCoordinate(tip.x), formatCoordinate(tip.y),
        formatCoordinate(base1.x), formatCoordinate(base1.y),
        formatCoordinate(base2.x), formatCoordinate(base2.y)
      ))
    else
      None

  private def calculateCentroid(vertices: List[Vertex]): BigPoint =
    if vertices.nonEmpty then
      val sumX = vertices.map(_.coords.x).sum
      val sumY = vertices.map(_.coords.y).sum
      BigPoint(sumX / vertices.length, sumY / vertices.length)
    else
      BigPoint(BigDecimal(0), BigDecimal(0))

  private def calculateDirection(from: BigPoint, to: BigPoint): BigPoint =
    val angle = from.angleTo(to)
    BigPoint.fromPolar(BigDecimal(1.0), angle)

  private def createAngleBisectorDirection(halfEdge: HalfEdge, inward: Boolean = true): BigPoint =
    val prevEdge = halfEdge.prev.get
    val nextEdge = halfEdge.next.get
    val origin = toBigPointFromVertex(halfEdge.origin)

    val prevDest = toBigPointFromVertex(prevEdge.origin)
    val nextDest = toBigPointFromVertex(nextEdge.twin.get.origin)

    val toPrev = calculateDirection(origin, prevDest)
    val toNext = calculateDirection(origin, nextDest)

    val bisector = BigPoint(toPrev.x + toNext.x, toPrev.y + toNext.y)
    val direction = calculateDirection(BigPoint(BigDecimal(0), BigDecimal(0)), bisector)

    if inward then direction else BigPoint(-direction.x, -direction.y)

  private def createAngleLabel(halfEdge: HalfEdge, direction: BigPoint, scale: Double, strokeWidth: Double, color: String): String =
    val origin = toBigPointFromVertex(halfEdge.origin)
    val angleText = f"${halfEdge.angle.get.toRational.toDouble}%.0f°"
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
      val vertices = tilingDCEL.vertices.map(toBigPointFromVertex)
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

          val (x1, y1) = toBigPointFromVertex(edge.origin).toSvgCoords(scale)
          val (x2, y2) = toBigPointFromVertex(twin.origin).toSvgCoords(scale)

          Some(s"""      <line x1="$x1" y1="$y1" x2="$x2" y2="$y2" />""")
      }.mkString("\n")

      // Generate arrows for half-edges
      def createHalfEdgeArrows(halfEdges: List[HalfEdge]): String =
        halfEdges.flatMap { halfEdge =>
          val origin = toBigPointFromVertex(halfEdge.origin)
          val destination = toBigPointFromVertex(halfEdge.twin.get.origin)
          createArrow(origin, destination, scale, strokeWidth * 3).map(_.toSvgPolygon)
        }.mkString("\n")

      val innerFaceArrows = createHalfEdgeArrows(tilingDCEL.innerFaces.flatMap(_.halfEdgesSafe))
      val outerFaceArrows = createHalfEdgeArrows(tilingDCEL.getBoundaryEdges.toOption.get)

      // Generate angle labels
      val innerAngleLabels = tilingDCEL.innerFaces.flatMap { face =>
        val centroid = calculateCentroid(face.getVertices.getOrElse(List.empty))
        face.halfEdgesSafe.map { halfEdge =>
          val origin = toBigPointFromVertex(halfEdge.origin)
          val direction = calculateDirection(origin, centroid)
          createAngleLabel(halfEdge, direction, scale, strokeWidth, "purple")
        }
      }.mkString("\n")

      val outerAngleLabels = tilingDCEL.getBoundaryEdges.toOption.get.map { halfEdge =>
        val centroid = if tilingDCEL.innerFaces.nonEmpty then
          calculateCentroid(tilingDCEL.innerFaces.head.getVertices.getOrElse(List.empty))
        else BigPoint(BigDecimal(0), BigDecimal(0))

        val origin = toBigPointFromVertex(halfEdge.origin)
        val inwardDirection = calculateDirection(origin, centroid)
        val outwardDirection = BigPoint(-inwardDirection.x, -inwardDirection.y)

        createAngleLabel(halfEdge, outwardDirection, scale, strokeWidth, "orange")
      }.mkString("\n")

      // Generate boundary polygon and arrows
      val (boundaryPolygon, boundaryArrows) = tilingDCEL.boundary match
        case vertices if vertices.nonEmpty =>
          val points = vertices.map { v =>
            val (x, y) = toBigPointFromVertex(v).toSvgCoords(scale)
            s"$x,$y"
          }.mkString(" ")

          val arrows = vertices.zipWithIndex.flatMap { case (v1, i) =>
            val v2 = vertices((i + 1) % vertices.length)
            createArrow(toBigPointFromVertex(v1), toBigPointFromVertex(v2), scale, strokeWidth * 4).map(_.toSvgPolygon)
          }.mkString("\n")

          (Some(s"""      <polygon points="$points" />"""), arrows)
        case _ => (None, "")

      // Generate vertex elements
      val vertexCircles = tilingDCEL.vertices.map { v =>
        val (cx, cy) = toBigPointFromVertex(v).toSvgCoords(scale)
        s"""      <circle cx="$cx" cy="$cy" r="${strokeWidth * 2}" />"""
      }.mkString("\n")

      val vertexLabels = tilingDCEL.vertices.map { v =>
        val point = toBigPointFromVertex(v).scaled(scale).flippedY
        val x = formatCoordinate(point.x + strokeWidth * 2.5)
        val y = formatCoordinate(point.y - strokeWidth * 2.5)
        s"""      <text x="$x" y="$y">${v.id}</text>"""
      }.mkString("\n")

      val faceLabels = tilingDCEL.innerFaces.flatMap { face =>
        val vertices = face.getVertices.getOrElse(List.empty)
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