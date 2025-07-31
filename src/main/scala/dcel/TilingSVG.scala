package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{BigLineSegment, BigPoint, BigRadian, format}

import scala.collection.mutable

object TilingSVG:

  extension (bigPoint: BigPoint)

    def toSvgCoords(scale: Double): (String, String) =
      val scaledPoint: BigPoint = bigPoint.scaled(scale).flippedY
      (scaledPoint.x.format, scaledPoint.y.format)

  case class Arrow(tipX: String, tipY: String, baseX1: String, baseY1: String, baseX2: String, baseY2: String):
    def toSvgPolygon: String = s"""      <polygon points="$tipX,$tipY $baseX1,$baseY1 $baseX2,$baseY2" />"""

  private def createArrow(origin: BigPoint, destination: BigPoint, scale: Double, arrowSize: Double): Option[Arrow] =
    val distance = origin.distanceTo(destination)
    if distance > BigDecimal(BigDecimalGeometry.ACCURACY) then
      val segment = BigLineSegment(origin, destination)
      val midPoint = segment.midPoint
      val directionAngle = origin.angleTo(destination)
      val unitDirection = BigPoint.fromPolar(BigDecimal(1.0), directionAngle)
      val perpAngle = directionAngle + BigRadian.TAU_4
      val unitPerpendicular = BigPoint.fromPolar(BigDecimal(1.0), perpAngle)
      val scaledMidPoint = midPoint.scaled(scale).flippedY
      val arrowSizeBD = BigDecimal(arrowSize)
      val tip = scaledMidPoint.plus(BigPoint(unitDirection.x * arrowSizeBD, -unitDirection.y * arrowSizeBD))
      val baseOffset = arrowSizeBD * BigDecimal(0.4)
      val base1 = scaledMidPoint.plus(BigPoint(unitPerpendicular.x * baseOffset, -unitPerpendicular.y * baseOffset))
      val base2 = scaledMidPoint.plus(BigPoint(unitPerpendicular.x * (-baseOffset), -unitPerpendicular.y * (-baseOffset)))
      Some(Arrow(tip.x.format, tip.y.format, base1.x.format, base1.y.format, base2.x.format, base2.y.format))
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
    val origin = halfEdge.origin.coords
    val prevDest = prevEdge.origin.coords
    val nextDest = nextEdge.twin.get.origin.coords
    val toPrev = calculateDirection(origin, prevDest)
    val toNext = calculateDirection(origin, nextDest)
    val bisector = BigPoint(toPrev.x + toNext.x, toPrev.y + toNext.y)
    val direction = calculateDirection(BigPoint(BigDecimal(0), BigDecimal(0)), bisector)
    if inward then
      direction
    else
      BigPoint(-direction.x, -direction.y)

  private def createAngleLabel(halfEdge: HalfEdge, direction: BigPoint, scale: Double, strokeWidth: Double): String =
    val origin = halfEdge.origin.coords
    val angleText = f"${halfEdge.angle.get.toRational.toDouble}%.0f°"
    val labelDistance = strokeWidth * 8
    val labelX = (origin.x * scale + direction.x * labelDistance).format
    val labelY = (-origin.y * scale - direction.y * labelDistance).format
    s"""      <text x="$labelX" y="$labelY">$angleText</text>"""

  private def createSvgSection(title: String, content: String, attributes: String = ""): String =
    if content.nonEmpty then
      s"""    <!-- $title -->
         |    <g$attributes>
         |$content
         |    </g>""".stripMargin
    else
      ""

  extension (tilingDCEL: TilingDCEL)

    /**
     * Generates an SVG representation of the tiling.
     * The width, height, and viewBox are automatically calculated to fit the tiling at the given scale.
     */
    def toScalableVectorGraphics(
      strokeWidth: Double = 1.0,
      padding: Double = 20.0,
      scale: Double = 50.0,
      showHalfEdgeTraversal: Boolean = false
    ): String =
      if tilingDCEL.vertices.isEmpty then
        return """<svg width="0" height="0"></svg>"""
      val vertices = tilingDCEL.vertices.map(_.coords)
      val scaledVertices = vertices.map(_.scaled(scale).flippedY)
      val minX = scaledVertices.map(_.x).min
      val maxX = scaledVertices.map(_.x).max
      val minY = scaledVertices.map(_.y).min
      val maxY = scaledVertices.map(_.y).max
      val viewBox = (minX - padding, minY - padding, (maxX - minX) + 2 * padding, (maxY - minY) + 2 * padding)
      val (width, height) = (viewBox._3.toInt, viewBox._4.toInt)

      // Generate edge lines
      val drawnEdges = mutable.Set.empty[HalfEdge]
      val edgeLines =
        tilingDCEL.halfEdges.flatMap {
          edge =>
            if drawnEdges.contains(edge) || edge.twin.isEmpty then
              None
            else
              val twin = edge.twin.get
              drawnEdges ++= List(edge, twin)
              val (x1, y1) = edge.origin.coords.toSvgCoords(scale)
              val (x2, y2) = twin.origin.coords.toSvgCoords(scale)
              Some(s"""      <line x1="$x1" y1="$y1" x2="$x2" y2="$y2" />""")
        }.mkString("\n")

      // Generate arrows for half-edges
      def createHalfEdgeArrows(halfEdges: List[HalfEdge]): String =
        halfEdges.flatMap {
          halfEdge =>
            val origin = halfEdge.origin.coords
            val destination = halfEdge.twin.get.origin.coords
            createArrow(origin, destination, scale, strokeWidth * 3).map(_.toSvgPolygon)
        }.mkString("\n")

      val innerFaceArrows = createHalfEdgeArrows(tilingDCEL.innerFaces.flatMap(_.halfEdgesSafe))
      val outerFaceArrows = createHalfEdgeArrows(tilingDCEL.getBoundaryEdges.toOption.get)

      // Generate angle labels
      val innerAngleLabels =
        tilingDCEL.innerFaces.flatMap {
          face =>
            val centroid = calculateCentroid(face.getVertices.getOrElse(List.empty))
            face.halfEdgesSafe.map {
              halfEdge =>
                val origin = halfEdge.origin.coords
                val direction = calculateDirection(origin, centroid)
                createAngleLabel(halfEdge, direction, scale, strokeWidth)
            }
        }.mkString("\n")
      val outerAngleLabels =
        tilingDCEL.getBoundaryEdges.toOption.get.map {
          halfEdge =>
            val centroid =
              if tilingDCEL.innerFaces.nonEmpty then
                calculateCentroid(tilingDCEL.innerFaces.head.getVertices.getOrElse(List.empty))
              else
                BigPoint(BigDecimal(0), BigDecimal(0))
            val origin = halfEdge.origin.coords
            val inwardDirection = calculateDirection(origin, centroid)
            val outwardDirection = BigPoint(-inwardDirection.x, -inwardDirection.y)
            createAngleLabel(halfEdge, outwardDirection, scale, strokeWidth)
        }.mkString("\n")

      // Generate boundary polygon and arrows
      val (boundaryPolygon, boundaryArrows) =
        tilingDCEL.boundary match
          case vertices if vertices.nonEmpty =>
            val points = vertices.map {
              v =>
                val (x, y) = v.coords.toSvgCoords(scale)
                s"$x,$y"
            }.mkString(" ")
            val arrows = vertices.zipWithIndex.flatMap {
              case (v1, i) =>
                val v2 = vertices((i + 1) % vertices.length)
                createArrow(v1.coords, v2.coords, scale, strokeWidth * 6).map(_.toSvgPolygon)
            }.mkString("\n")
            (Some(s"""      <polygon points="$points" />"""), arrows)
          case _ =>
            (None, "")

      // Generate vertex elements
      val vertexCircles =
        tilingDCEL.vertices.map {
          v =>
            val (cx, cy) = v.coords.toSvgCoords(scale)
            s"""      <circle cx="$cx" cy="$cy" r="${strokeWidth * 2}" />"""
        }.mkString("\n")
      val vertexLabels =
        tilingDCEL.vertices.map {
          v =>
            val point = v.coords.scaled(scale).flippedY
            val x = (point.x + strokeWidth * 2.5).format
            val y = (point.y - strokeWidth * 2.5).format
            s"""      <text x="$x" y="$y">${v.id}</text>"""
        }.mkString("\n")
        
      // Generate face elements  
      val faceLabels =
        tilingDCEL.innerFaces.flatMap {
          face =>
            val vertices = face.getVertices.getOrElse(List.empty)
            if (vertices.nonEmpty) {
              val (x, y) = calculateCentroid(vertices).toSvgCoords(scale)
              Some(s"""      <text x="$x" y="$y">${face.id}</text>""")
            } else {
              None
            }
        }.mkString("\n")

      val traversalArrows =
        if showHalfEdgeTraversal then
          tilingDCEL.innerFaces.flatMap {
            face =>
              val halfEdges = face.halfEdgesSafe
              if halfEdges.length > 1 then
                val looped = halfEdges :+ halfEdges.head
                looped.sliding(2).flatMap {
                  case he1 :: he2 :: Nil =>
                    for
                      dest1 <- he1.destination
                      dest2 <- he2.destination
                      mid1 = BigLineSegment(he1.origin.coords, dest1.coords).midPoint
                      mid2 = BigLineSegment(he2.origin.coords, dest2.coords).midPoint
                      arrow <- createArrow(mid1, mid2, scale, strokeWidth * 2.5)
                    yield arrow.toSvgPolygon
                  case _ =>
                    None
                }
              else
                Nil
          }.mkString("\n")
        else
          ""

      // Build sections
      val boundarySection = boundaryPolygon.fold("") {
        polygon =>
          createSvgSection("Boundary Highlight", polygon, s""" stroke="red" stroke-width="${strokeWidth * 3}" fill="none"""")
            + "\n"
            + createSvgSection("Boundary Direction Arrows", boundaryArrows, s""" fill="red" stroke="red" stroke-width="${strokeWidth * 0.5}"""")
      }
      val traversalSection =
        createSvgSection("Half-Edge Face Traversal", traversalArrows, s""" fill="darkcyan" stroke="darkcyan" stroke-width="${strokeWidth * 0.5}"""")
      val sections =
        List(createSvgSection("Edges", edgeLines, s""" stroke="black" stroke-width="$strokeWidth""""), boundarySection, createSvgSection("Inner Face Half-Edge Direction Arrows", innerFaceArrows, s""" fill="blue" stroke="blue" stroke-width="${strokeWidth * 0.5}""""), createSvgSection("Outer Face Half-Edge Direction Arrows", outerFaceArrows, s""" fill="black" stroke="black" stroke-width="${strokeWidth * 0.5}""""), traversalSection, createSvgSection("Vertices", vertexCircles, """ fill="red""""), createSvgSection("Vertex Labels", vertexLabels, s""" font-size="${(strokeWidth * 8).toInt}" fill="darkblue""""), createSvgSection("Face Labels", faceLabels, s""" font-size="${(strokeWidth * 6).toInt}" fill="green" text-anchor="middle" dominant-baseline="middle""""), createSvgSection("Inner Angle Labels", innerAngleLabels, s""" font-size="${(strokeWidth * 5).toInt}" fill="purple" text-anchor="middle" dominant-baseline="middle""""), createSvgSection("Outer Angle Labels", outerAngleLabels, s""" font-size="${(strokeWidth * 5).toInt}" fill="orange" text-anchor="middle" dominant-baseline="middle"""")).filter(_.nonEmpty).mkString("\n")
      val formattedViewBox =
        s"${viewBox._1.format} ${viewBox._2.format} ${viewBox._3.format} ${viewBox._4.format}"
      s"""<svg width="$width" height="$height" viewBox="$formattedViewBox" xmlns="http://www.w3.org/2000/svg">
         |  <g>
         |$sections
         |  </g>
         |</svg>""".stripMargin
