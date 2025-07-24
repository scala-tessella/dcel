package io.github.scala_tessella
package dcel

import spire.implicits.*

import scala.collection.mutable

object TilingSVG:

  extension (tilingDCEL: TilingDCEL)

    /**
     * Generates an SVG representation of the tiling.
     * The width, height, and viewBox are automatically calculated to fit the tiling at the given scale.
     *
     * @param strokeWidth The width of the edge lines.
     * @param padding     The padding around the tiling within the SVG viewBox.
     * @param scale       The factor by which to scale the tiling coordinates.
     * @return A String containing the SVG markup.
     */
    def toScalableVectorGraphics(
                                  strokeWidth: Double = 1.0,
                                  padding: Double = 20.0,
                                  scale: Double = 50.0
                                ): String =
      if tilingDCEL.vertices.isEmpty then return """<svg width="0" height="0"></svg>"""

      // Calculate the bounding box of the DRAWN coordinates (including Y-flip)
      val drawnMinX = tilingDCEL.vertices.map(_.coords.x).min * scale
      val drawnMaxX = tilingDCEL.vertices.map(_.coords.x).max * scale
      val drawnMinY = tilingDCEL.vertices.map(v => -v.coords.y).min * scale  // Note the negation
      val drawnMaxY = tilingDCEL.vertices.map(v => -v.coords.y).max * scale  // Note the negation

      // Calculate viewBox based on actual drawn coordinates
      val viewBoxMinX = drawnMinX - padding
      val viewBoxMinY = drawnMinY - padding
      val viewBoxWidth = (drawnMaxX - drawnMinX) + 2 * padding
      val viewBoxHeight = (drawnMaxY - drawnMinY) + 2 * padding

      // Calculate the SVG canvas dimensions to match the viewBox
      val width = viewBoxWidth.toInt
      val height = viewBoxHeight.toInt

      // Use a mutable set to ensure each edge is drawn only once
      val drawnEdges = mutable.Set.empty[HalfEdge]
      val edgeLines = tilingDCEL.halfEdges.map { edge =>
        if drawnEdges.contains(edge) || edge.twin.isEmpty then None
        else
          val twinEdge = edge.twin.get
          val p1 = edge.origin
          val p2 = twinEdge.origin
          drawnEdges ++= List(edge, twinEdge) // Mark both halves as drawn
          // Y-coordinates are negated for proper SVG orientation
          Some(s"""      <line x1="${p1.coords.x * scale}" y1="${-p1.coords.y * scale}" x2="${p2.coords.x * scale}" y2="${-p2.coords.y * scale}" />""")
      }.filter(_.isDefined).map(_.get).mkString("\n")

      // Create inner face half-edge visualizations with direction arrows and angle labels
      val innerFaceHalfEdges = tilingDCEL.innerFaces.flatMap { face =>
        val halfEdges = face.halfEdges
        halfEdges.map { halfEdge =>
          val origin = halfEdge.origin
          val destination = halfEdge.twin.get.origin

          // Calculate midpoint for arrow and angle label
          val midX = (origin.coords.x + destination.coords.x) * scale / 2
          val midY = -(origin.coords.y + destination.coords.y) * scale / 2

          // Calculate direction vector for arrow
          val dx = (destination.coords.x - origin.coords.x) * scale
          val dy = -(destination.coords.y - origin.coords.y) * scale
          val length = spire.math.sqrt(dx * dx + dy * dy)
          val arrowSize = strokeWidth * 3

          val arrow = if length > 0 then
            val unitX = dx / length
            val unitY = dy / length

            // Arrow tip (slightly offset from midpoint towards destination)
            val tipX = midX + unitX * arrowSize
            val tipY = midY + unitY * arrowSize

            // Arrow base (perpendicular to direction)
            val perpX = -unitY * arrowSize * 0.4
            val perpY = unitX * arrowSize * 0.4
            val baseX1 = midX + perpX
            val baseY1 = midY + perpY
            val baseX2 = midX - perpX
            val baseY2 = midY - perpY

            Some(s"""      <polygon points="$tipX,$tipY $baseX1,$baseY1 $baseX2,$baseY2" />""")
          else None

          // Create angle label at the origin vertex
          val angleText = f"${halfEdge.angle.toRational.toDouble}%.0f°"
          val labelOffsetX = strokeWidth * 3
          val labelOffsetY = strokeWidth * 3
          val angleLabelX = origin.coords.x * scale + labelOffsetX
          val angleLabelY = -origin.coords.y * scale - labelOffsetY

          val angleLabel = s"""      <text x="$angleLabelX" y="$angleLabelY" font-size="${(strokeWidth * 5).toInt}" fill="purple">$angleText</text>"""

          (arrow, angleLabel)
        }
      }

      val innerFaceArrows = innerFaceHalfEdges.flatMap(_._1).mkString("\n")
      val angleLabels = innerFaceHalfEdges.map(_._2).mkString("\n")

      // Create boundary polygon with direction arrows
      val (boundaryPolygon, boundaryArrows) = tilingDCEL.boundary match
        case vertices if vertices.nonEmpty =>
          val points = vertices.map { v =>
            s"${v.coords.x * scale},${-v.coords.y * scale}"
          }.mkString(" ")

          // Create arrows at the midpoint of each boundary edge
          val arrows = vertices.zipWithIndex.map { case (v1, i) =>
            val v2 = vertices((i + 1) % vertices.length)
            val midX = (v1.coords.x + v2.coords.x) * scale / 2
            val midY = -(v1.coords.y + v2.coords.y) * scale / 2

            // Calculate direction vector and normalize it
            val dx = (v2.coords.x - v1.coords.x) * scale
            val dy = -(v2.coords.y - v1.coords.y) * scale
            val length = spire.math.sqrt(dx * dx + dy * dy)
            val arrowSize = strokeWidth * 4

            if length > 0 then
              val unitX = dx / length
              val unitY = dy / length

              // Arrow tip
              val tipX = midX + unitX * arrowSize
              val tipY = midY + unitY * arrowSize

              // Arrow base (perpendicular to direction)
              val perpX = -unitY * arrowSize * 0.5
              val perpY = unitX * arrowSize * 0.5
              val baseX1 = midX + perpX
              val baseY1 = midY + perpY
              val baseX2 = midX - perpX
              val baseY2 = midY - perpY

              Some(s"""      <polygon points="$tipX,$tipY $baseX1,$baseY1 $baseX2,$baseY2" />""")
            else None
          }.filter(_.isDefined).map(_.get).mkString("\n")

          (Some(s"""      <polygon points="$points" />"""), arrows)
        case _ => (None, "")

      val vertexCircles = tilingDCEL.vertices.map { v =>
        s"""      <circle cx="${v.coords.x * scale}" cy="${-v.coords.y * scale}" r="${strokeWidth * 2}" />"""
      }.mkString("\n")

      val vertexLabels = tilingDCEL.vertices.map { v =>
        s"""      <text x="${v.coords.x * scale + strokeWidth * 2.5}" y="${-v.coords.y * scale - strokeWidth * 2.5}">${v.id}</text>"""
      }.mkString("\n")

      // Calculate face labels at the centroid of each inner face
      val faceLabels = tilingDCEL.innerFaces.map { face =>
        val faceVertices = face.getVertices
        if faceVertices.nonEmpty then
          val centroidX = faceVertices.map(_.coords.x).sum / faceVertices.length
          val centroidY = faceVertices.map(_.coords.y).sum / faceVertices.length
          Some(s"""      <text x="${centroidX * scale}" y="${-centroidY * scale}" text-anchor="middle" dominant-baseline="middle">${face.id}</text>""")
        else
          None
      }.filter(_.isDefined).map(_.get).mkString("\n")

      val boundarySection = boundaryPolygon match
        case Some(polygon) =>
          s"""    <!-- Boundary Highlight -->
             |    <g stroke="red" stroke-width="${strokeWidth * 3}" fill="none">
             |$polygon
             |    </g>
             |    <!-- Boundary Direction Arrows -->
             |    <g fill="red" stroke="red" stroke-width="${strokeWidth * 0.5}">
             |$boundaryArrows
             |    </g>""".stripMargin
        case None => ""

      val innerFaceSection = if innerFaceArrows.nonEmpty then
        s"""    <!-- Inner Face Half-Edge Direction Arrows -->
           |    <g fill="blue" stroke="blue" stroke-width="${strokeWidth * 0.5}">
           |$innerFaceArrows
           |    </g>""".stripMargin
      else ""

      val angleSection = if angleLabels.nonEmpty then
        s"""    <!-- Angle Labels -->
           |    <g>
           |$angleLabels
           |    </g>""".stripMargin
      else ""

      s"""<svg width="$width" height="$height" viewBox="$viewBoxMinX $viewBoxMinY $viewBoxWidth $viewBoxHeight" xmlns="http://www.w3.org/2000/svg">
         |  <g>
         |    <!-- Edges -->
         |    <g stroke="black" stroke-width="$strokeWidth">
         |$edgeLines
         |    </g>
         |$boundarySection
         |$innerFaceSection
         |    <!-- Vertices -->
         |    <g fill="red">
         |$vertexCircles
         |    </g>
         |    <!-- Vertex Labels -->
         |    <g font-size="${(strokeWidth * 8).toInt}" fill="darkblue">
         |$vertexLabels
         |    </g>
         |    <!-- Face Labels -->
         |    <g font-size="${(strokeWidth * 6).toInt}" fill="green">
         |$faceLabels
         |    </g>
         |$angleSection
         |  </g>
         |</svg>""".stripMargin