package io.github.scala_tessella
package dcel

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

      val vertexCircles = tilingDCEL.vertices.map { v =>
        s"""      <circle cx="${v.coords.x * scale}" cy="${-v.coords.y * scale}" r="${strokeWidth * 2}" />"""
      }.mkString("\n")

      val vertexLabels = tilingDCEL.vertices.map { v =>
        s"""      <text x="${v.coords.x * scale + strokeWidth * 2.5}" y="${-v.coords.y * scale - strokeWidth * 2.5}">${v.id}</text>"""
      }.mkString("\n")

      s"""<svg width="$width" height="$height" viewBox="$viewBoxMinX $viewBoxMinY $viewBoxWidth $viewBoxHeight" xmlns="http://www.w3.org/2000/svg">
         |  <g>
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
         |</svg>""".stripMargin