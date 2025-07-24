package io.github.scala_tessella
package dcel

import scala.collection.mutable

object TilingSVG:

  extension (tilingDCEL: TilingDCEL)

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
    def toScalableVectorGraphics(
       width: Int = 800,
       height: Int = 600,
       strokeWidth: Double = 1.0,
       padding: Double = 20.0,
       scale: Double = 50.0
     ): String =
      if tilingDCEL.vertices.isEmpty then return s"""<svg width="$width" height="$height"></svg>"""

      // Calculate the bounding box of the SCALED vertices to set the viewBox
      val minX = tilingDCEL.vertices.map(_.coords.x).min * scale
      val maxX = tilingDCEL.vertices.map(_.coords.x).max * scale
      val minY = tilingDCEL.vertices.map(_.coords.y).min * scale
      val maxY = tilingDCEL.vertices.map(_.coords.y).max * scale

      val viewBoxMinX = minX - padding
      val viewBoxMinY = minY - padding
      val viewBoxWidth = (maxX - minX) + 2 * padding
      val viewBoxHeight = (maxY - minY) + 2 * padding

      // Use a mutable set to ensure each edge is drawn only once
      val drawnEdges = mutable.Set.empty[HalfEdge]
      val edgeLines = tilingDCEL.halfEdges.map { edge =>
        if drawnEdges.contains(edge) || edge.twin.isEmpty then None
        else
          val twinEdge = edge.twin.get
          val p1 = edge.origin
          val p2 = twinEdge.origin
          drawnEdges ++= List(edge, twinEdge) // Mark both halves as drawn
          // Y-coordinates are negated to be flipped back by the group transform.
          Some(s"""      <line x1="${p1.coords.x * scale}" y1="${-p1.coords.y * scale}" x2="${p2.coords.x * scale}" y2="${-p2.coords.y * scale}" />""")
      }.filter(_.isDefined).map(_.get).mkString("\n")

      val vertexCircles = tilingDCEL.vertices.map { v =>
        s"""      <circle cx="${v.coords.x * scale}" cy="${-v.coords.y * scale}" r="${strokeWidth * 2}" />"""
      }.mkString("\n")

      val vertexLabels = tilingDCEL.vertices.map { v =>
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
         |</svg>""".stripMargin
