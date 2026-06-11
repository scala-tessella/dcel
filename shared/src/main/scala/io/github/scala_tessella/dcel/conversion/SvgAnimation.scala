package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.TilingDCEL
import io.github.scala_tessella.dcel.TilingUniformity.scanUniformityTree
import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.*
import io.github.scala_tessella.dcel.structure.VertexId
import io.github.scala_tessella.dcel.conversion.SvgDsl.*
import io.github.scala_tessella.dcel.conversion.SvgRendering.*
import io.github.scala_tessella.dcel.conversion.TilingSVG.{toScalableVectorGraphics, SvgOptions}

/** Animated SVG export for [[TilingDCEL]]: [[toUniformityAnimation]] walks through the refinement steps of
  * the uniformity tree as an SMIL-animated drawing. Static tiling SVG lives in [[TilingSVG]].
  */
object SvgAnimation:

  extension (tiling: TilingDCEL)

    /** Renders the tiling as an animated SVG that walks through the steps of [[TilingDCEL.uniformityTree]]
      * (via `scanUniformityTree`). Each step highlights the next layer of vertex-class refinement, pausing
      * between steps so the viewer can follow the grouping.
      *
      * @param vertexRadius
      *   Radius of the highlight circle drawn around each vertex being grouped.
      * @param animationDuration
      *   Seconds spent on each step's transition.
      * @param pauseBetweenSteps
      *   Seconds of dwell between steps.
      */
    def toUniformityAnimation(
        strokeWidth: Double = 1.0,
        padding: Double = 20.0,
        scale: Double = 50.0,
        vertexRadius: Double = 10.0,
        animationDuration: Double = 2.0,
        pauseBetweenSteps: Double = 0.5
    ): String =
      val trees = tiling.scanUniformityTree
      if trees.isEmpty then
        return tiling.toScalableVectorGraphics(SvgOptions(strokeWidth, padding, scale))

      val totalSteps    = trees.length
      val stepDuration  = animationDuration / totalSteps
      val cycleDuration = animationDuration + pauseBetweenSteps

      def getKeyTimes: String =
        val stepTimes   = (0 until totalSteps).map: i =>
          f"${i * stepDuration / cycleDuration}%.4f"
        val endAnimTime = f"${animationDuration / cycleDuration}%.4f"
        (stepTimes :+ endAnimTime :+ "1").mkString(";")

      // Build per-step vertex->colorIndex map
      val vertexToColorAtStep: Map[Int, Map[VertexId, Int]] =
        trees.zipWithIndex
          .map: (tree, stepIndex) =>
            val leaves = tree.flattenLeaves
            val m      =
              leaves.zipWithIndex
                .flatMap: (vertexIds, colorIndex) =>
                  vertexIds.map: vid =>
                    vid -> colorIndex
                .toMap
            stepIndex -> m
          .toMap

      // ViewBox
      val vertices        = tiling.vertices.map(_.coords)
      val viewBox         = calculateViewBox(vertices, scale, padding)
      val (width, height) = viewBox.dimensions

      val sb = new StringBuilder()
      sb.append(
        s"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="${viewBox.formatted}" width="$width" height="$height">"""
      )
      sb.append("\n")

      // Styles
      sb.append("  <style>\n")
      val maxColors = trees.map(_.sizeLeaves).max
      for colorIndex <- 0 until maxColors do
        val color = uniformColorMap.getOrElse(colorIndex, "gray")
        sb.append(s"    .uniformity-$colorIndex { fill: $color; }\n")
      sb.append("  </style>\n\n")

      // Edges
      sb.append("  <!-- Edges -->\n")
      sb.append(s"""  <g stroke="black" stroke-width="$strokeWidth">""")
      sb.append("\n")
      val edgeLines = createEdgeLines(tiling, scale)
      edgeLines.foreach: elem =>
        sb.append("    ").append(elem.toString).append("\n")
      sb.append("  </g>\n\n")

      // Animated vertices
      sb.append("  <!-- Animated Uniformity Vertices -->\n")
      sb.append(s"""  <g id="vertices-uniformity-animated" stroke="none">""")
      sb.append("\n")

      for vertex <- tiling.innerVertices do
        val vid         = vertex.id
        val (x, y)      = vertex.coords.toSvgCoords(scale)
        val colorSeqIdx = (0 until totalSteps)
          .map: i =>
            vertexToColorAtStep.get(i)
              .flatMap:
                _.get(vid)
              .getOrElse(0)

        val colorSeq =
          colorSeqIdx
            .map: ci =>
              uniformColorMap.getOrElse(ci, "gray")

        // Determine visibility at each step: visible if vertex is in the tree at that distance
        val visibilitySeq = (0 until totalSteps).map: i =>
          val verticesAtStep = trees(i).flattenLeaves.flatten.toSet
          if verticesAtStep.contains(vid) then "visible" else "hidden"

        sb.append(
          s"""    <circle cx="$x" cy="$y" r="$vertexRadius" fill="${colorSeq.head}" visibility="${visibilitySeq.head}">"""
        ).append("\n"): Unit

        val keyTimes = getKeyTimes

        // Color animation
        val values = (colorSeq :+ colorSeq.last :+ colorSeq.head).mkString(";")
        sb.append(
          s"""      <animate attributeName="fill" values="$values" keyTimes="$keyTimes" dur="${cycleDuration}s" repeatCount="indefinite" calcMode="discrete"/>"""
        ).append("\n"): Unit

        // Visibility animation
        val visValues = (visibilitySeq :+ visibilitySeq.last :+ visibilitySeq.head).mkString(";")
        sb.append(
          s"""      <animate attributeName="visibility" values="$visValues" keyTimes="$keyTimes" dur="${cycleDuration}s" repeatCount="indefinite" calcMode="discrete"/>"""
        ).append("\n"): Unit

        sb.append("    </circle>\n")

      sb.append("  </g>\n")
      // Vertex labels (reusing logic from createVertexElements)
      sb.append("\n  <!-- Vertex Labels -->\n")
      sb.append(s"""  <g font-size="${(strokeWidth * 8).toInt}" fill="darkblue">""")
      sb.append("\n")

      for vertex <- tiling.innerVertices do
        val point  = vertex.coords
        val offset = strokeWidth * 2.5
        val (x, y) = point.toSvgLabelCoords(scale, offset, -offset)
        sb.append(s"""    <text x="$x" y="$y">${vertex.id.value}</text>""").append("\n")

      sb.append("  </g>\n")

      // Distance label - single text with animated content
      sb.append("\n  <!-- Distance Label -->\n")
      val labelX = viewBox.minX + BigDecimal(10)
      val labelY = viewBox.minY + BigDecimal(20)

      // Multiple overlapping text elements, each visible during its step
      for i <- 0 until totalSteps do
        val keyTimes = getKeyTimes

        val visValues        = (0 until totalSteps)
          .map: j =>
            if j == i then "visible" else "hidden"
        :+ "hidden" :+ "hidden"
        val visibilityValues = visValues.mkString(";")

        sb.append(
          s"""  <text x="${
              labelX.format
            }" y="${
              labelY.format
            }" font-family="Arial" font-size="14" fill="black" visibility="hidden">"""
        ).append("\n"): Unit
        sb.append(s"""    Distance: $i""").append("\n"): Unit
        sb.append(
          s"""    <animate attributeName="visibility" values="$visibilityValues" keyTimes="$keyTimes" dur="${
              cycleDuration
            }s" repeatCount="indefinite" calcMode="discrete"/>"""
        ).append("\n"): Unit
        sb.append("  </text>\n")

      // Classes label
      sb.append("\n  <!-- Classes Label -->\n")
      val classesLabelY = viewBox.minY + BigDecimal(35)

      for i <- 0 until totalSteps do
        val keyTimes = getKeyTimes

        val visValues        = (0 until totalSteps).map: j =>
          if j == i then "visible" else "hidden"
        :+ "hidden" :+ "hidden"
        val visibilityValues = visValues.mkString(";")

        val numClasses = trees(i).sizeLeaves

        sb.append(
          s"""  <text x="${
              labelX.format
            }" y="${
              classesLabelY.format
            }" font-family="Arial" font-size="14" fill="black" visibility="hidden">"""
        ).append("\n"): Unit
        sb.append(s"""    Classes: $numClasses""").append("\n"): Unit
        sb.append(
          s"""    <animate attributeName="visibility" values="$visibilityValues" keyTimes="$keyTimes" dur="${
              cycleDuration
            }s" repeatCount="indefinite" calcMode="discrete"/>"""
        ).append("\n"): Unit
        sb.append("  </text>\n")

      sb.append("</svg>")
      sb.toString
