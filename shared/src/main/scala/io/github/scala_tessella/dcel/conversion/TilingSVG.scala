package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.*
import io.github.scala_tessella.dcel.geometry.{BigDecimalGeometry, BigLineSegment, BigPoint, BigRadian}
import io.github.scala_tessella.dcel.structure.{FaceId, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.dcel.{TilingDCEL, TilingError}
import io.github.scala_tessella.dcel.TilingUniformity.scanUniformityTreeAlt
import spire.implicits.*

import scala.collection.mutable
import scala.xml.*

object TilingSVG:

  extension (bigPoint: BigPoint)
    private def toSvgCoords(scale: Double): (String, String) =
      val scaledPoint: BigPoint = bigPoint.scaled(scale).flippedY
      (scaledPoint.x.format, scaledPoint.y.format)

  private case class Arrow(
      tipX: String,
      tipY: String,
      baseX1: String,
      baseY1: String,
      baseX2: String,
      baseY2: String
  ):
    val formatted: String = s"$tipX,$tipY $baseX1,$baseY1 $baseX2,$baseY2"

  private case class ViewBox(minX: BigDecimal, minY: BigDecimal, width: BigDecimal, height: BigDecimal):
    val formatted: String      = s"${minX.format} ${minY.format} ${width.format} ${height.format}"
    val dimensions: (Int, Int) = (width.toInt, height.toInt)

  // Public options for callers (ergonomic API)
  case class SvgOptions(
      strokeWidth: Double = 1.0,
      padding: Double = 20.0,
      scale: Double = 50.0,
      showHalfEdgeTraversal: Boolean = false,
      leavingEdgeMarkers: Boolean = false,
      faceIdsOnEdges: Boolean = false,
      showUniformity: Boolean = false
  )

  private case class SvgConfig(
      strokeWidth: Double,
      padding: Double,
      scale: Double,
      showHalfEdgeTraversal: Boolean,
      leavingEdgeMarkers: Boolean,
      faceIdsOnEdges: Boolean,
      showUniformity: Boolean
  )

  private def toConfig(opts: SvgOptions): SvgConfig =
    SvgConfig(
      strokeWidth = opts.strokeWidth,
      padding = opts.padding,
      scale = opts.scale,
      showHalfEdgeTraversal = opts.showHalfEdgeTraversal,
      leavingEdgeMarkers = opts.leavingEdgeMarkers,
      faceIdsOnEdges = opts.faceIdsOnEdges,
      showUniformity = opts.showUniformity
    )

  private def createArrow(
      origin: BigPoint,
      destination: BigPoint,
      scale: Double,
      arrowSize: Double
  ): Option[Arrow] =
    val distance = origin.distanceTo(destination)
    if distance <= BigDecimal(BigDecimalGeometry.ACCURACY) then None
    else
      val segment           = BigLineSegment(origin, destination)
      val midPoint          = segment.midPoint
      val directionAngle    = origin.angleTo(destination)
      val unitDirection     = BigPoint.fromPolar(BigDecimal(1.0), directionAngle)
      val perpAngle         = directionAngle + BigRadian.TAU_4
      val unitPerpendicular = BigPoint.fromPolar(BigDecimal(1.0), perpAngle)

      val scaledMidPoint = midPoint.scaled(scale).flippedY
      val arrowSizeBD    = BigDecimal(arrowSize)
      val baseOffset     = arrowSizeBD * BigDecimal(0.4)

      val tip   = scaledMidPoint.plus(BigPoint(unitDirection.x * arrowSizeBD, -unitDirection.y * arrowSizeBD))
      val base1 =
        scaledMidPoint.plus(BigPoint(unitPerpendicular.x * baseOffset, -unitPerpendicular.y * baseOffset))
      val base2 = scaledMidPoint.plus(BigPoint(
        unitPerpendicular.x * (-baseOffset),
        -unitPerpendicular.y * (-baseOffset)
      ))

      Some(Arrow(tip.x.format, tip.y.format, base1.x.format, base1.y.format, base2.x.format, base2.y.format))

  private def calculateCentroid(vertices: List[Vertex]): BigPoint =
    vertices.map(_.coords).centroid

  private def calculateDirection(from: BigPoint, to: BigPoint): BigPoint =
    BigPoint.fromPolar(BigDecimal(1.0), from.angleTo(to))

  // Helper to create MetaData more idiomatically
  private def attrs(tuples: (String, Any)*): MetaData =
    tuples.foldRight[MetaData](Null) { case ((key, value), acc) =>
      new UnprefixedAttribute(key, value.toString, acc)
    }

  // New: a small wrapper to avoid passing `null` at call sites.
  // prefix = None means “no namespace prefix”
  private def elem(
      label: String,
      attributes: MetaData = Null,
      scope: NamespaceBinding = TopScope,
      children: Seq[Node] = Seq.empty,
      prefix: Option[String] = None,
      minimizeEmpty: Boolean = true
  ): Elem =
    Elem(prefix.orNull, label, attributes, scope, minimizeEmpty, children*)

  // ---------- Elem helpers (null-free call sites) ----------

  private def textAt(x: String, y: String, content: String, more: MetaData = Null): Elem =
    val attributes = new UnprefixedAttribute("x", x, new UnprefixedAttribute("y", y, more))
    elem("text", attributes, children = Seq(Text(content)))

  private def polygonElem(points: String, more: MetaData = Null): Elem =
    elem("polygon", new UnprefixedAttribute("points", points, more))

  private def lineElem(x1: String, y1: String, x2: String, y2: String, more: MetaData = Null): Elem =
    val attributes =
      new UnprefixedAttribute(
        "x1",
        x1,
        new UnprefixedAttribute(
          "y1",
          y1,
          new UnprefixedAttribute("x2", x2, new UnprefixedAttribute("y2", y2, more))
        )
      )
    elem("line", attributes)

  private def circleElem(cx: String, cy: String, r: String, more: MetaData = Null): Elem =
    val attributes = new UnprefixedAttribute(
      "cx",
      cx,
      new UnprefixedAttribute("cy", cy, new UnprefixedAttribute("r", r, more))
    )
    elem("circle", attributes)

  private def gElem(children: Seq[Node], attributes: MetaData = Null): Elem =
    elem("g", attributes, children = children)

  private def svgElem(width: String, height: String, viewBox: String, children: Seq[Node]): Elem =
    val attributes =
      new UnprefixedAttribute(
        "width",
        width,
        new UnprefixedAttribute(
          "height",
          height,
          new UnprefixedAttribute(
            "viewBox",
            viewBox,
            new UnprefixedAttribute("xmlns", "http://www.w3.org/2000/svg", Null)
          )
        )
      )
    elem("svg", attributes, children = children)
  // ---------- end helpers ----------

  private def createAngleLabel(halfEdge: HalfEdge, direction: BigPoint, config: SvgConfig): Elem =
    val origin        = halfEdge.origin.coords
    val angleText     = f"${halfEdge.angle.get.toRational.toDouble}%.0f°"
    val labelDistance = config.strokeWidth * 8
    val labelX        = (origin.x * config.scale + direction.x * labelDistance).format
    val labelY        = (-origin.y * config.scale - direction.y * labelDistance).format
    textAt(labelX, labelY, angleText)

  private def createSvgSection(title: String, content: Seq[Node], attributes: MetaData = Null): NodeSeq =
    if content.isEmpty then NodeSeq.Empty
    else Seq(Comment(s" $title "), gElem(content, attributes))

  private def calculateViewBox(vertices: List[BigPoint], scale: Double, padding: Double): ViewBox =
    if vertices.isEmpty then ViewBox(BigDecimal(0), BigDecimal(0), BigDecimal(0), BigDecimal(0))
    else
      val scaledVertices           = vertices.map(_.scaled(scale).flippedY)
      val xs                       = scaledVertices.map(_.x)
      val ys                       = scaledVertices.map(_.y)
      val (minX, maxX, minY, maxY) = (xs.min, xs.max, ys.min, ys.max)
      ViewBox(minX - padding, minY - padding, (maxX - minX) + 2 * padding, (maxY - minY) + 2 * padding)

  private def createHalfEdgeArrows(halfEdges: List[HalfEdge], config: SvgConfig): Seq[Elem] =
    halfEdges.flatMap { halfEdge =>

      for
        destination <- halfEdge.destination
        arrow       <- createArrow(halfEdge.origin.coords, destination.coords, config.scale, config.strokeWidth * 3)
      yield polygonElem(arrow.formatted)
    }

  private def createEdgeLines(tilingDCEL: TilingDCEL, scale: Double): Seq[Elem] =
    val drawnEdges = mutable.Set.empty[HalfEdge]
    tilingDCEL.halfEdges.flatMap { edge =>

      if drawnEdges.contains(edge) || edge.twin.isEmpty then None
      else
        val twin     = edge.twin.get
        drawnEdges ++= List(edge, twin)
        val (x1, y1) = edge.origin.coords.toSvgCoords(scale)
        val (x2, y2) = twin.origin.coords.toSvgCoords(scale)
        Some(lineElem(x1, y1, x2, y2))
    }

  private def createAngleLabels(tilingDCEL: TilingDCEL, config: SvgConfig): (Seq[Elem], Seq[Elem]) =
    val innerAngleLabels = tilingDCEL.innerFaces.flatMap { face =>
      val centroid = calculateCentroid(face.getVerticesUnsafe)
      face.halfEdgesUnsafe.map { halfEdge =>
        val direction = calculateDirection(halfEdge.origin.coords, centroid)
        createAngleLabel(halfEdge, direction, config)
      }
    }

    val outerAngleLabels = tilingDCEL.boundaryEdges.map { halfEdge =>
      val centroid         = tilingDCEL.innerFaces.headOption
        .map(_.getVerticesUnsafe)
        .filter(_.nonEmpty)
        .map(calculateCentroid)
        .getOrElse(BigPoint(0, 0))
      val inwardDirection  = calculateDirection(halfEdge.origin.coords, centroid)
      val outwardDirection = BigPoint(-inwardDirection.x, -inwardDirection.y)
      createAngleLabel(halfEdge, outwardDirection, config)
    }

    (innerAngleLabels, outerAngleLabels)

  private def createBoundaryElements(tilingDCEL: TilingDCEL, config: SvgConfig): Option[Elem] =
    tilingDCEL.boundaryVertices match
      case vertices if vertices.nonEmpty =>
        val points = vertices.map { v =>
          val (x, y) = v.coords.toSvgCoords(config.scale)
          s"$x,$y"
        }.mkString(" ")
        Some(polygonElem(points))
      case _                             => None

  private def createVertexElements(tilingDCEL: TilingDCEL, config: SvgConfig): (Seq[Elem], Seq[Elem]) =

    def uniformColorMap: Map[Int, String] =
      Map(
        0  -> "yellow",
        1  -> "orange",
        2  -> "violet",
        3  -> "green",
        4  -> "brown",
        5  -> "pink",
        6  -> "deeppink",
        7  -> "darkkhaki",
        8  -> "blueviolet",
        9  -> "lime",
        10 -> "lightgreen",
        11 -> "lightblue",
        12 -> "lightcoral",
        13 -> "lightseagreen",
        14 -> "lightskyblue",
        15 -> "lightsalmon",
        16 -> "yellowgreen",
        17 -> "lightgoldenrodyellow",
        18 -> "lightgray",
        19 -> "slategray",
        20 -> "crimson",
        21 -> "tomato",
        22 -> "goldenrod",
        23 -> "darkorange",
        24 -> "olive",
        25 -> "seagreen",
        26 -> "teal",
        27 -> "steelblue",
        28 -> "royalblue",
        29 -> "navy",
        30 -> "indigo",
        31 -> "mediumvioletred",
        32 -> "sienna",
        33 -> "chocolate",
        34 -> "peru",
        35 -> "darkturquoise",
        36 -> "cadetblue",
        37 -> "mediumseagreen",
        38 -> "cornflowerblue",
        39 -> "darkmagenta",
        40 -> "firebrick",
        41 -> "darkgoldenrod",
        42 -> "forestgreen",
        43 -> "mediumaquamarine",
        44 -> "darkcyan",
        45 -> "dodgerblue",
        46 -> "slateblue",
        47 -> "orchid",
        48 -> "darkslategray",
        49 -> "maroon"
      )

    val circles =
      if config.showUniformity then
        val classes  = tilingDCEL.uniformityTree.flattenLeaves
        val indexMap = classes.zipWithIndex.flatMap((vertexIds, index) => vertexIds.map((_, index))).toMap
        tilingDCEL.vertices.map { v =>
          val index      = indexMap.get(v.id)
          val multiplier = if index.isDefined then 20 else 2
          val (cx, cy)   = v.coords.toSvgCoords(config.scale)
          circleElem(
            cx,
            cy,
            (config.strokeWidth * multiplier).toString,
            attrs("fill" -> index.map(uniformColorMap).getOrElse("red"))
          )
        }
      else
        tilingDCEL.vertices.map { v =>
          val (cx, cy) = v.coords.toSvgCoords(config.scale)
          circleElem(cx, cy, (config.strokeWidth * 2).toString)
        }

    val labels = tilingDCEL.vertices.map { v =>
      val point = v.coords.scaled(config.scale).flippedY
      val x     = (point.x + config.strokeWidth * 2.5).format
      val y     = (point.y - config.strokeWidth * 2.5).format
      textAt(x, y, v.id.value)
    }

    (circles, labels)

  private def createFaceLabels(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[Elem] =
    tilingDCEL.innerFaces.map { face =>
      val (x, y) = calculateCentroid(face.getVerticesUnsafe).toSvgCoords(config.scale)
      textAt(x, y, face.id.value)
    }

  private def createTraversalArrows(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[Elem] =
    if !config.showHalfEdgeTraversal then Nil
    else
      tilingDCEL.innerFaces.flatMap { face =>
        val halfEdges = face.halfEdgesUnsafe
        if halfEdges.length <= 1 then Nil
        else
          val looped = halfEdges :+ halfEdges.head
          looped.sliding(2).flatMap {
            case he1 :: he2 :: Nil =>
              for
                dest1 <- he1.destination
                dest2 <- he2.destination
                mid1   = BigLineSegment(he1.origin.coords, dest1.coords).midPoint
                mid2   = BigLineSegment(he2.origin.coords, dest2.coords).midPoint
                arrow <- createArrow(mid1, mid2, config.scale, config.strokeWidth * 2.5)
              yield polygonElem(arrow.formatted)
            case _                 => None
          }
      }

  private def createLeavingEdgeMarkers(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[Elem] =
    if !config.leavingEdgeMarkers then Nil
    else
      tilingDCEL.vertices.flatMap { vertex =>

        for
          edge  <- vertex.leaving
          dest  <- edge.destination
          arrow <- createArrow(
                     vertex.coords,
                     BigLineSegment(vertex.coords, dest.coords).midPoint,
                     config.scale,
                     config.strokeWidth * 2
                   )
        yield polygonElem(arrow.formatted)
      }

  private def createFaceIdsOnEdges(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[Elem] =
    if !config.faceIdsOnEdges then Nil
    else
      tilingDCEL.halfEdges.flatMap { edge =>

        for
          dest <- edge.destination
          face <- edge.incidentFace
        yield
          val origin      = edge.origin.coords
          val destination = dest.coords
          val segment     = BigLineSegment(origin, destination)
          val midPoint    = segment.midPoint

          // Transform midpoint to SVG coordinates first
          val (midX, midY) = midPoint.toSvgCoords(config.scale)

          // Calculate direction in SVG coordinate space
          val (originX, originY) = origin.toSvgCoords(config.scale)
          val (destX, destY)     = destination.toSvgCoords(config.scale)

          val dx = BigDecimal(destX) - BigDecimal(originX)
          val dy = BigDecimal(destY) - BigDecimal(originY)

          // Calculate perpendicular offset in SVG space (to the left of the direction)
          val offsetDistance = config.strokeWidth * 4
          val length         = spire.math.sqrt(dx.pow(2) + dy.pow(2))

          val perpX = if length > BigDecimal(BigDecimalGeometry.ACCURACY) then -dy * offsetDistance / length
          else BigDecimal(0)
          val perpY = if length > BigDecimal(BigDecimalGeometry.ACCURACY) then dx * offsetDistance / length
          else BigDecimal(0)

          val textX = (BigDecimal(midX) - perpX).format
          val textY = (BigDecimal(midY) - perpY).format

          textAt(textX, textY, face.id.value)
      }

  extension (tiling: TilingDCEL)

    /** Generates an SVG representation of the tiling. The width, height, and viewBox are automatically
      * calculated to fit the tiling at the given scale.
      */
    def toScalableVectorGraphics(
        strokeWidth: Double = 1.0,
        padding: Double = 20.0,
        scale: Double = 50.0,
        showHalfEdgeTraversal: Boolean = false,
        leavingEdgeMarkers: Boolean = false,
        faceIdsOnEdges: Boolean = false,
        showUniformity: Boolean = false
    ): String =
      val svg: Elem =
        if tiling.vertices.isEmpty then
          svgElem("0", "0", "0 0 0 0", Seq.empty)
        else
          val config          =
            SvgConfig(
              strokeWidth,
              padding,
              scale,
              showHalfEdgeTraversal,
              leavingEdgeMarkers,
              faceIdsOnEdges,
              showUniformity
            )
          val vertices        = tiling.vertices.map(_.coords)
          val viewBox         = calculateViewBox(vertices, scale, padding)
          val (width, height) = viewBox.dimensions

          // Generate all elements
          val edgeLines                            = createEdgeLines(tiling, scale)
          val innerFaceArrows                      = createHalfEdgeArrows(tiling.innerFaces.flatMap(_.halfEdgesUnsafe), config)
          val outerFaceArrows                      = createHalfEdgeArrows(tiling.boundaryEdges, config)
          val (innerAngleLabels, outerAngleLabels) = createAngleLabels(tiling, config)
          val boundaryPolygon                      = createBoundaryElements(tiling, config)
          val (vertexCircles, vertexLabels)        = createVertexElements(tiling, config)
          val faceLabels                           = createFaceLabels(tiling, config)
          val traversalArrows                      = createTraversalArrows(tiling, config)
          val leavingEdgeMarkersSvg                = createLeavingEdgeMarkers(tiling, config)
          val faceIdsOnEdgesSvg                    = createFaceIdsOnEdges(tiling, config)

          // Build sections
          val boundarySection = boundaryPolygon.map(polygon =>
            createSvgSection(
              "Boundary Highlight",
              Seq(polygon),
              attrs("stroke" -> "red", "stroke-width" -> strokeWidth * 3, "fill" -> "none")
            )
          ).getOrElse(NodeSeq.Empty)

          val sections = List(
            createSvgSection("Edges", edgeLines, attrs("stroke" -> "black", "stroke-width" -> strokeWidth)),
            boundarySection,
            createSvgSection(
              "Inner Face Half-Edge Direction Arrows",
              innerFaceArrows,
              attrs("fill" -> "blue", "stroke" -> "blue", "stroke-width" -> strokeWidth * 0.5)
            ),
            createSvgSection(
              "Outer Face Half-Edge Direction Arrows",
              outerFaceArrows,
              attrs("fill" -> "black", "stroke" -> "black", "stroke-width" -> strokeWidth * 0.5)
            ),
            createSvgSection(
              "Half-Edge Face Traversal",
              traversalArrows,
              attrs("fill" -> "darkcyan", "stroke" -> "darkcyan", "stroke-width" -> strokeWidth * 0.5)
            ),
            createSvgSection(
              "Leaving Edge Markers",
              leavingEdgeMarkersSvg,
              attrs("fill" -> "yellow", "stroke" -> "yellow", "stroke-width" -> 1.5 * strokeWidth)
            ),
            createSvgSection(
              "Face Ids On Edges Labels",
              faceIdsOnEdgesSvg,
              attrs(
                "font-size"          -> (strokeWidth * 4).toInt,
                "text-anchor"        -> "middle",
                "alignment-baseline" -> "middle",
                "fill"               -> "blue"
              )
            ),
            createSvgSection("Vertices", vertexCircles, attrs("fill" -> "red")),
            createSvgSection(
              "Vertex Labels",
              vertexLabels,
              attrs("font-size" -> (strokeWidth * 8).toInt, "fill" -> "darkblue")
            ),
            createSvgSection(
              "Face Labels",
              faceLabels,
              attrs(
                "font-size"         -> (strokeWidth * 6).toInt,
                "fill"              -> "green",
                "text-anchor"       -> "middle",
                "dominant-baseline" -> "middle"
              )
            ),
            createSvgSection(
              "Inner Angle Labels",
              innerAngleLabels,
              attrs(
                "font-size"         -> (strokeWidth * 5).toInt,
                "fill"              -> "purple",
                "text-anchor"       -> "middle",
                "dominant-baseline" -> "middle"
              )
            ),
            createSvgSection(
              "Outer Angle Labels",
              outerAngleLabels,
              attrs(
                "font-size"         -> (strokeWidth * 5).toInt,
                "fill"              -> "orange",
                "text-anchor"       -> "middle",
                "dominant-baseline" -> "middle"
              )
            )
          ).flatten

          svgElem(
            width = width.toString,
            height = height.toString,
            viewBox = viewBox.formatted,
            children = Seq(gElem(sections))
          )

      new PrettyPrinter(120, 2).format(svg)

    // New ergonomic overload using options
    def toScalableVectorGraphics(options: SvgOptions): String =
      val config = toConfig(options)
      // Delegate to the existing implementation for consistency
      toScalableVectorGraphics(
        strokeWidth = config.strokeWidth,
        padding = config.padding,
        scale = config.scale,
        showHalfEdgeTraversal = config.showHalfEdgeTraversal,
        leavingEdgeMarkers = config.leavingEdgeMarkers,
        faceIdsOnEdges = config.faceIdsOnEdges,
        showUniformity = config.showUniformity
      )

    def toUniformityAnimation(
        graphStyle: GraphStyle = GraphStyle(),
        animationDuration: Double = 2.0,
        pauseBetweenSteps: Double = 0.5
    ): String =
      val trees = tiling.scanUniformityTreeAlt
      if trees.isEmpty then
        return tiling.toSVG(graphStyle)

      val totalSteps    = trees.length
      val stepDuration  = animationDuration / totalSteps
      val cycleDuration = animationDuration + pauseBetweenSteps

      // Get all unique vertex IDs and create a stable color mapping
      val allVertexIds = tiling.innerVertices.map(_.id).toSet

      // For each step, determine which color class each vertex belongs to
      val vertexToColorAtStep: Map[Int, Map[VertexId, Int]] =
        trees.zipWithIndex.map { case (tree, stepIndex) =>
          val leaves        = tree.flattenLeaves
          val vertexToColor = leaves.zipWithIndex.flatMap { case (vertexIds, colorIndex) =>
            vertexIds.map(vid => vid -> colorIndex)
          }.toMap
          stepIndex -> vertexToColor
        }.toMap

      // Generate the base SVG structure
      val baseDoc    = tiling.toDoc(graphStyle.copy(showLabels = false))
      val svgElement = baseDoc.root

      // Extract viewBox and dimensions
      val viewBox = svgElement.attribute("viewBox").getOrElse("")
      val width   = svgElement.attribute("width").getOrElse("800")
      val height  = svgElement.attribute("height").getOrElse("800")

      // Start building the animated SVG
      val sb = new StringBuilder()
      sb.append(s"""<svg xmlns="http://www.w3.org/2000/svg" """)
      sb.append(s"""viewBox="$viewBox" width="$width" height="$height">""")
      sb.append("\n")

      // Add a style section for the color palette
      sb.append("  <style>\n")
      val maxColors = trees.map(_.sizeLeaves).max
      for colorIndex <- 0 until maxColors do
        val color = graphStyle.palette.getColor(colorIndex)
        sb.append(s"    .uniformity-$colorIndex { fill: $color; }\n")
      sb.append("  </style>\n\n")

      // Add defs section for animations
      sb.append("  <defs>\n")

      // Create keyframe animations for each possible color transition
      val colorTransitions = (0 until maxColors).flatMap { fromColor =>

        (0 until maxColors).map { toColor =>

          (fromColor, toColor)
        }
      }.distinct

      for (fromColor, toColor) <- colorTransitions do
        sb.append(s"""    <animate id="anim-$fromColor-to-$toColor" attributeName="class" """)
        sb.append(s"""from="uniformity-$fromColor" to="uniformity-$toColor" """)
        sb.append(s"""dur="${stepDuration}s" fill="freeze"/>\n""")

      sb.append("  </defs>\n\n")

      // Add the base tiling geometry (edges and faces) from the base document
      val baseChildren = svgElement.child.filterNot(node =>
        node.label == "g" && node.attribute("id").exists(_.startsWith("vertices"))
      )

      for child <- baseChildren do
        sb.append("  ")
        sb.append(child.toString.replaceAll("\n", "\n  "))
        sb.append("\n")

      // Add animated uniformity dots for each vertex
      sb.append("\n  <!-- Animated Uniformity Vertices -->\n")
      sb.append(s"""  <g id="vertices-uniformity-animated" stroke="none">""")
      sb.append("\n")

      for vertex <- tiling.innerVertices do
        val vid    = vertex.id
        val coords = vertex.coords
        val x      = coords.x
        val y      = -coords.y // Flip Y for SVG coordinate system

        // Determine initial color
        val initialColor = vertexToColorAtStep.get(0).flatMap(_.get(vid)).getOrElse(0)

        sb.append(s"""    <circle cx="$x" cy="$y" r="${graphStyle.vertexRadius}" """)
        sb.append(s"""class="uniformity-$initialColor">""")
        sb.append("\n")

        // Add animation sequence
        for stepIndex <- 0 until totalSteps do
          val currentColor = vertexToColorAtStep.get(stepIndex).flatMap(_.get(vid)).getOrElse(0)
          val nextColor    = if stepIndex < totalSteps - 1 then
            vertexToColorAtStep.get(stepIndex + 1).flatMap(_.get(vid)).getOrElse(currentColor)
          else
            currentColor

          if currentColor != nextColor then
            val beginTime = stepIndex * stepDuration
            sb.append(s"""      <animate attributeName="class" """)
            sb.append(s"""from="uniformity-$currentColor" to="uniformity-$nextColor" """)
            sb.append(s"""begin="${beginTime}s" dur="${stepDuration}s" fill="freeze"/>""")
            sb.append("\n")

        // Loop the animation
        sb.append(s"""      <animate attributeName="class" """)
        sb.append(s"""to="uniformity-${initialColor}" """)
        sb.append(s"""begin="${cycleDuration}s" dur="0.01s" fill="freeze"/>""")
        sb.append("\n")

        // Restart animation
        sb.append(s"""      <set attributeName="class" to="uniformity-${initialColor}" """)
        sb.append(s"""begin="${cycleDuration}s"/>""")
        sb.append("\n")

        sb.append("    </circle>\n")

      sb.append("  </g>\n")

      // Add distance label that updates
      sb.append("\n  <!-- Distance Label -->\n")
      val labelX = viewBox.split(" ").lift(0).flatMap(_.toDoubleOption).getOrElse(0.0) + 10
      val labelY = viewBox.split(" ").lift(1).flatMap(_.toDoubleOption).getOrElse(0.0) + 20

      sb.append(s"""  <text x="$labelX" y="$labelY" font-family="Arial" font-size="14" fill="black">""")
      sb.append("\n")
      sb.append(s"""    Distance: <tspan id="distance-value">0</tspan>""")
      sb.append("\n")

      for stepIndex <- 0 until totalSteps do
        val beginTime = stepIndex * stepDuration
        sb.append(s"""    <set attributeName="textContent" to="Distance: $stepIndex" """)
        sb.append(s"""begin="${beginTime}s" fill="freeze"/>""")
        sb.append("\n")

      sb.append(s"""    <set attributeName="textContent" to="Distance: 0" """)
      sb.append(s"""begin="${cycleDuration}s"/>""")
      sb.append("\n")

      sb.append("  </text>\n")

      // Add restart animation for infinite loop
      sb.append(s"""\n  <animate attributeName="visibility" from="visible" to="visible" """)
      sb.append(s"""dur="${cycleDuration}s" repeatCount="indefinite"/>""")
      sb.append("\n")

      sb.append("</svg>")

      sb.toString

    /** Serializes the complete structure of a [[TilingDCEL]] into XML metadata, which can be embedded within
      * an SVG. This metadata includes all vertices, half-edges, and faces, along with their properties and
      * relationships, ensuring that the [[TilingDCEL]] can be fully reconstructed later.
      */
    def toMetadata: String =

      val halfEdgeIds: Map[HalfEdge, Int] = tiling.halfEdges.zipWithIndex.toMap

      val vertexNodes  = tiling.vertices.map { v =>
        val attrsList = List(
          Some("id" -> v.id.value),
          Some("x"  -> v.coords.x.toString),
          Some("y"  -> v.coords.y.toString),
          v.leaving.flatMap(halfEdgeIds.get).map(id => "leaving" -> id)
        ).flatten
        elem("vertex", attrs(attrsList*))
      }
      val verticesElem = elem("vertices", children = vertexNodes)

      val halfEdgeNodes = tiling.halfEdges.zipWithIndex.map { case (he, id) =>
        val attrsList = List(
          Some("id"     -> id),
          Some("origin" -> he.origin.id.value),
          he.twin.flatMap(halfEdgeIds.get).map(twinId => "twin" -> twinId),
          he.next.flatMap(halfEdgeIds.get).map(nextId => "next" -> nextId),
          he.prev.flatMap(halfEdgeIds.get).map(prevId => "prev" -> prevId),
          he.incidentFace.map(f => "face" -> f.id.value),
          he.angle.map(a => "angle" -> a.toRational)
        ).flatten
        elem("half-edge", attrs(attrsList*))
      }
      val halfEdgesElem = elem("half-edges", children = halfEdgeNodes)

      val faceNodes = tiling.faces.map { f =>
        val attrsList = List(
          Some("id" -> f.id.value),
          f.outerComponent.flatMap(halfEdgeIds.get).map(id => "outer-component" -> id)
        ).flatten
        val innerIds  = f.innerComponents.flatMap(_.flatMap(halfEdgeIds.get))
        val allAttrs  = if innerIds.nonEmpty then
          attrsList :+ ("inner-components" -> innerIds.mkString(","))
        else
          attrsList
        elem("face", attrs(allAttrs*))
      }
      val facesElem = elem("faces", children = faceNodes)

      val dcelElem = elem(
        "tessella-dcel",
        children = Seq(verticesElem, halfEdgesElem, facesElem),
        prefix = Some("tessella"),
        scope = NamespaceBinding("tessella", "https://github.com/scala-tessella/tessella", TopScope)
      )

      dcelElem.toString

  /** Deserializes the XML metadata that includes all vertices, half-edges, and faces, along with their
    * properties and relationships, and fully reconstructs the complete structure of a [[TilingDCEL]].
    */
  def fromMetadata(metadata: String): Either[TilingError, TilingDCEL] =
    TilingSVGPlatform.fromMetadata(metadata)
