package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.geometry.*
import io.github.scala_tessella.dcel.structure.{HalfEdge, Vertex}
import io.github.scala_tessella.dcel.{TilingDCEL, TilingError}
import io.github.scala_tessella.dcel.conversion.SvgDsl.*
import io.github.scala_tessella.dcel.conversion.SvgRendering.*

/** Static SVG export for [[TilingDCEL]]. The headline entry point surfaces as the `toScalableVectorGraphics`
  * extension (`toSVG` on the type itself delegates here).
  *
  * Render styling — stroke width, padding, scale, plus toggles for half-edge traversal arrows, leaving-edge
  * markers, per-face id labels and uniformity colouring — is bundled in [[SvgOptions]] for the ergonomic
  * overload. The plain overload exposes the same knobs as positional parameters.
  *
  * Round-trip serialisation is provided through the SVG `<metadata>` element: see [[fromMetadata]] (here) and
  * the `toMetadata` extension; both delegate to [[SvgMetadata]].
  *
  * Related renderers: [[SimplePolygonSVG]] (individual polygons, symmetry axes, parallelogon tiling) and
  * [[SvgAnimation]] (animated uniformity refinement); the shared drawing primitives live in the
  * package-private [[SvgRendering]].
  */
object TilingSVG:

  /** Bundle of rendering options for the SVG output. Pass to the
    * `toScalableVectorGraphics(options: SvgOptions)` overload to control all knobs in one record instead of a
    * long positional argument list.
    *
    * @param strokeWidth
    *   Width of the edge lines in SVG user units. Defaults to `1.0`.
    * @param padding
    *   Whitespace around the tiling inside the SVG viewBox, in SVG user units. Defaults to `20.0`.
    * @param scale
    *   Factor applied to model-space coordinates when emitting them as SVG. Defaults to `50.0`.
    * @param showHalfEdgeTraversal
    *   When true, draws arrows on each half-edge indicating the traversal direction (useful for debugging
    *   topology).
    * @param leavingEdgeMarkers
    *   When true, marks each vertex's `leaving` half-edge — i.e. the half-edge picked as the canonical
    *   representative for that vertex.
    * @param faceIdsOnEdges
    *   When true, labels each face's id on its half-edges.
    * @param showUniformity
    *   When true, colours faces by their uniformity-tree group (see [[TilingDCEL.uniformityTree]]).
    */
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

  private def createAngleLabel(halfEdge: HalfEdge, direction: BigPoint, config: SvgConfig): String =
    val origin           = halfEdge.origin.coords
    val angleText        = f"${halfEdge.angle.get.toRational.toDouble}%.0f°"
    val labelDistance    = config.strokeWidth * 8
    val (labelX, labelY) = origin.toSvgLabelCoords(
      config.scale,
      direction.x.toDouble * labelDistance,
      -direction.y.toDouble * labelDistance
    )
    textAt(labelX, labelY, angleText)

  private def createHalfEdgeArrows(halfEdges: List[HalfEdge], config: SvgConfig): Seq[String] =
    val segments =
      halfEdges
        .sortBy:
          _.idUnsafe
        .map: halfEdge =>
          (halfEdge.origin.coords, halfEdge.destinationUnsafe.coords)
    createArrowsFromPoints(segments, config.scale, config.strokeWidth * 3)

  private def createAngleLabels(tilingDCEL: TilingDCEL, config: SvgConfig): (Seq[String], Seq[String]) =
    val innerAngleLabels =
      sortedInnerFaces(tilingDCEL).flatMap: face =>
        val centroid = calculateCentroid(face.getVerticesUnsafe)
        face.halfEdgesUnsafe.map: halfEdge =>
          val direction = calculateDirection(halfEdge.origin.coords, centroid)
          createAngleLabel(halfEdge, direction, config)

    val tilingCentroid =
      tilingDCEL.innerFaces.headOption
        .map: face =>
          face.getVerticesUnsafe
        .filter: vertices =>
          vertices.nonEmpty
        .map: vertices =>
          calculateCentroid(vertices)
        .getOrElse(BigPoint.origin)

    val outerAngleLabels = sortedBoundaryEdges(tilingDCEL).map: halfEdge =>
      val inwardDirection  = calculateDirection(halfEdge.origin.coords, tilingCentroid)
      val outwardDirection = BigPoint(-inwardDirection.x, -inwardDirection.y)
      createAngleLabel(halfEdge, outwardDirection, config)

    (innerAngleLabels, outerAngleLabels)

  private def vertexToSvg(
      vertex: Vertex,
      label: String,
      config: SvgConfig,
      radiusMultiplier: Double = 4.0,
      more: Attrs = Nil
  ): (String, String) =
    val (cx, cy) = vertex.coords.toSvgCoords(config.scale)
    val circle   = circleElem(cx, cy, (config.strokeWidth * radiusMultiplier).toString, more)

    val offset   = config.strokeWidth * 2.5
    val (lx, ly) = vertex.coords.toSvgLabelCoords(config.scale, offset, -offset)
    val text     = textAt(lx, ly, label)
    (circle, text)

  private def createBoundaryElements(tilingDCEL: TilingDCEL, config: SvgConfig): Option[String] =
    tilingDCEL.boundaryVerticesUnsafe match
      case vertices if vertices.nonEmpty =>
        val points =
          vertices
            .map: vertex =>
              val (x, y) = vertex.coords.toSvgCoords(config.scale)
              s"$x,$y"
            .mkString(" ")
        Some(polygonElem(points))
      case _                             => None

  private def createVertexElements(tilingDCEL: TilingDCEL, config: SvgConfig): (Seq[String], Seq[String]) =
    val classes  = if config.showUniformity then tilingDCEL.uniformityTree.flattenLeaves else Nil
    val indexMap =
      classes
        .zipWithIndex
        .flatMap: (ids, idx) =>
          ids.map:
            _ -> idx
        .toMap

    sortedVertices(tilingDCEL)
      .map: vertex =>
        val colorIdx                 = indexMap.get(vertex.id)
        val (radiusMultiplier, meta) = colorIdx match
          case Some(idx) => (20.0, attrs("fill" -> uniformColorMap.getOrElse(idx, "red")))
          case None      => (2.0, Nil)
        vertexToSvg(vertex, vertex.id.toString, config, radiusMultiplier, meta)
      .unzip

  private def createFaceLabels(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[String] =
    sortedInnerFaces(tilingDCEL).map: face =>
      val (x, y) = calculateCentroid(face.getVerticesUnsafe).toSvgCoords(config.scale)
      textAt(x, y, face.id.toString)

  private def createTraversalArrows(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[String] =
    if !config.showHalfEdgeTraversal then Nil
    else
      val segments =
        sortedInnerFaces(tilingDCEL).flatMap: face =>
          val halfEdges = face.halfEdgesUnsafe
          if halfEdges.length <= 1 then Nil
          else
            val looped = halfEdges :+ halfEdges.head
            looped.sliding(2).flatMap:
              case he1 :: he2 :: Nil =>
                for
                  dest1 <- he1.destination
                  dest2 <- he2.destination
                  mid1   = BigLineSegment(he1.origin.coords, dest1.coords).midPoint
                  mid2   = BigLineSegment(he2.origin.coords, dest2.coords).midPoint
                yield (mid1, mid2)
              case _                 => None
      createArrowsFromPoints(segments, config.scale, config.strokeWidth * 2.5)

  private def createLeavingEdgeMarkers(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[String] =
    if !config.leavingEdgeMarkers then Nil
    else
      val segments =
        sortedVertices(tilingDCEL).flatMap: vertex =>
          for
            edge <- vertex.leaving
            dest <- edge.destination
          yield (vertex.coords, BigLineSegment(vertex.coords, dest.coords).midPoint)
      createArrowsFromPoints(segments, config.scale, config.strokeWidth * 2)

  private def createFaceIdsOnEdges(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[String] =
    if !config.faceIdsOnEdges then Nil
    else
      sortedHalfEdges(tilingDCEL).flatMap: edge =>
        for
          dest <- edge.destination
          face <- edge.incidentFace
        yield
          val origin      = edge.origin.coords
          val destination = dest.coords
          val segment     = BigLineSegment(origin, destination)
          val midPoint    = segment.midPoint

          // Calculate direction in SVG coordinate space
          val originSvg      = origin.scaled(config.scale).flippedY
          val destinationSvg = destination.scaled(config.scale).flippedY
          val dx             = destinationSvg.x - originSvg.x
          val dy             = destinationSvg.y - originSvg.y

          // Calculate perpendicular offset in SVG space (to the left of the direction)
          val offsetDistance = config.strokeWidth * 4
          // ADR-0009 candidate A: Math.sqrt on Double.
          val length         =
            val dxD = dx.toDouble
            val dyD = dy.toDouble
            BigDecimal(Math.sqrt(dxD * dxD + dyD * dyD))

          val perpX = if length > BigDecimal(BigDecimalGeometry.ACCURACY) then -dy * offsetDistance / length
          else BigDecimal(0)
          val perpY = if length > BigDecimal(BigDecimalGeometry.ACCURACY) then dx * offsetDistance / length
          else BigDecimal(0)

          val (textX, textY) = midPoint.toSvgLabelCoords(config.scale, -perpX.toDouble, -perpY.toDouble)

          textAt(textX, textY, face.id.toString)

  extension (tiling: TilingDCEL)

    /** Generates an SVG XML string. */
    def toScalableVectorGraphicsXml(
        strokeWidth: Double = 1.0,
        padding: Double = 20.0,
        scale: Double = 50.0,
        showHalfEdgeTraversal: Boolean = false,
        leavingEdgeMarkers: Boolean = false,
        faceIdsOnEdges: Boolean = false,
        showUniformity: Boolean = false
    ): String =
      if tiling.vertices.isEmpty then
        """<svg width="0" height="0" viewBox="0 0 0 0" xmlns="http://www.w3.org/2000/svg"/>"""
      else
        val config   =
          SvgConfig(
            strokeWidth,
            padding,
            scale,
            showHalfEdgeTraversal,
            leavingEdgeMarkers,
            faceIdsOnEdges,
            showUniformity
          )
        val vertices = tiling.vertices.map(_.coords)
        val viewBox  = calculateViewBox(vertices, scale, padding)

        // Generate all elements
        val edgeLines                            = createEdgeLines(tiling, scale)
        val innerFaceArrows                      = createHalfEdgeArrows(tiling.innerFaces.flatMap(_.halfEdgesUnsafe), config)
        val outerFaceArrows                      = createHalfEdgeArrows(tiling.boundaryEdgesUnsafe, config)
        val (innerAngleLabels, outerAngleLabels) = createAngleLabels(tiling, config)
        val boundaryPolygon                      = createBoundaryElements(tiling, config)
        val (vertexCircles, vertexLabels)        = createVertexElements(tiling, config)
        val faceLabels                           = createFaceLabels(tiling, config)
        val traversalArrows                      = createTraversalArrows(tiling, config)
        val leavingEdgeMarkersSvg                = createLeavingEdgeMarkers(tiling, config)
        val faceIdsOnEdgesSvg                    = createFaceIdsOnEdges(tiling, config)

        // Build sections
        val boundarySection = boundaryPolygon.flatMap(polygon =>
          createSvgSection(
            "Boundary Highlight",
            Seq(polygon),
            attrs("stroke" -> "red", "stroke-width" -> strokeWidth * 3, "fill" -> "none")
          )
        )

        val sections = assembleSections(
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
        )

        svgWithViewBox(viewBox, Seq(gElem(sections)))

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
      toScalableVectorGraphicsXml(
        strokeWidth,
        padding,
        scale,
        showHalfEdgeTraversal,
        leavingEdgeMarkers,
        faceIdsOnEdges,
        showUniformity
      )

    /** Ergonomic overload of [[toScalableVectorGraphics]] that takes a single [[SvgOptions]] bundle in place
      * of the positional argument list. Identical output.
      */
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

    /** @see [[SvgMetadata.toMetadataXml]] */
    def toMetadataXml: String =
      SvgMetadata.toMetadataXml(tiling)

    /** Same as [[toMetadataXml]]: emits the `<tessella:tessella-dcel>` metadata element capturing the
      * tiling's full DCEL state for round-trip via [[fromMetadata]].
      */
    def toMetadata: String =
      SvgMetadata.toMetadataXml(tiling)

  /** @see [[SvgMetadata.fromMetadata]] */
  def fromMetadata(metadata: String): Either[TilingError, TilingDCEL] =
    SvgMetadata.fromMetadata(metadata)
