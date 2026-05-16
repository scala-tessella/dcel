package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.*
import io.github.scala_tessella.dcel.geometry.*
import io.github.scala_tessella.dcel.structure.{Face, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.dcel.{TilingDCEL, TilingError}
import io.github.scala_tessella.dcel.TilingUniformity.scanUniformityTree
import io.github.scala_tessella.dcel.conversion.SvgDsl.*
import io.github.scala_tessella.ring_seq.SymmetryOps.{
  AxisLocation,
  Edge as SymmetryEdge,
  Vertex as SymmetryVertex
}
import scala.collection.mutable

/** SVG export for [[TilingDCEL]] and for individual [[SimplePolygon]] shapes. The headline entry points
  * surface as extension methods on `TilingDCEL` (`toSVG` on the type itself; `toScalableVectorGraphics`
  * here) and on `SimplePolygon` (`toSVG`, `toParallelogonTiling`).
  *
  * Render styling — stroke width, padding, scale, plus toggles for half-edge traversal arrows, leaving-edge
  * markers, per-face id labels and uniformity colouring — is bundled in [[SvgOptions]] for the ergonomic
  * overload. The plain overload exposes the same knobs as positional parameters.
  *
  * Round-trip serialisation is provided through the SVG `<metadata>` element: see [[fromMetadata]] (here)
  * and the `toMetadata` extension; both delegate to [[SvgMetadata]].
  */
object TilingSVG:

  extension (bigPoint: BigPoint)
    private def toSvgCoords(scale: Double): (String, String) =
      val scaledPoint: BigPoint = bigPoint.scaled(scale).flippedY
      (scaledPoint.x.format, scaledPoint.y.format)

    private def toSvgLabelCoords(scale: Double, offsetX: Double, offsetY: Double): (String, String) =
      val scaledPoint: BigPoint = bigPoint.scaled(scale).flippedY
      ((scaledPoint.x + BigDecimal(offsetX)).format, (scaledPoint.y + BigDecimal(offsetY)).format)

  private case class Arrow(
      tipX: String,
      tipY: String,
      baseX1: String,
      baseY1: String,
      baseX2: String,
      baseY2: String
  ):
    val formatted: String = s"$tipX,$tipY $baseX1,$baseY1 $baseX2,$baseY2"

  /** Bundle of rendering options for the SVG output. Pass to the
    * `toScalableVectorGraphics(options: SvgOptions)` overload to control all knobs in one record instead
    * of a long positional argument list.
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

  private def sortedVertices(tiling: TilingDCEL): List[Vertex] =
    tiling.vertices.sortBy:
      _.id.value

  private def sortedInnerFaces(tiling: TilingDCEL): List[Face] =
    tiling.innerFaces.sortBy:
      _.id.value

  private def sortedHalfEdges(tiling: TilingDCEL): List[HalfEdge] =
    tiling.halfEdges.sortBy:
      _.idUnsafe

  private def sortedBoundaryEdges(tiling: TilingDCEL): List[HalfEdge] =
    tiling.boundaryEdgesUnsafe.sortBy:
      _.idUnsafe

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

      val tip   = scaledMidPoint + BigPoint(unitDirection.x * arrowSizeBD, -unitDirection.y * arrowSizeBD)
      val base1 =
        scaledMidPoint + BigPoint(unitPerpendicular.x * baseOffset, -unitPerpendicular.y * baseOffset)
      val base2 =
        scaledMidPoint + BigPoint(unitPerpendicular.x * -baseOffset, -unitPerpendicular.y * -baseOffset)

      Some(Arrow(tip.x.format, tip.y.format, base1.x.format, base1.y.format, base2.x.format, base2.y.format))

  private def calculateCentroid(vertices: List[Vertex]): BigPoint =
    vertices.map(_.coords).centroid

  private def calculateDirection(from: BigPoint, to: BigPoint): BigPoint =
    BigPoint.fromPolar(BigDecimal(1.0), from.angleTo(to))

  private def createArrowsFromPoints(
      segments: Seq[(BigPoint, BigPoint)],
      config: SvgConfig,
      arrowSize: Double
  ): Seq[String] =
    segments.flatMap: (origin, destination) =>
      createArrow(origin, destination, config.scale, arrowSize).map: arrow =>
        polygonElem(arrow.formatted)

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

  private def createSvgSection(title: String, content: Seq[String], attributes: Attrs = Nil): Option[String] =
    if content.isEmpty then None
    else Some(Seq(comment(title), gElem(content, attributes)).mkString("\n"))

  private def assembleSections(sections: Option[String]*): Seq[String] =
    sections.flatten.toSeq

  private def createHalfEdgeArrows(halfEdges: List[HalfEdge], config: SvgConfig): Seq[String] =
    val segments =
      halfEdges
        .sortBy:
          _.idUnsafe
        .map: halfEdge =>
          (halfEdge.origin.coords, halfEdge.destinationUnsafe.coords)
    createArrowsFromPoints(segments, config, config.strokeWidth * 3)

  private def createEdgeLines(tilingDCEL: TilingDCEL, scale: Double): Seq[String] =
    val drawnEdges = mutable.Set.empty[HalfEdge]
    sortedHalfEdges(tilingDCEL).flatMap: halfEdge =>
      if drawnEdges.contains(halfEdge) || halfEdge.twin.isEmpty then None
      else
        val twin     = halfEdge.twin.get
        drawnEdges ++= List(halfEdge, twin)
        val (x1, y1) = halfEdge.origin.coords.toSvgCoords(scale)
        val (x2, y2) = twin.origin.coords.toSvgCoords(scale)
        Some(lineElem(x1, y1, x2, y2))

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

  private def uniformColorMap: Map[Int, String] =
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
      createArrowsFromPoints(segments, config, config.strokeWidth * 2.5)

  private def createLeavingEdgeMarkers(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[String] =
    if !config.leavingEdgeMarkers then Nil
    else
      val segments =
        sortedVertices(tilingDCEL).flatMap: vertex =>
          for
            edge <- vertex.leaving
            dest <- edge.destination
          yield (vertex.coords, BigLineSegment(vertex.coords, dest.coords).midPoint)
      createArrowsFromPoints(segments, config, config.strokeWidth * 2)

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

  extension (simple: SimplePolygon)

    private def section(
        vertices: List[BigPoint],
        strokeWidth: Double = 1.0,
        padding: Double = 30.0,
        scale: Double = 50.0,
        showReflection: Boolean = false,
        showRotation: Boolean = false
    ): String =

      // Generate all elements
      val boundaryPolygon =
        val points =
          vertices
            .map: vertex =>
              val (x, y) = vertex.toSvgCoords(scale)
              s"$x,$y"
            .mkString(" ")
        Some(polygonElem(points))

      val (vertexCircles, vertexLabels) = // createVertexElements(tiling, config)
        val circles =
          vertices.map: vertex =>
            val (cx, cy) = vertex.toSvgCoords(scale)
            circleElem(cx, cy, (strokeWidth * 2).toString)

        val labels = vertices.indices.map: index =>
          val offset   = strokeWidth * 2.5
          val (lx, ly) = vertices(index).toSvgLabelCoords(scale, offset, -offset)
          textAt(lx, ly, s"$index") // - ${simple.toAngles(index)}°")

        (circles, labels)

      def axisVertex(location: AxisLocation): BigPoint =
        location match
          case vertex: SymmetryVertex => vertices(vertex.i)
          case edge: SymmetryEdge     => BigLineSegment(vertices(edge.i), vertices(edge.j)).midPoint

      val reflections =
        simple.reflectionalIndexPairs.map: (location, oppositeLocation) =>
          val (x1, y1) = axisVertex(location).toSvgCoords(scale)
          val (x2, y2) = axisVertex(oppositeLocation).toSvgCoords(scale)
          lineElem(x1, y1, x2, y2)

      val rotations =
        simple.rotationalIndices match
          case _ :: Nil => Seq.empty[String]
          case indices  =>
            val ends     = indices.map: index =>
              vertices(index)
            val (x1, y1) = ends.centroid.toSvgCoords(scale)
            ends.map: end =>
              val (x2, y2) = end.toSvgCoords(scale)
              lineElem(x1, y1, x2, y2)

      // Build sections
      val boundarySection =
        boundaryPolygon.flatMap: polygon =>
          createSvgSection(
            "Boundary Highlight",
            Seq(polygon),
            attrs("stroke" -> "red", "stroke-width" -> strokeWidth * 3, "fill" -> "none")
          )

      val reflection =
        if showReflection then
          createSvgSection(
            "Reflection axes",
            reflections,
            attrs("stroke" -> "green", "stroke-dasharray" -> "2")
          )
        else None

      val rotation =
        if showRotation then
          createSvgSection(
            "Rotation axes",
            rotations,
            attrs("stroke" -> "orange", "stroke-width" -> "3", "stroke-dasharray" -> "1")
          )
        else None

      val sections = assembleSections(
        boundarySection,
        createSvgSection("Vertices", vertexCircles, attrs("fill" -> "darkred")),
        createSvgSection(
          "Vertex Labels",
          vertexLabels,
          attrs("font-size" -> (strokeWidth * 8).toInt, "fill" -> "blue")
        ),
        reflection,
        rotation
      )
      gElem(sections)

    /** Renders the polygon as an SVG string at unit scale (subject to `scale`). Used by
      * [[SimplePolygon.toSVG]] and available directly when you want to control rendering knobs.
      *
      * @param strokeWidth
      *   Edge stroke width in SVG user units.
      * @param padding
      *   Padding around the polygon inside the viewBox.
      * @param scale
      *   Coordinate scale factor.
      * @param showReflection
      *   When true, overlays the polygon's reflection-symmetry axes.
      * @param showRotation
      *   When true, overlays the polygon's rotational-symmetry centre and representative vertices.
      */
    def toScalableVectorG(
        strokeWidth: Double = 1.0,
        padding: Double = 30.0,
        scale: Double = 50.0,
        showReflection: Boolean = false,
        showRotation: Boolean = false
    ): String =
      val vertices = simple.toBigPoints
      val viewBox  = calculateViewBox(vertices, scale, padding)
      svgWithViewBox(
        viewBox,
        Seq(gElem(Seq(section(vertices, showReflection = showReflection, showRotation = showRotation))))
      )

    /** Chooses from the result of the `parallelogonIndices` an origin index and two repeat ones to
      * quadruplicate the tiling along its parallel segments.
      *
      * @return
      *   a triple of boundary vertex indices, or None if the polygon is not a parallelogon
      */
    private def parallelogonTranslationIndices: Option[(Int, Int, Int)] =
      simple.parallelogonIndices match
        case a :: b :: c :: _ :: Nil    => Option((a, b, c))
        case a :: _ :: c :: _ :: e :: _ => Option((a, c, e))
        case _                          => None

    /** For a parallelogon (see `SimplePolygon.canTileTorus`), renders the polygon together with three
      * translated copies showing how it tiles the plane periodically. For non-parallelogons, falls back to
      * [[toScalableVectorG]].
      */
    def toParallelogonTiling(
        strokeWidth: Double = 1.0,
        padding: Double = 30.0,
        scale: Double = 50.0
    ): String =
      val vertices =
        BigLineSegment(BigPoint.origin, BigPoint(1, 0)).unitPath(simple.toAngles)

      simple.parallelogonTranslationIndices match
        case None              => toScalableVectorG(strokeWidth, padding, scale)
        case Some((o, r1, r2)) =>

          val origin            = vertices(o).scaled(scale)
          val repeat            = vertices(r1).scaled(scale)
          val repeatOnOtherAxis = vertices(r2).scaled(scale)

          val diffTwo   = (repeat - origin).scaled(1.1)
          val diffThree = (repeatOnOtherAxis - origin).scaled(1.1)

          val diffFour = diffTwo + diffThree
          val one      = section(vertices)

          val viewBox         =
            calculateViewBox(vertices, scale, padding)
          val viewBoxAdjusted =
            viewBox.copy(
              minX = viewBox.minX,
              minY = viewBox.minY - diffFour.y.abs,
              width = viewBox.width + diffFour.x.abs,
              height = viewBox.height + diffFour.y.abs
            )

          svgWithViewBox(
            viewBoxAdjusted,
            Seq(
              gElem(Seq(one)),
              gElem(Seq(one), attrs("transform" -> s"translate(${diffTwo.x}, ${-diffTwo.y})")),
              gElem(Seq(one), attrs("transform" -> s"translate(${diffThree.x}, ${-diffThree.y})")),
              gElem(Seq(one), attrs("transform" -> s"translate(${diffFour.x}, ${-diffFour.y})"))
            )
          )

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

    /** Ergonomic overload of [[toScalableVectorGraphics]] that takes a single [[SvgOptions]] bundle in
      * place of the positional argument list. Identical output.
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

    /** Renders the tiling as an animated SVG that walks through the steps of
      * [[TilingDCEL.uniformityTree]] (via `scanUniformityTree`). Each step highlights the next layer of
      * vertex-class refinement, pausing between steps so the viewer can follow the grouping.
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
        val colorSeqIdx =
          (0 until totalSteps)
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
        val visibilitySeq =
          (0 until totalSteps).map: i =>
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

        val visValues        =
          (0 until totalSteps)
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

        val visValues        =
          (0 until totalSteps).map: j =>
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
