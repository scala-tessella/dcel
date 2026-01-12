package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.*
import io.github.scala_tessella.dcel.geometry.*
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.dcel.{TilingDCEL, TilingError, ValidationError, NotFoundError}
import io.github.scala_tessella.dcel.TilingUniformity.scanUniformityTree
import io.github.scala_tessella.dcel.Utils.{associateValues, sequence, traverse}
import io.github.scala_tessella.ring_seq.SymmetryOps.{
  AxisLocation,
  Edge as SymmetryEdge,
  Vertex as SymmetryVertex
}
import spire.implicits.*
import spire.math.Rational

import scala.collection.mutable
import scala.util.Try
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

  // Helper to create MetaData more idiomatically
  private def attrs(tuples: (String, Any)*): MetaData =
    tuples.foldRight[MetaData](Null):
      case ((key, value), acc) => new UnprefixedAttribute(key, value.toString, acc)

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
    elem("line", attrs("x1" -> x1, "y1" -> y1, "x2" -> x2, "y2" -> y2).append(more))

  private def circleElem(cx: String, cy: String, r: String, more: MetaData = Null): Elem =
    elem("circle", attrs("cx" -> cx, "cy" -> cy, "r" -> r).append(more))

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
    halfEdges.flatMap: halfEdge =>
      for
        arrow <- createArrow(
                   halfEdge.origin.coords,
                   halfEdge.destinationUnsafe.coords,
                   config.scale,
                   config.strokeWidth * 3
                 )
      yield polygonElem(arrow.formatted)

  private def createEdgeLines(tilingDCEL: TilingDCEL, scale: Double): Seq[Elem] =
    val drawnEdges = mutable.Set.empty[HalfEdge]
    tilingDCEL.halfEdges.flatMap: halfEdge =>
      if drawnEdges.contains(halfEdge) || halfEdge.twin.isEmpty then None
      else
        val twin     = halfEdge.twin.get
        drawnEdges ++= List(halfEdge, twin)
        val (x1, y1) = halfEdge.origin.coords.toSvgCoords(scale)
        val (x2, y2) = twin.origin.coords.toSvgCoords(scale)
        Some(lineElem(x1, y1, x2, y2))

  private def createAngleLabels(tilingDCEL: TilingDCEL, config: SvgConfig): (Seq[Elem], Seq[Elem]) =
    val innerAngleLabels =
      tilingDCEL.innerFaces.flatMap: face =>
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

    val outerAngleLabels = tilingDCEL.boundaryEdges.map: halfEdge =>
      val inwardDirection  = calculateDirection(halfEdge.origin.coords, tilingCentroid)
      val outwardDirection = BigPoint(-inwardDirection.x, -inwardDirection.y)
      createAngleLabel(halfEdge, outwardDirection, config)

    (innerAngleLabels, outerAngleLabels)

  private def vertexToSvg(vertex: Vertex, label: String, config: SvgConfig, radiusMultiplier: Double = 4.0, more: MetaData = Null): (Elem, Elem) =
    val (cx, cy) = vertex.coords.toSvgCoords(config.scale)
    val circle = circleElem(cx, cy, (config.strokeWidth * radiusMultiplier).toString, more)

    val point = vertex.coords.scaled(config.scale).flippedY
    val lx = (point.x + config.strokeWidth * 2.5).format
    val ly = (point.y - config.strokeWidth * 2.5).format
    val text = textAt(lx, ly, label)
    (circle, text)

  private def createBoundaryElements(tilingDCEL: TilingDCEL, config: SvgConfig): Option[Elem] =
    tilingDCEL.boundaryVertices match
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

  private def createVertexElements(tilingDCEL: TilingDCEL, config: SvgConfig): (Seq[Elem], Seq[Elem]) =
    val classes = if config.showUniformity then tilingDCEL.uniformityTree.flattenLeaves else Nil
    val indexMap =
      classes
        .zipWithIndex
        .flatMap: (ids, idx) =>
          ids.map:
            _ -> idx
        .toMap

    tilingDCEL.vertices
      .map: vertex =>
        val colorIdx = indexMap.get(vertex.id)
        val (radiusMultiplier, meta) = colorIdx match
          case Some(idx) => (20.0, attrs("fill" -> uniformColorMap.getOrElse(idx, "red")))
          case None => (2.0, Null)
        vertexToSvg(vertex, vertex.id.toString, config, radiusMultiplier, meta)
      .unzip

  private def createSimpleVertexElements(vertices: List[Vertex], config: SvgConfig): (Seq[Elem], Seq[Elem]) =
    vertices
      .map: vertex =>
        vertexToSvg(vertex, vertex.id.toString, config)
      .unzip

  private def createIndexVertexElements(
    vertices: List[(Vertex, Int)],
    config: SvgConfig
  ): (Seq[Elem], Seq[Elem]) =
    vertices
      .map: (vertex, index) =>
        vertexToSvg(vertex, s"${vertex.id.value} - $index", config)
      .unzip

  private def createFaceLabels(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[Elem] =
    tilingDCEL.innerFaces.map: face =>
      val (x, y) = calculateCentroid(face.getVerticesUnsafe).toSvgCoords(config.scale)
      textAt(x, y, face.id.toString)

  private def createTraversalArrows(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[Elem] =
    if !config.showHalfEdgeTraversal then Nil
    else
      tilingDCEL.innerFaces.flatMap: face =>
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
                arrow <- createArrow(mid1, mid2, config.scale, config.strokeWidth * 2.5)
              yield polygonElem(arrow.formatted)
            case _                 => None

  private def createLeavingEdgeMarkers(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[Elem] =
    if !config.leavingEdgeMarkers then Nil
    else
      tilingDCEL.vertices.flatMap: vertex =>
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

  private def createFaceIdsOnEdges(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[Elem] =
    if !config.faceIdsOnEdges then Nil
    else
      tilingDCEL.halfEdges.flatMap: edge =>
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

          textAt(textX, textY, face.id.toString)

  extension (simple: SimplePolygon)

    private def section(
        vertices: List[BigPoint],
        strokeWidth: Double = 1.0,
        padding: Double = 30.0,
        scale: Double = 50.0,
        showReflection: Boolean = false,
        showRotation: Boolean = false
    ): Elem =

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
          val point = vertices(index).scaled(scale).flippedY
          val x     = (point.x + strokeWidth * 2.5).format
          val y     = (point.y - strokeWidth * 2.5).format
          textAt(x, y, s"$index") // - ${simple.toAngles(index)}°")

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
          case _ :: Nil => NodeSeq.Empty
          case indices  =>
            val ends     = indices.map: index =>
              vertices(index)
            val (x1, y1) = ends.centroid.toSvgCoords(scale)
            ends.map: end =>
              val (x2, y2) = end.toSvgCoords(scale)
              lineElem(x1, y1, x2, y2)

      // Build sections
      val boundarySection =
        boundaryPolygon
          .map: polygon =>
            createSvgSection(
              "Boundary Highlight",
              Seq(polygon),
              attrs("stroke" -> "red", "stroke-width" -> strokeWidth * 3, "fill" -> "none")
            )
          .getOrElse(NodeSeq.Empty)

      val reflection =
        if showReflection then
          createSvgSection(
            "Reflection axes",
            reflections,
            attrs("stroke" -> "green", "stroke-dasharray" -> "2")
          )
        else
          NodeSeq.Empty

      val rotation =
        if showRotation then
          createSvgSection(
            "Rotation axes",
            rotations,
            attrs("stroke" -> "orange", "stroke-width" -> "3", "stroke-dasharray" -> "1")
          )
        else
          NodeSeq.Empty

      val sections = List(
        boundarySection,
        createSvgSection("Vertices", vertexCircles, attrs("fill" -> "darkred")),
        createSvgSection(
          "Vertex Labels",
          vertexLabels,
          attrs("font-size" -> (strokeWidth * 8).toInt, "fill" -> "blue")
        ),
        reflection,
        rotation
      ).flatten
      gElem(sections)

    def toScalableVectorG(
        strokeWidth: Double = 1.0,
        padding: Double = 30.0,
        scale: Double = 50.0,
        showReflection: Boolean = false,
        showRotation: Boolean = false
    ): String =
      val svg: Elem =

        val vertices        = simple.toBigPoints
        val viewBox         = calculateViewBox(vertices, scale, padding)
        val (width, height) = viewBox.dimensions

        svgElem(
          width = width.toString,
          height = height.toString,
          viewBox = viewBox.formatted,
          children =
            Seq(gElem(section(vertices, showReflection = showReflection, showRotation = showRotation)))
        )

      new PrettyPrinter(120, 2).format(svg)

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

//          println(s"origin: $origin, repeat: $repeat, repeatOnOtherAxis: $repeatOnOtherAxis")
          val diffTwo   = (repeat - origin).scaled(1.1)
//          println(s"diffTwo: $diffTwo")
          val diffThree = (repeatOnOtherAxis - origin).scaled(1.1)
//          println(s"diffThree: $diffThree")

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
          val (width, height) = viewBoxAdjusted.dimensions

          val svg =
            svgElem(
              width = width.toString,
              height = height.toString,
              viewBox = viewBoxAdjusted.formatted,
              children = Seq(
                gElem(one),
                gElem(one, attrs("transform" -> s"translate(${diffTwo.x}, ${-diffTwo.y})")),
                gElem(one, attrs("transform" -> s"translate(${diffThree.x}, ${-diffThree.y})")),
                gElem(one, attrs("transform" -> s"translate(${diffFour.x}, ${-diffFour.y})"))
              )
            )

          new PrettyPrinter(120, 2).format(svg)

  extension (tiling: TilingDCEL)

    /** Generates an SVG XML element. */
    def toScalableVectorGraphicsXml(
        strokeWidth: Double = 1.0,
        padding: Double = 20.0,
        scale: Double = 50.0,
        showHalfEdgeTraversal: Boolean = false,
        leavingEdgeMarkers: Boolean = false,
        faceIdsOnEdges: Boolean = false,
        showUniformity: Boolean = false
    ): Elem =
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

      svg

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
      new PrettyPrinter(120, 2)
        .format(
          toScalableVectorGraphicsXml(
            strokeWidth,
            padding,
            scale,
            showHalfEdgeTraversal,
            leavingEdgeMarkers,
            faceIdsOnEdges,
            showUniformity
          )
        )

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

        // Build keyTimes: allocate time proportionally, hold last value during pause
        val stepTimes   =
          (0 until totalSteps).map: i =>
            f"${i * stepDuration / cycleDuration}%.4f"
        val endAnimTime = f"${animationDuration / cycleDuration}%.4f"
        val keyTimes    = (stepTimes :+ endAnimTime :+ "1").mkString(";")

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
        val point = vertex.coords.scaled(scale).flippedY
        val x     = (point.x + strokeWidth * 2.5).format
        val y     = (point.y - strokeWidth * 2.5).format
        sb.append(s"""    <text x="$x" y="$y">${vertex.id.value}</text>""").append("\n")

      sb.append("  </g>\n")

      // Distance label - single text with animated content
      sb.append("\n  <!-- Distance Label -->\n")
      val labelX = viewBox.minX + BigDecimal(10)
      val labelY = viewBox.minY + BigDecimal(20)

      // Multiple overlapping text elements, each visible during its step
      for i <- 0 until totalSteps do
        val stepTimes   = (0 until totalSteps).map(j =>
          f"${
              j * stepDuration / cycleDuration
            }%.4f"
        )
        val endAnimTime = f"${
            animationDuration / cycleDuration
          }%.4f"
        val keyTimes    = (stepTimes :+ endAnimTime :+ "1").mkString(";")

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
        val stepTimes   = (0 until totalSteps).map: j =>
          f"${
              j * stepDuration / cycleDuration
            }%.4f"
        val endAnimTime = f"${
            animationDuration / cycleDuration
          }%.4f"
        val keyTimes    = (stepTimes :+ endAnimTime :+ "1").mkString(";")

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

    /** Serializes the complete structure of a [[TilingDCEL]] into XML metadata, which can be embedded within
      * an SVG. This metadata includes all vertices, half-edges, and faces, along with their properties and
      * relationships, ensuring that the [[TilingDCEL]] can be fully reconstructed later.
      */
    def toMetadataXml: Elem =

      val halfEdgeIds: Map[HalfEdge, Int] = tiling.halfEdges.zipWithIndex.toMap

      val vertexNodes  = tiling.vertices.map: vertex =>
        val attrsList = List(
          Some("id" -> vertex.id.toString),
          Some("x"  -> vertex.coords.x.toString),
          Some("y"  -> vertex.coords.y.toString),
          vertex.leaving
            .flatMap: halfEdge =>
              halfEdgeIds.get(halfEdge)
            .map: id =>
              "leaving" -> id
        ).flatten
        elem("vertex", attrs(attrsList*))
      val verticesElem = elem("vertices", children = vertexNodes)

      val halfEdgeNodes =
        tiling.halfEdges.zipWithIndex
          .map: (halfEdge, id) =>
            val attrsList = List(
              Some("id"     -> id),
              Some("origin" -> halfEdge.origin.id.toString),
              halfEdge.twin
                .flatMap: twinHalfEdge =>
                  halfEdgeIds.get(twinHalfEdge)
                .map: twinId =>
                  "twin" -> twinId,
              halfEdge.next
                .flatMap: nextHalfEdge =>
                  halfEdgeIds.get(nextHalfEdge)
                .map: nextId =>
                  "next" -> nextId,
              halfEdge.prev
                .flatMap: prevHalfEdge =>
                  halfEdgeIds.get(prevHalfEdge)
                .map: prevId =>
                  "prev" -> prevId,
              halfEdge.incidentFace.map: face =>
                "face" -> face.id.toString,
              halfEdge.angle.map: angleDegree =>
                "angle" -> angleDegree.toRational
            ).flatten
            elem("half-edge", attrs(attrsList*))
      val halfEdgesElem = elem("half-edges", children = halfEdgeNodes)

      val faceNodes = tiling.faces.map: f =>
        val attrsList =
          List(
            Some("id" -> f.id.toString),
            f.outerComponent
              .flatMap: halfEdge =>
                halfEdgeIds.get(halfEdge)
              .map: id =>
                "outer-component" -> id
          ).flatten
        val innerIds  =
          f.innerComponents
            .flatMap: maybeHalfEdge =>
              maybeHalfEdge.flatMap: halfEdge =>
                halfEdgeIds.get(halfEdge)
        val allAttrs  = if innerIds.nonEmpty then
          attrsList :+ ("inner-components" -> innerIds.mkString(","))
        else
          attrsList
        elem("face", attrs(allAttrs*))
      val facesElem = elem("faces", children = faceNodes)

      elem(
        "tessella-dcel",
        children = Seq(verticesElem, halfEdgesElem, facesElem),
        prefix = Some("tessella"),
        scope = NamespaceBinding("tessella", "https://github.com/scala-tessella/tessella", TopScope)
      )

    def toMetadata: String =
      toMetadataXml.toString

  /** Deserializes the XML metadata that includes all vertices, half-edges, and faces, along with their
    * properties and relationships, and fully reconstructs the complete structure of a [[TilingDCEL]].
    */
  def fromMetadata(metadata: String): Either[TilingError, TilingDCEL] =
    TilingSVGPlatform.fromMetadata(metadata)

  private[conversion] def reconstructDCEL(
      vertexAttrs: List[Map[String, String]],
      halfEdgeAttrs: List[Map[String, String]],
      faceAttrs: List[Map[String, String]]
  ): Either[TilingError, TilingDCEL] =
    def getAttr(attrs: Map[String, String], owner: String, name: String): Either[ValidationError, String] =
      attrs.get(name)
        .filter:
          _.nonEmpty
        .toRight(ValidationError(s"$owner missing '$name'"))

    def attrAs[T](
        attrs: Map[String, String],
        owner: String,
        name: String,
        f: String => T,
        typeName: String
    ): Either[ValidationError, T] =
      for
        s <- getAttr(attrs, owner, name)
        r <- Try(f(s)).toEither.left.map: e =>
               ValidationError(s"Invalid $typeName in $owner attribute '$name': ${e.getMessage}")
      yield r

    for
      vertices <- vertexAttrs
                    .map: attrs =>
                      for
                        id <- attrAs(attrs, "vertex", "id", _.toInt, "Int")
                        x  <- attrAs(attrs, "vertex", "x", BigDecimal.apply, "BigDecimal")
                        y  <- attrAs(attrs, "vertex", "y", BigDecimal.apply, "BigDecimal")
                      yield Vertex(VertexId(id), BigPoint(x, y))
                    .sequence
      vertexMap = vertices.associateValues:
                    _.id

      heIdAndAttrs <- halfEdgeAttrs
                        .map: attrs =>
                          for id <- attrAs(attrs, "half-edge", "id", _.toInt, "Int")
                          yield id -> attrs
                        .sequence
      heAllocated  <- heIdAndAttrs
                        .map: (id, attrs) =>
                          for
                            originId <- attrAs(attrs, "half-edge", "origin", _.toInt, "Int")
                            origin   <- vertexMap.get(VertexId(originId)).toRight(
                                          NotFoundError("Vertex for half-edge origin", originId.toString)
                                        )
                          yield id -> HalfEdge(origin)
                        .sequence
      halfEdgeMap   = heAllocated.toMap
      halfEdges     = heAllocated
                        .sortBy((id, _) => id)
                        .map((_, halfEdge) => halfEdge)

      faces  <- faceAttrs.map(attrs =>
                  for id <- attrAs(attrs, "face", "id", _.toInt, "Int")
                  yield Face(FaceId(id))
                ).sequence
      faceMap = faces.associateValues:
                  _.id

      _ <- vertexAttrs
             .zip(vertices)
             .map: (attrs, vertex) =>
               attrs.get("leaving").traverse: leavingIdStr =>
                 for
                   leavingId   <- Try(leavingIdStr.toInt).toEither.left.map: _ =>
                                    ValidationError(s"Invalid leaving ID: $leavingIdStr")
                   leavingEdge <- halfEdgeMap.get(leavingId).toRight(
                                    NotFoundError("Leaving edge", leavingId.toString)
                                  )
                 yield vertex.leaving = Some(leavingEdge)
             .sequence

      _ <- heIdAndAttrs.map((id, attrs) =>
             val he = halfEdgeMap(id)
             for
               twinId       <- attrAs(attrs, "half-edge", "twin", _.toInt, "Int")
               twinEdge     <- halfEdgeMap.get(twinId).toRight(NotFoundError("Twin edge", twinId.toString))
               _             = he.twin = Some(twinEdge)
               nextId       <- attrAs(attrs, "half-edge", "next", _.toInt, "Int")
               nextEdge     <- halfEdgeMap.get(nextId).toRight(NotFoundError("Next edge", nextId.toString))
               _             = he.next = Some(nextEdge)
               prevId       <- attrAs(attrs, "half-edge", "prev", _.toInt, "Int")
               prevEdge     <- halfEdgeMap.get(prevId).toRight(NotFoundError("Prev edge", prevId.toString))
               _             = he.prev = Some(prevEdge)
               faceId       <- attrAs(attrs, "half-edge", "face", _.toInt, "Int")
               incidentFace <- faceMap.get(FaceId(faceId)).toRight(
                                 NotFoundError("Incident face", faceId.toString)
                               )
               _             = he.incidentFace = Some(incidentFace)
               angleStr     <- getAttr(attrs, "half-edge", "angle")
               _             = he.angle = Some(AngleDegree(Rational(angleStr)))
             yield ()
           ).sequence

      _ <- faceAttrs.zip(faces)
             .map: (attrs, f) =>
               for
                 _ <- attrs.get("outer-component").traverse(ocIdStr =>
                        for
                          ocId   <- Try(ocIdStr.toInt).toEither.left.map: _ =>
                                      ValidationError(s"Invalid outer-component ID: $ocIdStr")
                          ocEdge <- halfEdgeMap.get(ocId).toRight(
                                      NotFoundError("Outer component edge", ocId.toString)
                                    )
                        yield f.outerComponent = Some(ocEdge)
                      )
                 _ <- attrs.get("inner-components")
                        .filter:
                          _.nonEmpty
                        .traverse: icIdsStr =>
                          for
                            ids     <- icIdsStr.split(',').toList
                                         .map: idStr =>
                                           Try(idStr.trim.toInt).toEither.left.map: _ =>
                                             ValidationError(s"Invalid inner-component ID: $idStr")
                                         .sequence
                            icEdges <- ids
                                         .map: id =>
                                           halfEdgeMap.get(id).toRight(
                                             NotFoundError("Inner component edge", id.toString)
                                           )
                                         .sequence
                          yield f.innerComponents = icEdges.map:
                            Some(_)
               yield ()
             .sequence

      outerFace <- faceMap.get(FaceId.outerId).toRight(
                     ValidationError("Outer face (ID 0) not found in metadata")
                   )
      innerFaces = faces.filterNot:
                     _.id == FaceId.outerId
      tiling     = TilingDCEL.fromUntrusted(vertices, halfEdges, innerFaces, outerFace)
      validated <- if vertices.isEmpty && halfEdges.isEmpty && innerFaces.isEmpty then
        Right(TilingDCEL.empty)
      else tiling
    yield validated
