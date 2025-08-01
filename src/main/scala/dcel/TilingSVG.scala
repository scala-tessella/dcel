package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{BigLineSegment, BigPoint, BigRadian, format}

import spire.implicits.*

import scala.collection.mutable
import scala.xml.*

object TilingSVG:

  extension (bigPoint: BigPoint)
    def toSvgCoords(scale: Double): (String, String) =
      val scaledPoint: BigPoint = bigPoint.scaled(scale).flippedY
      (scaledPoint.x.format, scaledPoint.y.format)

  case class Arrow(tipX: String, tipY: String, baseX1: String, baseY1: String, baseX2: String, baseY2: String)

  case class ViewBox(minX: BigDecimal, minY: BigDecimal, width: BigDecimal, height: BigDecimal):
    val formatted: String = s"${minX.format} ${minY.format} ${width.format} ${height.format}"
    val dimensions: (Int, Int) = (width.toInt, height.toInt)

  case class SvgConfig(strokeWidth: Double, padding: Double, scale: Double,
                       showHalfEdgeTraversal: Boolean, leavingEdgeMarkers: Boolean, faceIdsOnEdges: Boolean)

  private def createArrow(origin: BigPoint, destination: BigPoint, scale: Double, arrowSize: Double): Option[Arrow] =
    val distance = origin.distanceTo(destination)
    if distance <= BigDecimal(BigDecimalGeometry.ACCURACY) then None
    else
      val segment = BigLineSegment(origin, destination)
      val midPoint = segment.midPoint
      val directionAngle = origin.angleTo(destination)
      val unitDirection = BigPoint.fromPolar(BigDecimal(1.0), directionAngle)
      val perpAngle = directionAngle + BigRadian.TAU_4
      val unitPerpendicular = BigPoint.fromPolar(BigDecimal(1.0), perpAngle)

      val scaledMidPoint = midPoint.scaled(scale).flippedY
      val arrowSizeBD = BigDecimal(arrowSize)
      val baseOffset = arrowSizeBD * BigDecimal(0.4)

      val tip = scaledMidPoint.plus(BigPoint(unitDirection.x * arrowSizeBD, -unitDirection.y * arrowSizeBD))
      val base1 = scaledMidPoint.plus(BigPoint(unitPerpendicular.x * baseOffset, -unitPerpendicular.y * baseOffset))
      val base2 = scaledMidPoint.plus(BigPoint(unitPerpendicular.x * (-baseOffset), -unitPerpendicular.y * (-baseOffset)))

      Some(Arrow(tip.x.format, tip.y.format, base1.x.format, base1.y.format, base2.x.format, base2.y.format))

  private def calculateCentroid(vertices: List[Vertex]): BigPoint =
    vertices match
      case Nil => BigPoint(0, 0)
      case vs =>
        val (sumX, sumY) = vs.map(_.coords).foldLeft((BigDecimal(0), BigDecimal(0))) {
          case ((x, y), point) => (x + point.x, y + point.y)
        }
        BigPoint(sumX / vs.length, sumY / vs.length)

  private def calculateDirection(from: BigPoint, to: BigPoint): BigPoint =
    BigPoint.fromPolar(BigDecimal(1.0), from.angleTo(to))

  private def createAngleLabel(halfEdge: HalfEdge, direction: BigPoint, config: SvgConfig): Elem =
    val origin = halfEdge.origin.coords
    val angleText = f"${halfEdge.angle.get.toRational.toDouble}%.0f°"
    val labelDistance = config.strokeWidth * 8
    val labelX = (origin.x * config.scale + direction.x * labelDistance).format
    val labelY = (-origin.y * config.scale - direction.y * labelDistance).format
    <text x={labelX} y={labelY}>{angleText}</text>

  private def createSvgSection(title: String, content: Seq[Node], attributes: MetaData = Null): NodeSeq =
    if content.isEmpty then NodeSeq.Empty
    else Seq(Comment(s" $title "), <g>{content}</g> % attributes)

  private def calculateViewBox(vertices: List[BigPoint], scale: Double, padding: Double): ViewBox =
    if vertices.isEmpty then ViewBox(BigDecimal(0), BigDecimal(0), BigDecimal(0), BigDecimal(0))
    else
      val scaledVertices = vertices.map(_.scaled(scale).flippedY)
      val xs = scaledVertices.map(_.x)
      val ys = scaledVertices.map(_.y)
      val (minX, maxX, minY, maxY) = (xs.min, xs.max, ys.min, ys.max)
      ViewBox(minX - padding, minY - padding, (maxX - minX) + 2 * padding, (maxY - minY) + 2 * padding)

  private def createHalfEdgeArrows(halfEdges: List[HalfEdge], config: SvgConfig): Seq[Elem] =
    halfEdges.flatMap { halfEdge =>
      for
        destination <- halfEdge.destination
        arrow <- createArrow(halfEdge.origin.coords, destination.coords, config.scale, config.strokeWidth * 3)
      yield <polygon points={s"${arrow.tipX},${arrow.tipY} ${arrow.baseX1},${arrow.baseY1} ${arrow.baseX2},${arrow.baseY2}"}/>
    }

  private def createEdgeLines(tilingDCEL: TilingDCEL, scale: Double): Seq[Elem] =
    val drawnEdges = mutable.Set.empty[HalfEdge]
    tilingDCEL.halfEdges.flatMap { edge =>
      if drawnEdges.contains(edge) || edge.twin.isEmpty then None
      else
        val twin = edge.twin.get
        drawnEdges ++= List(edge, twin)
        val (x1, y1) = edge.origin.coords.toSvgCoords(scale)
        val (x2, y2) = twin.origin.coords.toSvgCoords(scale)
        Some(<line x1={x1} y1={y1} x2={x2} y2={y2}/>)
    }

  private def createAngleLabels(tilingDCEL: TilingDCEL, config: SvgConfig): (Seq[Elem], Seq[Elem]) =
    val innerAngleLabels = tilingDCEL.innerFaces.flatMap { face =>
      val centroid = calculateCentroid(face.getVertices.getOrElse(List.empty))
      face.halfEdgesSafe.map { halfEdge =>
        val direction = calculateDirection(halfEdge.origin.coords, centroid)
        createAngleLabel(halfEdge, direction, config)
      }
    }

    val outerAngleLabels = tilingDCEL.getBoundaryEdges.getOrElse(Nil).map { halfEdge =>
      val centroid = tilingDCEL.innerFaces.headOption
        .flatMap(_.getVertices.toOption)
        .filter(_.nonEmpty)
        .map(calculateCentroid)
        .getOrElse(BigPoint(0, 0))
      val inwardDirection = calculateDirection(halfEdge.origin.coords, centroid)
      val outwardDirection = BigPoint(-inwardDirection.x, -inwardDirection.y)
      createAngleLabel(halfEdge, outwardDirection, config)
    }

    (innerAngleLabels, outerAngleLabels)

  private def createBoundaryElements(tilingDCEL: TilingDCEL, config: SvgConfig): Option[Elem] =
    tilingDCEL.boundary match
      case vertices if vertices.nonEmpty =>
        val points = vertices.map { v =>
          val (x, y) = v.coords.toSvgCoords(config.scale)
          s"$x,$y"
        }.mkString(" ")
        Some(<polygon points={points}/>)
      case _ => None

  private def createVertexElements(tilingDCEL: TilingDCEL, config: SvgConfig): (Seq[Elem], Seq[Elem]) =
    val circles = tilingDCEL.vertices.map { v =>
      val (cx, cy) = v.coords.toSvgCoords(config.scale)
        <circle cx={cx} cy={cy} r={(config.strokeWidth * 2).toString}/>
    }

    val labels = tilingDCEL.vertices.map { v =>
      val point = v.coords.scaled(config.scale).flippedY
      val x = (point.x + config.strokeWidth * 2.5).format
      val y = (point.y - config.strokeWidth * 2.5).format
      <text x={x} y={y}>{v.id}</text>
    }

    (circles, labels)

  private def createFaceLabels(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[Elem] =
    tilingDCEL.innerFaces.flatMap { face =>
      face.getVertices.toOption.filter(_.nonEmpty).map { vertices =>
        val (x, y) = calculateCentroid(vertices).toSvgCoords(config.scale)
        <text x={x} y={y}>{face.id}</text>
      }
    }

  private def createTraversalArrows(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[Elem] =
    if !config.showHalfEdgeTraversal then Nil
    else tilingDCEL.innerFaces.flatMap { face =>
      val halfEdges = face.halfEdgesSafe
      if halfEdges.length <= 1 then Nil
      else
        val looped = halfEdges :+ halfEdges.head
        looped.sliding(2).flatMap {
          case he1 :: he2 :: Nil =>
            for
              dest1 <- he1.destination
              dest2 <- he2.destination
              mid1 = BigLineSegment(he1.origin.coords, dest1.coords).midPoint
              mid2 = BigLineSegment(he2.origin.coords, dest2.coords).midPoint
              arrow <- createArrow(mid1, mid2, config.scale, config.strokeWidth * 2.5)
            yield <polygon points={s"${arrow.tipX},${arrow.tipY} ${arrow.baseX1},${arrow.baseY1} ${arrow.baseX2},${arrow.baseY2}"}/>
          case _ => None
        }
    }

  private def createLeavingEdgeMarkers(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[Elem] =
    if !config.leavingEdgeMarkers then Nil
    else tilingDCEL.vertices.flatMap { vertex =>
      for
        edge <- vertex.leaving
        dest <- edge.destination
        arrow <- createArrow(vertex.coords, BigLineSegment(vertex.coords, dest.coords).midPoint, config.scale, config.strokeWidth * 2)
      yield <polygon points={s"${arrow.tipX},${arrow.tipY} ${arrow.baseX1},${arrow.baseY1} ${arrow.baseX2},${arrow.baseY2}"}/>
    }

  private def createFaceIdsOnEdges(tilingDCEL: TilingDCEL, config: SvgConfig): Seq[Elem] =
    if !config.faceIdsOnEdges then Nil
    else tilingDCEL.halfEdges.flatMap { edge =>
      for
        dest <- edge.destination
        face <- edge.incidentFace
      yield
        val origin = edge.origin.coords
        val destination = dest.coords
        val segment = BigLineSegment(origin, destination)
        val midPoint = segment.midPoint

        // Transform midpoint to SVG coordinates first
        val (midX, midY) = midPoint.toSvgCoords(config.scale)

        // Calculate direction in SVG coordinate space
        val (originX, originY) = origin.toSvgCoords(config.scale)
        val (destX, destY) = destination.toSvgCoords(config.scale)

        val dx = BigDecimal(destX) - BigDecimal(originX)
        val dy = BigDecimal(destY) - BigDecimal(originY)

        // Calculate perpendicular offset in SVG space (to the left of direction)
        val offsetDistance = config.strokeWidth * 4
        val length = spire.math.sqrt(dx.pow(2) + dy.pow(2))

        val perpX = if length > BigDecimal(BigDecimalGeometry.ACCURACY) then -dy * offsetDistance / length else BigDecimal(0)
        val perpY = if length > BigDecimal(BigDecimalGeometry.ACCURACY) then dx * offsetDistance / length else BigDecimal(0)

        val textX = (BigDecimal(midX) - perpX).format
        val textY = (BigDecimal(midY) - perpY).format

        <text x={textX} y={textY}>{face.id}</text>
    }

  // Helper to create MetaData more idiomatically
  private def attrs(tuples: (String, Any)*): MetaData =
    tuples.foldRight[MetaData](Null) { case ((key, value), acc) =>
      new UnprefixedAttribute(key, value.toString, acc)
    }

  extension (tilingDCEL: TilingDCEL)

    /**
     * Generates an SVG representation of the tiling.
     * The width, height, and viewBox are automatically calculated to fit the tiling at the given scale.
     */
    def toScalableVectorGraphics(
      strokeWidth: Double = 1.0,
      padding: Double = 20.0,
      scale: Double = 50.0,
      showHalfEdgeTraversal: Boolean = false,
      leavingEdgeMarkers: Boolean = false,
      faceIdsOnEdges: Boolean = false
    ): String =
      if tilingDCEL.vertices.isEmpty then
        return <svg width="0" height="0"></svg>.toString()

      val config = SvgConfig(strokeWidth, padding, scale, showHalfEdgeTraversal, leavingEdgeMarkers, faceIdsOnEdges)
      val vertices = tilingDCEL.vertices.map(_.coords)
      val viewBox = calculateViewBox(vertices, scale, padding)
      val (width, height) = viewBox.dimensions

      // Generate all elements
      val edgeLines = createEdgeLines(tilingDCEL, scale)
      val innerFaceArrows = createHalfEdgeArrows(tilingDCEL.innerFaces.flatMap(_.halfEdgesSafe), config)
      val outerFaceArrows = createHalfEdgeArrows(tilingDCEL.getBoundaryEdges.getOrElse(Nil), config)
      val (innerAngleLabels, outerAngleLabels) = createAngleLabels(tilingDCEL, config)
      val boundaryPolygon = createBoundaryElements(tilingDCEL, config)
      val (vertexCircles, vertexLabels) = createVertexElements(tilingDCEL, config)
      val faceLabels = createFaceLabels(tilingDCEL, config)
      val traversalArrows = createTraversalArrows(tilingDCEL, config)
      val leavingEdgeMarkersSvg = createLeavingEdgeMarkers(tilingDCEL, config)
      val faceIdsOnEdgesSvg = createFaceIdsOnEdges(tilingDCEL, config)

      // Build sections
      val boundarySection = boundaryPolygon.map(polygon =>
        createSvgSection("Boundary Highlight", Seq(polygon), attrs("stroke" -> "red", "stroke-width" -> strokeWidth * 3, "fill" -> "none"))
      ).getOrElse(NodeSeq.Empty)

      val sections = List(
        createSvgSection("Edges", edgeLines, attrs("stroke" -> "black", "stroke-width" -> strokeWidth)),
        boundarySection,
        createSvgSection("Inner Face Half-Edge Direction Arrows", innerFaceArrows, attrs("fill" -> "blue", "stroke" -> "blue", "stroke-width" -> strokeWidth * 0.5)),
        createSvgSection("Outer Face Half-Edge Direction Arrows", outerFaceArrows, attrs("fill" -> "black", "stroke" -> "black", "stroke-width" -> strokeWidth * 0.5)),
        createSvgSection("Half-Edge Face Traversal", traversalArrows, attrs("fill" -> "darkcyan", "stroke" -> "darkcyan", "stroke-width" -> strokeWidth * 0.5)),
        createSvgSection("Leaving Edge Markers", leavingEdgeMarkersSvg, attrs("fill" -> "yellow", "stroke" -> "yellow", "stroke-width" -> strokeWidth * 1.5)),
        createSvgSection("Face Ids On Edges Labels", faceIdsOnEdgesSvg, attrs("font-size" -> (strokeWidth * 4).toInt, "text-anchor" -> "middle", "alignment-baseline" -> "middle", "fill" -> "blue")),
        createSvgSection("Vertices", vertexCircles, attrs("fill" -> "red")),
        createSvgSection("Vertex Labels", vertexLabels, attrs("font-size" -> (strokeWidth * 8).toInt, "fill" -> "darkblue")),
        createSvgSection("Face Labels", faceLabels, attrs("font-size" -> (strokeWidth * 6).toInt, "fill" -> "green", "text-anchor" -> "middle", "dominant-baseline" -> "middle")),
        createSvgSection("Inner Angle Labels", innerAngleLabels, attrs("font-size" -> (strokeWidth * 5).toInt, "fill" -> "purple", "text-anchor" -> "middle", "dominant-baseline" -> "middle")),
        createSvgSection("Outer Angle Labels", outerAngleLabels, attrs("font-size" -> (strokeWidth * 5).toInt, "fill" -> "orange", "text-anchor" -> "middle", "dominant-baseline" -> "middle"))
      ).flatten

      val svg =
        <svg width={width.toString} height={height.toString} viewBox={viewBox.formatted} xmlns="http://www.w3.org/2000/svg">
          <g>
            {sections}
          </g>
        </svg>

      new PrettyPrinter(120, 2).format(svg)