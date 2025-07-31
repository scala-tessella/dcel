package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{BigLineSegment, BigPoint, BigRadian, format}

import scala.collection.mutable
import scala.xml.*

object TilingSVG:

  extension (bigPoint: BigPoint)

    def toSvgCoords(scale: Double): (String, String) =
      val scaledPoint: BigPoint = bigPoint.scaled(scale).flippedY
      (scaledPoint.x.format, scaledPoint.y.format)

  case class Arrow(tipX: String, tipY: String, baseX1: String, baseY1: String, baseX2: String, baseY2: String)

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

  private def createAngleLabel(halfEdge: HalfEdge, direction: BigPoint, scale: Double, strokeWidth: Double): Elem =
    val origin = halfEdge.origin.coords
    val angleText = f"${halfEdge.angle.get.toRational.toDouble}%.0f°"
    val labelDistance = strokeWidth * 8
    val labelX = (origin.x * scale + direction.x * labelDistance).format
    val labelY = (-origin.y * scale - direction.y * labelDistance).format
    <text x={labelX} y={labelY}>{angleText}</text>

  private def createSvgSection(title: String, content: Seq[Node], attributes: MetaData = Null): NodeSeq =
    if content.nonEmpty then
      Seq(
        Comment(s" $title "),
        <g>{content}</g> % attributes
      )
    else
      NodeSeq.Empty

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

      val vertices = tilingDCEL.vertices.map(_.coords)
      val scaledVertices = vertices.map(_.scaled(scale).flippedY)
      val minX = scaledVertices.map(_.x).min
      val maxX = scaledVertices.map(_.x).max
      val minY = scaledVertices.map(_.y).min
      val maxY = scaledVertices.map(_.y).max
      val viewBoxTuple = (minX - padding, minY - padding, (maxX - minX) + 2 * padding, (maxY - minY) + 2 * padding)
      val (width, height) = (viewBoxTuple._3.toInt, viewBoxTuple._4.toInt)

      // Generate edge lines
      val drawnEdges = mutable.Set.empty[HalfEdge]
      val edgeLines: Seq[Elem] =
        tilingDCEL.halfEdges.flatMap { edge =>
          if drawnEdges.contains(edge) || edge.twin.isEmpty then
            None
          else
            val twin = edge.twin.get
            drawnEdges ++= List(edge, twin)
            val (x1, y1) = edge.origin.coords.toSvgCoords(scale)
            val (x2, y2) = twin.origin.coords.toSvgCoords(scale)
            Some(<line x1={x1} y1={y1} x2={x2} y2={y2}/>)
        }

      // Generate arrows for half-edges
      def createHalfEdgeArrows(halfEdges: List[HalfEdge]): Seq[Elem] =
        halfEdges.flatMap { halfEdge =>
          for
            destination <- halfEdge.destination
            arrow <- createArrow(halfEdge.origin.coords, destination.coords, scale, strokeWidth * 3)
          yield <polygon points={s"${arrow.tipX},${arrow.tipY} ${arrow.baseX1},${arrow.baseY1} ${arrow.baseX2},${arrow.baseY2}"}/>
        }

      val innerFaceArrows = createHalfEdgeArrows(tilingDCEL.innerFaces.flatMap(_.halfEdgesSafe))
      val outerFaceArrows = createHalfEdgeArrows(tilingDCEL.getBoundaryEdges.getOrElse(Nil))

      // Generate angle labels
      val innerAngleLabels: Seq[Elem] =
        tilingDCEL.innerFaces.flatMap { face =>
          val centroid = calculateCentroid(face.getVertices.getOrElse(List.empty))
          face.halfEdgesSafe.map { halfEdge =>
            val origin = halfEdge.origin.coords
            val direction = calculateDirection(origin, centroid)
            createAngleLabel(halfEdge, direction, scale, strokeWidth)
          }
        }
      val outerAngleLabels: Seq[Elem] =
        tilingDCEL.getBoundaryEdges.getOrElse(Nil).map { halfEdge =>
          val centroid =
            if tilingDCEL.innerFaces.nonEmpty then
              calculateCentroid(tilingDCEL.innerFaces.head.getVertices.getOrElse(List.empty))
            else
              BigPoint(BigDecimal(0), BigDecimal(0))
          val origin = halfEdge.origin.coords
          val inwardDirection = calculateDirection(origin, centroid)
          val outwardDirection = BigPoint(-inwardDirection.x, -inwardDirection.y)
          createAngleLabel(halfEdge, outwardDirection, scale, strokeWidth)
        }

      // Generate boundary polygon and arrows
      val (boundaryPolygon, boundaryArrows): (Option[Elem], Seq[Elem]) =
        tilingDCEL.boundary match
          case vertices if vertices.nonEmpty =>
            val points = vertices.map { v =>
              val (x, y) = v.coords.toSvgCoords(scale)
              s"$x,$y"
            }.mkString(" ")
            val arrows = vertices.zipWithIndex.flatMap {
              case (v1, i) =>
                val v2 = vertices((i + 1) % vertices.length)
                createArrow(v1.coords, v2.coords, scale, strokeWidth * 6)
                  .map(a => <polygon points={s"${a.tipX},${a.tipY} ${a.baseX1},${a.baseY1} ${a.baseX2},${a.baseY2}"}/>)
            }
            (Some(<polygon points={points}/>), arrows)
          case _ =>
            (None, Nil)

      // Generate vertex elements
      val vertexCircles: Seq[Elem] =
        tilingDCEL.vertices.map { v =>
          val (cx, cy) = v.coords.toSvgCoords(scale)
            <circle cx={cx} cy={cy} r={(strokeWidth * 2).toString}/>
        }
      val vertexLabels: Seq[Elem] =
        tilingDCEL.vertices.map { v =>
          val point = v.coords.scaled(scale).flippedY
          val x = (point.x + strokeWidth * 2.5).format
          val y = (point.y - strokeWidth * 2.5).format
          <text x={x} y={y}>{v.id}</text>
        }

      // Generate face elements
      val faceLabels: Seq[Elem] =
        tilingDCEL.innerFaces.flatMap { face =>
          val vertices = face.getVertices.getOrElse(List.empty)
          if vertices.nonEmpty then
            val (x, y) = calculateCentroid(vertices).toSvgCoords(scale)
            Some(<text x={x} y={y}>{face.id}</text>)
          else
            None
        }

      val traversalArrows: Seq[Elem] =
        if showHalfEdgeTraversal then
          tilingDCEL.innerFaces.flatMap { face =>
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
                  yield <polygon points={s"${arrow.tipX},${arrow.tipY} ${arrow.baseX1},${arrow.baseY1} ${arrow.baseX2},${arrow.baseY2}"}/>
                case _ =>
                  None
              }
            else
              Nil
          }
        else
          Nil

      val leavingEdgeMarkersSvg: Seq[Elem] =
        if leavingEdgeMarkers then
          tilingDCEL.vertices.flatMap { vertex =>
            for
              edge <- vertex.leaving
              dest <- edge.destination
              arrow <- createArrow(vertex.coords, BigLineSegment(vertex.coords, dest.coords).midPoint, scale, strokeWidth * 2)
            yield <polygon points={s"${arrow.tipX},${arrow.tipY} ${arrow.baseX1},${arrow.baseY1} ${arrow.baseX2},${arrow.baseY2}"}/>
          }
        else Nil

      val faceIdsOnEdgesSvg: Seq[Elem] =
        if faceIdsOnEdges then
          tilingDCEL.halfEdges.flatMap { edge =>
            for
              dest <- edge.destination
              face <- edge.incidentFace
            yield
              val origin = edge.origin.coords
              val destination = dest.coords
              val segment = BigLineSegment(origin, destination)
              val midPoint = segment.midPoint
              val directionAngle = origin.angleTo(destination)
              val perpAngle = directionAngle + BigRadian.TAU_4
              val unitPerpendicular = BigPoint.fromPolar(BigDecimal(1.0), perpAngle)
              val offset = unitPerpendicular.scaled((strokeWidth * 4).toInt)
              val (textX, textY) = midPoint.plus(offset).toSvgCoords(scale)
              <text x={textX} y={textY}>{face.id}</text>
          }
        else Nil

      // Build sections
      def attrs(tuples: (String, Any)*): MetaData =
        tuples.foldRight[MetaData](Null) { case ((key, value), acc) =>
          new UnprefixedAttribute(key, value.toString, acc)
        }

      val boundarySection = boundaryPolygon.map(polygon =>
        createSvgSection("Boundary Highlight", Seq(polygon), attrs("stroke" -> "red", "stroke-width" -> strokeWidth * 3, "fill" -> "none")) ++
          createSvgSection("Boundary Direction Arrows", boundaryArrows, attrs("fill" -> "red", "stroke" -> "red", "stroke-width" -> strokeWidth * 0.5))
      ).getOrElse(NodeSeq.Empty)

      val sections =
        List(
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

      val formattedViewBox =
        s"${viewBoxTuple._1.format} ${viewBoxTuple._2.format} ${viewBoxTuple._3.format} ${viewBoxTuple._4.format}"
      val svg =
        <svg width={width.toString} height={height.toString} viewBox={formattedViewBox} xmlns="http://www.w3.org/2000/svg">
          <g>
            {sections}
          </g>
        </svg>

      new PrettyPrinter(120, 2).format(svg)