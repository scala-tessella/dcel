package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.geometry.*
import io.github.scala_tessella.dcel.conversion.SvgDsl.*
import io.github.scala_tessella.dcel.conversion.SvgRendering.*
import io.github.scala_tessella.ring_seq.SymmetryOps.{
  AxisLocation,
  Edge as SymmetryEdge,
  Vertex as SymmetryVertex
}

/** SVG export for individual [[geometry.SimplePolygon]] shapes: [[toScalableVectorG]] renders the polygon
  * (optionally with its reflection/rotation symmetry axes), [[toParallelogonTiling]] shows how a parallelogon
  * tiles the plane by translation. Tiling-level SVG export lives in [[TilingSVG]].
  */
object SimplePolygonSVG:

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

    /** Renders the polygon as an SVG string at unit scale (subject to `scale`). Used by `SimplePolygon.toSVG`
      * and available directly when you want to control rendering knobs.
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
