package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.TilingDCEL
import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.*
import io.github.scala_tessella.dcel.geometry.*
import io.github.scala_tessella.dcel.structure.{Face, HalfEdge, Vertex}
import io.github.scala_tessella.dcel.conversion.SvgDsl.*

import scala.collection.mutable

/** Shared SVG drawing primitives behind [[TilingSVG]], [[SimplePolygonSVG]] and [[SvgAnimation]]:
  * model-to-SVG coordinate mapping, arrow heads, section assembly, the uniformity colour palette and the
  * deterministic (id-sorted) component orderings every renderer draws in.
  */
private[conversion] object SvgRendering:

  extension (bigPoint: BigPoint)
    private[conversion] def toSvgCoords(scale: Double): (String, String) =
      val scaledPoint: BigPoint = bigPoint.scaled(scale).flippedY
      (scaledPoint.x.format, scaledPoint.y.format)

    private[conversion] def toSvgLabelCoords(
        scale: Double,
        offsetX: Double,
        offsetY: Double
    ): (String, String) =
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

  private[conversion] def calculateCentroid(vertices: List[Vertex]): BigPoint =
    vertices.map(_.coords).centroid

  private[conversion] def calculateDirection(from: BigPoint, to: BigPoint): BigPoint =
    BigPoint.fromPolar(BigDecimal(1.0), from.angleTo(to))

  private[conversion] def createArrowsFromPoints(
      segments: Seq[(BigPoint, BigPoint)],
      scale: Double,
      arrowSize: Double
  ): Seq[String] =
    segments.flatMap: (origin, destination) =>
      createArrow(origin, destination, scale, arrowSize).map: arrow =>
        polygonElem(arrow.formatted)

  private[conversion] def createSvgSection(
      title: String,
      content: Seq[String],
      attributes: Attrs = Nil
  ): Option[String] =
    if content.isEmpty then None
    else Some(Seq(comment(title), gElem(content, attributes)).mkString("\n"))

  private[conversion] def assembleSections(sections: Option[String]*): Seq[String] =
    sections.flatten.toSeq

  private[conversion] def sortedVertices(tiling: TilingDCEL): List[Vertex] =
    tiling.vertices.sortBy:
      _.id.value

  private[conversion] def sortedInnerFaces(tiling: TilingDCEL): List[Face] =
    tiling.innerFaces.sortBy:
      _.id.value

  private[conversion] def sortedHalfEdges(tiling: TilingDCEL): List[HalfEdge] =
    tiling.halfEdges.sortBy:
      _.idUnsafe

  private[conversion] def sortedBoundaryEdges(tiling: TilingDCEL): List[HalfEdge] =
    tiling.boundaryEdgesUnsafe.sortBy:
      _.idUnsafe

  private[conversion] def createEdgeLines(tilingDCEL: TilingDCEL, scale: Double): Seq[String] =
    val drawnEdges = mutable.Set.empty[HalfEdge]
    sortedHalfEdges(tilingDCEL).flatMap: halfEdge =>
      if drawnEdges.contains(halfEdge) || halfEdge.twin.isEmpty then None
      else
        val twin     = halfEdge.twin.get
        drawnEdges ++= List(halfEdge, twin)
        val (x1, y1) = halfEdge.origin.coords.toSvgCoords(scale)
        val (x2, y2) = twin.origin.coords.toSvgCoords(scale)
        Some(lineElem(x1, y1, x2, y2))

  private[conversion] def uniformColorMap: Map[Int, String] =
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
