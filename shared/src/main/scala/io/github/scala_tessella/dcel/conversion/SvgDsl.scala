package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.*
import io.github.scala_tessella.dcel.geometry.BigPoint

import scala.math.BigDecimal.RoundingMode

/** Low-level XML/tag builders and view-box helpers used by the SVG exporter. Kept separate from the
  * domain-specific rendering logic in [[TilingSVG]] so both rendering and metadata round-trip can share the
  * same primitives.
  */
private[conversion] object SvgDsl:

  type Attrs = Seq[(String, String)]

  def attrs(tuples: (String, Any)*): Attrs =
    tuples.map: (key, value) =>
      key -> value.toString

  def renderAttrs(attributes: Attrs): String =
    if attributes.isEmpty then ""
    else
      attributes
        .map: (key, value) =>
          s""" $key="$value""""
        .mkString

  def escapeText(content: String): String =
    content
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")

  def tag(
      label: String,
      attributes: Attrs = Nil,
      children: Seq[String] = Nil,
      selfClosing: Boolean = false
  ): String =
    if children.isEmpty && selfClosing then s"<$label${renderAttrs(attributes)}/>"
    else
      val body = children.mkString("\n")
      s"<$label${renderAttrs(attributes)}>$body</$label>"

  def comment(text: String): String =
    s"<!-- $text -->"

  def textAt(x: String, y: String, content: String, more: Attrs = Nil): String =
    val attributes = attrs("x" -> x, "y" -> y) ++ more
    tag("text", attributes, Seq(escapeText(content)))

  def polygonElem(points: String, more: Attrs = Nil): String =
    tag("polygon", attrs("points" -> points) ++ more, selfClosing = true)

  def lineElem(x1: String, y1: String, x2: String, y2: String, more: Attrs = Nil): String =
    tag("line", attrs("x1" -> x1, "y1" -> y1, "x2" -> x2, "y2" -> y2) ++ more, selfClosing = true)

  def circleElem(cx: String, cy: String, r: String, more: Attrs = Nil): String =
    tag("circle", attrs("cx" -> cx, "cy" -> cy, "r" -> r) ++ more, selfClosing = true)

  def gElem(children: Seq[String], attributes: Attrs = Nil): String =
    tag("g", attributes, children)

  def svgElem(width: String, height: String, viewBox: String, children: Seq[String]): String =
    tag(
      "svg",
      attrs(
        "width"   -> width,
        "height"  -> height,
        "viewBox" -> viewBox,
        "xmlns"   -> "http://www.w3.org/2000/svg"
      ),
      children
    )

  case class ViewBox(minX: BigDecimal, minY: BigDecimal, width: BigDecimal, height: BigDecimal):
    private def rounded(value: BigDecimal): BigDecimal =
      value.setScale(0, RoundingMode.CEILING)
    val formatted: String                              =
      s"${minX.format} ${minY.format} ${rounded(width).format} ${rounded(height).format}"
    val dimensions: (Int, Int)                         = (rounded(width).toInt, rounded(height).toInt)

  def calculateViewBox(vertices: List[BigPoint], scale: Double, padding: Double): ViewBox =
    if vertices.isEmpty then ViewBox(BigDecimal(0), BigDecimal(0), BigDecimal(0), BigDecimal(0))
    else
      val scaledVertices           = vertices.map(_.scaled(scale).flippedY)
      val xs                       = scaledVertices.map(_.x)
      val ys                       = scaledVertices.map(_.y)
      val (minX, maxX, minY, maxY) = (xs.min, xs.max, ys.min, ys.max)
      ViewBox(minX - padding, minY - padding, (maxX - minX) + 2 * padding, (maxY - minY) + 2 * padding)

  def svgWithViewBox(viewBox: ViewBox, children: Seq[String]): String =
    val (width, height) = viewBox.dimensions
    svgElem(width.toString, height.toString, viewBox.formatted, children)
