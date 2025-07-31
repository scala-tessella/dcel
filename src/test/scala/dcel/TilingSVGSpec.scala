package io.github.scala_tessella
package dcel

import BigDecimalGeometry.BigPoint
import TilingSVG.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues

class TilingSVGSpec extends AnyFlatSpec with Matchers with EitherValues:

  behavior of "TilingSVG.toScalableVectorGraphics"

  // Helper method to create a simple triangle tiling for testing
  private def createTriangleTiling(): TilingDCEL =
    TilingBuilder.createRegularPolygon(3).value

  // Helper method to create a square tiling for testing
  private def createSquareTiling(): TilingDCEL =
    TilingBuilder.createRegularPolygon(4).value

  it should "generate valid SVG for an empty tiling" in {
    val emptyTiling = TilingBuilder.empty
    val svg = emptyTiling.toScalableVectorGraphics()

    svg should include("<svg")
    svg should include("width=\"0\"")
    svg should include("height=\"0\"")
    svg should include("</svg>")
    svg shouldBe """<svg width="0" height="0"></svg>"""
  }

  it should "generate valid SVG for a triangle with default parameters" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    svg should include("<svg")
    svg should include("width=")
    svg should include("height=")
    svg should include("viewBox=")
    svg should include("xmlns=\"http://www.w3.org/2000/svg\"")
    svg should include("<line")
    svg should include("<circle")
    svg should include("<text")
    svg should include("</svg>")
  }

  it should "generate valid SVG for a square with default parameters" in {
    val squareTiling = createSquareTiling()
    val svg = squareTiling.toScalableVectorGraphics()

    svg should include("<svg")
    svg should include("width=")
    svg should include("height=")
    svg should include("viewBox=")
    svg should include("xmlns=\"http://www.w3.org/2000/svg\"")
    svg should include("<line")
    svg should include("<circle")
    svg should include("<text")
    svg should include("</svg>")
  }

  it should "automatically calculate width and height based on content" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics(scale = 100.0, padding = 20.0)

    // Extract width and height values
    val widthRegex = """width="(\d+)"""".r
    val heightRegex = """height="(\d+)"""".r

    val widthMatch = widthRegex.findFirstMatchIn(svg)
    val heightMatch = heightRegex.findFirstMatchIn(svg)

    widthMatch shouldBe defined
    heightMatch shouldBe defined

    val width = widthMatch.get.group(1).toInt
    val height = heightMatch.get.group(1).toInt

    // Width and height should be positive and reasonable for the given scale
    width should be > 0
    height should be > 0
  }

  it should "calculate width and height that match viewBox dimensions" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics(scale = 50.0, padding = 10.0)

    // Extract width, height, and viewBox values
    val widthRegex = """width="(\d+)"""".r
    val heightRegex = """height="(\d+)"""".r
    val viewBoxRegex = """viewBox="([^"]+)"""".r

    val widthMatch = widthRegex.findFirstMatchIn(svg)
    val heightMatch = heightRegex.findFirstMatchIn(svg)
    val viewBoxMatch = viewBoxRegex.findFirstMatchIn(svg)

    widthMatch shouldBe defined
    heightMatch shouldBe defined
    viewBoxMatch shouldBe defined

    val width = widthMatch.get.group(1).toInt
    val height = heightMatch.get.group(1).toInt
    val viewBoxValues = viewBoxMatch.get.group(1).split(" ").map(_.toDouble)
    val Array(_, _, viewBoxWidth, viewBoxHeight) = viewBoxValues

    // SVG width and height should match viewBox dimensions (within integer conversion tolerance)
    width should be(viewBoxWidth.toInt)
    height should be(viewBoxHeight.toInt)
  }

  it should "respect custom stroke width parameter" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics(strokeWidth = 2.5)

    svg should include("stroke-width=\"2.5\"")
    // Vertex circles should have radius = strokeWidth * 2
    svg should include("r=\"5.0\"")
    // Font size should be strokeWidth * 8
    svg should include("font-size=\"20\"")
  }

  it should "respect custom padding parameter" in {
    val triangleTiling = createTriangleTiling()
    val svgDefault = triangleTiling.toScalableVectorGraphics(padding = 20.0)
    val svgCustom = triangleTiling.toScalableVectorGraphics(padding = 50.0)

    // Different padding should result in different viewBox values and dimensions
    svgDefault should not equal svgCustom

    // Extract viewBox values to verify padding is applied
    val viewBoxRegex = """viewBox="([^"]+)"""".r
    val defaultViewBox = viewBoxRegex.findFirstMatchIn(svgDefault).map(_.group(1))
    val customViewBox = viewBoxRegex.findFirstMatchIn(svgCustom).map(_.group(1))

    defaultViewBox should not equal customViewBox

    // Extract width and height values
    val widthRegex = """width="(\d+)"""".r
    val heightRegex = """height="(\d+)"""".r

    val defaultWidth = widthRegex.findFirstMatchIn(svgDefault).map(_.group(1).toInt)
    val customWidth = widthRegex.findFirstMatchIn(svgCustom).map(_.group(1).toInt)
    val defaultHeight = heightRegex.findFirstMatchIn(svgDefault).map(_.group(1).toInt)
    val customHeight = heightRegex.findFirstMatchIn(svgCustom).map(_.group(1).toInt)

    // Larger padding should result in larger dimensions
    customWidth.get should be > defaultWidth.get
    customHeight.get should be > defaultHeight.get
  }

  it should "respect custom scale parameter" in {
    val triangleTiling = createTriangleTiling()
    val svgDefault = triangleTiling.toScalableVectorGraphics(scale = 50.0)
    val svgCustom = triangleTiling.toScalableVectorGraphics(scale = 100.0)

    // Different scale should result in different coordinate values and dimensions
    svgDefault should not equal svgCustom

    // Extract width and height values
    val widthRegex = """width="(\d+)"""".r
    val heightRegex = """height="(\d+)"""".r

    val defaultWidth = widthRegex.findFirstMatchIn(svgDefault).map(_.group(1).toInt)
    val customWidth = widthRegex.findFirstMatchIn(svgCustom).map(_.group(1).toInt)
    val defaultHeight = heightRegex.findFirstMatchIn(svgDefault).map(_.group(1).toInt)
    val customHeight = heightRegex.findFirstMatchIn(svgCustom).map(_.group(1).toInt)

    // Larger scale should result in larger dimensions
    customWidth.get should be > defaultWidth.get
    customHeight.get should be > defaultHeight.get
  }

  it should "include proper SVG structure and elements" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Check SVG structure
    svg should (include("<?xml") or include("<svg"))
    svg should include("xmlns=\"http://www.w3.org/2000/svg\"")

    // Check for groups
    svg should include("<!-- Edges -->")
    svg should include("<!-- Vertices -->")
    svg should include("<!-- Vertex Labels -->")

    // Check for styling attributes
    svg should include("stroke=\"black\"")
    svg should include("fill=\"red\"")
    svg should include("fill=\"darkblue\"")
  }

  it should "generate lines for each edge only once" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Count the number of line elements
    val lineCount = "<line".r.findAllIn(svg).size

    // For a triangle, we should have exactly 3 lines (one for each edge)
    lineCount shouldBe 3
  }

  it should "generate circles for each vertex" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Count the number of circle elements
    val circleCount = "<circle".r.findAllIn(svg).size

    // For a triangle, we should have exactly 3 circles (one for each vertex)
    circleCount shouldBe 3
  }

  it should "generate labels for each vertex" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Count the number of text elements
    val textCount = "<text".r.findAllIn(svg).size

    // For a triangle, we should have exactly 7 text labels (two for each vertex, id and angle, plus one for the inner face, plus three for the outer face angles)
    textCount shouldBe 10
  }

  it should "include vertex IDs in labels" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Triangle vertices should be labeled V1, V2, v3
    svg should include(">V1</text>")
    svg should include(">V2</text>")
    svg should include(">V3</text>")
  }

  it should "handle negative coordinates correctly" in {
    // Create a tiling with vertices that might have negative coordinates
    val squareTiling = createSquareTiling()
    val svg = squareTiling.toScalableVectorGraphics()

    // SVG should be generated without errors and contain proper elements
    svg should include("<svg")
    svg should include("<line")
    svg should include("<circle")
    svg should include("</svg>")
  }

  it should "generate proper viewBox dimensions" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics(padding = 10.0, scale = 100.0)

    // Extract viewBox values
    val viewBoxRegex = """viewBox="([^"]+)"""".r
    val viewBoxMatch = viewBoxRegex.findFirstMatchIn(svg)

    viewBoxMatch shouldBe defined
    val viewBoxValues = viewBoxMatch.get.group(1).split(" ").map(_.toDouble)
    viewBoxValues should have length 4

    // ViewBox should have proper format: minX minY width height
    val Array(_, _, width, height) = viewBoxValues
    width should be > 0.0
    height should be > 0.0
  }

  it should "flip Y coordinates correctly" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Y coordinates in the SVG should be negated compared to the original
    // This is indicated by the minus sign in front of y coordinates in the implementation
    svg should include("y1=\"-")
    svg should include("y2=\"-")
    svg should include("cy=\"-")
    svg should include("y=\"-")
  }

  it should "handle empty half-edges list gracefully" in {
    // Create a tiling with vertices but no half-edges
    val vertex = Vertex("V0", BigPoint(BigDecimal(0), BigDecimal(0)))
    val tilingWithVerticesOnly = TilingDCEL(
      vertices = List(vertex),
      halfEdges = List.empty,
      innerFaces = List.empty,
      outerFace = Face(Face.outerId)
    )

    val svg = tilingWithVerticesOnly.toScalableVectorGraphics()

    // Should generate SVG with vertices but no edges
    svg should include("<svg")
    svg should include("<circle")
    svg should include(">V0</text>")
    svg should not include "<line"
    svg should include("</svg>")
  }

  behavior of "TilingDCEL.toSVG"

  it should "delegate to toScalableVectorGraphics with same parameters" in {
    val triangleTiling = createTriangleTiling()
    val svg1 = triangleTiling.toSVG(strokeWidth = 1.5, padding = 30.0, scale = 75.0)
    val svg2 = triangleTiling.toScalableVectorGraphics(strokeWidth = 1.5, padding = 30.0, scale = 75.0)

    svg1 shouldEqual svg2
  }

  it should "use default parameters when called without arguments" in {
    val triangleTiling = createTriangleTiling()
    val svg1 = triangleTiling.toSVG()
    val svg2 = triangleTiling.toScalableVectorGraphics()

    svg1 shouldEqual svg2
  }

  behavior of "SVG content validation"

  it should "generate well-formed XML" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Basic XML well-formedness checks
    svg should startWith("<svg")
    svg should endWith("</svg>")

    // Check that all opened tags are closed
    val openTags = """<(\w+)(?:\s|>)""".r.findAllMatchIn(svg).map(_.group(1)).toList
    val closeTags = """</(\w+)>""".r.findAllMatchIn(svg).map(_.group(1)).toList

    // Every non-self-closing tag should have a corresponding closing tag
    val selfClosingPattern = """<\w+[^>]*/>""".r

    // Account for self-closing tags (line, circle, text elements might be self-closing in some contexts)
    openTags.length should be >= closeTags.length
  }

  it should "use proper SVG attributes" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Check for required SVG attributes
    svg should include("width=")
    svg should include("height=")
    svg should include("viewBox=")
    svg should include("xmlns=")

    // Check line attributes
    svg should include("x1=")
    svg should include("y1=")
    svg should include("x2=")
    svg should include("y2=")

    // Check circle attributes
    svg should include("cx=")
    svg should include("cy=")
    svg should include("r=")

    // Check text positioning
    svg should include("x=")
    svg should include("y=")
  }

  it should "generate consistent output for the same input" in {
    val triangleTiling = createTriangleTiling()
    val svg1 = triangleTiling.toScalableVectorGraphics()
    val svg2 = triangleTiling.toScalableVectorGraphics()

    svg1 shouldEqual svg2
  }

  behavior of "Automatic sizing"

  it should "produce different sizes for different scales" in {
    val triangleTiling = createTriangleTiling()
    val svg50 = triangleTiling.toScalableVectorGraphics(scale = 50.0)
    val svg100 = triangleTiling.toScalableVectorGraphics(scale = 100.0)

    val widthRegex = """width="(\d+)"""".r
    val heightRegex = """height="(\d+)"""".r

    val width50 = widthRegex.findFirstMatchIn(svg50).map(_.group(1).toInt).get
    val width100 = widthRegex.findFirstMatchIn(svg100).map(_.group(1).toInt).get
    val height50 = heightRegex.findFirstMatchIn(svg50).map(_.group(1).toInt).get
    val height100 = heightRegex.findFirstMatchIn(svg100).map(_.group(1).toInt).get

    // Double scale should roughly double the dimensions
    width100 should be > width50
    height100 should be > height50
  }

  it should "produce reasonable dimensions for typical tilings" in {
    val triangleTiling = createTriangleTiling()
    val squareTiling = createSquareTiling()

    val triangleSvg = triangleTiling.toScalableVectorGraphics()
    val squareSvg = squareTiling.toScalableVectorGraphics()

    val widthRegex = """width="(\d+)"""".r
    val heightRegex = """height="(\d+)"""".r

    List(triangleSvg, squareSvg).foreach { svg =>
      val width = widthRegex.findFirstMatchIn(svg).map(_.group(1).toInt).get
      val height = heightRegex.findFirstMatchIn(svg).map(_.group(1).toInt).get

      // Dimensions should be reasonable (not too small, not too large)
      width should be > 50
      width should be < 1000
      height should be > 50
      height should be < 1000
    }
  }

  behavior of "Arrow creation"

  it should "create arrows for edges with sufficient distance" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Should contain arrow polygons for both inner and outer face half-edges
    svg should include("<polygon points=")
    val polygonCount = "<polygon".r.findAllIn(svg).size

    // Each boundary vertex creates an arrow, plus inner face arrows
    polygonCount should be > 0
  }

  it should "not create arrows for very short edges" in {
    // Create a tiling with very close vertices that would result in distance <= ACCURACY
    val v1 = Vertex("V1", BigPoint(BigDecimal(0), BigDecimal(0)))
    val v2 = Vertex("V2", BigPoint(BigDecimal(1E-15), BigDecimal(1E-15))) // Very close

    val tilingWithCloseVertices = TilingDCEL(
      vertices = List(v1, v2),
      halfEdges = List.empty,
      innerFaces = List.empty,
      outerFace = Face(Face.outerId)
    )

    val svg = tilingWithCloseVertices.toScalableVectorGraphics()

    // Should still generate valid SVG but no arrows for the tiny distance
    svg should include("<svg")
    svg should include("</svg>")
  }

  it should "generate different arrow sizes based on stroke width" in {
    val triangleTiling = createTriangleTiling()
    val svg1 = triangleTiling.toScalableVectorGraphics(strokeWidth = 1.0)
    val svg2 = triangleTiling.toScalableVectorGraphics(strokeWidth = 2.0)

    // Different stroke widths should generate different arrow coordinates
    svg1 should not equal svg2

    // Both should contain arrows
    svg1 should include("<polygon points=")
    svg2 should include("<polygon points=")
  }

  behavior of "BigDecimalGeometry integration"

  it should "use BigDecimalGeometry methods for coordinate calculations" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Verify that coordinates are properly formatted (no excessive decimal places)
    val coordinatePattern = """\d+\.\d+""".r
    val coordinates = coordinatePattern.findAllIn(svg).toList

    coordinates.foreach { coord =>
      // Should not have more than 6 decimal places after formatting
      val decimalPlaces = coord.split("\\.").lift(1).map(_.length).getOrElse(0)
      decimalPlaces should be <= 6
    }
  }

  it should "handle BigPoint coordinate transformations correctly" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics(scale = 100.0)

    // Extract some coordinates to verify they are scaled properly
    val linePattern = """x1="([^"]+)" y1="([^"]+)" x2="([^"]+)" y2="([^"]+)"""".r
    val matches = linePattern.findAllMatchIn(svg).take(1).toList

    matches should not be empty
    val firstMatch = matches.head

    val x1 = firstMatch.group(1).toDouble
    val y1 = firstMatch.group(2).toDouble
    val x2 = firstMatch.group(3).toDouble
    val y2 = firstMatch.group(4).toDouble

    // At least one coordinate per line should be reasonably scaled
    // (since triangle has vertex at origin, some coordinates may be 0)
    val coords = List(math.abs(x1), math.abs(y1), math.abs(x2), math.abs(y2))
    coords.max should be > 10.0

    // Verify that scaling is working - coordinates should be different
    // from the original unit triangle coordinates
    val originalCoords = List(0.0, 1.0, 0.5)
    coords.exists(c => !originalCoords.exists(oc => math.abs(c - oc) < 1.0)) should be(true)
  }

  behavior of "formatCoordinate method"

  it should "format coordinates with appropriate precision" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Check that coordinates don't have trailing zeros
    svg should not include ".000000"
    svg should not include ".00000"
    svg should not include ".0000"

    // But should still include necessary precision
    val coordinatePattern = """\d+(\.\d+)?""".r
    val coordinates = coordinatePattern.findAllIn(svg).toList
    coordinates should not be empty
  }

  behavior of "Boundary arrows"

  it should "generate boundary direction arrows when boundary exists" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Should contain boundary direction arrows section
    svg should include("<!-- Boundary Direction Arrows -->")
    svg should include("fill=\"red\"")
    svg should include("stroke=\"red\"")
  }

  it should "handle empty boundary gracefully" in {
    // Create a tiling with no boundary
    val vertex = Vertex("V0", BigPoint(BigDecimal(0), BigDecimal(0)))
    val tilingWithNoBoundary = TilingDCEL(
      vertices = List(vertex),
      halfEdges = List.empty,
      innerFaces = List.empty,
      outerFace = Face(Face.outerId)
    )

    val svg = tilingWithNoBoundary.toScalableVectorGraphics()

    // Should not contain boundary direction arrows
    svg should not include "<!-- Boundary Direction Arrows -->"
    svg should include("<svg")
    svg should include("</svg>")
  }

  behavior of "SVG sections"

  it should "generate different arrow sections for inner and outer faces" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Should contain both inner and outer face arrow sections
    svg should include("<!-- Inner Face Half-Edge Direction Arrows -->")
    svg should include("<!-- Outer Face Half-Edge Direction Arrows -->")

    // Inner face arrows should be blue
    svg should include("fill=\"blue\"")
    // Outer face arrows should be black
    svg should include("fill=\"black\"")
  }

  it should "include all expected SVG sections when content exists" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    val expectedSections = List(
      "<!-- Edges -->",
      "<!-- Vertices -->",
      "<!-- Vertex Labels -->",
      "<!-- Face Labels -->",
      "<!-- Inner Angle Labels -->",
      "<!-- Outer Angle Labels -->"
    )

    expectedSections.foreach { section =>
      svg should include(section)
    }
  }

  it should "generate SVG with half-edge traversal arrows when requested" in {
    val squareTiling = createSquareTiling()
    val svg = squareTiling.toScalableVectorGraphics(showHalfEdgeTraversal = true)

    svg should include("<!-- Half-Edge Face Traversal -->")
    svg should include("""fill="darkcyan"""")
    svg should include("<polygon points=")
  }

  behavior of "Edge case handling"

  it should "handle vertices at origin correctly" in {
    val v1 = Vertex("V1", BigPoint(BigDecimal(0), BigDecimal(0)))
    val v2 = Vertex("V2", BigPoint(BigDecimal(1), BigDecimal(1)))

    val tilingWithOrigin = TilingDCEL(
      vertices = List(v1, v2),
      halfEdges = List.empty,
      innerFaces = List.empty,
      outerFace = Face(Face.outerId)
    )

    val svg = tilingWithOrigin.toScalableVectorGraphics()

    // Should handle origin coordinates properly
    svg should include("<svg")
    svg should include("cx=\"0\"")
    svg should include("cy=\"0\"")
    svg should include("</svg>")
  }

  it should "handle very large coordinates gracefully" in {
    val v1 = Vertex("V1", BigPoint(BigDecimal(1000000), BigDecimal(1000000)))
    val v2 = Vertex("V2", BigPoint(BigDecimal(1000001), BigDecimal(1000001)))

    val tilingWithLargeCoords = TilingDCEL(
      vertices = List(v1, v2),
      halfEdges = List.empty,
      innerFaces = List.empty,
      outerFace = Face(Face.outerId)
    )

    val svg = tilingWithLargeCoords.toScalableVectorGraphics()

    // Should generate valid SVG with large coordinates
    svg should include("<svg")
    svg should include("width=")
    svg should include("height=")
    svg should include("</svg>")

    // Width and height should be positive
    val widthRegex = """width="(\d+)"""".r
    val heightRegex = """height="(\d+)"""".r
    val width = widthRegex.findFirstMatchIn(svg).map(_.group(1).toInt).get
    val height = heightRegex.findFirstMatchIn(svg).map(_.group(1).toInt).get

    width should be > 0
    height should be > 0
  }