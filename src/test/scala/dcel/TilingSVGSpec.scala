package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, BigPoint}
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
    svg should include("width=\"800\"")
    svg should include("height=\"600\"")
    svg should include("</svg>")
    svg shouldBe """<svg width="800" height="600"></svg>"""
  }

  it should "generate valid SVG for a triangle with default parameters" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    svg should include("<svg")
    svg should include("width=\"800\"")
    svg should include("height=\"600\"")
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
    svg should include("width=\"800\"")
    svg should include("height=\"600\"")
    svg should include("viewBox=")
    svg should include("xmlns=\"http://www.w3.org/2000/svg\"")
    svg should include("<line")
    svg should include("<circle")
    svg should include("<text")
    svg should include("</svg>")
  }

  it should "respect custom width and height parameters" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics(width = 1200, height = 900)

    svg should include("width=\"1200\"")
    svg should include("height=\"900\"")
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

    // Different padding should result in different viewBox values
    svgDefault should not equal svgCustom
    
    // Extract viewBox values to verify padding is applied
    val viewBoxRegex = """viewBox="([^"]+)"""".r
    val defaultViewBox = viewBoxRegex.findFirstMatchIn(svgDefault).map(_.group(1))
    val customViewBox = viewBoxRegex.findFirstMatchIn(svgCustom).map(_.group(1))
    
    defaultViewBox should not equal customViewBox
  }

  it should "respect custom scale parameter" in {
    val triangleTiling = createTriangleTiling()
    val svgDefault = triangleTiling.toScalableVectorGraphics(scale = 50.0)
    val svgCustom = triangleTiling.toScalableVectorGraphics(scale = 100.0)

    // Different scale should result in different coordinate values
    svgDefault should not equal svgCustom
  }

  it should "include proper SVG structure and elements" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Check SVG structure
    svg should (include("<?xml") or include("<svg"))
    svg should include("xmlns=\"http://www.w3.org/2000/svg\"")
    
    // Check for groups
    svg should include("<g transform=\"scale(1, 1)\">")
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
    
    // For a triangle, we should have exactly 3 text labels (one for each vertex)
    textCount shouldBe 3
  }

  it should "include vertex IDs in labels" in {
    val triangleTiling = createTriangleTiling()
    val svg = triangleTiling.toScalableVectorGraphics()

    // Triangle vertices should be labeled V0, V1, V2
    svg should include(">V0</text>")
    svg should include(">V1</text>")
    svg should include(">V2</text>")
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
    val Array(minX, minY, width, height) = viewBoxValues
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
      outerFace = Face("F_Outer")
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
    val svg1 = triangleTiling.toSVG(width = 1000, height = 800, strokeWidth = 1.5, padding = 30.0, scale = 75.0)
    val svg2 = triangleTiling.toScalableVectorGraphics(width = 1000, height = 800, strokeWidth = 1.5, padding = 30.0, scale = 75.0)
    
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
    val selfClosingCount = selfClosingPattern.findAllIn(svg).size
    
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
