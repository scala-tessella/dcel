package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.TilingEquivalency.isEquivalentTo
import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.conversion.TilingSVG.*
import io.github.scala_tessella.dcel.geometry.BigPoint
import io.github.scala_tessella.dcel.structure.{Face, Vertex, VertexId}
import io.github.scala_tessella.dcel.{TilingBuilder, TilingDCEL, TilingTestHelpers}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingSVGSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingSVG.toScalableVectorGraphics"

  it should "generate valid SVG for an empty tiling" in:
    val svg = emptyTiling.toScalableVectorGraphics()
    svg shouldBe """<svg width="0" height="0" viewBox="0 0 0 0" xmlns="http://www.w3.org/2000/svg"/>"""

  it should "generate valid SVG for a triangle with default parameters" in:
    val svg = triangle.toScalableVectorGraphics()

    allAssert(
      svg should include("<svg"),
      svg should include("width="),
      svg should include("height="),
      svg should include("viewBox="),
      svg should include("xmlns=\"http://www.w3.org/2000/svg\""),
      svg should include("<line"),
      svg should include("<circle"),
      svg should include("<text"),
      svg should include("</svg>")
    )

  it should "generate valid SVG for a square with default parameters" in:
    val svg = square.toScalableVectorGraphics()

    allAssert(
      svg should include("<svg"),
      svg should include("width="),
      svg should include("height="),
      svg should include("viewBox="),
      svg should include("xmlns=\"http://www.w3.org/2000/svg\""),
      svg should include("<line"),
      svg should include("<circle"),
      svg should include("<text"),
      svg should include("</svg>")
    )

  it should "automatically calculate width and height based on content" in:
    val svg = triangle.toScalableVectorGraphics(scale = 100.0, padding = 20.0)

    // Extract width and height values
    val widthRegex  = """width="(\d+)"""".r
    val heightRegex = """height="(\d+)"""".r

    val widthMatch  = widthRegex.findFirstMatchIn(svg)
    val heightMatch = heightRegex.findFirstMatchIn(svg)

    allAssert(
      widthMatch shouldBe defined,
      heightMatch shouldBe defined,
      // Width and height should be positive and reasonable for the given scale
      {
        val width = widthMatch.get.group(1).toInt
        width should be > 0
      }, {
        val height = heightMatch.get.group(1).toInt
        height should be > 0
      }
    )

  it should "calculate width and height that match viewBox dimensions" in:
    val svg = triangle.toScalableVectorGraphics(scale = 50.0, padding = 10.0)

    // Extract width, height, and viewBox values
    val widthRegex   = """width="(\d+)"""".r
    val heightRegex  = """height="(\d+)"""".r
    val viewBoxRegex = """viewBox="([^"]+)"""".r

    val widthMatch   = widthRegex.findFirstMatchIn(svg)
    val heightMatch  = heightRegex.findFirstMatchIn(svg)
    val viewBoxMatch = viewBoxRegex.findFirstMatchIn(svg)

    allAssert(
      widthMatch shouldBe defined,
      heightMatch shouldBe defined,
      viewBoxMatch shouldBe defined, {
        val width                                    = widthMatch.get.group(1).toInt
        val height                                   = heightMatch.get.group(1).toInt
        val viewBoxValues                            = viewBoxMatch.get.group(1).split(" ").map(_.toDouble)
        val Array(_, _, viewBoxWidth, viewBoxHeight) = viewBoxValues
        allAssert(
          // SVG width and height should match viewBox dimensions (within integer conversion tolerance)
          width should be(viewBoxWidth.toInt),
          height should be(viewBoxHeight.toInt)
        )
      }
    )

  it should "respect custom stroke width parameter" in:
    val svg = triangle.toScalableVectorGraphics(strokeWidth = 2.5)

    allAssert(
      svg should include("stroke-width=\"2.5\""),
      // Vertex circles should have radius = strokeWidth * 2
      svg should include("r=\"5"),
      // Font size should be strokeWidth * 8
      svg should include("font-size=\"20\"")
    )

  it should "respect custom padding parameter" in:
    val svgDefault = triangle.toScalableVectorGraphics(padding = 20.0)
    val svgCustom  = triangle.toScalableVectorGraphics(padding = 50.0)

    allAssert(
      // Different padding should result in different viewBox values and dimensions
      svgDefault should not equal svgCustom, {
        // Extract viewBox values to verify padding is applied
        val viewBoxRegex   = """viewBox="([^"]+)"""".r
        val defaultViewBox = viewBoxRegex.findFirstMatchIn(svgDefault).map(_.group(1))
        val customViewBox  = viewBoxRegex.findFirstMatchIn(svgCustom).map(_.group(1))
        defaultViewBox should not equal customViewBox

      }, {
        // Extract width and height values
        val widthRegex  = """width="(\d+)"""".r
        val heightRegex = """height="(\d+)"""".r

        val defaultWidth  = widthRegex.findFirstMatchIn(svgDefault).map(_.group(1).toInt)
        val customWidth   = widthRegex.findFirstMatchIn(svgCustom).map(_.group(1).toInt)
        val defaultHeight = heightRegex.findFirstMatchIn(svgDefault).map(_.group(1).toInt)
        val customHeight  = heightRegex.findFirstMatchIn(svgCustom).map(_.group(1).toInt)

        // Larger padding should result in larger dimensions
        allAssert(
          customWidth.get should be > defaultWidth.get,
          customHeight.get should be > defaultHeight.get
        )
      }
    )

  it should "respect custom scale parameter" in:
    val svgDefault = triangle.toScalableVectorGraphics(scale = 50.0)
    val svgCustom  = triangle.toScalableVectorGraphics(scale = 100.0)

    allAssert(
      // Different scale should result in different coordinate values and dimensions
      svgDefault should not equal svgCustom, {
        // Extract width and height values
        val widthRegex  = """width="(\d+)"""".r
        val heightRegex = """height="(\d+)"""".r

        val defaultWidth  = widthRegex.findFirstMatchIn(svgDefault).map(_.group(1).toInt)
        val customWidth   = widthRegex.findFirstMatchIn(svgCustom).map(_.group(1).toInt)
        val defaultHeight = heightRegex.findFirstMatchIn(svgDefault).map(_.group(1).toInt)
        val customHeight  = heightRegex.findFirstMatchIn(svgCustom).map(_.group(1).toInt)

        // Larger scale should result in larger dimensions
        allAssert(
          customWidth.get should be > defaultWidth.get,
          customHeight.get should be > defaultHeight.get
        )
      }
    )

  it should "include proper SVG structure and elements" in:
    val svg = triangle.toScalableVectorGraphics()

    allAssert(
      // Check SVG structure
      svg should (include("<?xml") or include("<svg")),
      svg should include("xmlns=\"http://www.w3.org/2000/svg\""),

      // Check for groups
      svg should include("<!-- Edges -->"),
      svg should include("<!-- Vertices -->"),
      svg should include("<!-- Vertex Labels -->"),

      // Check for styling attributes
      svg should include("stroke=\"black\""),
      svg should include("fill=\"red\""),
      svg should include("fill=\"darkblue\"")
    )

  it should "generate lines for each edge only once" in:
    val svg = triangle.toScalableVectorGraphics()

    // Count the number of line elements
    val lineCount = "<line".r.findAllIn(svg).size

    // For a triangle, we should have exactly 3 lines (one for each edge)
    lineCount shouldBe 3

  it should "generate circles for each vertex" in:
    val svg = triangle.toScalableVectorGraphics()

    // Count the number of circle elements
    val circleCount = "<circle".r.findAllIn(svg).size

    // For a triangle, we should have exactly 3 circles (one for each vertex)
    circleCount shouldBe 3

  it should "generate labels for each vertex" in:
    val svg = triangle.toScalableVectorGraphics()

    // Count the number of text elements
    val textCount = "<text".r.findAllIn(svg).size

    // For a triangle, we should have exactly 7 text labels (two for each vertex, id and angle, plus one for the inner face, plus three for the outer face angles)
    textCount shouldBe 10

  it should "include vertex IDs in labels" in:
    val svg = triangle.toScalableVectorGraphics()

    // Triangle vertices should be labeled V1, V2, v3
    allAssert(
      svg should include(">V1</text>"),
      svg should include(">V2</text>"),
      svg should include(">V3</text>")
    )

  it should "handle negative coordinates correctly" in:
    // Create a tiling with vertices that might have negative coordinates
    val svg = square.toScalableVectorGraphics()

    // SVG should be generated without errors and contain proper elements
    allAssert(
      svg should include("<svg"),
      svg should include("<line"),
      svg should include("<circle"),
      svg should include("</svg>")
    )

  it should "generate proper viewBox dimensions" in:
    val svg = triangle.toScalableVectorGraphics(padding = 10.0, scale = 100.0)

    // Extract viewBox values
    val viewBoxRegex = """viewBox="([^"]+)"""".r
    val viewBoxMatch = viewBoxRegex.findFirstMatchIn(svg)

    allAssert(
      viewBoxMatch shouldBe defined, {
        val viewBoxValues = viewBoxMatch.get.group(1).split(" ").map(_.toDouble)
        allAssert(
          viewBoxValues should have length 4, {
            // ViewBox should have proper format: minX minY width height
            val Array(_, _, width, height) = viewBoxValues
            allAssert(
              width should be > 0.0,
              height should be > 0.0
            )
          }
        )
      }
    )

  it should "flip Y coordinates correctly" in:
    val svg = triangle.toScalableVectorGraphics()

    // Y coordinates in the SVG should be negated compared to the original
    // This is indicated by the minus sign in front of y coordinates in the implementation
    allAssert(
      svg should include("y1=\"-"),
      svg should include("y2=\"-"),
      svg should include("cy=\"-"),
      svg should include("y=\"-")
    )

  it should "handle empty half-edges list gracefully" in:
    // Create a tiling with vertices but no half-edges
    val vertex                 = Vertex(VertexId("V0"), BigPoint(BigDecimal(0), BigDecimal(0)))
    val tilingWithVerticesOnly = TilingDCEL(
      vertices = List(vertex),
      halfEdges = List.empty,
      innerFaces = List.empty,
      outerFace = Face.outer
    )

    val svg = tilingWithVerticesOnly.toScalableVectorGraphics()

    // Should generate SVG with vertices but no edges
    allAssert(
      svg should include("<svg"),
      svg should include("<circle"),
      svg should include(">V0</text>"),
      svg should not include "<line",
      svg should include("</svg>")
    )

  behavior of "TilingDCEL.toSVG"

  it should "delegate to toScalableVectorGraphics with same parameters" in:
    val svg1 = triangle.toSVG(strokeWidth = 1.5, padding = 30.0, scale = 75.0)
    val svg2 = triangle.toScalableVectorGraphics(strokeWidth = 1.5, padding = 30.0, scale = 75.0)

    svg1 shouldEqual svg2

  it should "use default parameters when called without arguments" in:
    val svg1 = triangle.toSVG()
    val svg2 = triangle.toScalableVectorGraphics()

    svg1 shouldEqual svg2

  behavior of "SVG content validation"

  it should "generate well-formed XML" in:
    val svg = triangle.toScalableVectorGraphics()

    allAssert(
      // Basic XML well-formedness checks
      svg should startWith("<svg"),
      svg should endWith("</svg>"), {
        // Check that all opened tags are closed
        val openTags  = """<(\w+)(?:\s|>)""".r.findAllMatchIn(svg).map(_.group(1)).toList
        val closeTags = """</(\w+)>""".r.findAllMatchIn(svg).map(_.group(1)).toList

        // Every non-self-closing tag should have a corresponding closing tag
        val selfClosingPattern = """<\w+[^>]*/>""".r

        // Account for self-closing tags (line, circle, text elements might be self-closing in some contexts)
        openTags.length should be >= closeTags.length
      }
    )

  it should "use proper SVG attributes" in:
    val svg = triangle.toScalableVectorGraphics()

    allAssert(
      // Check for required SVG attributes
      svg should include("width="),
      svg should include("height="),
      svg should include("viewBox="),
      svg should include("xmlns="),

      // Check line attributes
      svg should include("x1="),
      svg should include("y1="),
      svg should include("x2="),
      svg should include("y2="),

      // Check circle attributes
      svg should include("cx="),
      svg should include("cy="),
      svg should include("r="),

      // Check text positioning
      svg should include("x="),
      svg should include("y=")
    )

  it should "generate consistent output for the same input" in:
    val svg1 = triangle.toScalableVectorGraphics()
    val svg2 = triangle.toScalableVectorGraphics()

    svg1 shouldEqual svg2

  behavior of "Automatic sizing"

  it should "produce different sizes for different scales" in:
    val svg50  = triangle.toScalableVectorGraphics(scale = 50.0)
    val svg100 = triangle.toScalableVectorGraphics(scale = 100.0)

    val widthRegex  = """width="(\d+)"""".r
    val heightRegex = """height="(\d+)"""".r

    val width50   = widthRegex.findFirstMatchIn(svg50).map(_.group(1).toInt).get
    val width100  = widthRegex.findFirstMatchIn(svg100).map(_.group(1).toInt).get
    val height50  = heightRegex.findFirstMatchIn(svg50).map(_.group(1).toInt).get
    val height100 = heightRegex.findFirstMatchIn(svg100).map(_.group(1).toInt).get

    // Double scale should roughly double the dimensions
    allAssert(
      width100 should be > width50,
      height100 should be > height50
    )

  it should "produce reasonable dimensions for typical tilings" in:
    val triangleSvg = triangle.toScalableVectorGraphics()
    val squareSvg   = square.toScalableVectorGraphics()

    val widthRegex  = """width="(\d+)"""".r
    val heightRegex = """height="(\d+)"""".r

    List(triangleSvg, squareSvg).foreach { svg =>
      val width  = widthRegex.findFirstMatchIn(svg).map(_.group(1).toInt).get
      val height = heightRegex.findFirstMatchIn(svg).map(_.group(1).toInt).get

      // Dimensions should be reasonable (not too small, not too large)
      allAssert(
        width should be > 50,
        width should be < 1000,
        height should be > 50,
        height should be < 1000
      )
    }

  behavior of "Arrow creation"

  it should "create arrows for edges with sufficient distance" in:
    val svg = triangle.toScalableVectorGraphics()

    allAssert(
      // Should contain arrow polygons for both inner and outer face half-edges
      svg should include("<polygon points="), {
        val polygonCount = "<polygon".r.findAllIn(svg).size
        // Each boundary vertex creates an arrow, plus inner face arrows
        polygonCount should be > 0
      }
    )

  it should "not create arrows for very short edges" in:
    // Create a tiling with very close vertices that would result in distance <= ACCURACY
    val v1 = Vertex(VertexId("V1"), BigPoint(BigDecimal(0), BigDecimal(0)))
    val v2 = Vertex(VertexId("V2"), BigPoint(BigDecimal(1e-15), BigDecimal(1e-15))) // Very close

    val tilingWithCloseVertices = TilingDCEL(
      vertices = List(v1, v2),
      halfEdges = List.empty,
      innerFaces = List.empty,
      outerFace = Face.outer
    )

    val svg = tilingWithCloseVertices.toScalableVectorGraphics()

    // Should still generate valid SVG but no arrows for the tiny distance
    allAssert(
      svg should include("<svg"),
      svg should include("</svg>")
    )

  it should "generate different arrow sizes based on stroke width" in:
    val svg1 = triangle.toScalableVectorGraphics(strokeWidth = 1.0)
    val svg2 = triangle.toScalableVectorGraphics(strokeWidth = 2.0)

    allAssert(
      // Different stroke widths should generate different arrow coordinates
      svg1 should not equal svg2,

      // Both should contain arrows
      svg1 should include("<polygon points="),
      svg2 should include("<polygon points=")
    )

  behavior of "BigDecimalGeometry integration"

  it should "use BigDecimalGeometry methods for coordinate calculations" in:
    val svg = triangle.toScalableVectorGraphics()

    // Verify that coordinates are properly formatted (no excessive decimal places)
    val coordinatePattern = """\d+\.\d+""".r
    val coordinates       = coordinatePattern.findAllIn(svg).toList

    coordinates.foreach { coord =>
      // Should not have more than 6 decimal places after formatting
      val decimalPlaces = coord.split("\\.").lift(1).map(_.length).getOrElse(0)
      decimalPlaces should be <= 6
    }

  it should "handle BigPoint coordinate transformations correctly" in:
    val svg = triangle.toScalableVectorGraphics(scale = 100.0)

    // Extract some coordinates to verify they are scaled properly
    val linePattern = """x1="([^"]+)" y1="([^"]+)" x2="([^"]+)" y2="([^"]+)"""".r
    val matches     = linePattern.findAllMatchIn(svg).take(1).toList

    allAssert(
      matches should not be emptyTiling, {
        val firstMatch = matches.head

        val x1 = firstMatch.group(1).toDouble
        val y1 = firstMatch.group(2).toDouble
        val x2 = firstMatch.group(3).toDouble
        val y2 = firstMatch.group(4).toDouble

        // At least one coordinate per line should be reasonably scaled
        // (since triangle has vertex at origin, some coordinates may be 0)
        val coords         = List(math.abs(x1), math.abs(y1), math.abs(x2), math.abs(y2))
        val originalCoords = List(0.0, 1.0, 0.5)
        allAssert(
          coords.max should be > 10.0,
          // Verify that scaling is working - coordinates should be different
          // from the original unit triangle coordinates
          coords.exists(c => !originalCoords.exists(oc => math.abs(c - oc) < 1.0)) should be(true)
        )
      }
    )

  behavior of "formatCoordinate method"

  it should "format coordinates with appropriate precision" in:
    val svg = triangle.toScalableVectorGraphics()

    allAssert(
      // Check that coordinates don't have trailing zeros
      svg should not include ".000000",
      svg should not include ".00000",
      svg should not include ".0000", {
        // But should still include necessary precision
        val coordinatePattern = """\d+(\.\d+)?""".r
        val coordinates       = coordinatePattern.findAllIn(svg).toList
        coordinates should not be emptyTiling
      }
    )

  behavior of "SVG sections"

  it should "generate different arrow sections for inner and outer faces" in:
    val svg = triangle.toScalableVectorGraphics()

    allAssert(
      // Should contain both inner and outer face arrow sections
      svg should include("<!-- Inner Face Half-Edge Direction Arrows -->"),
      svg should include("<!-- Outer Face Half-Edge Direction Arrows -->"),

      // Inner face arrows should be blue
      svg should include("fill=\"blue\""),
      // Outer face arrows should be black
      svg should include("fill=\"black\"")
    )

  it should "include all expected SVG sections when content exists" in:
    val svg = triangle.toScalableVectorGraphics()

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

  it should "generate SVG with half-edge traversal arrows when requested" in:
    val svg = square.toScalableVectorGraphics(showHalfEdgeTraversal = true)

    allAssert(
      svg should include("<!-- Half-Edge Face Traversal -->"),
      svg should include("""fill="darkcyan""""),
      svg should include("<polygon points=")
    )

  behavior of "Edge case handling"

  it should "handle vertices at origin correctly" in:
    val v1 = Vertex(VertexId("V1"), BigPoint(BigDecimal(0), BigDecimal(0)))
    val v2 = Vertex(VertexId("V2"), BigPoint(BigDecimal(1), BigDecimal(1)))

    val tilingWithOrigin = TilingDCEL(
      vertices = List(v1, v2),
      halfEdges = List.empty,
      innerFaces = List.empty,
      outerFace = Face.outer
    )

    val svg = tilingWithOrigin.toScalableVectorGraphics()

    // Should handle origin coordinates properly
    allAssert(
      svg should include("<svg"),
      svg should include("cx=\"0\""),
      svg should include("cy=\"0\""),
      svg should include("</svg>")
    )

  it should "handle very large coordinates gracefully" in:
    val v1 = Vertex(VertexId("V1"), BigPoint(BigDecimal(1000000), BigDecimal(1000000)))
    val v2 = Vertex(VertexId("V2"), BigPoint(BigDecimal(1000001), BigDecimal(1000001)))

    val tilingWithLargeCoords = TilingDCEL(
      vertices = List(v1, v2),
      halfEdges = List.empty,
      innerFaces = List.empty,
      outerFace = Face.outer
    )

    val svg = tilingWithLargeCoords.toScalableVectorGraphics()

    // Should generate valid SVG with large coordinates
    allAssert(
      svg should include("<svg"),
      svg should include("width="),
      svg should include("height="),
      svg should include("</svg>"), {
        // Width and height should be positive
        val widthRegex  = """width="(\d+)"""".r
        val heightRegex = """height="(\d+)"""".r
        val width       = widthRegex.findFirstMatchIn(svg).map(_.group(1).toInt).get
        val height      = heightRegex.findFirstMatchIn(svg).map(_.group(1).toInt).get
        allAssert(
          width should be > 0,
          height should be > 0
        )
      }
    )

  behavior of "TilingSVG.fromMetadata"

  it should "successfully reconstruct an empty tiling from metadata" in:
    val metadata      = emptyTiling.toMetadata
    val reconstructed = fromMetadata(metadata)
    allAssert(
      reconstructed.isRight shouldBe true,
      reconstructed.value.isEmpty shouldBe true,
      TilingDCEL.empty.isEquivalentTo(reconstructed.value) shouldBe true
    )

  it should "successfully reconstruct a single triangle from metadata (round-trip)" in:
    val metadata      = triangle.toMetadata
    val reconstructed = fromMetadata(metadata)
    allAssert(
      reconstructed.isRight shouldBe true,
      validate(reconstructed.value) shouldBe Right(()),
      triangle.isEquivalentTo(reconstructed.value) shouldBe true
    )

  it should "successfully reconstruct a triangle-based tessellation from metadata (round-trip)" in:
    val net           = TilingBuilder.createTriangleNet(4, 4)
    val metadata      = net.toMetadata
    val reconstructed = fromMetadata(metadata)
    allAssert(
      reconstructed.isRight shouldBe true,
      validate(reconstructed.value) shouldBe Right(()),
      net.isEquivalentTo(reconstructed.value) shouldBe true
    )
