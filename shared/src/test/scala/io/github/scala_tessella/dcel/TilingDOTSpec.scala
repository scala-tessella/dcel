package io.github.scala_tessella.dcel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import TilingDOT.*

class TilingDOTSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingDOT.toCompleteDOT"

  it should "generate a DOT skeleton for an empty tiling" in {
    val dot = emptyTiling.toCompleteDOT
    allAssert(
      dot should include("digraph TilingDCEL"),
      dot should include("label=\"TilingDCEL Topology\""),
      dot should include("subgraph cluster_vertices"),
      dot should include("subgraph cluster_faces"),
      dot should include("subgraph cluster_halfedges"),
      dot.trim should endWith("}")
    )
  }

  it should "include all vertices, faces, and half-edges for a triangle" in {
    val dot          = triangle.toCompleteDOT
    val numVertices  = triangle.vertices.length
    val numFaces     = triangle.faces.length // includes outer face
    val numHalfEdges = triangle.halfEdges.length

    val vertexNodeRegex = """\[\s*label="V [^"]*"\]""".r
    val faceNodeRegex   = """\[\s*label="F [^"]*"\]""".r
    val heNodeRegex     = """\[\s*label="HE \d+"\]""".r

    allAssert(
      vertexNodeRegex.findAllIn(dot).size shouldBe numVertices,
      faceNodeRegex.findAllIn(dot).size shouldBe numFaces,
      heNodeRegex.findAllIn(dot).size shouldBe numHalfEdges
    )
  }

  it should "emit expected relation edges for a triangle" in {
    val dot          = triangle.toCompleteDOT
    val numHalfEdges = triangle.halfEdges.length
    val numVertices  = triangle.vertices.length
    val facesCount   = triangle.faces.length

    def count(label: String): Int =
      s"""\\[label="$label"\\]""".r.findAllIn(dot).size

    // Each vertex should have exactly one leaving edge if well-formed
    val leavingDefined = triangle.vertices.count(_.leaving.isDefined)

    allAssert(
      count("origin") shouldBe numHalfEdges,
      count("dest") shouldBe numHalfEdges,
      count("twin") shouldBe numHalfEdges,
      count("next") shouldBe numHalfEdges,
      count("prev") shouldBe numHalfEdges,
      count("face") shouldBe numHalfEdges,
      count("leaving") shouldBe leavingDefined,

      // Outer component link present for each face with an outerComponent defined
      count("outer") shouldBe facesCount
    )
  }

  it should "use stable node prefixes v:, f:, e: and include known vertex ids" in {
    val dot = triangle.toCompleteDOT

    allAssert(
      dot should include(s""""v:${V1.value}""""),
      dot should include(s""""v:${V2.value}""""),
      dot should include(s""""v:${V3.value}""""),
      dot should include("subgraph cluster_vertices"),
      dot should include("subgraph cluster_faces"),
      dot should include("subgraph cluster_halfedges"),
      // Half-edge node prefix present
      dot should include("e:")
    )
  }

  it should "produce consistent output for the same tiling" in {
    val dot1 = square.toCompleteDOT
    val dot2 = square.toCompleteDOT
    dot1 shouldEqual dot2
  }
