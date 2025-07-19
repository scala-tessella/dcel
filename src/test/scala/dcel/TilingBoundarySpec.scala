package io.github.scala_tessella
package dcel

import BigDecimalGeometry.BigPoint

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingBoundarySpec extends AnyFlatSpec with Matchers with EitherValues:

  behavior of "TilingDCEL.boundary"

  it should "return the vertices of the boundary in clockwise order for a square" in {
    // Create a square using the TilingBuilder
    val tiling = TilingBuilder.createRegularPolygon(4).value

    // Get the boundary
    val boundaryVertices = tiling.boundary
    val boundaryVertexIds = boundaryVertices.map(_.id)

    // The TilingBuilder creates vertices V0, V1, V2, V3 in a counter-clockwise order.
    // The outer face's half-edges are traversed clockwise.
    // Tracing the `next` pointers from the outer face's starting edge (origin V0)
    // yields the sequence V0, V3, V2, V1.
    boundaryVertices.length shouldBe 4
    boundaryVertexIds shouldBe Vector("V0", "V3", "V2", "V1")
  }

  it should "return an empty vector if the outer face has no outer component" in {
    // Create a DCEL where the outer face is not linked to any edges
    val vertex = Vertex("V0", BigPoint(0, 0))
    val outerFace = Face("F_Outer") // Note: outerFace.outerComponent is None
    val tiling = TilingDCEL(
      vertices = List(vertex),
      halfEdges = Nil,
      innerFaces = Nil,
      outerFace = outerFace
    )

    tiling.boundary shouldBe Vector.empty
  }

  it should "return the single vertex of a boundary loop of one edge" in {
    // A strange but valid case for the traversal logic
    val v = Vertex("V0", BigPoint(0, 0))
    val f = Face("F_Outer")
    val edge = HalfEdge(v)
    edge.next = Some(edge) // Edge loops back to itself
    f.outerComponent = Some(edge)
    val tiling = TilingDCEL(List(v), List(edge), Nil, f)

    tiling.boundary.map(_.id) shouldBe Vector("V0")
  }