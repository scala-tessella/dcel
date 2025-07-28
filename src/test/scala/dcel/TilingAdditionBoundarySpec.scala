package io.github.scala_tessella
package dcel

import io.github.scala_tessella.dcel.BigDecimalGeometry.{ACCURACY, AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.TilingAdditionBoundary.findBoundaryDivision
import io.github.scala_tessella.dcel.TilingAddition.calculateNewVertices
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spire.implicits.*
import spire.math.abs

class TilingAdditionBoundarySpec extends AnyFlatSpec with Matchers {

  behavior of "TilingAdditionBoundary.findBoundaryDivision"

  it should "identify one shared edge when adding a square to a square" in {
    // A square boundary, vertices are CW
    val v1 = Vertex("V1", BigPoint(1, 0))
    val v2 = Vertex("V2", BigPoint(1, 1))
    val v3 = Vertex("V3", BigPoint(0, 1))
    val v4 = Vertex("V4", BigPoint(0, 0))
    val boundary = Vector(v1, v4, v3, v2)

    val (shared, news) = findBoundaryDivision(boundary, v1, 4)

    shared.map(_.id) shouldBe List("V1", "V4")
    news should have size 2

    // For a square starting from edge v4->v1 (from (0,0) to (1,0))
    // The new vertices should complete the square going counter-clockwise
    val expectedP1 = BigPoint(1, -1)  // Going down from (1,0)
    val expectedP2 = BigPoint(0, -1)  // Going left from (1,-1)

    // Check if the points are approximately equal to our expected points
    val hasExpectedP1 = news.exists(p => p.almostEquals(expectedP1))
    val hasExpectedP2 = news.exists(p => p.almostEquals(expectedP2))

    hasExpectedP1 shouldBe true
    hasExpectedP2 shouldBe true
  }

  it should "identify two shared edges when closing a triangle gap" in {
    // Create a boundary where adding a triangle would share two edges
    // This represents a scenario where we have an "open" triangular gap that can be closed
    val v1 = Vertex("V1", BigPoint(0, 0))
    val v2 = Vertex("V2", BigPoint(1, 0))
    val v3 = Vertex("V3", BigPoint(0.5, BigDecimal("0.8660254037844386"))) // Height of equilateral triangle
    val boundary = Vector(v1, v2, v3)

    // When we add a triangle starting from v1, it should find that v2 and v3 are also shared
    // because the triangle we're adding exactly fills the gap
    val (shared, news) = findBoundaryDivision(boundary, v1, 3)

    shared.map(_.id) should contain theSameElementsInOrderAs List("V1", "V2", "V3")
    news shouldBe empty
  }

  it should "identify partial shared edges correctly" in {
    // Test a case where only part of the new polygon overlaps with existing boundary
    val v1 = Vertex("V1", BigPoint(0, 0))
    val v2 = Vertex("V2", BigPoint(1, 0))
    val v3 = Vertex("V3", BigPoint(1, 1))
    val v4 = Vertex("V4", BigPoint(0, 1))
    val boundary = Vector(v1, v2, v3, v4)

    // Adding a triangle from v1 should share edge v1->v2, but not more
    val (shared, news) = findBoundaryDivision(boundary, v1, 3)

    shared.map(_.id) shouldBe List("V1", "V2")
    news should have size 1

    // The third vertex of the equilateral triangle should be at (0.5, √3/2)
    val expectedThirdVertex = BigPoint(BigDecimal("0.5"), BigDecimal("0.8660254037844386"))
    val hasExpectedVertex = news.exists(p => p.almostEquals(expectedThirdVertex))

    hasExpectedVertex shouldBe true
  }

  it should "return nothing if attachment vertex is not on boundary" in {
    val v1 = Vertex("V1", BigPoint(1, 0))
    val v2 = Vertex("V2", BigPoint(1, 1))
    val v3 = Vertex("V3", BigPoint(0, 1))
    val boundary = Vector(v1, v2, v3)
    val nonBoundaryVertex = Vertex("V4", BigPoint(10, 10))

    val (shared, news) = findBoundaryDivision(boundary, nonBoundaryVertex, 5)

    shared shouldBe empty
    news shouldBe empty
  }

}