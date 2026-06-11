package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingSymmetry.*
import io.github.scala_tessella.dcel.geometry.BigPoint
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Oracle tests for the copy operations: [[TilingSymmetry]] independently reports a tiling's reflection axes
  * and rotational symmetry order, and those are exactly the isometries under which adding a copy must
  * reproduce the original (the copy lands on itself and deduplicates). This cross-checks the mirror / rotate
  * / fan operations against a detector that knows nothing about them.
  */
class TilingSymmetryOracleSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private def pointOf(tiling: Tiling, location: BoundaryLocation): BigPoint =
    location match
      case BoundaryVertex(i)  => tiling.coordinates(i)
      case BoundaryEdge(i, j) => (tiling.coordinates(i) + tiling.coordinates(j)) / BigDecimal(2)

  /** Mirroring across every detected reflection axis must reproduce the tiling (same face count, valid). */
  private def reflectionAxesReproduce(tiling: Tiling): Assertion =
    val axes       = tiling.reflectionalVertexIds
    val expected   = Right(tiling.innerFaces.size)
    val faceCounts =
      axes.map: (locationA, locationB) =>
        tiling.maybeAddMirroredCopy(pointOf(tiling, locationA), pointOf(tiling, locationB))
          .map(_.innerFaces.size)
    withClue(s"axes $axes, results $faceCounts: "):
      allAssert(
        axes should not be empty,
        faceCounts.forall(_ == expected) shouldBe true
      )

  /** Fanning at the detected rotational order about the centroid must reproduce the tiling. */
  private def rotationReproduces(tiling: Tiling): Assertion =
    val order    = tiling.rotationalSymmetryOrder
    val centroid = tiling.coordinates.values.toList.centroid
    val result   = tiling.fanAround(centroid, order).map(_.innerFaces.size)
    withClue(s"rotational order $order, result $result: "):
      allAssert(
        order should be >= 2,
        result shouldBe Right(tiling.innerFaces.size)
      )

  behavior of "isometry copies vs TilingSymmetry (reflection oracle)"

  it should "reproduce a triangle across each detected reflection axis" in:
    reflectionAxesReproduce(triangle)

  it should "reproduce a square across each detected reflection axis" in:
    reflectionAxesReproduce(square)

  it should "reproduce a rhombus across each detected reflection axis" in:
    reflectionAxesReproduce(rhombus)

  it should "reproduce a hexagon across each detected reflection axis" in:
    reflectionAxesReproduce(hexagon)

  it should "reproduce a dodecagon across each detected reflection axis" in:
    reflectionAxesReproduce(dodecagon)

  behavior of "isometry copies vs TilingSymmetry (rotation oracle)"

  it should "reproduce a triangle fanned at its rotational order" in:
    rotationReproduces(triangle)

  it should "reproduce a square fanned at its rotational order" in:
    rotationReproduces(square)

  it should "reproduce a rhombus fanned at its rotational order" in:
    rotationReproduces(rhombus)

  it should "reproduce a hexagon fanned at its rotational order" in:
    rotationReproduces(hexagon)

  it should "reproduce a dodecagon fanned at its rotational order" in:
    rotationReproduces(dodecagon)
