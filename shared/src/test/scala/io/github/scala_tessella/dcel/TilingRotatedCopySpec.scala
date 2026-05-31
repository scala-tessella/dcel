package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Step 2 of ADR-0011: `maybeAddRotatedCopy`.
  *
  * Rotation reuses the orientation-preserving primitive `addIsometricCopy` (no winding reversal), so the new
  * concerns here are: the degrees->radians conversion, the clockwise-as-rendered sign convention, and the
  * trig float error (~1e-15) sitting safely under the 1e-10 coincidence threshold.
  */
class TilingRotatedCopySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private def centroidOf(tiling: TilingDCEL): BigPoint =
    tiling.coordinates.values.toList.centroid

  private def midpoint(a: BigPoint, b: BigPoint): BigPoint =
    (a + b) / BigDecimal(2)

  /** Counterclockwise rotation in the internal (y-up) model frame — the reference the implementation must
    * match for a positive `degrees` (ADR-0011: positive = clockwise as rendered = CCW in the model frame).
    */
  private def rotatedCCW(point: BigPoint, center: BigPoint, degrees: Double): BigPoint =
    point.rotatedAround(center, AngleDegree(degrees.toInt).toBigRadian)

  private def containsPoint(tiling: TilingDCEL, p: BigPoint): Boolean =
    tiling.coordinates.values.exists(_.almostEquals(p))

  behavior of "TilingDCEL.maybeAddRotatedCopy"

  it should "keep an empty tiling" in:
    emptyTiling.maybeAddRotatedCopy(BigPoint.origin, AngleDegree(90)).value.isEmpty shouldBe true

  it should "reproduce a square rotated 90 degrees about its centre (full overlap, trig precision)" in:
    val result = square.maybeAddRotatedCopy(centroidOf(square), AngleDegree(90)).value
    allAssert(
      result.vertices.size shouldBe 4,
      result.innerFaces.size shouldBe 1,
      validate(result).isRight shouldBe true
    )

  it should "reproduce a hexagon rotated 60 degrees about its centre (full overlap, irrational coords)" in:
    val result = hexagon.maybeAddRotatedCopy(centroidOf(hexagon), AngleDegree(60)).value
    allAssert(
      result.vertices.size shouldBe 6,
      result.innerFaces.size shouldBe 1,
      validate(result).isRight shouldBe true
    )

  it should "place an adjacent square when rotated 180 degrees about an edge midpoint" in:
    val coords = square.coordinates
    val center = midpoint(coords(V1), coords(V2))
    val result = square.maybeAddRotatedCopy(center, AngleDegree(180)).value
    allAssert(
      result.vertices.size shouldBe 6,
      result.innerFaces.size shouldBe 2,
      validate(result).isRight shouldBe true
    )

  it should "form a rhombus when a triangle is rotated 180 degrees about an edge midpoint" in:
    val coords = triangle.coordinates
    val center = midpoint(coords(V1), coords(V2))
    val result = triangle.maybeAddRotatedCopy(center, AngleDegree(180)).value
    allAssert(
      result.vertices.size shouldBe 4,
      result.innerFaces.size shouldBe 2,
      validate(result).isRight shouldBe true
    )

  it should "rotate counterclockwise in the model frame for a positive angle (clockwise as rendered)" in:
    // Rotate the square 90 degrees about corner V1. A positive angle must send the far corner V3 to the CCW
    // image, not the CW one — this pins the sign convention against an accidental negation.
    val coords      = square.coordinates
    val center      = coords(V1)
    val expectedCCW = rotatedCCW(coords(V3), center, 90.0)
    val expectedCW  = rotatedCCW(coords(V3), center, -90.0)
    val result      = square.maybeAddRotatedCopy(center, AngleDegree(90)).value
    allAssert(
      result.innerFaces.size shouldBe 2,
      validate(result).isRight shouldBe true,
      containsPoint(result, expectedCCW) shouldBe true,
      containsPoint(result, expectedCW) shouldBe false
    )

  it should "reject a square rotated 45 degrees about its centre (partial overlap)" in:
    square.maybeAddRotatedCopy(centroidOf(square), AngleDegree(45)).isLeft shouldBe true

  it should "reject a triangle rotated 180 degrees about a vertex (single-vertex contact)" in:
    triangle.maybeAddRotatedCopy(triangle.coordinates(V1), AngleDegree(180)).isLeft shouldBe true
