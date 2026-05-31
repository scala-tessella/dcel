package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.BigPoint
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Step 3 of ADR-0011: `maybeAddMirroredCopy`.
  *
  * Reflection is the orientation-reversing isometry: the copy's half-edge wiring is rebuilt with reversed
  * orientation (`reflectedDouble`) so the face-on-the-left invariant survives. A wrong winding would make the
  * merged tiling fail topology/geometry validation, so every "valid" assertion here is also an
  * orientation-correctness check. The reflection coordinate map is exact in `BigDecimal` (no trig).
  */
class TilingMirroredCopySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private def midpoint(a: BigPoint, b: BigPoint): BigPoint =
    (a + b) / BigDecimal(2)

  private def containsPoint(tiling: TilingDCEL, p: BigPoint): Boolean =
    tiling.coordinates.values.exists(_.almostEquals(p))

  behavior of "TilingDCEL.maybeAddMirroredCopy"

  it should "keep an empty tiling" in:
    val (a, b) = (BigPoint.origin, BigPoint(BigDecimal(1), BigDecimal(0)))
    emptyTiling.maybeAddMirroredCopy(a, b).value.isEmpty shouldBe true

  it should "reject a degenerate axis (two coincident points)" in:
    val p = square.coordinates(V1)
    square.maybeAddMirroredCopy(p, p).isLeft shouldBe true

  it should "place an adjacent square when mirrored across one of its edges" in:
    // Reflect across the edge V2-V3; the copy lands on the far side, sharing that whole edge.
    val coords = square.coordinates
    val result = square.maybeAddMirroredCopy(coords(V2), coords(V3)).value
    allAssert(
      result.vertices.size shouldBe 6,
      result.innerFaces.size shouldBe 2,
      result.halfEdges.size shouldBe 14,
      validate(result).isRight shouldBe true
    )

  it should "form a rhombus when a triangle is mirrored across one of its edges" in:
    // The classic construction: reflecting a triangle across an edge yields two triangles sharing it.
    val coords = triangle.coordinates
    val result = triangle.maybeAddMirroredCopy(coords(V1), coords(V2)).value
    allAssert(
      result.vertices.size shouldBe 4,
      result.innerFaces.size shouldBe 2,
      validate(result).isRight shouldBe true
    )

  it should "reproduce the original when mirrored across its own symmetry axis (full overlap)" in:
    // The square's vertical centre line (through the midpoints of its top and bottom edges) is a reflection
    // symmetry axis: the copy lands exactly on the original.
    val coords = square.coordinates
    val axisP1 = midpoint(coords(V1), coords(V2))
    val axisP2 = midpoint(coords(V3), coords(V4))
    val result = square.maybeAddMirroredCopy(axisP1, axisP2).value
    allAssert(
      result.vertices.size shouldBe 4,
      result.innerFaces.size shouldBe 1,
      validate(result).isRight shouldBe true
    )

  it should "mirror a two-square row across its central edge back onto itself" in:
    val coords     = square.coordinates
    val twoSquares = square.maybeAddMirroredCopy(coords(V2), coords(V3)).value
    // Mirror the row across the same shared edge: each square maps onto the other, reproducing the row.
    val result     = twoSquares.maybeAddMirroredCopy(coords(V2), coords(V3)).value
    allAssert(
      result.innerFaces.size shouldBe twoSquares.innerFaces.size,
      result.vertices.size shouldBe twoSquares.vertices.size,
      validate(result).isRight shouldBe true
    )

  it should "place the mirrored vertices at their exact reflected coordinates" in:
    val coords     = square.coordinates
    val expectedV1 = coords(V1).reflectedAcross(coords(V2), coords(V3))
    val expectedV4 = coords(V4).reflectedAcross(coords(V2), coords(V3))
    val result     = square.maybeAddMirroredCopy(coords(V2), coords(V3)).value
    allAssert(
      containsPoint(result, expectedV1) shouldBe true,
      containsPoint(result, expectedV4) shouldBe true
    )

  it should "reject a copy that partially overlaps without coinciding" in:
    // An axis just inside the square (parallel to an edge but off the symmetry line) reflects the square onto
    // a partially overlapping position whose vertices land mid-edge.
    val coords = square.coordinates
    val a      = midpoint(coords(V1), midpoint(coords(V1), coords(V2))) // quarter point on edge V1-V2
    val b      = a + (coords(V4) - coords(V1))                          // parallel to edge V1-V4
    square.maybeAddMirroredCopy(a, b).isLeft shouldBe true
