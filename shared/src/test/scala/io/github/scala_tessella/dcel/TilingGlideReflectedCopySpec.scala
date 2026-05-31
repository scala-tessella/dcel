package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.BigPoint
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `maybeAddGlideReflectedCopy`: the fourth plane isometry (reflect + slide along the axis).
  *
  * Orientation-reversing like a plain reflection, so it rides the same `reflectedDouble` winding-reversal
  * wiring; the glide adds an exact translation by `axisP2 - axisP1` along the axis. A single convex tile
  * glide-reflected only ever touches the original at a vertex (the pure reflection already gives the unique
  * edge-sharing copy, and any glide slides it off), so the valid cases here come from self-overlap.
  */
class TilingGlideReflectedCopySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingDCEL.maybeAddGlideReflectedCopy"

  it should "keep an empty tiling" in:
    emptyTiling.maybeAddGlideReflectedCopy(BigPoint.origin, BigPoint(BigDecimal(0), BigDecimal(1)))
      .value.isEmpty shouldBe true

  it should "reject a degenerate axis (two coincident points)" in:
    val p = square.coordinates(V1)
    square.maybeAddGlideReflectedCopy(p, p).isLeft shouldBe true

  it should "stack a 2x1 strip into a 2x2 block (glide along its vertical centre axis)" in:
    // The strip is symmetric across its vertical centre line, so the reflection maps it onto itself and the
    // glide (axisP2 - axisP1 = one row upward) stacks a second row exactly on top. The orientation-reversal
    // wiring still runs, so a wrong winding would fail validation even though the coordinates map onto self.
    val strip  = TilingBuilder.createRhombusNet(2, 1).value
    val xs     = strip.coordinates.values.map(_.x)
    val ys     = strip.coordinates.values.map(_.y)
    val cx     = (xs.min + xs.max) / BigDecimal(2)
    val axisP1 = BigPoint(cx, ys.min)
    val axisP2 = BigPoint(cx, ys.max)
    val result = strip.maybeAddGlideReflectedCopy(axisP1, axisP2).value
    allAssert(
      result.innerFaces.size shouldBe 4,
      result.vertices.size shouldBe 9,
      validate(result).isRight shouldBe true
    )

  it should "reject a single square glide-reflected across an edge (single-vertex contact)" in:
    val coords = square.coordinates
    square.maybeAddGlideReflectedCopy(coords(V1), coords(V2)).isLeft shouldBe true
