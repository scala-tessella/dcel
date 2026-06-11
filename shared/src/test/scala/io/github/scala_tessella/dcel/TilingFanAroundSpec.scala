package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.{BigPoint, RegularPolygon}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `fanAround(center, order)`: the general, full-ring fan around an arbitrary point.
  *
  * Built on the rotation + merge-with-face-dedup machinery (ADR-0011). The headline case is the building step
  * of the Dürer pentagonal fractal: mirror a pentagon across an edge, then fan the central pentagon's
  * centroid at order 5 to get the six-pentagon cluster.
  */
class TilingFanAroundSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private def centroidOf(tiling: Tiling): BigPoint =
    tiling.coordinates.values.toList.centroid

  private def pentagon: Tiling =
    TilingBuilder.createRegularPolygon(RegularPolygon(5))

  behavior of "TilingDCEL.fanAround"

  it should "keep an empty tiling" in:
    emptyTiling.fanAround(BigPoint.origin, 5).value.isEmpty shouldBe true

  it should "reject an order below 2" in:
    allAssert(
      square.fanAround(centroidOf(square), 1).isLeft shouldBe true,
      square.fanAround(centroidOf(square), 0).isLeft shouldBe true
    )

  it should "reproduce a square fanned around its own centroid at its symmetry order" in:
    // A square is 4-fold symmetric about its centre: every copy lands on the original and dedups to one.
    val result = square.fanAround(centroidOf(square), 4).value
    allAssert(
      result.innerFaces.size shouldBe 1,
      result.vertices.size shouldBe 4,
      validate(result).isRight shouldBe true
    )

  it should "fan a square around a corner into a 2x2 block (full ring, interior vertex)" in:
    // Four squares meeting at the corner fill 4*90 = 360 degrees, making the corner an interior vertex.
    val corner = square.coordinates(V1)
    val result = square.fanAround(corner, 4).value
    allAssert(
      result.innerFaces.size shouldBe 4,
      result.vertices.size shouldBe 9,
      validate(result).isRight shouldBe true
    )

  it should "build the six-pentagon Dürer cluster (mirror across an edge, then fan order 5)" in:
    val base         = pentagon
    val centre       = centroidOf(base)
    val coords       = base.coordinates
    val twoPentagons = base.maybeAddMirroredCopy(coords(V1), coords(V2)).value
    val cluster      = twoPentagons.fanAround(centre, 5).value
    allAssert(
      twoPentagons.innerFaces.size shouldBe 2,
      cluster.innerFaces.size shouldBe 6,
      validate(cluster).isRight shouldBe true
    )

  it should "support iterating the fan (fan the six-pentagon cluster's centre again is a no-op overlap)" in:
    // Re-fanning the cluster around the SAME centre at order 5 maps it onto itself: still six pentagons.
    val centre  = centroidOf(pentagon)
    val coords  = pentagon.coordinates
    val cluster = pentagon.maybeAddMirroredCopy(coords(V1), coords(V2)).value.fanAround(centre, 5).value
    val again   = cluster.fanAround(centre, 5).value
    allAssert(
      again.innerFaces.size shouldBe cluster.innerFaces.size,
      validate(again).isRight shouldBe true
    )

  it should "reject a fan whose wedges overlap improperly (square at order 5)" in:
    // 72 degrees is not a symmetry of the square: the rotated copies overlap the original partially.
    square.fanAround(centroidOf(square), 5).isLeft shouldBe true
