package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.TilingTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BigBoxSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "BigBox.contains"

  it should "contain points inside and on the border" in {
    val box = BigBox(BigPoint(0, 0), BigPoint(10, 5))
    allAssert(
      box.contains(BigPoint(0, 0)) shouldBe true,  // min corner
      box.contains(BigPoint(10, 5)) shouldBe true, // max corner
      box.contains(BigPoint(10, 0)) shouldBe true, // border
      box.contains(BigPoint(5, 2)) shouldBe true   // strictly inside
    )
  }

  it should "not contain points outside" in {
    val box = BigBox(BigPoint(0, 0), BigPoint(10, 5))
    allAssert(
      box.contains(BigPoint(-0.0001, 0)) shouldBe false,
      box.contains(BigPoint(0, -0.0001)) shouldBe false,
      box.contains(BigPoint(10.0001, 5)) shouldBe false,
      box.contains(BigPoint(5, 5.0001)) shouldBe false
    )
  }

  behavior of "BigBox.intersects"

  it should "detect intersection including edge and corner touching" in {
    val a = BigBox(BigPoint(0, 0), BigPoint(10, 10))

    val overlapping   = BigBox(BigPoint(5, 5), BigPoint(15, 15))
    val sharingEdge   = BigBox(BigPoint(10, 2), BigPoint(20, 8))
    val sharingCorner = BigBox(BigPoint(10, 10), BigPoint(12, 12))
    val contained     = BigBox(BigPoint(2, 2), BigPoint(8, 8))
    val disjointRight = BigBox(BigPoint(10.0001, 0), BigPoint(20, 10))
    val disjointAbove = BigBox(BigPoint(0, 10.0001), BigPoint(10, 20))
    val disjointDiag  = BigBox(BigPoint(10.0001, 10.0001), BigPoint(20, 20))

    allAssert(
      a.intersects(overlapping) shouldBe true,
      a.intersects(sharingEdge) shouldBe true,
      a.intersects(sharingCorner) shouldBe true,
      a.intersects(contained) shouldBe true,
      a.intersects(disjointRight) shouldBe false,
      a.intersects(disjointAbove) shouldBe false,
      a.intersects(disjointDiag) shouldBe false
    )
  }

  behavior of "BigBox.expand"

  it should "expand symmetrically by given amount" in {
    val box   = BigBox(BigPoint(1, 2), BigPoint(3, 4))
    val grown = box.expand(BigDecimal(2))
    allAssert(
      grown.min.almostEquals(BigPoint(-1, 0)) shouldBe true,
      grown.max.almostEquals(BigPoint(5, 6)) shouldBe true
    )
  }

  it should "handle zero and negative expansion" in {
    val box = BigBox(BigPoint(1, 1), BigPoint(2, 2))
    allAssert(
      box.expand(0).min.almostEquals(box.min) shouldBe true,
      box.expand(0).max.almostEquals(box.max) shouldBe true,
      box.expand(-0.5).min.almostEquals(BigPoint(1.5, 1.5)) shouldBe true,
      box.expand(-0.5).max.almostEquals(BigPoint(1.5, 1.5)) shouldBe true
    )
  }

  behavior of "BigBox.fromPoints"

  it should "create origin box for empty input" in {
    val b = BigBox.fromPoints(Nil)
    allAssert(
      b.min.almostEquals(BigPoint.origin) shouldBe true,
      b.max.almostEquals(BigPoint.origin) shouldBe true
    )
  }

  it should "bound a set of points" in {
    val pts = List(BigPoint(-1, 2), BigPoint(3, -2), BigPoint(0, 0), BigPoint(1, 5))
    val b   = BigBox.fromPoints(pts)
    allAssert(
      b.min.almostEquals(BigPoint(-1, -2)) shouldBe true,
      b.max.almostEquals(BigPoint(3, 5)) shouldBe true,
      pts.forall(p => b.contains(p)) shouldBe true
    )
  }

  behavior of "BigBox.fromSegment"

  it should "create a box covering the segment endpoints" in {
    val p1 = BigPoint(-2, 3)
    val p2 = BigPoint(4, -1)
    val s  = BigLineSegment(p1, p2)
    val b  = BigBox.fromSegment(s)
    allAssert(
      b.min.almostEquals(BigPoint(-2, -1)) shouldBe true,
      b.max.almostEquals(BigPoint(4, 3)) shouldBe true,
      b.contains(p1) shouldBe true,
      b.contains(p2) shouldBe true
    )
  }
