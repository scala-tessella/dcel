package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.BigPoint
import io.github.scala_tessella.dcel.structure.VertexId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `repeatAlong` / `repeatGrid`: the translational counterpart of `fanAround` — repeat the tiling along a
  * vector (a strip) or over a 2-D lattice (a grid), merging and validating each copy.
  */
class TilingRepeatSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private def edge(tiling: TilingDCEL, a: VertexId, b: VertexId): (BigPoint, BigPoint) =
    val coords = tiling.coordinates
    (coords(a), coords(b))

  behavior of "TilingDCEL.repeatAlong"

  it should "keep an empty tiling" in:
    emptyTiling.repeatAlong(BigPoint.origin, BigPoint(BigDecimal(1), BigDecimal(0)), 3)
      .value.isEmpty shouldBe true

  it should "reject a count below 1" in:
    val (from, to) = edge(square, V1, V2)
    square.repeatAlong(from, to, 0).isLeft shouldBe true

  it should "reproduce the original for a count of 1" in:
    val (from, to) = edge(square, V1, V2)
    val result     = square.repeatAlong(from, to, 1).value
    allAssert(
      result.innerFaces.size shouldBe 1,
      result.vertices.size shouldBe square.vertices.size,
      validate(result).isRight shouldBe true
    )

  it should "build a row of three squares stepped by one edge" in:
    val (from, to) = edge(square, V1, V2)
    val result     = square.repeatAlong(from, to, 3).value
    allAssert(
      result.innerFaces.size shouldBe 3,
      result.vertices.size shouldBe 8,
      result.halfEdges.size shouldBe 20,
      validate(result).isRight shouldBe true
    )

  it should "reject a triangle stepped by one edge (single-vertex contact)" in:
    val (from, to) = edge(triangle, V1, V2)
    triangle.repeatAlong(from, to, 2).isLeft shouldBe true

  it should "reject a disconnected repetition (step far away)" in:
    val from = square.coordinates(V1)
    val to   = BigPoint(from.x + BigDecimal(10), from.y)
    square.repeatAlong(from, to, 2).isLeft shouldBe true

  behavior of "TilingDCEL.repeatGrid"

  it should "build a 3x3 block of squares" in:
    val coords = square.coordinates
    val result = square.repeatGrid(coords(V1), coords(V2), 3, coords(V4), 3).value
    allAssert(
      result.innerFaces.size shouldBe 9,
      result.vertices.size shouldBe 16,
      validate(result).isRight shouldBe true
    )

  it should "agree with a directly-built square net" in:
    val coords = square.coordinates
    val net3x3 = TilingBuilder.createRhombusNet(3, 3).value
    val grid   = square.repeatGrid(coords(V1), coords(V2), 3, coords(V4), 3).value
    allAssert(
      grid.innerFaces.size shouldBe net3x3.innerFaces.size,
      grid.vertices.size shouldBe net3x3.vertices.size,
      grid.halfEdges.size shouldBe net3x3.halfEdges.size,
      validate(grid).isRight shouldBe true
    )
