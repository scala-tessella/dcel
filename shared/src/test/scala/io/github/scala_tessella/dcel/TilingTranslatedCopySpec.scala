package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.BigPoint
import io.github.scala_tessella.dcel.structure.VertexId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Step 1 of ADR-0011: `maybeAddTranslatedCopy`.
  *
  * Translation is the exact isometry (point-difference, no trig), so it is the right place to nail down the
  * "outside, or exactly follows the composition" merge contract — in particular the full-face-overlap
  * deduplication that [[TilingMerge]] gains here.
  */
class TilingTranslatedCopySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  /** The vector of the boundary edge from `a` to `b`, taken from the original fixture's coordinates. */
  private def edgeVector(tiling: TilingDCEL, a: VertexId, b: VertexId): (BigPoint, BigPoint) =
    val coords = tiling.coordinates
    (coords(a), coords(b))

  behavior of "TilingDCEL.maybeAddTranslatedCopy"

  it should "keep an empty tiling" in:
    val (from, to) = (BigPoint.origin, BigPoint(BigDecimal(1), BigDecimal(0)))
    val result     = emptyTiling.maybeAddTranslatedCopy(from, to)
    allAssert(
      result.value.isEmpty shouldBe true
    )

  it should "reproduce the original under a zero translation (from == to)" in:
    val origin = square.coordinates(V1)
    val result = square.maybeAddTranslatedCopy(origin, origin).value
    allAssert(
      result.vertices.size shouldBe square.vertices.size,
      result.halfEdges.size shouldBe square.halfEdges.size,
      result.innerFaces.size shouldBe 1,
      validate(result).isRight shouldBe true
    )

  it should "place a square next to a square when translated by one edge" in:
    val (from, to) = edgeVector(square, V1, V2)
    val result     = square.maybeAddTranslatedCopy(from, to).value
    allAssert(
      result.vertices.size shouldBe 6,
      result.innerFaces.size shouldBe 2,
      result.halfEdges.size shouldBe 14,
      validate(result).isRight shouldBe true
    )

  it should "reject a triangle translated by one edge (single-vertex contact, not edge-to-edge)" in:
    // A lone triangle cannot tile edge-to-edge by pure translation: translating by edge V1->V2 makes the
    // copy touch the original at only the shared vertex, a pinch point that is neither "outside" nor
    // "exactly following the composition". (Two triangles share an edge only under reflection/rotation.)
    val (from, to) = edgeVector(triangle, V1, V2)
    val result     = triangle.maybeAddTranslatedCopy(from, to)
    allAssert(
      result.isLeft shouldBe true
    )

  it should "deduplicate a fully overlapping face into a single tessellation (three squares in a row)" in:
    // Build a two-square row, then translate it by the same edge vector so its left square lands exactly on
    // the existing right square. The overlap is a full face: it must be unified, not doubled.
    val (from, to) = edgeVector(square, V1, V2)
    val twoSquares = square.maybeAddTranslatedCopy(from, to).value
    val result     = twoSquares.maybeAddTranslatedCopy(from, to).value
    allAssert(
      result.innerFaces.size shouldBe 3,
      result.vertices.size shouldBe 8,
      result.halfEdges.size shouldBe 20,
      validate(result).isRight shouldBe true
    )

  it should "agree with a directly-built 2x1 net translated into a 3x1 net" in:
    val net2x1     = TilingBuilder.createRhombusNet(2, 1).value
    val net3x1     = TilingBuilder.createRhombusNet(3, 1).value
    // horizontal unit step taken from the net's own bottom-left edge
    val (from, to) = edgeVector(net2x1, V1, V2)
    val grown      = net2x1.maybeAddTranslatedCopy(from, to).value
    allAssert(
      grown.vertices.size shouldBe net3x1.vertices.size,
      grown.innerFaces.size shouldBe net3x1.innerFaces.size,
      grown.halfEdges.size shouldBe net3x1.halfEdges.size,
      validate(grown).isRight shouldBe true
    )

  it should "reject a copy that partially overlaps without coinciding (half-edge translation)" in:
    val coords = square.coordinates
    val from   = coords(V1)
    val to     = (coords(V1) + coords(V2)) / BigDecimal(2) // half of edge V1->V2
    val result = square.maybeAddTranslatedCopy(from, to)
    allAssert(
      result.isLeft shouldBe true
    )

  it should "reject a disconnected copy translated far away" in:
    val origin = square.coordinates(V1)
    val far    = BigPoint(origin.x + BigDecimal(10), origin.y + BigDecimal(10))
    val result = square.maybeAddTranslatedCopy(origin, far)
    allAssert(
      result.isLeft shouldBe true
    )
