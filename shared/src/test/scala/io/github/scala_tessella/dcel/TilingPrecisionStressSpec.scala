package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Precision stress for the copy operations under large and deeply-iterated patterns.
  *
  * Only rotation introduces floating error (~R · 1e-15, via `Math.cos/sin`); translation and reflection are
  * exact in `BigDecimal`. The coincidence threshold for merging is 1e-10, so vertex matching survives as long
  * as `R · 1e-15 < 1e-10`, i.e. patterns up to ~1e5 units across — far beyond any realistic tiling. These
  * tests confirm coincidence detection holds at practical scale and depth rather than probing that limit.
  */
class TilingPrecisionStressSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private def grid(side: Int): Tiling =
    TilingBuilder.createRhombusNet(side, side).value

  private def centroidOf(tiling: Tiling): BigPoint =
    tiling.coordinates.values.toList.centroid

  behavior of "copy operations under precision stress"

  it should "stay exact along a long translated strip (no drift)" in:
    // Translation is exact BigDecimal arithmetic: a 50-square row stays valid with no accumulated error.
    val from   = BigPoint.origin
    val to     = BigPoint(BigDecimal(1), BigDecimal(0))
    val result = square.repeatAlong(from, to, 50).value
    allAssert(
      result.innerFaces.size shouldBe 50,
      validate(result).isRight shouldBe true
    )

  it should "stay coincident when a large grid is rotated onto itself (far-field rotation)" in:
    // An 8x8 grid is 4-fold symmetric about its centre; fanning order 4 rotates copies whose far corners sit
    // ~5.7 units out. Coincidence must still collapse the four copies back to the original 64 faces.
    val base   = grid(8)
    val result = base.fanAround(centroidOf(base), 4).value
    allAssert(
      result.innerFaces.size shouldBe base.innerFaces.size,
      validate(result).isRight shouldBe true
    )

  it should "stay exact when a large grid is mirrored onto itself" in:
    // Reflection is exact: mirroring an 8x8 grid across its vertical centre line reproduces it.
    val base   = grid(8)
    val ys     = base.coordinates.values.map(_.y)
    val cx     = centroidOf(base).x
    val result = base.maybeAddMirroredCopy(BigPoint(cx, ys.min), BigPoint(cx, ys.max)).value
    allAssert(
      result.innerFaces.size shouldBe base.innerFaces.size,
      validate(result).isRight shouldBe true
    )

  it should "close a full rotational ring built by chained (compounded) rotations" in:
    // Chaining maybeAddRotatedCopy rotates the GROWING flower each step, so the sixth triangle's coordinates
    // are five compounded rotations of the first. The ring must still close into a 6-triangle flower with a
    // 360-degree interior vertex at the apex.
    val apex   = triangle.coordinates(V1)
    val flower = (1 to 5).foldLeft[Either[TilingError, Tiling]](Right(triangle)): (acc, _) =>
      acc.flatMap(_.maybeAddRotatedCopy(apex, AngleDegree(60)))
    allAssert(
      flower.map(_.innerFaces.size) shouldBe Right(6),
      flower.flatMap(validate).isRight shouldBe true
    )

  it should "stay valid across many iterated translate-and-merge steps" in:
    // Deep iteration: each step translates the whole growing row by one unit and merges, so the overlap
    // deduplicates and the row grows by one square. 30 sequential merges -> 31 squares, validated at the end.
    val step  = BigPoint(BigDecimal(1), BigDecimal(0))
    val grown = (1 to 30).foldLeft[Either[TilingError, Tiling]](Right(square)): (acc, _) =>
      acc.flatMap: current =>
        val origin = current.coordinates(V1)
        current.maybeAddTranslatedCopy(origin, origin + step)
    allAssert(
      grown.map(_.innerFaces.size) shouldBe Right(31),
      grown.flatMap(validate).isRight shouldBe true
    )
