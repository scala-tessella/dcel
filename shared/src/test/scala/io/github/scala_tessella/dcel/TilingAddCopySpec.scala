package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Step 4 of ADR-0011: the unified `maybeAddCopy(Isometry)` entry point and its three named wrappers. */
class TilingAddCopySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private def midpoint(a: BigPoint, b: BigPoint): BigPoint =
    (a + b) / BigDecimal(2)

  behavior of "TilingDCEL.maybeAddCopy"

  it should "dispatch a Translation to the same result as maybeAddTranslatedCopy" in:
    val coords     = square.coordinates
    val (from, to) = (coords(V1), coords(V2))
    val viaCopy    = square.maybeAddCopy(Isometry.Translation(from, to)).value
    val viaWrapper = square.maybeAddTranslatedCopy(from, to).value
    allAssert(
      viaCopy.vertices.size shouldBe viaWrapper.vertices.size,
      viaCopy.innerFaces.size shouldBe viaWrapper.innerFaces.size,
      viaCopy.halfEdges.size shouldBe viaWrapper.halfEdges.size,
      validate(viaCopy).isRight shouldBe true
    )

  it should "dispatch a Rotation to the same result as maybeAddRotatedCopy" in:
    val coords     = square.coordinates
    val center     = midpoint(coords(V1), coords(V2))
    val viaCopy    = square.maybeAddCopy(Isometry.Rotation(center, AngleDegree(180))).value
    val viaWrapper = square.maybeAddRotatedCopy(center, AngleDegree(180)).value
    allAssert(
      viaCopy.vertices.size shouldBe viaWrapper.vertices.size,
      viaCopy.innerFaces.size shouldBe viaWrapper.innerFaces.size,
      validate(viaCopy).isRight shouldBe true
    )

  it should "dispatch a Reflection to the same result as maybeAddMirroredCopy" in:
    val coords     = square.coordinates
    val viaCopy    = square.maybeAddCopy(Isometry.Reflection(coords(V2), coords(V3))).value
    val viaWrapper = square.maybeAddMirroredCopy(coords(V2), coords(V3)).value
    allAssert(
      viaCopy.vertices.size shouldBe viaWrapper.vertices.size,
      viaCopy.innerFaces.size shouldBe viaWrapper.innerFaces.size,
      validate(viaCopy).isRight shouldBe true
    )

  it should "reject a Reflection with a degenerate axis" in:
    val p = square.coordinates(V1)
    square.maybeAddCopy(Isometry.Reflection(p, p)).isLeft shouldBe true

  it should "dispatch a GlideReflection to the same result as maybeAddGlideReflectedCopy" in:
    val strip      = TilingBuilder.createRhombusNet(2, 1).value
    val xs         = strip.coordinates.values.map(_.x)
    val ys         = strip.coordinates.values.map(_.y)
    val cx         = (xs.min + xs.max) / BigDecimal(2)
    val axisP1     = BigPoint(cx, ys.min)
    val axisP2     = BigPoint(cx, ys.max)
    val viaCopy    = strip.maybeAddCopy(Isometry.GlideReflection(axisP1, axisP2)).value
    val viaWrapper = strip.maybeAddGlideReflectedCopy(axisP1, axisP2).value
    allAssert(
      viaCopy.innerFaces.size shouldBe viaWrapper.innerFaces.size,
      viaCopy.vertices.size shouldBe viaWrapper.vertices.size,
      validate(viaCopy).isRight shouldBe true
    )
