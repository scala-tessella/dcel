package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingEquivalency.{translatedDouble, verticallyReflectedCopy}
import io.github.scala_tessella.dcel.VertexTypes.*
import io.github.scala_tessella.dcel.geometry.{BigPoint, BigRadian, RegularPolygon}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Foundations of the Krotenheerdt enumeration (OEIS A068600): the 15 valid vertex types over the
  * {3,4,6,8,12} alphabet, the partial-fan pruning table, and the congruence key used for sound search-state
  * deduplication.
  */
class KrotenheerdtSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "VertexTypes.validSignatures"

  it should "be exactly the 15 valid vertex types over {3,4,6,8,12}" in:
    validSignatures shouldBe Set(
      List(3, 3, 3, 3, 3, 3),
      List(3, 3, 3, 3, 6),
      List(3, 3, 3, 4, 4),
      List(3, 3, 4, 3, 4),
      List(3, 3, 4, 12),
      List(3, 4, 3, 12),
      List(3, 3, 6, 6),
      List(3, 6, 3, 6),
      List(3, 4, 4, 6),
      List(3, 4, 6, 4),
      List(4, 4, 4, 4),
      List(3, 12, 12),
      List(4, 6, 12),
      List(4, 8, 8),
      List(6, 6, 6)
    ).map(normalize)

  it should "use the octagon only in 4.8.8" in:
    validSignatures.filter(_.contains(8)) shouldBe Set(normalize(List(4, 8, 8)))

  behavior of "VertexTypes partial fans"

  it should "accept fans that extend to a valid type" in:
    allAssert(
      isExtendableFan(List(8)) shouldBe true,
      isExtendableFan(List(8, 8)) shouldBe true,
      isExtendableFan(List(3, 3, 3, 4)) shouldBe true,
      isExtendableFan(List(4, 3, 4)) shouldBe true,
      isExtendableFan(List(12, 4)) shouldBe true
    )

  it should "reject fans that cannot be completed" in:
    allAssert(
      isExtendableFan(List(8, 8, 8)) shouldBe false,
      isExtendableFan(List(6, 8)) shouldBe false,
      isExtendableFan(List(12, 12, 12)) shouldBe false,
      isExtendableFan(List(6, 6, 4)) shouldBe false
    )

  it should "recognise complete vertices up to rotation and reflection" in:
    allAssert(
      isCompleteVertex(List(8, 4, 8)) shouldBe true,
      isCompleteVertex(List(12, 3, 12)) shouldBe true,
      isCompleteVertex(List(3, 3, 3, 3)) shouldBe false,
      isCompleteVertex(List(4, 4, 4, 4, 4)) shouldBe false
    )

  behavior of "PatchCanonical.congruenceKey"

  private def asymmetricPatch: Tiling =
    // triangle + square + triangle: no nontrivial self-symmetry
    triangle
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(4)).value
      .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(3)).value

  it should "be invariant under translation and rotation" in:
    val patch      = asymmetricPatch
    val key        = PatchCanonical.congruenceKey(patch)
    val delta      = BigPoint(BigDecimal(7), BigDecimal(-3))
    val translated = patch.translatedDouble(_ + delta, identity, identity)
    val rotated    =
      patch.translatedDouble(_.rotatedAround(BigPoint(1, 2), BigRadian(0.7)), identity, identity)
    allAssert(
      PatchCanonical.congruenceKey(translated) shouldBe key,
      PatchCanonical.congruenceKey(rotated) shouldBe key
    )

  it should "be invariant under reflection" in:
    val patch = asymmetricPatch
    PatchCanonical.congruenceKey(patch.verticallyReflectedCopy) shouldBe
      PatchCanonical.congruenceKey(patch)

  it should "separate non-congruent patches" in:
    val withSquare   = triangle.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(4)).value
    val withTriangle = triangle.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
    PatchCanonical.congruenceKey(withSquare) should not be PatchCanonical.congruenceKey(withTriangle)
