package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.AngleDegree
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The [[Tiling]] certified type (ADR-0017): lifecycle (`from` / `empty`), subtyping into the raw
  * [[TilingDCEL]] query surface, and rejection of broken structures.
  */
class TilingSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "Tiling.from"

  it should "certify a valid tiling" in:
    Tiling.from(square).isRight shouldBe true

  it should "certify the structurally empty tiling as the blank canvas" in:
    Tiling.from(TilingDCEL.empty).value shouldBe Tiling.empty

  it should "reject a tiling with a distorted interior angle" in:
    val broken = square
    broken.innerFaces.head.outerComponent.get.angle = Some(AngleDegree(89))
    Tiling.from(broken).isLeft shouldBe true

  it should "reject a tiling with a full-circle interior angle" in:
    val broken = square
    broken.halfEdges.head.angle = Some(AngleDegree(360))
    Tiling.from(broken).isLeft shouldBe true

  behavior of "Tiling as a TilingDCEL subtype"

  it should "answer raw queries without widening or imports" in:
    val tiling: Tiling = Tiling.from(square).value
    allAssert(
      tiling.vertices should have size 4,
      tiling.innerFaces should have size 1,
      tiling.isEmpty shouldBe false,
      tiling.boundaryVertices.value should have size 4
    )

  it should "widen to TilingDCEL where the raw type is expected" in:
    val tiling: Tiling      = Tiling.from(triangle).value
    val widened: TilingDCEL = tiling
    widened.vertices should have size 3

  it should "certify the same underlying instance, preserving equality with it" in:
    val raw            = square
    val tiling: Tiling = Tiling.from(raw).value
    (tiling: TilingDCEL) shouldBe raw

  behavior of "Tiling.empty"

  it should "be structurally the empty TilingDCEL" in:
    (Tiling.empty: TilingDCEL) shouldBe TilingDCEL.empty

  behavior of "Tiling.doubleArea"

  it should "never certify an invalid doubling (regression: triangle plus pentagon)" in:
    // Found by the ADR-0017 certification property: on this non-convex parallelogon boundary,
    // rawDouble's merge left a boundary edge without an angle while still returning Right.
    // doubleArea now validates the merged result, so the outcome is either a Left or a
    // re-certifiable tiling - never a corrupt success.
    import io.github.scala_tessella.dcel.geometry.RegularPolygon
    val outcomes =
      for
        vertexId <- List(V1, V2, V3)
        grown    <- triangle.maybeAddRegularPolygonToBoundary(vertexId, RegularPolygon(5)).toOption
      yield grown.doubleArea match
        case Right(doubled) => Tiling.from(doubled).isRight
        case Left(_)        => true
    allAssert(
      outcomes should not be empty,
      outcomes.forall(identity) shouldBe true
    )
