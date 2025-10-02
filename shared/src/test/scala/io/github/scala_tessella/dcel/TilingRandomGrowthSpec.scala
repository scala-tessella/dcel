package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.structure.VertexId
import io.github.scala_tessella.dcel.Polygon.RegularPolygon
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.mutable.ListBuffer
import scala.util.Random

class TilingRandomGrowthSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with TilingTestHelpers:

  private val genInitialSides: Gen[Int] = Gen.oneOf(3, 4, 6)
  private val genSteps: Gen[Int]        = Gen.choose(1, 12)

  private def mk(s: Int): TilingDCEL =
    // Guard against ScalaCheck shrinking producing invalid values (e.g., 0)
    TilingBuilder.createRegularPolygon(RegularPolygon(math.max(3, s)))

  private def validateAll(t: TilingDCEL): Assertion =
    allAssert(
      TilingDCEL.validateTopologically(t).isRight shouldBe true,
      TilingDCEL.validateGeometrically(t).isRight shouldBe true,
      TilingDCEL.validateSpatially(t).isRight shouldBe true
    )

  private val rng = new Random(0xfaceb00c)

  private def pickBoundaryStart(t: TilingDCEL): Option[VertexId] =
    val b = t.boundaryEdges
    if b.isEmpty then None else Some(b(rng.nextInt(b.length)).origin.id)

  private val genSides: Gen[Int] = Gen.oneOf(3, 4, 6)

  it should "grow random regular polygons at boundary edges and remain valid after each successful step" in
    forAll(genInitialSides, genSteps) { (initialSides, steps) =>
      var t = mk(initialSides)
      allAssert(
        validateAll(t),
        allAssert(
          {
            var k          = 0
            val assertions = ListBuffer[Assertion]()
            while k < steps do
              val maybeStart = pickBoundaryStart(t)
              val s          = genSides.sample.getOrElse(3)
              val tried      =
                for
                  start <- maybeStart.toRight("no-boundary")
                  next  <- t.maybeAddRegularPolygonToBoundary(start, RegularPolygon(s))
                yield next

              tried match
                case Right(updated) =>
                  t = updated
                  assertions += validateAll(t)
                case Left(_)        => // If the random pick is invalid, we simply skip this iteration
              k += 1
            assertions.toList
          }*
        )
      )
    }
