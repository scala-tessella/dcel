package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.{
  TilingBuilder,
  TilingDCEL,
  TilingError,
  TilingTestHelpers
}
import io.github.scala_tessella.dcel.TilingValidation.validateTopologically
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

/** ADR-0009 methodology step 4 — cross-check the library's `SimplePolygon.fromUntrusted` pipeline (whatever
  * primitives it currently uses) against the pre-ADR-0009 Spire-`BigDecimal` reference in
  * [[SpireBigDecimalReference.fromUntrusted]]. A disagreement here would be the "silent precision regression"
  * the ADR's acceptance criteria single out as the risk.
  *
  * On the `perf/geometry-double` branch (candidates A+C+G) this compares the library path —
  * `Math.{cos,sin,sqrt,atan2}` + sweep-line simplicity on `BigDecimal` coordinates — against the pure
  * Spire-`BigDecimal` pipeline.
  *
  * Cases: 2 000 random angle vectors + polygons harvested from `PropertyBasedDCELSpec`-style random-growth
  * tilings. The seeded `Random` makes the cross-check deterministic across runs and platforms.
  *
  * The ADR-0009 acceptance bar was ≥ 10 000 random angle vectors and ≥ 1 000 harvested polygons; that bar
  * protected the Spire→Double migration moment and is now history (see ADR-0010). The values here are a
  * regression smoke — enough seeded coverage to catch systematic divergence on the first failing run.
  */
class ValidationLibraryCrossCheckSpec
    extends AnyFlatSpec
    with Matchers
    with TilingTestHelpers:

  private def outcomeClass[A](either: Either[TilingError, A]): String =
    either match
      case Right(_)                                             => "Right"
      case Left(_: io.github.scala_tessella.dcel.GeometryError) => "Left/Geometry"
      case Left(_: io.github.scala_tessella.dcel.SpatialError)  => "Left/Spatial"
      case Left(_)                                              => "Left/Other"

  /** Assert both implementations agree on the Right/Left kind. Error *message* may differ (different backends
    * format floats differently); what matters is the validation verdict.
    */
  private def agreeOn(angles: Vector[AngleDegree]): Unit =
    val actual    = SimplePolygon.fromUntrusted(angles)
    val reference = SpireBigDecimalReference.fromUntrusted(angles)
    val actualK   = outcomeClass(actual)
    val refK      = outcomeClass(reference)
    if actualK != refK then
      val degrees = angles
        .map: a =>
          a.toRational.toString
        .mkString("[", ", ", "]")
      fail(
        s"Disagreement on $degrees: production=$actualK (${actual.left.toOption.map(_.message).getOrElse("ok")}) " +
          s"vs reference=$refK (${reference.left.toOption.map(_.message).getOrElse("ok")})"
      )

  behavior of "SimplePolygon.fromUntrusted vs Spire-BigDecimal reference"

  /** A mix of sizes covering realistic and stress cases, plus a bias towards sides the tiler actually uses —
    * the reference pipeline was most sensitive to larger N (ADR note).
    */
  private val sizeWeights: Array[Int] =
    // size N=3 .. N=20; weights favour 3/4/6 (most common), still touch 12+.
    Array(
      /* 3 */ 18, /* 4 */ 18, /* 5 */ 8, /* 6 */ 18, /* 7 */ 6, /* 8 */ 6,
      /* 9 */ 4, /* 10 */ 4, /* 11 */ 3, /* 12 */ 6, /* 13 */ 2, /* 14 */ 2,
      /* 15 */ 2, /* 16 */ 1, /* 17 */ 1, /* 18 */ 1, /* 19 */ 1, /* 20 */ 1
    )

  private def weightedSize(rng: Random): Int =
    val total = sizeWeights.sum
    var r     = rng.nextInt(total)
    var i     = 0
    while i < sizeWeights.length && r >= sizeWeights(i) do
      r -= sizeWeights(i)
      i += 1
    3 + i

  /** Random integer-degree angle vectors. Half are "close-to-valid" (last angle is the exact remainder needed
    * to hit alphaSum), half are pure random (mostly fail the angle-sum check — exercises Left paths).
    */
  private def randomAngles(rng: Random): Vector[AngleDegree] =
    val n = weightedSize(rng)
    if rng.nextBoolean() then
      // "close to valid": n-1 random, last filled to close angleSum
      val total     = 180 * (n - 2)
      val heads     = Vector.fill(n - 1)(rng.nextInt(359) + 1)
      val remainder = total - heads.sum
      if remainder <= 0 || remainder >= 360 then
        heads.map(AngleDegree(_)) :+ AngleDegree(rng.nextInt(359) + 1)
      else
        (heads :+ remainder).map(AngleDegree(_))
    else
      Vector.fill(n)(AngleDegree(rng.nextInt(359) + 1))

  it should "agree on outcome class for 2 000 random integer-degree angle vectors" in:
    val rng = new Random(0xa5a5a5a5L)
    val N   = 2_000
    var i   = 0
    while i < N do
      agreeOn(randomAngles(rng))
      i += 1

  it should "agree on outcome class for all regular N-gons N ∈ [3, 120]" in:
    // Anchors the closure/self-intersection pipeline on known-valid inputs
    // the full way up to the range the centagon ring and the
    // `BigLineSegmentSpec` closure test cover.
    for n <- 3 to 120 do
      agreeOn(RegularPolygon(n).angles)

  /** Random-growth driver mirroring `PropertyBasedDCELSpec.tryRandomAddition`: for each generated tiling we
    * harvest every angle vector that `validateGeometrically` would submit to `fromUntrusted` (inner-face
    * angles and boundary interior/exterior views) and cross-check it.
    */
  private def harvestValidationInputs(tiling: TilingDCEL): List[Vector[AngleDegree]] =
    val faces            =
      tiling.innerFaces.flatMap: face =>
        face.halfEdges.toOption.map: edges =>
          edges.flatMap(_.angle).toVector
    val interiorBoundary =
      tiling.boundaryVertices.toOption
        .filter(_.length >= 3)
        .map: boundaryVertices =>
          boundaryVertices
            .flatMap: vertex =>
              vertex.currentInteriorAngleSum(tiling.outerFace).toOption
            .toVector
        .toList
    val exteriorBoundary =
      tiling.boundaryEdges.toOption
        .filter(_.length >= 3)
        .map: boundaryEdges =>
          boundaryEdges.flatMap(_.angle).map(_.conjugate).toVector
        .toList
    (faces ::: interiorBoundary ::: exteriorBoundary).filter(_.nonEmpty)

  it should "agree on outcome class for polygons harvested from random-growth tilings" in:
    val rng       = new Random(0xdeadbeefL)
    val initial   = Array(3, 4, 6)
    val attempts  = 24
    val tilings   = 100
    val steps     = 10
    var harvested = 0
    var i         = 0
    while i < tilings do
      var t: TilingDCEL = TilingBuilder.createRegularPolygon(RegularPolygon(initial(rng.nextInt(3))))
      var st            = 0
      while st < steps do
        var a      = 0
        var placed = false
        while a < attempts && !placed do
          val edges = t.boundaryEdgesUnsafe
          if edges.nonEmpty then
            val start = edges(rng.nextInt(edges.length)).origin.id
            val s     = 3 + rng.nextInt(4) match
              case 5 => 6
              case x => x
            t.maybeAddRegularPolygonToBoundary(start, RegularPolygon(s)) match
              case Right(next) if validateTopologically(next).isRight =>
                t = next
                placed = true
              case _                                                  => ()
          a += 1
        st += 1
      harvestValidationInputs(t).foreach: v =>
        agreeOn(v)
        harvested += 1
      i += 1
    info(s"cross-checked $harvested polygon candidates from $tilings random-growth tilings")
    withClue(s"harvested = $harvested"):
      harvested should be >= 250
