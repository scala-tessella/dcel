package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.{GeometryError, SpatialError, TilingError}
import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY
import spire.implicits.*

import scala.collection.mutable
import scala.util.boundary

/** Test-scope reference implementation of `SimplePolygon.fromUntrusted` on the pre-ADR-0009
  * `BigLineSegment.unitPath` / `BigPoint.isSimplePolygon` pipeline.
  *
  * On `perf/geometry-double` (candidate A+C+G) the production library primitives have been rewritten:
  *   - `BigPoint.fromPolar` now uses `Math.{cos,sin}` on `Double`
  *   - `BigLineSegment.length` / `horizontalAngle` now use `Math.{sqrt,atan2}` on `Double`
  *   - `BigPoint.isSimplePolygon` delegates to `IntersectionDetection.hasSelfIntersection`
  *
  * So this reference **cannot** call those helpers — it would just mirror production. It therefore inlines
  * its own Spire-`BigDecimal` trig, `sqrt`, and the original O(n²) pair-loop self-intersection test. Any
  * divergence between this reference and production is the "silent precision regression" the ADR's acceptance
  * criteria single out.
  *
  * Kept in test scope on purpose — it is not a fallback, it is an oracle.
  */
object SpireBigDecimalReference:

  /** Pre-change `BigLineSegment.unitPath`: `spire.math.cos/sin` on `BigDecimal`, accumulating in `BigPoint`
    * space. Specialised to the canonical `(origin, (1,0))` starting segment used by
    * `SimplePolygon.fromUntrusted`, so initial heading is 0.
    */
  private def refUnitPath(angles: Vector[AngleDegree]): List[BigPoint] =
    val n         = angles.length
    val points    = mutable.ListBuffer[BigPoint](BigPoint.origin, BigPoint(1, 0))
    var currentPt = BigPoint(1, 0)
    var heading   = BigDecimal(0) // angleTo((1,0)) from origin
    val one       = BigDecimal(1)
    var i         = 1
    while i < n - 1 do
      val turnRad = BigDecimal(spire.math.pi) * (angles(i).supplement.toRational / 180).toDouble
      heading += turnRad
      val dx      = one * spire.math.cos(heading)
      val dy      = one * spire.math.sin(heading)
      currentPt = BigPoint(currentPt.x + dx, currentPt.y + dy)
      points.append(currentPt)
      i += 1
    points.toList

  /** Pre-change `BigPoint.isSimplePolygon`: O(n²) pair loop with `interiorIntersects` on
    * `BigPoint.orientation`. Reuses `BigLineSegment.interiorIntersects` which is pure `BigDecimal` arithmetic
    * and is untouched by A+C+G.
    */
  private def refIsSimplePolygon(points: List[BigPoint]): Boolean =
    val n = points.length
    if n < 4 then return true
    if !points.hasNoAlmostEqualPoints() then return false

    val segments =
      (0 until n).iterator
        .map: i =>
          BigLineSegment(points(i), points((i + 1) % n))
        .toArray

    boundary:
      var i = 0
      while i < n do
        var j = i + 1
        while j < n do
          if i != (j + 1) % n && j != (i + 1) % n && !(i == 0 && j == n - 1) then
            if segments(i).interiorIntersects(segments(j)) then boundary.break(false)
          j += 1
        i += 1
      true

  /** Pre-change `BigPoint.distanceTo` via `BigLineSegment.length`: `spire.math.sqrt` on `BigDecimal`.
    */
  private def refDistance(p: BigPoint, q: BigPoint): BigDecimal =
    val dx = p.x - q.x
    val dy = p.y - q.y
    spire.math.sqrt(dx.pow(2) + dy.pow(2))

  def fromUntrusted(
      angles: Vector[AngleDegree]
  ): Either[TilingError, SimplePolygon] =
    val n = angles.length
    if n < 3 then
      Left(GeometryError("A simple polygon must have at least 3 sides."))
    else if angles.exists(_.isFullCircle) then
      Left(GeometryError("The polygon cannot have full circles as interior angles."))
    else
      val angleSum         = angles.map(_.normalised).sumExact
      val expectedAngleSum = SimplePolygon.alphaSum(n)
      if (angleSum - expectedAngleSum).toRational.abs > ACCURACY then
        Left(GeometryError(
          f"The sum of interior angles is incorrect for a polygon with $n unit sides."
        ))
      else
        val vertices = refUnitPath(angles)
        if !refIsSimplePolygon(vertices) then
          Left(SpatialError("The polygon is self-intersecting."))
        else
          val lastEdgeLength = refDistance(vertices.head, vertices.last)
          if spire.math.abs(lastEdgeLength - 1.0) > ACCURACY then
            Left(SpatialError(
              f"The polygon does not close. The final edge has length $lastEdgeLength%.4f instead of 1.0."
            ))
          else
            Right(SimplePolygon(angles))
