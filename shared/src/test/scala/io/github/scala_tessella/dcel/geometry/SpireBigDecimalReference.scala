package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.{GeometryError, SpatialError, TilingError}
import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY
import spire.implicits.*

/** Test-scope reference implementation of `SimplePolygon.fromUntrusted` on the pre-ADR-0009
  * `BigLineSegment.unitPath` / `BigPoint.isSimplePolygon` pipeline.
  *
  * The production function in `SimplePolygon.fromUntrusted` reroutes through `DoubleGeometry` (ADR-0009
  * candidate B). This object preserves the original `BigDecimal`+Spire path so methodology step 4 can
  * cross-check Right/Left agreement over ≥ 10 000 generated cases. Any divergence here is exactly the "silent
  * precision regression" the ADR singles out as the risk.
  *
  * Kept in test scope on purpose — it is not a fallback, it is an oracle.
  */
object SpireBigDecimalReference:

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
        val vertices = BigLineSegment(BigPoint.origin, BigPoint(1, 0)).unitPath(angles)
        if !vertices.isSimplePolygon then
          Left(SpatialError("The polygon is self-intersecting."))
        else
          val lastEdgeLength = vertices.head.distanceTo(vertices.last)
          if spire.math.abs(lastEdgeLength - 1.0) > ACCURACY then
            Left(SpatialError(
              f"The polygon does not close. The final edge has length $lastEdgeLength%.4f instead of 1.0."
            ))
          else
            Right(SimplePolygon(angles))
