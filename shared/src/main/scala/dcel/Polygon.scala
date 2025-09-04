package dcel

import dcel.BigDecimalGeometry.{ACCURACY, AngleDegree}

/** Methods to deal with regular polygons in tiling */
object Polygon:

  object SimplePolygon:

    def alphaSum(sides: Int): AngleDegree =
      AngleDegree(180) * (sides - 2)

    /**
     * Validates the list of interior angles for a simple polygon.
     * It checks that no angle is a full circle and that the sum of the angles is correct.
     *
     * @param angles A list of interior angles in degrees.
     * @return Either a String with an error message or Unit if validation succeeds.
     */
    def validatePolygonAngles(angles: List[AngleDegree]): Either[TilingError, Unit] =
      val n = angles.length
      if angles.exists(_.isFullCircle) then
        Left(GeometryError("The polygon cannot have full circles as interior angles."))
      else
        val angleSum = angles.map(_.normalised).sum2
        val expectedAngleSum = alphaSum(n)
        if (angleSum - expectedAngleSum).toRational.abs > ACCURACY then
          Left(GeometryError(f"The sum of interior angles is incorrect for a polygon with $n sides. Expected ${expectedAngleSum.toRational.toDouble}%.2f, but got ${angleSum.toRational.toDouble}%.2f."))
        else
          Right(())

  /** Unit regular polygon with the given number of sides */
  opaque type RegularPolygon = Int

  /** Companion object for [[RegularPolygon]] */
  object RegularPolygon:

    /** Create a [[RegularPolygon]] of given sides
     *
     * @param sides number of sides
     * @throws IllegalArgumentException if sides <= 2
     */
    def apply(sides: Int): RegularPolygon =
      if sides > 2 then sides
      else throw new IllegalArgumentException(s"Invalid number of sides: $sides")

  extension (sides: RegularPolygon)

    /** @return the underlying number of sides */
    def toSides: Int =
      sides

    def alpha: AngleDegree =
      SimplePolygon.alphaSum(sides) / sides
