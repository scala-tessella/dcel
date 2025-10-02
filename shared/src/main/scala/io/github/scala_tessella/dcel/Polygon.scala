package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.AngleDegree
import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY

/** Methods to deal with regular polygons in tiling */
object Polygon:

  opaque type SimplePolygon = Vector[AngleDegree]

  object SimplePolygon:

    def alphaSum(sides: Int): AngleDegree =
      AngleDegree(180) * (sides - 2)

    /** Validates the list of interior angles for a simple polygon.
      *
      * @param angles
      *   A list of interior angles in degrees.
      * @throws IllegalArgumentException
      *   if there are fewer than 3 angles, any angle is a full circle, or the sum of the angles is not
      *   correct.
      */
    def apply(angles: Vector[AngleDegree]): SimplePolygon =
      val n = angles.length
      if n < 3 then
        throw new IllegalArgumentException("A simple polygon must have at least 3 sides.")
      if angles.exists(_.isFullCircle) then
        throw new IllegalArgumentException("The polygon cannot have full circles as interior angles.")
      else
        val angleSum         = angles.map(_.normalised).sum2
        val expectedAngleSum = alphaSum(n)
        if (angleSum - expectedAngleSum).toRational.abs > ACCURACY then
          throw new IllegalArgumentException(
            f"The sum of interior angles is incorrect for a polygon with $n sides. Expected ${expectedAngleSum.toRational.toDouble}%.2f, but got ${angleSum.toRational.toDouble}%.2f."
          )
        else
          angles

  extension (angles: SimplePolygon)

    /** @return the underlying number of sides */
    def toAngles: Vector[AngleDegree] =
      angles

  /** Unit regular polygon with the given number of sides */
  opaque type RegularPolygon = Int

  /** Companion object for [[RegularPolygon]] */
  object RegularPolygon:

    /** Create a [[RegularPolygon]] of given sides
      *
      * @param sides
      *   number of sides
      * @throws IllegalArgumentException
      *   if sides <= 2
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

    def angles: Vector[AngleDegree] =
      Vector.fill(sides)(sides.alpha)
