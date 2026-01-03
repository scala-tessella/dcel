package io.github.scala_tessella.dcel.geometry

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

  /** Creates a RegularPolygon from a given interior angle.
    *
    * @param alpha
    *   The interior angle of the polygon in degrees.
    * @return
    *   A RegularPolygon with a corresponding number of sides.
    * @throws IllegalArgumentException
    *   If the provided interior angle does not form a valid regular polygon.
    */
  def fromInteriorAngle(alpha: AngleDegree): RegularPolygon =
    360 / alpha.supplement.toRational match
      case n if n.isWhole => n.toInt
      case _              => throw new IllegalArgumentException(s"Invalid interior angle: $alpha")

  extension (sides: RegularPolygon)

    /** @return the underlying number of sides */
    def toSides: Int =
      sides

    def alpha: AngleDegree =
      SimplePolygon.alphaSum(sides) / sides

    def angles: Vector[AngleDegree] =
      Vector.fill(sides)(sides.alpha)
