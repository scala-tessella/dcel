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

  extension (sides: RegularPolygon)

    /** @return the underlying number of sides */
    def toSides: Int =
      sides

    def alpha: AngleDegree =
      SimplePolygon.alphaSum(sides) / sides

    def angles: Vector[AngleDegree] =
      Vector.fill(sides)(sides.alpha)
