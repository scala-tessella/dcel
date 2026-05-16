package io.github.scala_tessella.dcel.geometry

/** A unit-side regular polygon, opaque-typed over its number of sides. Two regular polygons are equal iff
  * they have the same side count; an ordering by side count is provided in the companion.
  *
  * Construct via [[RegularPolygon.apply]] (`RegularPolygon(6)`) or via [[RegularPolygon.fromInteriorAngle]]
  * when only the interior angle is known. Use [[toSides]], [[alpha]], [[angles]] to read back the
  * geometric attributes.
  */
opaque type RegularPolygon = Int

/** Companion object for [[RegularPolygon]]. */
object RegularPolygon:

  given Ordering[RegularPolygon] with
    def compare(x: RegularPolygon, y: RegularPolygon): Int =
      x.toSides.compare(y.toSides)

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
    val external = alpha.supplement.toRational
    if external == 0 then
      throw new IllegalArgumentException(s"Invalid interior angle: $alpha")
    else
      360 / external match
        case n if n.isWhole => n.toInt
        case _              => throw new IllegalArgumentException(s"Invalid interior angle: $alpha")

  extension (sides: RegularPolygon)

    /** @return the underlying number of sides */
    def toSides: Int =
      sides

    /** The interior angle at any vertex (uniform across vertices because the polygon is regular). */
    def alpha: AngleDegree =
      SimplePolygon.alphaSum(sides) / sides

    /** The vector of all interior angles. By construction every entry equals [[alpha]] and there are
      * [[toSides]] entries.
      */
    def angles: Vector[AngleDegree] =
      Vector.fill(sides)(sides.alpha)
