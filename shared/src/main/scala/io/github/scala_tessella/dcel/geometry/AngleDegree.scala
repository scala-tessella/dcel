package io.github.scala_tessella.dcel.geometry

import spire.compat.numeric
import spire.implicits.*
import spire.math.Rational

import scala.annotation.targetName

/** An angle in degrees, opaque-typed over `spire.math.Rational` for exact arithmetic. Backed by `Rational` so
  * that values like `60`, `120`, `60/7` and their sums round-trip exactly — critical for the angle-sum
  * closure checks performed by [[SimplePolygon.fromUntrusted]] and
  * [[TilingValidation.validateGeometrically]].
  *
  * Construct from an `Int`, a `Rational`, or a [[BigRadian]] via [[AngleDegree.apply]]. The companion's
  * extensions cover arithmetic (`+`, `-`, `*`, `/`), normalisation, and the three usual transforms:
  * `inverted` (`-d`), `supplement` (`180 - d`), `conjugate` (`360 - d`).
  */
opaque type AngleDegree = Rational

/** Companion object for [[AngleDegree]]. Holds the smart constructors and the extension method set. */
object AngleDegree:

  given Ordering[AngleDegree] with
    def compare(x: AngleDegree, y: AngleDegree): Int =
      x.toRational.compare(y.toRational)

  private val R180: Rational = Rational(180)

  private val R360: Rational = Rational(360)

  /** Wraps a `Rational` as an [[AngleDegree]] without any range normalisation. */
  inline def apply(r: Rational): AngleDegree =
    r

  /** Wraps an `Int` count of degrees as an [[AngleDegree]]. */
  inline def apply(i: Int): AngleDegree =
    Rational(i)

  /** Converts a [[BigRadian]] value to degrees. The conversion uses `spire.math.pi` and may lose some
    * precision relative to a pure-rational input.
    */
  inline def apply(bigRadian: BigRadian): AngleDegree =
    Rational(bigRadian.toBigDecimal * 180 / spire.math.pi)

  extension (d: AngleDegree)

    /** Unwraps to the underlying `Rational` value. */
    inline def toRational: Rational =
      d

    /** ADR-0009 candidate A+C: direct `Double` construction. The pre-change pipeline did
      * `BigDecimal(spire.math.pi) * (d / 180).toDouble` only to narrow back to `Double` downstream in the
      * trig call — pure overhead.
      */
    def toBigRadian: BigRadian =
      BigRadian((d / 180).toDouble * Math.PI)

    /** Returns the angle (in degrees) >= 0 and < 360 */
    def normalised: AngleDegree =
      d.toRational.fmod(R360)

    /** True when [[normalised]] is zero — i.e. `d` is a multiple of 360°. */
    def isFullCircle: Boolean =
      normalised == Rational(0)

    /** Returns the angle (in degrees) that is -d. */
    def inverted: AngleDegree =
      -d

    /** Returns the angle (in degrees) that is 360 - d. */
    def conjugate: AngleDegree =
      R360 - d

    /** Returns the angle (in degrees) that is 180 - d. */
    def supplement: AngleDegree =
      R180 - d

    /** Sum of two angles. */
    @targetName("plusDegree")
    def +(that: AngleDegree): AngleDegree =
      d.toRational + that

    /** Difference of two angles. */
    @targetName("minusDegree")
    def -(that: AngleDegree): AngleDegree =
      d.toRational - that

    /** Multiply the angle by an integer factor. */
    @targetName("timesInt")
    def *(int: Int): AngleDegree =
      d.toRational * int

    /** Divide the angle by an integer divisor (exact rational division). */
    @targetName("divideInt")
    def /(int: Int): AngleDegree =
      d.toRational / int

  extension (degrees: Seq[AngleDegree])

    /** Exact rational sum of a sequence of angles — no floating-point drift, no normalisation. */
    def sumExact: AngleDegree =
      degrees
        .map:
          _.toRational
        .sum
