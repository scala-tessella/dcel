package io.github.scala_tessella.dcel.geometry

import spire.compat.numeric
import spire.implicits.*
import spire.math.Rational

import scala.annotation.targetName

opaque type AngleDegree = Rational

object AngleDegree:

  given Ordering[AngleDegree] with
    def compare(x: AngleDegree, y: AngleDegree): Int =
      x.toRational.compare(y.toRational)

  private val R180: Rational = Rational(180)

  private val R360: Rational = Rational(360)

  inline def apply(r: Rational): AngleDegree =
    r

  inline def apply(i: Int): AngleDegree =
    Rational(i)

  inline def apply(bigRadian: BigRadian): AngleDegree =
    Rational(bigRadian.toBigDecimal * 180 / spire.math.pi)

  extension (d: AngleDegree)

    inline def toRational: Rational =
      d

    def toBigRadian: BigRadian =
      BigRadian(BigDecimal(spire.math.pi) * (d / 180).toDouble)

    /** Returns the angle (in degrees) >= 0 and < 360 */
    def normalised: AngleDegree =
      d.toRational.fmod(R360)

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

    @targetName("plusDegree")
    def +(that: AngleDegree): AngleDegree =
      d.toRational + that

    @targetName("minusDegree")
    def -(that: AngleDegree): AngleDegree =
      d.toRational - that

    @targetName("timesInt")
    def *(int: Int): AngleDegree =
      d.toRational * int

    @targetName("divideInt")
    def /(int: Int): AngleDegree =
      d.toRational / int

  extension (degrees: Seq[AngleDegree])

    def sumExact: AngleDegree =
      degrees
        .map: 
          _.toRational
        .sum
