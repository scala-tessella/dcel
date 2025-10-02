package io.github.scala_tessella.dcel.geometry

import spire.compat.numeric
import spire.implicits.*
import spire.math.Rational

import scala.annotation.targetName

opaque type AngleDegree = Rational

object AngleDegree:

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

    def normalised: AngleDegree =
      d.toRational.fmod(Rational(360))

    def isFullCircle: Boolean =
      normalised == Rational(0)

    def inverted: AngleDegree =
      -d

    def conjugate: AngleDegree =
      Rational(360) - d

    def supplement: AngleDegree =
      Rational(180) - d

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

    def sum2: AngleDegree =
      degrees.map(_.toRational).sum
