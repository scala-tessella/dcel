package io.github.scala_tessella.dcel.geo

import io.github.scala_tessella.dcel.BigDecimalGeometry.ACCURACY

import scala.annotation.targetName

/** Standard unit of angular measure. */
opaque type BigRadian = BigDecimal

/** Companion object for [[BigRadian]] */
object BigRadian:
  /** Create a [[BigRadian]] from a `Double` */
  inline def apply(d: Double): BigRadian = BigDecimal(d)

  /** Create a [[BigRadian]] from a `BigDecimal` */
  inline def apply(b: BigDecimal): BigRadian = b

  /** Tau (2 * Pi), the circle constant. [[https://tauday.com/]] */
  val TAU: BigRadian = BigDecimal(spire.math.pi) * 2

  /** Pi, half of Tau. */
  val TAU_2: BigRadian = BigDecimal(spire.math.pi)
  val TAU_3: BigRadian = TAU / 3

  /** Half of Pi. */
  val TAU_4: BigRadian = BigDecimal(spire.math.pi) / 2
  val TAU_6: BigRadian = TAU_2 / 3

  extension (r: BigRadian)
    /** @return the underlying `BigDecimal` */
    inline def toBigDecimal: BigDecimal =
      r

    @targetName("plus")
    def +(that: BigRadian): BigRadian = r.toBigDecimal + that

    @targetName("minus")
    def -(that: BigRadian): BigRadian = r.toBigDecimal - that

    @targetName("times")
    def *(i: Int): BigRadian = r.toBigDecimal * i

    @targetName("divide")
    def /(i: Int): BigRadian = r.toBigDecimal / i

    /** Tests whether this `SpireRadian` is approximately equal to another, within given accuracy. */
    def almostEquals(that: BigRadian, accuracy: Double = ACCURACY): Boolean =
      (r - that).abs < BigDecimal(accuracy)
