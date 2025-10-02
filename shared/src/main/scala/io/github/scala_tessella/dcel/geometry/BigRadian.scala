package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY

import scala.annotation.targetName

/** Standard unit of angular measure. */
opaque type BigRadian = BigDecimal

/** Companion object for [[BigRadian]] */
object BigRadian:
  /** Create a [[BigRadian]] from a `Double` */
  inline def apply(d: Double): BigRadian = BigDecimal(d)

  /** Create a [[BigRadian]] from a `BigDecimal` */
  inline def apply(b: BigDecimal): BigRadian = b

  // Adjust scale if your geometry uses a specific MathContext.
  // Using spire.math.pi (likely Double) as seed; promote to BigDecimal once and reuse.
  private val PiBD: BigDecimal = BigDecimal(spire.math.pi)

  /** Tau (2 * Pi), the circle constant. [[https://tauday.com/]] */
  val TAU: BigRadian = PiBD * 2

  /** Pi, half of Tau. */
  val TAU_2: BigRadian = PiBD
  val TAU_3: BigRadian = TAU / 3

  /** Half of Pi. */
  val TAU_4: BigRadian = PiBD / 2
  val TAU_6: BigRadian = TAU_2 / 3

  // Typeclass instances for convenience where ordering is needed.
  given scala.math.Ordering[BigRadian] = scala.math.Ordering.by(_.toBigDecimal)
  given CanEqual[BigRadian, BigRadian] = CanEqual.derived

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

    /** Normalize angle into [0, TAU). */
    def normalizeTau: BigRadian =
      val twoPi = TAU.toBigDecimal
      val a     = r % twoPi
      if a < 0 then a + twoPi else a

    /** Normalize angle into (-Pi, Pi]. */
    def normalizePi: BigRadian =
      val a = r.normalizeTau.toBigDecimal
      if a > PiBD then a - TAU.toBigDecimal else a

    /** Modulo by TAU (remainder with sign of dividend). */
    def modTau: BigRadian =
      r.toBigDecimal % TAU.toBigDecimal

    /** Tests whether this BigRadian is approximately equal to another, within given accuracy. */
    def almostEquals(that: BigRadian, accuracy: BigDecimal = BigDecimal(ACCURACY)): Boolean =
      (r - that).abs <= accuracy
