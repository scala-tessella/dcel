package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY

import scala.annotation.targetName

/** Standard unit of angular measure.
  *
  * Backed by `Double` as of ADR-0009 candidate C. `AngleDegree.toBigRadian` already capped precision at
  * `Double` via `(d / 180).toDouble` on master, so the prior `BigDecimal` backing was pure allocation
  * overhead for callers that only fed the value to trig primitives. See ADR finding 1.
  */
opaque type BigRadian = Double

/** Companion object for [[BigRadian]] */
object BigRadian:
  /** Create a [[BigRadian]] from a `Double` */
  inline def apply(d: Double): BigRadian = d

  /** Create a [[BigRadian]] from a `BigDecimal`. Lossy — the `BigDecimal` is narrowed to `Double`. Kept for
    * callers that still hold `BigDecimal`.
    */
  inline def apply(b: BigDecimal): BigRadian = b.toDouble

  /** Tau (2 * Pi), the circle constant. [[https://tauday.com/]] */
  val TAU: BigRadian = 2.0 * Math.PI

  /** Pi, half of Tau. */
  val TAU_2: BigRadian = Math.PI
  val TAU_3: BigRadian = TAU / 3

  /** Half of Pi. */
  val TAU_4: BigRadian = Math.PI / 2
  val TAU_6: BigRadian = TAU_2 / 3

  // Typeclass instances for convenience where ordering is needed.
  given scala.math.Ordering[BigRadian] = scala.math.Ordering.Double.TotalOrdering
  given CanEqual[BigRadian, BigRadian] = CanEqual.derived

  extension (r: BigRadian)
    /** Narrow access as `BigDecimal`. Primarily kept for back-compat with callers that fed the value into
      * `spire.math.*` on `BigDecimal`; prefer `toDouble` in new code.
      */
    inline def toBigDecimal: BigDecimal =
      BigDecimal(r)

    /** @return the underlying `Double` */
    inline def toDouble: Double =
      r

    @targetName("plus")
    def +(that: BigRadian): BigRadian = r + that

    @targetName("minus")
    def -(that: BigRadian): BigRadian = r - that

    @targetName("times")
    def *(i: Int): BigRadian = r * i

    @targetName("divide")
    def /(i: Int): BigRadian = r / i

    /** Normalize angle into [0, TAU). */
    def normalizeTau: BigRadian =
      val a = r % TAU
      if a < 0 then a + TAU else a

    /** Normalize angle into (-Pi, Pi]. */
    def normalizePi: BigRadian =
      val a = r.normalizeTau
      if a > Math.PI then a - TAU else a

    /** Modulo by TAU (remainder with sign of dividend). */
    def modTau: BigRadian =
      r % TAU

    /** Tests whether this BigRadian is approximately equal to another, within given accuracy. */
    def almostEquals(that: BigRadian, accuracy: BigDecimal = BigDecimal(ACCURACY)): Boolean =
      Math.abs(r - that) <= accuracy.toDouble
