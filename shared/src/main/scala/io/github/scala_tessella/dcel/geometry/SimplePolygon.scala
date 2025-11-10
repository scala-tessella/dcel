package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY
import io.github.scala_tessella.ring_seq.RingSeq.sliceO

opaque type SimplePolygon = Vector[AngleDegree]

object SimplePolygon:

  def alphaSum(sides: Int): AngleDegree =
    AngleDegree(180) * (sides - 2)

  /** Validates the list of interior angles for a simple polygon.
    *
    * @param angles
    *   A list of interior angles in degrees.
    * @throws IllegalArgumentException
    *   if there are fewer than 3 angles, any angle is a full circle, or the sum of the angles is not correct.
    */
  def apply(angles: Vector[AngleDegree]): SimplePolygon =
    val n = angles.length
    if n < 3 then
      throw new IllegalArgumentException("A simple polygon must have at least 3 sides.")
    if angles.exists(_.isFullCircle) then
      throw new IllegalArgumentException("The polygon cannot have full circles as interior angles.")
    else
      val angleSum         = angles.map(_.normalised).sumExact
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

    /** Checks if the polygon can tile a torus.
      *
      * A polygon can tile a torus if it is a "parallelogon": a polygon with an even number of sides that can
      * be partitioned into pairs of opposite sides that are equal in length and parallel. For a simple
      * polygon with unit-length edges, this condition is verified by checking the exterior turning angles.
      *
      * The boundary of the polygon must be divisible into four segments (A, B, C, D) of lengths (l1, l2, l1,
      * l2) such that:
      *   - Segments A and C are opposite and parallel (their turn sequences are antiparallel).
      *   - Segments B and D are opposite and parallel.
      *
      * This is checked for all possible rotations of the polygon and all valid partitions.
      *
      * @return
      *   true if the polygon can tile a torus, false otherwise.
      * @see
      *   [[https://en.wikipedia.org/wiki/Parallelogon]]
      */
    def canTileTorus: Boolean =
      val n = angles.size

      // Must have at least 4 sides and an even number of them to form opposite pairs.
      if n < 4 || n % 2 != 0 then false
      else
        // Quick guard: a regular n-gon can tile a torus only if n=4 (a square).
        if angles.map(_.normalised.toRational).distinct.size == 1 then n == 4
        else
          val half = n / 2

          // Exterior turn at each vertex along the boundary (assuming unit edges).
          val turns: Vector[AngleDegree] = angles.map(a => AngleDegree(180) - a.normalised)

          // Compares two angles for equality within a small tolerance.
          def anglesEq(a: AngleDegree, b: AngleDegree): Boolean =
            val R360 = AngleDegree(360).toRational
            val d    = ((a - b).toRational % R360 + R360) % R360
            val md   = if d > R360 / 2 then R360 - d else d
            md <= ACCURACY

          // Checks if one sequence of turns is the negative of another (antiparallel).
          def areOpposite(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Boolean =
            xs.length == ys.length && xs.lazyZip(ys).forall((x, y) => anglesEq(x, AngleDegree(-y.toRational)))

          // Slices the circular `turns` vector.
          def circularSlice(start: Int, len: Int): Vector[AngleDegree] =
            turns.sliceO(start, start + len)

          // Iterate over all possible starting vertices `s` (rotations of the polygon)
          // and all possible splits `l1` of a half-boundary.
          (0 until n).exists { s =>

            (1 until half).exists { l1 =>
              val l2 = half - l1

              // The four segments of the boundary.
              val segA = circularSlice(s, l1)
              val segB = circularSlice(s + l1, l2)
              val segC = circularSlice(s + half, l1)
              val segD = circularSlice(s + half + l1, l2)

              def oppositeCheck(oppositeA: Vector[AngleDegree], oppositeB: Vector[AngleDegree]) =
                (areOpposite(segA, oppositeA) && areOpposite(segB, oppositeB)) ||
                  (areOpposite(segA, oppositeA.reverse) && areOpposite(segB, oppositeB.reverse))

              // Case 1: A is opposite C, B is opposite D. This can be direct or reversed.
              val ac_bd = oppositeCheck(segC, segD)

              // Case 2: A is opposite D, B is opposite C. Requires l1 == l2 (checked by `areOpposite`).
              val ad_bc = oppositeCheck(segD, segC)

              ac_bd || ad_bc
            }
          }

    def parallelogonIndices: Option[(Int, Int, Int, Int)] =
      ???
