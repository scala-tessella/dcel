package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.conversion.TilingSVG.toScalableVectorG
import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY
import io.github.scala_tessella.ring_seq.RingSeq.sliceO

/** Unit simple polygon with the given ordered interior angles */
opaque type SimplePolygon = Vector[AngleDegree]

object SimplePolygon:

  def alphaSum(sides: Int): AngleDegree =
    AngleDegree(180) * (sides - 2)

  /** Validates the list of interior angles for a unit simple polygon.
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
          f"The sum of interior angles is incorrect for a polygon with $n unit sides. Expected ${expectedAngleSum.toRational.toDouble}%.2f, but got ${angleSum.toRational.toDouble}%.2f."
        )
      else
        angles

  def apply(degrees: Int*): SimplePolygon =
    apply(degrees.map(AngleDegree(_)).toVector)

  private val areFitting: (AngleDegree, AngleDegree) => Boolean = _ == _.inverted

  /** Checks if one sequence of turns is the negative of another (antiparallel) when shifted by 1 or +
    * elements.
    *
    * @return
    *   Some(shift) if opposite, None otherwise
    */
  private def areOppositeShifted(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Option[Int] =
    if xs != ys || xs.length <= 1 then None
    else (1 to (xs.length / 2)).find(n => xs.drop(n).lazyZip(ys.dropRight(n)).forall(areFitting))

  /** Checks if one sequence of turns is the negative of another (antiparallel).
    *
    * @return
    *   Some(shift) if opposite, None otherwise
    */

  private def areOpposite(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Option[Int] =
    if xs.size != ys.size then None
    else if xs.lazyZip(ys).forall(areFitting) then Some(0)
    else areOppositeShifted(xs, ys)

  extension (angles: SimplePolygon)

    /** @return the underlying number of sides */
    def toAngles: Vector[AngleDegree] =
      angles

    /** Exterior turn at each vertex along the boundary */
    def toTurns: Vector[AngleDegree] =
      angles.map(_.normalised.supplement)

    def toSVG: String =
      angles.toScalableVectorG()

    def multiplySidesBy(n: Int = 1): SimplePolygon =
      if n < 1 then
        throw new IllegalArgumentException("A simple polygon must have sides of at least unit length.")
      else
        SimplePolygon(angles.flatMap(_ +: Vector.fill(n - 1)(AngleDegree(180))))

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
      parallelogonIndices.isDefined

    def parallelogonIndices: Option[(Int, Int, Int, Int)] =
      val n = angles.size

      // Must have at least 4 sides and an even number of them to form opposite pairs.
      if n < 4 || n % 2 != 0 then None
      else
        // Quick guard: a regular n-gon can tile a torus only if n=4 (a square).
        if angles.map(_.normalised.toRational).distinct.size == 1 then
          if n == 4 then
            println(List(List(0, 1, 2, 3)))
            Some(0, 1, 2, 3)
          else None
        else
          val half = n / 2

          // Exterior turn at each vertex along the boundary (assuming unit edges).
          val turns: Vector[AngleDegree] = toTurns

          // Slices the circular `turns` vector.
          def circularSlice(start: Int, len: Int): Vector[AngleDegree] =
            turns.sliceO(start, start + len).tail

          // Iterate over all possible starting vertices `s` (rotations of the polygon)
          // and all possible splits `l1` of a half-boundary.
          (0 until n).view.flatMap { s =>

            (1 until half).collectFirst {
              case l1 if {
                    val l2   = half - l1
                    val segA = circularSlice(s, l1)
                    val segB = circularSlice(s + l1, l2)
                    val segC = circularSlice(s + half, l1)
                    val segD = circularSlice(s + half + l1, l2)

                    (areOpposite(segA, segC).isDefined || areOpposite(segA, segC.reverse).isDefined)
                    && (areOpposite(segB, segD).isDefined || areOpposite(segB, segD.reverse).isDefined)
                  } =>
                (s, s + l1, s + half, s + half + l1)
            }
          }.headOption

    def parallelogonEquivalences: List[List[Int]] =
      val n = angles.size

      // Must have at least 4 sides and an even number of them to form opposite pairs.
      if n < 4 || n % 2 != 0 then Nil
      else
        // Quick guard: a regular n-gon can tile a torus only if n=4 (a square).
        if angles.map(_.normalised.toRational).distinct.size == 1 then
          if n == 4 then
            List(List(0, 1, 2, 3))
          else Nil
        else
          val half = n / 2

          // Exterior turn at each vertex along the boundary (assuming unit edges).
          val turns: Vector[AngleDegree] = toTurns

          // Slices the circular `turns` vector.
          def circularSlice(start: Int, len: Int): Vector[AngleDegree] =
            turns.sliceO(start, start + len).tail

          val (s, s_l1, s_half, s_half_li) = parallelogonIndices.get
          val l1                           = s_l1 - s
          val l2                           = half - l1
          val segA                         = circularSlice(s, l1)
          val segB                         = circularSlice(s + l1, l2)
          val segC                         = circularSlice(s + half, l1)
          val segD                         = circularSlice(s + half + l1, l2)

          def groupOpposite(startFirst: Int, len: Int, shift: Int): List[List[Int]] =
            val startOpposite = startFirst + half
            (0 to len).map(i =>
              val reverse = len - i + shift
              val added   =
                if i < shift then reverse % len
                else reverse
              List((startOpposite + added) % n, startFirst + i)
            ).toList

          def equivalenceGroups(unmatched: List[List[Int]]): List[List[Int]] =
            (0 until n).foldLeft(unmatched)((groups, index) =>
              val (found, unfound) = groups.partition(_.contains(index))
              found.flatten.distinct :: unfound
            ).map(_.sorted).sortBy(_.head)

          def minimumShift(segment: Vector[AngleDegree], opposite: Vector[AngleDegree]): Int =
            List(areOpposite(segment, opposite), areOpposite(segment, opposite.reverse)).flatten.min

          val oppositeAC = groupOpposite(s, l1, minimumShift(segA, segC))
          val oppositeBD = groupOpposite(s + l1, l2, minimumShift(segB, segD))

          equivalenceGroups(oppositeAC ::: oppositeBD)
