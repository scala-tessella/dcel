package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.conversion.TilingSVG.toScalableVectorG
import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY
import io.github.scala_tessella.ring_seq.RingSeq.sliceO

/** Unit simple polygon with the given ordered interior angles */
opaque type SimplePolygon = Vector[AngleDegree]

object SimplePolygon:

  enum ParallelogramTranslation:
    case Identity, SidesAC, SidesBD

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
      parallelogonIndices.nonEmpty

    /** Returns the indices of the vertices of the parallelogon, if found
      *
      * @return
      *   the ordered indices, 6 or 4 in the degenerate case of a 4-sides parallelogon, or Nil if the polygon
      *   is not a parallelogon
      */
    def parallelogonIndices: List[Int] =
      val n = angles.size
      if n < 4 || n % 2 != 0 then Nil
      else
        val half                       = n / 2
        val turns: Vector[AngleDegree] = toTurns

        def areOpposite(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Boolean =
          xs.lazyZip(ys).forall(areFitting)

        // Slices the circular `turns` vector.
        def circularSlice(start: Int, stop: Int): Vector[AngleDegree] =
          turns.sliceO(start, stop).drop(1)

//        val halfIndicesAlt =
//          for
//            i <- 0 until half - 1
//            j <- i until half - 1
//            k <- j + 1 until half
//          yield (i, j, k)
//        println(s"half indices $halfIndicesAlt")
//        println(half)
//        halfIndicesAlt.foreach((i, j, k) =>
//          println(s"i=$i, j=$j, k=$k")
//          val segA = circularSlice(i, j)
//          val segB = circularSlice(j, k)
//          val segC = circularSlice(k, i + half)
//          val segD = circularSlice(i + half, j + half)
//          val segE = circularSlice(j + half, k + half)
//          val segF = circularSlice(k + half, i + n)
//          println(s"segA: $segA, segB: $segB, segC: $segC, segD: $segD, segE: $segE, segF: $segF")
//        )

        // all combinations of indices for the start of three consecutive segments in the 6-sides parallelogon
        val halfIndices =
          (0 until half - 1).view.flatMap: i =>
            (i until half - 1).view.flatMap: j =>
              (j + 1 until half).map: k =>
                List(i, j, k)

        // separate the degenerate square results, where the first two indices are the same
        val (sqr, hex) = halfIndices.partition:
          (_: @unchecked) match { case i :: j :: _ => i == j }

        def completeHalf(ijk: List[Int]): List[Int] =
          ijk ::: ijk.map(_ + half)

        def isParallelogon(ijk: List[Int]): Boolean =
          val startStops = completeHalf(ijk) :+ (ijk.head + n)
          val segments   = startStops.sliding(2).collect {
            case start :: stop :: Nil => circularSlice(start, stop)
          }.toVector

          /** Checks that the 3 first segments fits into their opposites */
          def allSegmentsFitting(f: Vector[AngleDegree] => Vector[AngleDegree] = identity): Boolean =
            (0 to 2).forall: i =>
              areOpposite(segments(i), f(segments(i + 3)))

          allSegmentsFitting() || allSegmentsFitting(_.reverse)

        // give precedence to square results
        sqr.find(isParallelogon)
//          .orElse(hex.find(isParallelogon))
          // otherwise, find the result with the longest segment
          .orElse(hex.filter(isParallelogon).maxByOption(ijk =>
            List(ijk(1) - ijk(0), ijk(2) - ijk(1), ijk(0) + half - ijk(2)).max
          ))
          .map(completeHalf(_).distinct)
          .getOrElse(Nil)

    def parallelogonEquivalences: List[List[Int]] =
      val n = angles.size

      // connect the other indices of the segment to those of its antiparallel counterpart
      def connect(i0: Int, i1: Int, i3: Int): List[List[Int]] =
        (1 until i1 - i0).toList.map(i => List(i0 + i, (i3 - i + n) % n))

      parallelogonIndices match
        case sqr @ a :: b :: c :: d :: Nil     => sqr :: connect(a, b, d) ::: connect(b, c, a)
        case a :: b :: c :: d :: e :: f :: Nil =>
          List(a, c, e) :: List(b, d, f) :: connect(a, b, e) ::: connect(b, c, f) ::: connect(c, d, a)
        case _                                 => Nil

    def parallelogonTranslationIndices: Option[Map[ParallelogramTranslation, Int]] =
      (parallelogonIndices match
        case origin :: ac :: bd :: _ :: Nil    => Option(List(origin, ac, bd))
        case origin :: _ :: ac :: _ :: bd :: _ => Option(List(origin, ac, bd))
        case _                                 => None
      )
        .map(i => ParallelogramTranslation.values.zip(i).toMap)

    def parallelogonDoubleIndicesAlt: Option[(Int, Int)] =
      parallelogonIndices match
        case a :: b :: c :: d :: Nil if (b - a) < (c - b)                                  => Some(a, b)
        case a :: b :: c :: d :: Nil                                                       => Some(a, d)
        case a :: b :: c :: d :: e :: f :: Nil if (b - a) >= (c - b) && (b - a) >= (d - c) => Some((a, e))
        case a :: b :: c :: d :: e :: f :: Nil if (c - b) >= (b - a) && (c - b) >= (d - c) => Some((b, f))
        case a :: b :: c :: d :: e :: f :: Nil                                             => Some((c, a))
        case _                                                                             => None
