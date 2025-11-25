package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.conversion.TilingSVG.toScalableVectorG
import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY
import io.github.scala_tessella.ring_seq.RingSeq.sliceO

import scala.::

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
      parallelogonHexIndices.nonEmpty

    def parallelogonHexIndices: List[Int] =
      val n = angles.size
      if n < 4 || n % 2 != 0 then Nil
      else
        val half                       = n / 2
        val turns: Vector[AngleDegree] = toTurns

        def areOppositeSimple(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Boolean =
          xs.lazyZip(ys).forall(areFitting) || xs.reverse.lazyZip(ys).forall(areFitting)

        // Slices the circular `turns` vector.
        def circularSlice(start: Int, stop: Int): Vector[AngleDegree] =
          turns.sliceO(start, stop).drop(1)

//        val halfIndices =
//          for
//            i <- 0 until half - 1
//            j <- i until half - 1
//            k <- j + 1 until half
//          yield (i, j, k)
//        println(s"half indices $halfIndices")
//        println(half)
//        halfIndices.foreach((i, j, k) =>
//          println(s"i=$i, j=$j, k=$k")
//          val segA = circularSlice(i, j)
//          val segB = circularSlice(j, k)
//          val segC = circularSlice(k, i + half)
//          val segD = circularSlice(i + half, j + half)
//          val segE = circularSlice(j + half, k + half)
//          val segF = circularSlice(k + half, i + n)
//          println(s"segA: $segA, segB: $segB, segC: $segC, segD: $segD, segE: $segE, segF: $segF")
//        )
        val result =
          (0 until half - 1).view.flatMap { i =>

            (i until half - 1).view.flatMap { j =>

              (j + 1 until half).collectFirst {
                case k if {
                      val segA = circularSlice(i, j)
                      val segB = circularSlice(j, k)
                      val segC = circularSlice(k, i + half)
                      val segD = circularSlice(i + half, j + half)
                      val segE = circularSlice(j + half, k + half)
                      val segF = circularSlice(k + half, i + n)

//                println(s"i=$i, j=$j, k=$k")
                      areOppositeSimple(segA, segD) && areOppositeSimple(segB, segE) && areOppositeSimple(
                        segC,
                        segF
                      )
                    } =>
                  List(i, j, k, i + half, j + half, k + half)
              }
            }
          }.headOption.getOrElse(Nil).distinct
        println(result)
        result

    def parallelogonHexEquivalences: List[List[Int]] =
      val n = angles.size
      parallelogonHexIndices match
        case four @ a :: b :: c :: d :: Nil    =>
          four :: (1 until b - a).toList.map(i => List(a + i, (d - i + n) % n))
            ::: (1 until c - b).toList.map(i => List(b + i, (a - i + n) % n))
        case a :: b :: c :: d :: e :: f :: Nil =>
          List(a, c, e) :: List(b, d, f) :: (1 until b - a).toList.map(i => List(a + i, (e - i + n) % n))
            ::: (1 until c - b).toList.map(i => List(b + i, (f - i + n) % n))
            ::: (1 until d - c).toList.map(i => List(c + i, (a - i + n) % n))
        case _                                 => Nil

    def parallelogonTranslationHexIndices: Option[Map[ParallelogramTranslation, Int]] =
      (parallelogonHexIndices match
        case origin :: ac :: bd :: _ :: Nil    => Option(List(origin, ac, bd))
        case origin :: _ :: ac :: _ :: bd :: _ => Option(List(origin, ac, bd))
        case _                                 => None
      )
        .map(i => ParallelogramTranslation.values.zip(i).toMap)
