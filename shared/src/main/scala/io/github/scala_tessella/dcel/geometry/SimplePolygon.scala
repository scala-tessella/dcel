package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.{GeometryError, SpatialError, TilingError}
import io.github.scala_tessella.dcel.conversion.SimplePolygonSVG.toScalableVectorG
import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY
import io.github.scala_tessella.ring_seq.RingSeq.*
import io.github.scala_tessella.ring_seq.SymmetryOps.AxisLocation
import spire.implicits.*

/** A simple (non-self-intersecting) polygon with unit-length sides, opaque-typed over its ordered vector of
  * interior angles. "Simple" here is the standard geometric meaning — edges meet only at vertices.
  *
  * Construct from untrusted input via [[SimplePolygon.fromUntrusted]]; the smart constructor runs the full
  * closure / simplicity / angle-sum validation. The extensions on `SimplePolygon` expose readback, coordinate
  * generation, symmetry analysis and parallelogon detection (for torus tiling).
  */
opaque type SimplePolygon = Vector[AngleDegree]

/** Companion object for [[SimplePolygon]]. Holds the smart constructor [[fromUntrusted]] and the static
  * helper [[alphaSum]] used both internally and by callers building their own polygons.
  */
object SimplePolygon:

  /** Sum of interior angles for an `n`-sided simple polygon: `(n - 2) * 180°`. */
  def alphaSum(sides: Int): AngleDegree =
    AngleDegree(180) * (sides - 2)

  private[dcel] def apply(angles: Vector[AngleDegree]): SimplePolygon =
    angles

  private def degreesToAngles(degrees: Int*): Vector[AngleDegree] =
    degrees
      .map:
        AngleDegree(_)
      .toVector

  private[dcel] def apply(degrees: Int*): SimplePolygon =
    apply(degreesToAngles(degrees*))

  /** Constructs a simple unit-side-length polygon from a vector of internal angles.
    *
    * The vector of angles is assumed to be untrusted input, and this method performs validation to ensure
    * that the input corresponds to a valid simple polygon.
    *
    * Validation checks include:
    *   - The polygon must have at least 3 sides.
    *   - Internal angles cannot contain a full circle.
    *   - The sum of the internal angles must match the expected value for a polygon with the specified number
    *     of sides.
    *   - The polygon must not be self-intersecting.
    *   - The polygon's edges must form a closed loop.
    *
    * @param angles
    *   A vector of internal angles represented in degrees, which define the polygon. Each angle must be
    *   normalized and cannot represent a full circle.
    *
    * @return
    *   Either a `TilingError` describing the validation failure, or a `SimplePolygon` successfully
    *   constructed from the validated input angles.
    */
  def fromUntrusted(angles: Vector[AngleDegree]): Either[TilingError, SimplePolygon] =
    val n = angles.length
    if n < 3 then
      Left(GeometryError("A simple polygon must have at least 3 sides."))
    else if angles.exists(_.isFullCircle) then
      Left(GeometryError("The polygon cannot have full circles as interior angles."))
    else
      val angleSum         = angles.map(_.normalised).sumExact
      val expectedAngleSum = alphaSum(n)
      if (angleSum - expectedAngleSum).toRational.abs > ACCURACY then
        Left(GeometryError(
          f"The sum of interior angles is incorrect for a polygon with $n unit sides. Expected ${expectedAngleSum.toRational.toDouble}%.2f, but got ${angleSum.toRational.toDouble}%.2f."
        ))
      else
        val vertices = BigLineSegment(BigPoint.origin, BigPoint(1, 0)).unitPath(angles)
        if !vertices.isSimplePolygon then
          Left(SpatialError("The polygon is self-intersecting."))
        else
          val lastEdgeLength = vertices.head.distanceTo(vertices.last)
          if spire.math.abs(lastEdgeLength - 1.0) > ACCURACY then
            Left(SpatialError(
              f"The polygon does not close. The final edge has length $lastEdgeLength%.4f instead of 1.0."
            ))
          else
            Right(angles)

  def fromUntrusted(degrees: Int*): Either[TilingError, SimplePolygon] =
    fromUntrusted(degreesToAngles(degrees*))

  private def bigPointsFrom(angles: Vector[AngleDegree]): List[BigPoint] =
    BigLineSegment(BigPoint.origin, BigPoint(1, 0)).unitPath(angles)

  private val areFitting: (AngleDegree, AngleDegree) => Boolean = _ == _.inverted

  extension (angles: SimplePolygon)

    /** Unwraps the underlying interior-angle vector. */
    def toAngles: Vector[AngleDegree] =
      angles

    /** Exterior turn at each vertex along the boundary. */
    def toTurns: Vector[AngleDegree] =
      angles.map:
        _.normalised.supplement

    /** Cartesian coordinates of the polygon's vertices, starting at the origin with the first edge along the
      * positive x-axis.
      */
    def toBigPoints: List[BigPoint] =
      SimplePolygon.bigPointsFrom(angles.toAngles)

    /** SVG `<polygon points="…"/>` rendering of the polygon at unit scale. */
    def toSVG: String =
      angles.toScalableVectorG()

    /** Subdivides every side into `n` unit segments by inserting `n - 1` straight (180°) angles between each
      * original pair. The result is geometrically the same shape, just with finer vertex granularity.
      *
      * @throws IllegalArgumentException
      *   if `n < 1`.
      */
    def multiplySidesBy(n: Int = 1): SimplePolygon =
      if n < 1 then
        throw new IllegalArgumentException("A simple polygon must have sides of at least unit length.")
      else
        val straightAngles = Vector.fill(n - 1)(AngleDegree(180))
        SimplePolygon(
          angles.flatMap:
            _ +: straightAngles
        )

    /** Order of the polygon's cyclic (rotational) symmetry group: the largest `k` such that rotating the
      * angle vector by `n / k` positions yields the same polygon. Always between 1 and `n`.
      */
    def rotationalSymmetryOrder: Int =
      angles.rotationalSymmetry

    /** For each rotational-symmetry copy, the index of the representative vertex chosen as the "first" one
      * (picked by largest interior angle within the first segment, then offset by the segment size).
      */
    def rotationalIndices: List[Int] =
      val symmetryOrder = angles.rotationalSymmetryOrder
      val segmentSize   = angles.size / symmetryOrder
      val first         = (0 until segmentSize).maxBy: index =>
        angles(index).toRational
      (0 until symmetryOrder).toList.map: index =>
        first + index * segmentSize

    /** Number of reflection axes the polygon has. Together with [[rotationalSymmetryOrder]] characterises the
      * dihedral subgroup.
      */
    def reflectionalSymmetryOrder: Int =
      angles.symmetry

    /** For each reflection axis, the two ring-seq `AxisLocation`s (vertex or edge midpoint) it passes
      * through.
      */
    def reflectionalIndexPairs: List[(AxisLocation, AxisLocation)] =
      angles.reflectionalSymmetryAxes

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

    private[dcel] def isEquilateralTriangle: Boolean =
      angles.forall: angle =>
        angle == AngleDegree(180) || angle == AngleDegree(60)

    /** Returns the indices of the vertices of the parallelogon, if found
      *
      * @return
      *   the ordered indices, 6 or 4 in the degenerate case of a 4-sides parallelogon, or Nil if the polygon
      *   is not a parallelogon
      */
    def parallelogonIndices: List[Int] =
      val n = angles.size
      if n < 4 || n % 2 != 0 || isEquilateralTriangle then Nil
      else
        val half                       = n / 2
        val turns: Vector[AngleDegree] = toTurns

        def areOpposite(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Boolean =
          xs.lazyZip(ys).forall:
            areFitting

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
        val halfIndices = (0 until half - 1).view.flatMap: i =>
          (i until half - 1).view.flatMap: j =>
            (j + 1 until half).map: k =>
              List(i, j, k)

        // separate the degenerate square results, where the first two indices are the same
        val (sqr, hex) = halfIndices.partition: indexes =>
          (indexes: @unchecked) match
            case i :: j :: _ => i == j

        def completeHalf(ijk: List[Int]): List[Int] =
          ijk ::: ijk.map: index =>
            index + half

        def isParallelogon(ijk: List[Int]): Boolean =
          val startStops = completeHalf(ijk) :+ (ijk.head + n)
          val segments   = startStops.sliding(2)
            .collect:
              case start :: stop :: Nil => circularSlice(start, stop)
            .toVector

          /** Checks that the 3 first segments fits into their opposites */
          def allSegmentsFitting(f: Vector[AngleDegree] => Vector[AngleDegree] = identity): Boolean =
            (0 to 2).forall: i =>
              areOpposite(segments(i), f(segments(i + 3)))

          allSegmentsFitting() || allSegmentsFitting(_.reverse)

        def maxSegmentLength(ijk: List[Int]): Int =
          List(ijk(1) - ijk(0), ijk(2) - ijk(1), ijk(0) + half - ijk(2)).max

        // give precedence to square results
        sqr
          .find:
            isParallelogon
//          .orElse(hex.find(isParallelogon))
          // otherwise, find the result with the longest segment
          .orElse:
            hex
              .filter:
                isParallelogon
              .maxByOption:
                maxSegmentLength
          .map:
            completeHalf(_).distinct
          .getOrElse(Nil)

    /** Groups all vertex indices according to their equivalence in the parallelogon structure.
      *
      * @return
      *   A list of lists of indices
      */
    def parallelogonIndexClasses: List[List[Int]] =
      val n = angles.size

      // connect the other indices of the segment to those of its antiparallel counterpart
      def connect(i0: Int, i1: Int, i3: Int): List[List[Int]] = (1 until i1 - i0).toList.map: i =>
        List(i0 + i, (i3 - i + n) % n)

      parallelogonIndices match
        case sqr @ a :: b :: c :: d :: Nil     => sqr :: connect(a, b, d) ::: connect(b, c, a)
        case a :: b :: c :: d :: e :: f :: Nil =>
          List(a, c, e) :: List(b, d, f) :: connect(a, b, e) ::: connect(b, c, f) ::: connect(c, d, a)
        case _                                 => Nil

    /** Chooses from the result of the `parallelogonIndices` an origin index and a repeat one to double the
      * tiling along its longest parallel segment.
      *
      * @return
      *   a pair of boundary vertex indices, or None if the polygon is not a parallelogon
      */
    def parallelogonDoubleIndices: Option[(Int, Int)] =
      parallelogonIndices match
        case a :: b :: c :: d :: Nil if (b - a) < (c - b)                                  => Some(a, b)
        case a :: b :: c :: d :: Nil                                                       => Some(a, d)
        case a :: b :: c :: d :: e :: f :: Nil if (b - a) >= (c - b) && (b - a) >= (d - c) => Some((a, e))
        case a :: b :: c :: d :: e :: f :: Nil if (c - b) >= (b - a) && (c - b) >= (d - c) => Some((b, f))
        case a :: b :: c :: d :: e :: f :: Nil                                             => Some((c, a))
        case _                                                                             => None
