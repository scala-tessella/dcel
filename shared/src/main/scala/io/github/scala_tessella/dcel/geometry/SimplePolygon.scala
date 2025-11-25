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

  /** Checks if one sequence of turns is the negative of another (antiparallel) when shifted by 1 or +
    * elements.
    *
    * @return
    *   Some(shift) if opposite, None otherwise
    */
  private[dcel] def areOppositeShifted(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Option[Int] =
    if xs != ys || xs.length <= 1 then None
    // changed from xs.length to xs.length + 1
    else (1 to ((xs.length + 1) / 2)).find(n => xs.drop(n).lazyZip(ys.dropRight(n)).forall(areFitting))

  private[dcel] def areOppositeShiftedNew(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Option[Int] =
    if xs.length <= 1 then None
    // changed from xs.length to xs.length + 1
    else (1 to ((xs.length + 1) / 2)).find(n => xs.drop(n).lazyZip(ys.dropRight(n)).forall(areFitting))

  /** Checks if one sequence of turns is the negative of another (antiparallel).
    *
    * @return
    *   Some(shift) if opposite, None otherwise
    */
  private[dcel] def areOpposite(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Option[Int] =
    if xs.size != ys.size then None
    else if xs.lazyZip(ys).forall(areFitting) then Some(0)
    else areOppositeShifted(xs, ys)

  private[dcel] def areOppositeNew(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Option[Int] =
    if xs.size != ys.size then None
    else if xs.lazyZip(ys).forall(areFitting) then Some(0)
    else areOppositeShiftedNew(xs, ys)

  private[dcel] def areOppositeSimple(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Boolean =
    if xs.size != ys.size then throw new IllegalArgumentException("The two sequences must have the same length.")
    xs.lazyZip(ys).forall(areFitting) || xs.reverse.lazyZip(ys).forall(areFitting)

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

    def parallelogonHexIndices: List[Int] =
      val n = angles.size
      if n < 4 || n % 2 != 0 then Nil
      else
        val half = n / 2
        val turns: Vector[AngleDegree] = toTurns

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
                areOppositeSimple(segA, segD) && areOppositeSimple(segB, segE) && areOppositeSimple(segC, segF)
              } =>
                List(i, j, k, i + half, j + half, k + half)
            }
          }
        }.headOption.getOrElse(Nil).distinct
        println(result)
        result

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
//          println(s"n=$n, half=$half")

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

//                    println(s"----\ns=$s, l1=$l1, l2=$l2")
//                    println(s"segA: $segA, segC: $segC, segB: $segB, segD: $segD")
//                    println(s"areOpposite(segA, segC): ${areOpposite(segA, segC)}")
//                    println(s"areOpposite(segA, segC.reverse): ${areOpposite(segA, segC.reverse)}")
//                    println(s"areOpposite(segB, segD): ${areOpposite(segB, segD)}")
//                    println(s"areOpposite(segB, segD.reverse): ${areOpposite(segB, segD.reverse)}")

                    (areOpposite(segA, segC).isDefined || areOpposite(segA, segC.reverse).isDefined)
                    && (areOpposite(segB, segD).isDefined || areOpposite(segB, segD.reverse).isDefined)
                  } =>
                (s, s + l1, s + half, s + half + l1)
            }
          }.headOption

    def parallelogonIndicesNew: Option[(Int, Int, Int, Int)] =
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
          //          println(s"n=$n, half=$half")

          // Exterior turn at each vertex along the boundary (assuming unit edges).
          val turns: Vector[AngleDegree] = toTurns

          // Slices the circular `turns` vector.
          def circularSlice(start: Int, len: Int): Vector[AngleDegree] =
            turns.sliceO(start, start + len).tail

          val solutionsWithShits =
            for
              s <- 0 until half
              l1 <- 1 until half
              l2 = half - l1
              segA = circularSlice(s, l1)
              segB = circularSlice(s + l1, l2)
              segC = circularSlice(s + half, l1)
              segD = circularSlice(s + half + l1, l2)
//              _ = println(s"s:$s l1:$l1 segA:$segA segB:$segB segC:$segC segD:$segD")
              oppAC = areOppositeNew(segA, segC)
              oppACrev = areOppositeNew(segA, segC.reverse)
              oppBD = areOppositeNew(segB, segD)
              oppBDrev = areOppositeNew(segB, segD.reverse)
//              _ = println(s"oppAC:${oppAC.isDefined || oppACrev.isDefined} oppBD:${oppBD.isDefined || oppBDrev.isDefined}")
              if (oppAC.isDefined || oppACrev.isDefined) && (oppBD.isDefined || oppBDrev.isDefined)
            yield
              val minACshift = List(oppAC, oppACrev).flatten.min
              val minBDshift = List(oppBD, oppBDrev).flatten.min
              ((s, (s + l1) % n, (s + half) % n, (s + half + l1) % n), minACshift, minBDshift)

          println(solutionsWithShits)
          if solutionsWithShits.isEmpty then None else Option(solutionsWithShits.minBy((_, a, b) => a + b)._1)

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

//          println(areOpposite(segA, segC))
//          println(areOpposite(segA, segC.reverse))
//          println(areOpposite(segB, segD))
//          println(areOpposite(segB, segD.reverse))

          val oppositeAC = groupOpposite(s, l1, minimumShift(segA, segC))
          val oppositeBD = groupOpposite(s + l1, l2, minimumShift(segB, segD))

          equivalenceGroups(oppositeAC ::: oppositeBD)

    def parallelogonEquivalencesNew: List[List[Int]] =
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

          val (s, s_l1, s_half, s_half_li) = parallelogonIndicesNew.get
          val l1 = s_l1 - s
          val l2 = half - l1
          val segA = circularSlice(s, l1)
          val segB = circularSlice(s + l1, l2)
          val segC = circularSlice(s + half, l1)
          val segD = circularSlice(s + half + l1, l2)

          def groupOpposite(startFirst: Int, len: Int, shift: Int): List[List[Int]] =
            val startOpposite = startFirst + half
            (0 to len).map(i =>
              val reverse = len - i + shift
              val added =
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
            List(areOppositeNew(segment, opposite), areOppositeNew(segment, opposite.reverse)).flatten.min

          //          println(areOpposite(segA, segC))
          //          println(areOpposite(segA, segC.reverse))
          //          println(areOpposite(segB, segD))
          //          println(areOpposite(segB, segD.reverse))

          val oppositeAC = groupOpposite(s, l1, minimumShift(segA, segC))
          val oppositeBD = groupOpposite(s + l1, l2, minimumShift(segB, segD))

          equivalenceGroups(oppositeAC ::: oppositeBD)

    def parallelogonTranslationHexIndices: Option[Map[ParallelogramTranslation, Int]] =
      (parallelogonHexIndices match
        case origin :: ac :: bd :: _ :: Nil    => Option(List(origin, ac, bd))
        case origin :: _ :: ac :: _ :: bd :: _ => Option(List(origin, ac, bd))
        case _                                 => None
      )
        .map(i => ParallelogramTranslation.values.zip(i).toMap)

    def parallelogonTranslationIndices: Option[Map[ParallelogramTranslation, Int]] =
      parallelogonEquivalences match
        case Nil    => None
        case groups =>
          // Choose the pair of equivalent boundary vertices that defines the translation side
          val group = groups.find(_.size > 2).get
          val two   =
            if (group(1) - group.head) >= angles.size / 2 then group.takeRight(2)
            else group.take(2)
          val third = if group.size == 3 then group.diff(two).head else group(3)
          println(s"Indices, origin: ${two.head} and repeat: ${two.last} and repeatOnOtherAxis: $third")
          Option(Map(
            ParallelogramTranslation.Identity -> two.head,
            ParallelogramTranslation.SidesAC   -> two.last,
            ParallelogramTranslation.SidesBD   -> third
          ))

    def parallelogonTranslationIndicesNew: Option[Map[ParallelogramTranslation, Int]] =
      parallelogonEquivalencesNew match
        case Nil => None
        case groups =>
          // Choose the pair of equivalent boundary vertices that defines the translation side
          val group = groups.find(_.size > 2).get
          val two =
            if (group(1) - group.head) >= angles.size / 2 then group.takeRight(2)
            else group.take(2)
          val third = if group.size == 3 then group.diff(two).head else group(3)
          println(s"Indices, origin: ${two.head} and repeat: ${two.last} and repeatOnOtherAxis: $third")
          Option(Map(
            ParallelogramTranslation.Identity -> two.head,
            ParallelogramTranslation.SidesAC -> two.last,
            ParallelogramTranslation.SidesBD -> third
          ))

