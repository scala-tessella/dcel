package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY

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

    def isTorusTileable: Boolean =
      if angles.isEmpty then return false

      val n = angles.size

      // Must have at least 4 edges and even count to split into opposite pairs
      if n < 4 || (n % 2 != 0) then return false

      val half = n / 2

      // Quick guard: a regular n-gon (all angles equal) can tile a torus only if n = 4
      val distinctAngles = angles.map(_.normalised.toRational).distinct
      if distinctAngles.size == 1 then return n == 4

      // Exterior turn at each vertex along the boundary (unit edges)
      val turns: Vector[AngleDegree] = angles.map(a => AngleDegree(180) - a.normalised)

      inline def eqWithin(a: AngleDegree, b: AngleDegree): Boolean =
        val R360 = AngleDegree(360).toRational
        val d    = ((a - b).toRational % R360 + R360) % R360
        val md   = if d > R360 - d then R360 - d else d
        md <= ACCURACY

      def seqEq(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Boolean =
        xs.length == ys.length && xs.indices.forall(i => eqWithin(xs(i), ys(i)))

      def seqEqCyclic(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Boolean =
        if xs.length != ys.length then false
        else if xs.isEmpty then true
        else
          val L = xs.length
          var k = 0
          while k < L do
            var ok = true
            var i  = 0
            while i < L && ok do
              if !eqWithin(xs(i), ys((i + k) % L)) then ok = false
              i += 1
            end while
            if ok then return true
            k += 1
          end while
          false

      def seqEqRevCyclic(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Boolean =
        val yr = ys.reverse
        seqEqCyclic(xs, yr)

      def sliceCircular(start: Int, len: Int): Vector[AngleDegree] =
        if len <= 0 then Vector.empty
        else Vector.tabulate(len)(i => turns((start + i) % n))

      // Try all rotations and possible splits l1 + l2 = half with l1,l2 >= 1
      var s = 0
      while s < n do
        var l1 = 1
        while l1 < half do
          val l2    = half - l1
          val A     = sliceCircular(s, l1)
          val B     = sliceCircular(s + l1, l2)
          val C     = sliceCircular(s + l1 + l2, l1)
          val D     = sliceCircular(s + l1 + l2 + l1, l2)

          inline def neg(a: AngleDegree): AngleDegree = AngleDegree(0) - a
          def seqEqOpp(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Boolean =
            xs.length == ys.length && xs.indices.forall(i => eqWithin(xs(i), neg(ys(i))))
          def seqEqOppRev(xs: Vector[AngleDegree], ys: Vector[AngleDegree]): Boolean =
            val yr = ys.reverse
            seqEqOpp(xs, yr)

          val ac_bd =
            (seqEqOpp(A, C) && seqEqOpp(B, D)) || (seqEqOppRev(A, C) && seqEqOppRev(B, D))
          val ad_bc =
            (seqEqOpp(A, D) && seqEqOpp(B, C)) || (seqEqOppRev(A, D) && seqEqOppRev(B, C))
          if ac_bd || ad_bc then return true

          l1 += 1
        end while
        s += 1
      end while

      false
