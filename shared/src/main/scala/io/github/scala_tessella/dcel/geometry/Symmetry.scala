package io.github.scala_tessella.dcel.geometry

object Symmetry:

  /** A location on the cyclic list where a symmetry axis can pass through.
    *   - Int: The axis passes directly through the element at this index.
    *   - (Int, Int): The axis passes between the elements at these indices (an edge).
    */
  type AxisLocation = Int | (Int, Int)

  extension [A](list: List[A])

    /** Calculates the order of rotational symmetry. Returns the number of times the shape matches itself as
      * it rotates 360 degrees.
      *
      * @return
      *   An integer between 1 and list.size. 1 means no symmetry (only identity), list.size means a perfect
      *   circle (e.g., all elements equal).
      */
    def rotationalSymmetry: Int =
      if list.isEmpty then 1
      else
        val n              = list.size
        // Find the smallest shift 'k' (1 to n) that makes the list equal to itself
        val smallestPeriod = (1 to n).find(k =>
          // Optimization: We only need to check shifts that divide n
          n % k == 0 && list.indices.forall(i => list(i) == list((i + k) % n))
        ).getOrElse(n)

        n / smallestPeriod

    /** Calculates the indices of the of axes of reflectional symmetry.
      *
      * @return
      *   The indices of reflection lines.
      */
    def reflectionalSymmetryIndices: List[Int] =
      if list.isEmpty then Nil
      else
        val n              = list.size
        val doubled        = list ++ list
        val reverseDoubled = list.reverse ++ list.reverse

        // We check rotations of the reversed list against the original.
        // If list == rotate(reverse(list), k), it implies an axis of symmetry exists.
        // This is equivalent to counting how many shifts of the reversed list match the original.
        (0 until n).toList.filter: shift =>
          // Check if original list matches the reversed list shifted by 'shift'
          (0 until n).forall: i =>
            list(i) == reverseDoubled(i + shift)

    /** Calculates the axes of reflectional symmetry. Returns a list of pairs of locations where each axis
      * intersects the cycle.
      *
      * @return
      *   A list where each pair represents the two points on the cycle where the axis passes.
      */
    def reflectionalSymmetryAxes: List[(AxisLocation, AxisLocation)] =
      val n = list.size

      def edgeIndices(i: Int): (Int, Int) =
        (i, (i + 1) % n)

      def oppositeEdgeIndex(i: Int): Int =
        (i + n / 2) % n

      reflectionalSymmetryIndices.map: shift =>
        // The reflection maps index i to (n - 1 - shift - i) % n.
        // Fixed points satisfy 2*i == n - 1 - shift (mod n).
        // Let K = n - 1 - shift.
        val K          = (n - 1 - shift) % n
        val effectiveK = if K < 0 then K + n else K

        if n % 2 != 0 then
          // Odd n: Equation 2*i = K (mod n) has exactly one solution for vertices.
          // Inverse of 2 mod n is (n + 1) / 2.
          val v               = (effectiveK * (n + 1) / 2) % n
          // The axis must also pass through the midpoint of the opposite edge.
          // Edge index e is the edge starting at (v + n / 2) % n.
          val oppositeEdgeIdx = oppositeEdgeIndex(v)
          (v, edgeIndices(oppositeEdgeIdx))
        else
          // Even n
          if effectiveK % 2 == 0 then
            // K is even: 2*i = K (mod n) has two solutions for vertices.
            // i = K / 2 and i = K / 2 + n / 2.
            val v1 = effectiveK / 2
            val v2 = oppositeEdgeIndex(v1)
            (v1, v2)
          else
            // K is odd: No vertex solutions. Axis passes through two edges.
            // The geometric location is K / 2 (half-integer).
            // Corresponding to edge (K - 1) / 2 and opposite edge.
            val e1 = (effectiveK - 1) / 2
            val e2 = oppositeEdgeIndex(e1)
            (edgeIndices(e1), edgeIndices(e2))

    /** Calculates the number of axes of reflectional symmetry.
      *
      * @return
      *   The number of reflection lines.
      */
    def reflectionalSymmetry: Int =
      reflectionalSymmetryIndices.size
