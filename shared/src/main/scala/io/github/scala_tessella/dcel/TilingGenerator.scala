package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingEquivalency.{hasSameSizesOf, isBoundaryEquivalentTo}
import io.github.scala_tessella.dcel.TilingUniformity.{
  gonalitySampleInnerVertexIds,
  uniformityTreeUncompressed
}
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon}
import io.github.scala_tessella.dcel.structure.Vertex
import io.github.scala_tessella.ring_seq.RingSeq.rotationsAndReflections
import spire.math.Rational

import scala.collection.mutable
import scala.Ordering.Implicits.seqOrdering

object TilingGenerator:

  /** Represents a sequence of polygon sides around a vertex (e.g., List(3, 12, 12)). */
  type VertexSignature = List[Int]

  /** Pre-calculated valid vertex configurations for polygons 3, 4, 6, 12. */
  val validSignatures: Set[VertexSignature] =
    val sides     = List(3, 4, 6, 12)
    val solutions =
      for
        a <- sides
        b <- sides
        c <- sides
        if (AngleDegree.fromSides(a) + AngleDegree.fromSides(b) + AngleDegree.fromSides(
          c
        )).toRational <= Rational(360)
        d <- sides
        if (AngleDegree.fromSides(a) + AngleDegree.fromSides(b) + AngleDegree.fromSides(
          c
        ) + AngleDegree.fromSides(d)).toRational <= Rational(360)
        // We can stop at 6 triangles (3*60=360), so max depth 6 is enough
        e <- sides :+ 0 // 0 represents "stop"
        f <- sides :+ 0
      yield List(a, b, c, d, e, f).filter(_ > 0)

    solutions
      .filter(_.map(AngleDegree.fromSides).reduce(_ + _).isFullCircle)
      .map(normalizeSignature)
      .toSet

  private def normalizeSignature(sig: VertexSignature): VertexSignature =
    sig.rotationsAndReflections.min

  extension (d: AngleDegree)
    def isZero: Boolean = d == AngleDegree(0)

  extension (d: AngleDegree.type)
    def fromSides(sides: Int): AngleDegree = RegularPolygon(sides).alpha

  /** Finds n-uniform n-archimedean tilings.
    *
    * @param n
    *   The target uniformity order.
    * @param maxVertices
    *   Limit size of tiling for performance.
    * @return
    *   A list of representative tilings.
    */
  def findTilings(n: Int, maxVertices: Int = 50): List[TilingDCEL] =
    val found             = mutable.ListBuffer[TilingDCEL]()
    val visitedSignatures = mutable.HashSet[Set[VertexSignature]]()

    // Priority Queue for search: (Tiling, Distinct Signatures Found)
    // Ordered by number of vertices (DFS-like) or size of boundary (BFS-like)
    // Here we use a stack for DFS to find one complete example faster, or Queue for BFS.
    val queue = mutable.Queue[(TilingDCEL, Set[VertexSignature])]()

    // Initialize with single-vertex patches (Vertex Atlases)
    // To ensure we start with valid n-archimedean candidates, we could seed with combinations of n signatures.
    // However, growing from a single triangle/square is simpler.
    val seeds = List(3, 4, 6, 12).map(sides => TilingDCEL.createRegularPolygon(RegularPolygon(sides)))
    seeds.foreach(t => queue.enqueue((t, Set.empty)))
//    println(s"Found ${seeds.size} seeds")

    while queue.nonEmpty do
//      println(s"Queue size: ${queue.size}")
      val (tiling, knownSigs) = queue.dequeue()

      // 1. Check Pruning Conditions
      if tiling.innerFaces.size > 2 && knownSigs.size > n then
        () // Prune: too many vertex types
      else if tiling.vertices.size >= maxVertices then
        // 2. Check Success Condition
        // Check if fully surrounded vertices form exactly n classes
        val classesCount = tiling.uniformityTree.sizeLeaves

        if classesCount == n && knownSigs.size == n then
          // Deduplicate
          if !found.exists(_.isBoundaryEquivalentTo(tiling)) then
            found += tiling
//            println(s"Found candidate with $n classes: ${knownSigs.map(_.mkString("."))}")
      else
        // 3. Expansion Step
        expand(tiling, knownSigs) match
          case nextStates =>
//            println(s"Expanding ${nextStates.size} states")
            // Sort next states to prioritize "forced moves" (filling small gaps)
            // This is a crucial heuristic for performance
            queue.enqueueAll(nextStates)

    found.toList

  private def expand(
      tiling: TilingDCEL,
      currentSigs: Set[VertexSignature]
  ): List[(TilingDCEL, Set[VertexSignature])] =
    // Find boundary vertex with smallest non-zero gap
    val candidates = tiling.boundaryVertices.flatMap { v =>
      val currentAngle = v.currentInteriorAngleSumUnsafe(tiling.outerFace)
      val gap          = AngleDegree(360) - currentAngle
      if gap.isZero then None // Already full (should be internal, or on straight line boundary)
      else Some((v, gap))
    }.sortBy(_._2.toRational) // Smallest gap first

    if candidates.isEmpty then return Nil

    val (targetVertex, gap) = candidates.head

    val maxFaceId = tiling.innerFaces
      .map: face =>
        face.id.value
      .max
//    println(s"maxFaceId: $maxFaceId, targetVertex: $targetVertex, gap: $gap")

    // Try adding polygons
    val possibleMoves = List(3, 4, 6, 12).flatMap { sides =>
      val polyAngle = AngleDegree.fromSides(sides)
      // Heuristic: Polygon must fit in the gap
      // Floating point tolerance is handled by AngleDegree (Rational)
      if polyAngle.toRational <= gap.toRational + 0.0001 then // epsilon for rational?
        // Try to add polygon to the edge ending at targetVertex (CCW growth)
        // We need to find the correct edge incident to targetVertex on the boundary.
        // Usually, we want the edge where targetVertex is the *origin* (next polygon starts here)
        // or *destination*. TilingAddition.addRegularPolygonToBoundary takes "edge starting with vertex".

        val edgeOpt = tiling.boundaryEdges.find(_.origin == targetVertex)
        edgeOpt.map { edge =>
          tiling.maybeAddRegularPolygonToBoundary(edge.origin.id, RegularPolygon(sides))
        }
      else
        None
    }

    // Also handle "Hole Filling" / Zipping if gap is 0 (handled by maybeAddRegularPolygon implicitly if logic exists,
    // or by explicit hole filling logic in TilingAddition).
    // TilingAddition.addRegularPolygon checks for boundary intersections and holes.

    possibleMoves.collect { case Right(newTiling) =>
      // Check for dead angles (< 60 degrees) on the boundary
      val hasDeadAngles = newTiling.boundaryVertices.exists { v =>
        val currentAngle = v.currentInteriorAngleSumUnsafe(newTiling.outerFace)
        val gap          = AngleDegree(360) - currentAngle
        gap.toRational > Rational(0) && gap.toRational < Rational(60)
      }

      if hasDeadAngles then
        None
      else
        // Update signatures
        // We need to check if any *new* vertices became internal
        val newInternalVertices =
          newTiling.innerVertices.filterNot(v => tiling.innerVertices.exists(_.id == v.id))
        val newSigs             = newInternalVertices.map(getSignature(newTiling, _))

        // If any new signature is invalid, discard this path
        if newSigs.forall(validSignatures.contains) then
          Some((newTiling, currentSigs ++ newSigs))
        else
          None
    }.flatten

  private def getSignature(tiling: TilingDCEL, v: Vertex): VertexSignature =
    val edges = v.incidentEdgesUnsafe
    // Collect number of sides of incident faces
    val sides = edges.flatMap(_.incidentFace).filter(_ != tiling.outerFace).map(_.halfEdgesUnsafe.size)
    normalizeSignature(sides)

  extension (tilings: List[TilingDCEL])

    def distinctByBoundaryEquivalency: List[TilingDCEL] =
      tilings
        .foldLeft(List.empty[TilingDCEL]): (acc, tiling) =>
          if acc.exists: t =>
              t.hasSameSizesOf(tiling) && t.isBoundaryEquivalentTo(tiling)
          then
            acc
          else
            tiling :: acc
        .reverse

  extension (tilingsCollections: List[List[TilingDCEL]])

    /** Iterates through the groups and adds only the tilings that are not already present in the accumulator.
      * Since each group is already distinct, we skip checking for duplicates within the same group.
      *
      * @return
      *   Sequence of distinct tilings
      */
    def distinctByBoundaryEquivalency2: List[TilingDCEL] =
      tilingsCollections
        .foldLeft(List.empty[TilingDCEL]): (acc, group) =>
          val newUnique = group.filterNot: tiling =>
            acc.exists: existing =>
              existing.hasSameSizesOf(tiling) && existing.isBoundaryEquivalentTo(tiling)
          newUnique.foldLeft(acc): (list, tiling) =>
            tiling :: list
        .reverse

  extension (tiling: TilingDCEL)

    private def gonality: Int =
      tiling.uniformityTreeUncompressed(Option(0)).sizeLeaves

    def expandRotationally(order: Int): List[TilingDCEL] =
      if !List(2, 3, 4, 6).contains(order) then throw new IllegalArgumentException("Invalid order")

      // take the first segment of boundary vertices

      val boundaryVertexIds = tiling.boundaryVertices.map(_.id)
      if boundaryVertexIds.size % order != 0 then
        throw new IllegalArgumentException("Boundary not a multiple of order")
      val step = boundaryVertexIds.size / order

      // find in the segment the vertex with the lowest vertex id
      val edgeStart =
        (0 until step).minBy: index =>
          boundaryVertexIds(index).value

//      // find in the segment the vertex with the smallest interior angle sum
//      val angles = tiling.boundarySimplePolygon.toAngles
//      val edgeStartAlt = (0 until step).maxBy(i => angles(i).toRational)

      // try adding regular polygons of size 3, 4, 6, 12 to the edge starting at this vertex
      val additions =
        List(3, 4, 6, 12)
          .map: sides =>
            tiling
              .maybeAddRegularPolygonToBoundary(
                boundaryVertexIds(edgeStart),
                RegularPolygon(sides)
              )
              .map: tilingDCEL =>
                (sides, tilingDCEL)
          .flatMap: either =>
            either.toOption

      // for the success cases, repeat the addition symmetrically to the other segments
      additions.flatMap: (sides, grownTiling) =>
        (1 until order).foldLeft(Option(grownTiling)): (maybeGrown, i) =>
          maybeGrown.flatMap: grown =>
            grown
              .maybeAddRegularPolygonToBoundary(
                boundaryVertexIds(edgeStart + i * step),
                RegularPolygon(sides)
              )
              .toOption

  extension (tilings: List[TilingDCEL])

    def expandRotationallyMore(
        order: Int,
        steps: Int = 1,
        uniformity: Option[Int] = None,
        gonality: Option[Int] = None
    ): List[TilingDCEL] =
      if uniformity.exists: u =>
        gonality.exists: g =>
          u < g
      then
        throw new IllegalArgumentException("Uniformity cannot be lower than  gonality")
      val startingSize =
        tilings.size
      (0 until steps).foldLeft(tilings): (grownTilings, step) =>
        val (growable, alreadyGrownWithHoleFilling) = grownTilings.partition: tiling =>
          tiling.innerFaces.size == startingSize + order * step
        val nowGrown                                =
          growable
            .map: tiling =>
              tiling.expandRotationally(order)
                .filter: expandedTiling =>
                  expandedTiling.boundarySimplePolygon.toAngles.forall: angle =>
                    angle.toRational <= Rational(300)
                .filter: expandedTiling =>
                  (uniformity, gonality) match
                    case (None, None)       => true
                    case (Some(u), None)    => expandedTiling.uniformityTree.sizeLeaves <= u
                    case (None, Some(g))    => expandedTiling.gonalitySampleInnerVertexIds.size <= g
                    case (Some(u), Some(g)) =>
                      val trees           = expandedTiling.gonalityTreesUnsafe
                      val gonalityOrder   = trees.size
                      def uniformityOrder =
                        trees
                          .map: (_, tree) =>
                            tree.sizeLeaves
                          .sum

                      gonalityOrder <= g && uniformityOrder <= u

        (alreadyGrownWithHoleFilling :: nowGrown)
          .distinctByBoundaryEquivalency2
