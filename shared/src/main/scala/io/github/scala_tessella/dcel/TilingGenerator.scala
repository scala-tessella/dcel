package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingEquivalency.{
  boundarySignature,
  hasSameSizesOf,
  isBoundaryEquivalentTo
}
import io.github.scala_tessella.dcel.TilingUniformity.{
  gonalitySampleInnerVertexIds,
  regularPolygonsUnsafeFrom
}
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon}
import io.github.scala_tessella.dcel.structure.{Vertex, VertexId}
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

  /** Canonical key for search-state deduplication.
    *
    * `boundarySignature` is already canonicalized w.r.t. rotations/reflections, and known signatures are
    * normalized before hashing.
    */
  final private case class SearchStateKey(
      boundary: List[List[AngleDegree]],
      knownSignatures: Set[VertexSignature],
      innerFacesCount: Int
  )

  private def canonicalStateKey(
      tiling: TilingDCEL,
      knownSigs: Set[VertexSignature]
  ): SearchStateKey =
    SearchStateKey(
      boundary = tiling.boundarySignature,
      knownSignatures = knownSigs.map(normalizeSignature),
      innerFacesCount = tiling.innerFaces.size
    )

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
    val found = mutable.ListBuffer[TilingDCEL]()

    // Priority Queue for search: (Tiling, Distinct Signatures Found)
    // Ordered by number of vertices (DFS-like) or size of boundary (BFS-like)
    // Here we use a stack for DFS to find one complete example faster, or Queue for BFS.
    val queue         = mutable.Queue[(TilingDCEL, Set[VertexSignature])]()
    val visitedStates = mutable.HashSet[SearchStateKey]()

    def enqueueIfNew(tiling: TilingDCEL, knownSigs: Set[VertexSignature]): Unit =
      val stateKey = canonicalStateKey(tiling, knownSigs)
      if visitedStates.add(stateKey) then
        queue.enqueue((tiling, knownSigs))

    // Initialize with single-vertex patches (Vertex Atlases)
    // To ensure we start with valid n-archimedean candidates, we could seed with combinations of n signatures.
    // However, growing from a single triangle/square is simpler.
    val seeds = List(3, 4, 6, 12).map(sides => TilingDCEL.createRegularPolygon(RegularPolygon(sides)))
    seeds.foreach: tiling =>
      enqueueIfNew(tiling, Set.empty)
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
        expand(tiling, knownSigs).foreach: (nextTiling, nextKnownSigs) =>
          // Sort next states to prioritize "forced moves" (filling small gaps)
          // This is a crucial heuristic for performance
          enqueueIfNew(nextTiling, nextKnownSigs)

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
    val sides =
      edges.flatMap(_.incidentFace).filter(_ != tiling.outerFace).map(_.halfEdgesUnsafe.size)
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

  extension (tilingsCollections: List[List[GuardedTiling]])

    /** Iterates through the groups and adds only the tilings that are not already present in the accumulator.
      * Since each group is already distinct, we skip checking for duplicates within the same group.
      *
      * @return
      *   Sequence of distinct tilings
      */
    def distinctByBoundaryEquivalency3: List[GuardedTiling] =
      tilingsCollections
        .foldLeft(List.empty[GuardedTiling]): (acc, group) =>
          val newUnique = group.filterNot: (tiling, _) =>
            acc.exists: (existing, _) =>
              existing.hasSameSizesOf(tiling) && existing.isBoundaryEquivalentTo(tiling)
          newUnique.foldLeft(acc): (list, tiling) =>
            tiling :: list
        .reverse

  val validRotationalSymmetryOrders: Set[Int]    = Set(2, 3, 4, 6)
  val targetRegularPolygons: Set[RegularPolygon] =
    Set(3, 4, 6, 12).map:
      RegularPolygon.apply

  extension (tiling: TilingDCEL)

//    private def gonality: Int =
//      tiling.uniformityTreeUncompressed(Option(0)).sizeLeaves
//

    /** Expands the given tiling rotationally by attempting to add regular polygons to its boundary, ensuring
      * symmetry based on the specified rotational order.
      *
      * @param order
      *   The rotational symmetry order. Must be one of the [[validRotationalSymmetryOrders]].
      * @param regularPolygons
      *   The set of different regular polygons that will be added.
      * @return
      *   A list of resulting tilings (TilingDCEL) produced by the rotational expansion.
      * @throws IllegalArgumentException
      *   if the order is invalid or if the boundary size is not a multiple of the order.
      */
    def expandRotationally(
        order: Int,
        regularPolygons: Set[RegularPolygon] = targetRegularPolygons
    ): List[TilingDCEL] =
      require(validRotationalSymmetryOrders.contains(order), s"Invalid rotational symmetry: $order")

      // take the first segment of boundary vertices
      val boundaryVertexIds =
        tiling.boundaryVertices.map:
          _.id
      if boundaryVertexIds.size % order != 0 then
        throw new IllegalArgumentException("Boundary not a multiple of order")
      val segmentSize = boundaryVertexIds.size / order

      // find in the segment the vertex with the lowest vertex id
      val edgeStartIndex: Int =
        (0 until segmentSize).minBy: index =>
          boundaryVertexIds(index)

//      // find in the segment the vertex with the smallest interior angle sum
//      val angles = tiling.boundarySimplePolygon.toAngles
//      val edgeStartAlt = (0 until segmentSize).maxBy(i => angles(i).toRational)

      val initialVertexId = boundaryVertexIds(edgeStartIndex)

      /** Attempt initial addition, then propagate to other segments if successful */
      def maybeSymmetricAddition(regularPolygon: RegularPolygon): Option[TilingDCEL] =
        (1 until order).foldLeft(tiling.maybeAddRegularPolygonToBoundary(
          initialVertexId,
          regularPolygon
        ).toOption): (maybeTiling, segmentIndex) =>
          maybeTiling.flatMap: currentTiling =>
            val symmetricVertexId = boundaryVertexIds(edgeStartIndex + segmentIndex * segmentSize)
            currentTiling.maybeAddRegularPolygonToBoundary(symmetricVertexId, regularPolygon).toOption

      for
        regularPolygon <- regularPolygons.toList
        grownTiling    <- maybeSymmetricAddition(regularPolygon)
      yield grownTiling

  type GuardedTiling = (TilingDCEL, Option[Set[RegularPolygon]])

  extension (tilings: List[TilingDCEL])

    def expandRotationallyMoreOld(
        order: Int,
        steps: Int = 1,
        uniformity: Option[Int] = None,
        gonality: Option[Int] = None
    ): List[TilingDCEL] =
      if uniformity.exists: u =>
          gonality.exists: g =>
            u < g
      then
        throw new IllegalArgumentException("Uniformity cannot be lower than gonality")
      val startingSize =
        tilings.head.innerFaces.size
      (0 until steps).foldLeft(tilings): (grownTilings, step) =>
        val (growable, alreadyGrownWithHoleFilling) =
          grownTilings.partition: tiling =>
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
        throw new IllegalArgumentException("Uniformity cannot be lower than gonality")
      val startingSize                                =
        tilings.head.innerFaces.size
      val startingGuardedTilings: List[GuardedTiling] =
        tilings.map: t =>
          (t, None)
      (0 until steps).foldLeft(startingGuardedTilings): (grownTilings, step) =>
        val expectedTotalPolygons =
          startingSize + order * step

        def cutter(g: Int, tilingG: Int): Boolean =
          val limits = List(16, 32, 64, 128)
          limits.indices.forall: index =>
            !(g > (index + 1) && expectedTotalPolygons > limits(index) && tilingG == (index + 1))

        val (growable, alreadyGrownWithHoleFilling) =
          grownTilings.partition: (tiling, _) =>
            tiling.innerFaces.size == expectedTotalPolygons
        val nowGrown                                =
          growable
            .map: (tiling, maybeGuard) =>
              val expandedTilings =
                maybeGuard match
                  case Some(regularPolygons) => tiling.expandRotationally(order, regularPolygons)
                  case None                  => tiling.expandRotationally(order)
              expandedTilings
                .filter:
                  _.boundarySimplePolygon.toAngles.forall: angle =>
                    angle.toRational <= Rational(300)

        val nowGrown2: List[List[GuardedTiling]] =
          (uniformity, gonality) match
            case (None, None)       =>
              nowGrown.map:
                _.map:
                  (_, None)
            case (Some(u), None)    =>
              nowGrown.map:
                _.filter:
                  _.uniformityTree.sizeLeaves <= u
                .map:
                  (_, None)
            case (None, Some(g))    =>
              nowGrown.map:
                _.map: til =>
                  (til, til.gonalitySampleInnerVertexIds)
                .filter: (_, vertexIds) =>
                  vertexIds.size <= g
                .map: (til, vertexIds) =>
                  if vertexIds.size == g then
                    val regularPolygons =
                      vertexIds
                        .flatMap:
                          til.regularPolygonsUnsafeFrom
                        .toSet
                    (til, Some(regularPolygons))
                  else
                    (til, None)
            case (Some(u), Some(g)) =>
              nowGrown.map:
                _.map: til =>
                  (til, til.gonalityTreesUnsafe)
                .filter: (_, trees) =>
                  val gonalityOrder = trees.size

                  def uniformityOrder =
                    trees
                      .map: (_, tree) =>
                        tree.sizeLeaves
                      .sum

                  if !cutter(g, gonalityOrder) then
                    false
                  else
                    gonalityOrder <= g && uniformityOrder <= u
                .map: (til, trees) =>
                  if trees.size == g then
                    val regularPolygons =
                      trees
                        .flatMap: (regularPolygons, _) =>
                          regularPolygons
                        .toSet
                    (til, Some(regularPolygons))
                  else
                    (til, None)

        (alreadyGrownWithHoleFilling :: nowGrown2)
          .distinctByBoundaryEquivalency3
      .map: (tiling, _) =>
        tiling
