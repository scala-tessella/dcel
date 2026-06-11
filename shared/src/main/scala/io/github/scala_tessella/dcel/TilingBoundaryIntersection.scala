package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigLineSegment}
import io.github.scala_tessella.dcel.structure.{Face, HalfEdge, Vertex}
import io.github.scala_tessella.ring_seq.RingSeq.slidingO

import scala.annotation.tailrec

/** Geometric/topological helpers used while growing a tiling: detecting boundary self-intersections, locating
  * the edges shared between a new polygon and the existing boundary, and bundling the boundary angles that
  * result from a growth operation.
  *
  * These are the primitives the growth pipeline in [[TilingAddition]] calls to decide whether a proposed
  * addition is valid and, if so, which boundary edges it consumes.
  */
private[dcel] object TilingBoundaryIntersection:

  /** Boundary angles produced around a proposed polygon addition: the angle at the first existing boundary
    * vertex, the angle at the last one, and the interior angles for each newly introduced vertex in between.
    */
  case class BoundaryAngles(
      start: AngleDegree,
      end: AngleDegree,
      newVertices: List[AngleDegree]
  )

  /** A pair of optional half-edges marking the boundary window that will be rewritten by a growth operation.
    */
  case class BoundaryState(
      prev: Option[HalfEdge],
      next: Option[HalfEdge]
  )

  /** The result of locating edges that the new polygon will share with the existing boundary, plus the
    * residual boundary angles at the two endpoints and the corresponding counts.
    */
  case class SharedEdgesResult(
      sharedEdges: List[HalfEdge],
      startCheck: AngleDegree,
      startEdge: HalfEdge,
      startCounter: Int,
      endCheck: AngleDegree,
      endEdge: HalfEdge,
      endCounter: Int
  )

  // More descriptive boundary angle calculation
  def boundaryAngleForVertex(
      vertex: Vertex,
      outerFace: Face,
      additionalInteriorAngle: AngleDegree
  ): AngleDegree =
    val currentInteriorSum = vertex.currentInteriorAngleSumUnsafe(outerFace)
    (currentInteriorSum + additionalInteriorAngle).conjugate

  def findSharedEdges(
      edgeToBuildOn: HalfEdge,
      boundaryAngles: BoundaryAngles
  )(using outerFace: Face): Either[TilingError, SharedEdgesResult] =

    @tailrec
    def traverse(
        edge: HalfEdge,
        check: AngleDegree,
        angles: List[AngleDegree],
        acc: List[HalfEdge],
        getNext: HalfEdge => Option[HalfEdge],
        getVertex: HalfEdge => Vertex
    ): Either[TilingError, (List[HalfEdge], AngleDegree, HalfEdge)] =
      if check.toRational < 0 then Left(ValidationError("Angle wider than container"))
      else if !check.isFullCircle then Right((acc, check, edge))
      else if angles.isEmpty then Left(ValidationError("Same as container"))
      else
        getNext(edge) match
          case None           => Left(TopologyError("Boundary cycle is broken: edge without next/prev"))
          case Some(nextEdge) =>
            val nextCheck = boundaryAngleForVertex(getVertex(edge), outerFace, angles.head)
            traverse(nextEdge, nextCheck, angles.tail, edge :: acc, getNext, getVertex)

    for
      prevEdge                           <-
        edgeToBuildOn.prev.toRight(TopologyError("Boundary edge without prev"))
      nextEdge                           <-
        edgeToBuildOn.next.toRight(TopologyError("Boundary edge without next"))
      (prepended, startCheck, startEdge) <- traverse(
                                              prevEdge,
                                              boundaryAngles.start,
                                              boundaryAngles.newVertices.reverse,
                                              Nil,
                                              _.prev,
                                              _.origin
                                            )
      (appended, endCheck, endEdge)      <- traverse(
                                              nextEdge,
                                              boundaryAngles.end,
                                              boundaryAngles.newVertices,
                                              Nil,
                                              _.next,
                                              _.destinationUnsafe
                                            )
    yield SharedEdgesResult(
      sharedEdges = prepended ::: edgeToBuildOn :: appended.reverse,
      startCheck = startCheck,
      startEdge = startEdge,
      startCounter = prepended.length,
      endCheck = endCheck,
      endEdge = endEdge,
      endCounter = appended.length
    )

  def checkForBoundaryIntersections(
      adjustedTempVertices: List[Vertex],
      boundaryEdges: List[HalfEdge]
  ): Either[TilingError, Unit] =
    // Windows of fewer than two elements (degenerate input) describe no segment and are skipped;
    // pairs and sides stay index-aligned by construction.
    val verticesPairs: Vector[List[Vertex]] =
      adjustedTempVertices.sliding(2).filter(_.sizeIs == 2).toVector
    val newSides                            =
      verticesPairs.map: pair =>
        BigLineSegment(pair.head.coords, pair(1).coords)

    val edgesPairs: Vector[List[HalfEdge]] =
      boundaryEdges.slidingO(2).filter(_.sizeIs == 2).toVector
    val oldSides                           =
      edgesPairs.map: pair =>
        BigLineSegment(pair.head.origin.coords, pair(1).origin.coords)

    // Check for intersections
    if oldSides.hasProperIntersections(newSides) then
      // First-occurrence index per segment: O(1) decode instead of repeated linear indexOf scans.
      val newIndexOf: Map[BigLineSegment, Int] = newSides.zipWithIndex.reverse.toMap
      val oldIndexOf: Map[BigLineSegment, Int] = oldSides.zipWithIndex.reverse.toMap

      def description(newIndex: Int, oldIndex: Int): String =
        val vertexIds = verticesPairs(newIndex).map(_.id)
        val edgeIds   = edgesPairs(oldIndex).map(_.origin.id)
        s"${vertexIds.head}-${vertexIds(1)} with ${edgeIds.head}-${edgeIds(1)}"

      val edges =
        oldSides.properIntersections(newSides).toVector.map: (segment1, segment2) =>
          (newIndexOf.get(segment1), oldIndexOf.get(segment2)) match
            case (Some(i), Some(j)) => description(i, j)
            case _                  =>
              (newIndexOf.get(segment2), oldIndexOf.get(segment1)) match
                case (Some(i), Some(j)) => description(i, j)
                // Unreachable for segments produced from these very collections; degrade the
                // report rather than crash an error-message builder.
                case _                  => s"$segment1 with $segment2"
      Left(ValidationError(s"Boundary intersection: ${edges.mkString(", ")}"))
    else
      Right(())
