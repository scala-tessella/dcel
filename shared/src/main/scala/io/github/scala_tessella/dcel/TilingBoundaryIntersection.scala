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
        getNext: HalfEdge => HalfEdge,
        getVertex: HalfEdge => Vertex
    ): Either[TilingError, (List[HalfEdge], AngleDegree, HalfEdge)] =
      if check.toRational < 0 then Left(ValidationError("Angle wider than container"))
      else if !check.isFullCircle then Right((acc, check, edge))
      else if angles.isEmpty then Left(ValidationError("Same as container"))
      else
        val nextCheck = boundaryAngleForVertex(getVertex(edge), outerFace, angles.head)
        traverse(getNext(edge), nextCheck, angles.tail, edge :: acc, getNext, getVertex)

    for
      (prepended, startCheck, startEdge) <- traverse(
                                              edgeToBuildOn.prev.get,
                                              boundaryAngles.start,
                                              boundaryAngles.newVertices.reverse,
                                              Nil,
                                              _.prev.get,
                                              _.origin
                                            )
      (appended, endCheck, endEdge)      <- traverse(
                                              edgeToBuildOn.next.get,
                                              boundaryAngles.end,
                                              boundaryAngles.newVertices,
                                              Nil,
                                              _.next.get,
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
    def segmentsFromPairs[A](pairs: Vector[List[A]])(toSegment: (
        A,
        A
    ) => BigLineSegment): Vector[BigLineSegment] =
      pairs.map:
        case a :: b :: Nil => toSegment(a, b)
        case _             => throw new Error("Pairs not in list")

    def decodeIntersectionPairs(
        intersections: Vector[(BigLineSegment, BigLineSegment)],
        newSides: Vector[BigLineSegment],
        oldSides: Vector[BigLineSegment],
        verticesPairs: Vector[List[Vertex]],
        edgesPairs: Vector[List[HalfEdge]]
    ): Vector[(List[Vertex], List[HalfEdge])] =
      intersections.map: (segment1, segment2) =>
        val newIndex = newSides.indexOf(segment1)
        if newIndex >= 0 then
          val j = oldSides.indexOf(segment2)
          if j < 0 then throw new Error("Segment 2 not in list")
          (verticesPairs(newIndex), edgesPairs(j))
        else
          val oldIndex = oldSides.indexOf(segment1)
          if oldIndex < 0 then throw new Error("Intersection not in either list")
          val i        = newSides.indexOf(segment2)
          if i < 0 then throw new Error("Segment 2 not in list")
          (verticesPairs(i), edgesPairs(oldIndex))

    // Create line segments for the new boundary
    val verticesPairs = adjustedTempVertices.sliding(2).toVector
    val newSides      =
      segmentsFromPairs(verticesPairs): (p1, p2) =>
        BigLineSegment(p1.coords, p2.coords)

    val edgesPairs = boundaryEdges.slidingO(2).toVector
    val oldSides   =
      segmentsFromPairs(edgesPairs): (e1, e2) =>
        BigLineSegment(e1.origin.coords, e2.origin.coords)

    // Check for intersections
    if oldSides.hasProperIntersections(newSides) then
      val intersections = oldSides.properIntersections(newSides)
      val decoded       =
        decodeIntersectionPairs(intersections.toVector, newSides, oldSides, verticesPairs, edgesPairs)
      val vertexIds     =
        decoded.map: (verticesPair, edgesPair) =>
          (
            verticesPair.map: vertex =>
              vertex.id,
            edgesPair.map: halfEdge =>
              halfEdge.origin.id
          )
      val edges         =
        vertexIds.map: (e1, e2) =>
          s"${e1.head}-${e1(1)} with ${e2.head}-${e2(1)}"
      Left(ValidationError(s"Boundary intersection: ${edges.mkString(", ")}"))
    else
      Right(())
