package io.github.scala_tessella.dcel.structure

import io.github.scala_tessella.dcel.*
import io.github.scala_tessella.dcel.geometry.AngleDegree
import io.github.scala_tessella.ring_seq.RingSeq.slidingO

import scala.annotation.tailrec
import scala.collection.mutable

/** Represents a directed half-edge in the DCEL.
  *
  * @param origin
  *   The vertex from which this half-edge originates.
  * @param twin
  *   An optional reference to the half-edge that is its pair running in the opposite direction.
  * @param incidentFace
  *   An optional reference to the face to the left of this edge as you traverse from its origin vertex to its
  *   destination vertex.
  * @param next
  *   An optional reference to the next half-edge in the boundary traversal of its incident face.
  * @param prev
  *   An optional reference to the previous half-edge in the boundary traversal.
  * @param angle
  *   The angle of the corner at the origin vertex, inside the incident face.
  */
final class HalfEdge(
    val origin: Vertex,
    private[dcel] var twin: Option[HalfEdge] = None,
    private[dcel] var incidentFace: Option[Face] = None,
    private[dcel] var next: Option[HalfEdge] = None,
    private[dcel] var prev: Option[HalfEdge] = None,
    private[dcel] var angle: Option[AngleDegree] = None
):
  override def equals(obj: Any): Boolean = obj match
    case that: HalfEdge => this eq that
    case _              => false

  override def hashCode(): Int = System.identityHashCode(this)

  override def toString: String =
    val validationErrorSuffix =
      validate().toErrorSuffix
    val destinationStr        =
      destination
        .map: vertex =>
          vertex.id.value
        .getOrElse("?")
    s"HalfEdge ${origin.id} -> $destinationStr$validationErrorSuffix"

  def destination: Option[Vertex] =
    twin.map: halfEdge =>
      halfEdge.origin

  private[dcel] def destinationUnsafe: Vertex =
    twin.get.origin

  def isLoop: Option[Boolean] =
    destination.map: halfEdge =>
      halfEdge == origin

  def endpointsAsVertices: Option[(Vertex, Vertex)] =
    destination.map: halfEdge =>
      (origin, halfEdge)

  def key: Option[HalfEdgeId] =
    endpointsAsVertices.map: (orig, dest) =>
      (orig.id, dest.id)

  private[dcel] def keyUnsafe: HalfEdgeId =
    (origin.id, destinationUnsafe.id)

  private[dcel] def linkWith(that: HalfEdge): Unit =
    this.next = Some(that)
    that.prev = Some(this)

  private[dcel] def twinWith(that: HalfEdge): Unit =
    this.twin = Some(that)
    that.twin = Some(this)

  def isComplete: Boolean =
    twin.isDefined && incidentFace.isDefined && next.isDefined && prev.isDefined && angle.isDefined

  def validate(): Either[IncompleteError, Unit] =
    val errors = List(
      Option.when(twin.isEmpty)("Missing twin edge"),
      Option.when(incidentFace.isEmpty)("Missing incident face"),
      Option.when(next.isEmpty)("Missing next edge"),
      Option.when(prev.isEmpty)("Missing previous edge"),
      Option.when(angle.isEmpty)("Missing angle")
    ).flatten

    if errors.isEmpty then Right(())
    else Left(IncompleteError(errors.mkString(", ")))

  private def traverseUnsafe[T](direction: HalfEdge => Option[HalfEdge])(f: HalfEdge => T =
    identity): List[T] =
    val startEdge = this

    @tailrec
    def collectEdges(current: HalfEdge, acc: List[T]): List[T] =
      val updatedAcc = f(current) :: acc
      direction(current) match
        case Some(next) if next ne startEdge => collectEdges(next, updatedAcc)
        case _                               => updatedAcc.reverse

    collectEdges(startEdge, Nil)

  private[dcel] def vertexTraversalUnsafe[T](f: HalfEdge => T = identity): List[T] =
    traverseUnsafe[T](_.twin.flatMap(_.next)): halfEdge =>
      f(halfEdge)

  def vertexTraversal[T](f: HalfEdge => T = identity): Either[TopologyError, List[T]] =
    traverse[T](_.twin.flatMap(_.next)): halfEdge =>
      f(halfEdge)

  private def traverse[T](direction: HalfEdge => Option[HalfEdge])(f: HalfEdge => T =
    identity): Either[TopologyError, List[T]] =
    val startEdge = this
    val visited   = mutable.Set[HalfEdge]()

    @tailrec
    def collectEdges(current: HalfEdge, acc: List[T]): Either[TopologyError, List[T]] =
      if visited.contains(current) then
        Left(TopologyError(s"Cycle detected: edge from vertex ${current.origin.id} has already been visited"))
      else
        visited += current
        val updatedAcc = f(current) :: acc

        direction(current) match
          case Some(next) if next ne startEdge =>
            collectEdges(next, updatedAcc)
          case Some(_)                         =>
            // next == startEdge, we've completed the traversal
            Right(updatedAcc.reverse)
          case None                            =>
            Left(TopologyError(s"Broken edge chain: edge from vertex ${current.origin.id} has no next"))

    collectEdges(startEdge, Nil)

  private[dcel] def faceTraversalUnsafe[T](f: HalfEdge => T = identity): List[T] =
    traverseUnsafe[T](_.next): halfEdge =>
      f(halfEdge)

  def faceTraversal[T](f: HalfEdge => T = identity): Either[TopologyError, List[T]] =
    traverse[T](_.next): halfEdge =>
      f(halfEdge)

  def hasIncidentFace(face: Face): Boolean =
    incidentFace.contains(face)

  /** Generic graph isomorphism checker for tiling structures.
    *
    * @param that
    *   Starting edge in the other structure.
    * @param compareAngles
    *   Function to verify if two edges have compatible angles.
    * @param getNeighbors
    *   Function to map neighbors from edge A to edge B (handles orientation).
    */
  private[dcel] def traverseAndCompare(
      that: HalfEdge,
      compareAngles: (HalfEdge, HalfEdge) => Boolean,
      getNeighbors: (HalfEdge, HalfEdge) => List[(Option[HalfEdge], Option[HalfEdge])]
  ): Boolean =
    val queue   = mutable.Queue((this, that))
    val visited = mutable.Map(this -> that)

    while queue.nonEmpty do
      val (a, b) = queue.dequeue()

      // Compare Local Geometry
      if !compareAngles(a, b) then return false

      // Traverse Neighbors
      val neighborsIterator = getNeighbors(a, b).iterator
      while neighborsIterator.hasNext do
        neighborsIterator.next() match
          case (Some(na), Some(nb)) =>
            if visited.contains(na) then
              // Ensure consistency of existing mapping
              if visited(na) != nb then return false
            else
              // Establish new mapping
              visited(na) = nb
              queue.enqueue((na, nb))
          case (None, None)         => ()           // Consistent absence, both missing
          case _                    => return false // Structural mismatch

    true

  /** Checks if the tiling structure starting at edge is isomorphic to the structure starting at another edge
    *
    * Performs a synchronized traversal (BFS) of both structures.
    */
  def isStructurallyEquivalentTo(that: HalfEdge): Boolean =
    this.traverseAndCompare(
      that,
      compareAngles = (a, b) => a.angle == b.angle,
      getNeighbors = (a, b) =>
        List(
          (a.next, b.next), // Orientation preserved
          (a.twin, b.twin)
        )
    )

  /** Checks if the tiling structure starting at edge is reflectionally equivalent to the structure starting
    * at another edge.
    *
    * Performs a synchronized traversal (BFS) of both structures, comparing with the other's reflection. Since
    * reflection reverses orientation:
    *   - `a.next` matches `b.prev`
    *   - `a.twin` matches `b.twin`
    */
  def isReflectionallyEquivalentTo(that: HalfEdge): Boolean =
    this.traverseAndCompare(
      that,
      // For reflection, b is traversed backwards. The "origin" angle of b (backwards)
      // corresponds to the angle at b's destination in the graph, which is b.next.angle.
      compareAngles = (a, b) => a.angle == b.next.flatMap(_.angle),
      getNeighbors = (a, b) =>
        List(
          (a.next, b.prev), // Orientation reversed: next maps to prev
          (a.twin, b.twin)
        )
    )

object HalfEdge:

  def apply(
      origin: Vertex,
      twin: Option[HalfEdge] = None,
      incidentFace: Option[Face] = None,
      next: Option[HalfEdge] = None,
      prev: Option[HalfEdge] = None,
      angle: Option[AngleDegree] = None
  ): HalfEdge = new HalfEdge(origin, twin, incidentFace, next, prev, angle)

  private[dcel] def createTwinPair(v1: Vertex, v2: Vertex): (HalfEdge, HalfEdge) =
    val edge1 = HalfEdge(v1)
    val edge2 = HalfEdge(v2)
    edge1.twinWith(edge2)
    (edge1, edge2)

  private[dcel] def createTwinHalfEdges(
      origin: Vertex,
      destination: Vertex,
      boundaryFace: Face,
      innerFace: Face,
      boundaryAngle: AngleDegree,
      innerAngle: AngleDegree
  ): (HalfEdge, HalfEdge) =
    val boundaryEdge =
      HalfEdge(origin = origin, incidentFace = Some(boundaryFace), angle = Some(boundaryAngle))
    val innerEdge    = HalfEdge(
      origin = destination,
      twin = Some(boundaryEdge),
      incidentFace = Some(innerFace),
      angle = Some(innerAngle)
    )
    boundaryEdge.twinWith(innerEdge)
    (boundaryEdge, innerEdge)

  private[dcel] def insertBoundarySegment(
      prevEdge: HalfEdge,
      nextEdge: HalfEdge,
      segment: List[HalfEdge]
  ): Unit =
    prevEdge.linkWith(segment.head)
    segment.last.linkWith(nextEdge)
    segment.linkInSequence()

  extension (halfEdges: List[HalfEdge])

    private def linkIn(f: List[HalfEdge] => Iterator[List[HalfEdge]]): Unit =
      f(halfEdges).foreach:
        case e1 :: e2 :: Nil => e1.linkWith(e2)
        case _               => ()

    // Helper function to link edges in a cycle
    private[dcel] def linkInCycle(): Unit =
      linkIn(_.slidingO(2))

    // Helper function to link edges in a sequence
    private[dcel] def linkInSequence(): Unit =
      linkIn(_.sliding(2))

    private[dcel] def setIncidentFace(face: Face): Unit =
      halfEdges.foreach: halfEdge =>
        halfEdge.incidentFace = Some(face)

    private[dcel] def setAngle(angle: AngleDegree): Unit =
      halfEdges.foreach: halfEdge =>
        halfEdge.angle = Some(angle)

    private[dcel] def linkFace(face: Face, angle: AngleDegree): Unit =
      halfEdges.linkInCycle()
      halfEdges.setIncidentFace(face)
      face.outerComponent = halfEdges.headOption
      halfEdges.setAngle(angle)

    def interiorAnglesSum(outerFace: Face): AngleDegree =
      halfEdges
        .filterNot: halfEdge =>
          halfEdge.hasIncidentFace(outerFace)
        .flatMap: halfEdge =>
          halfEdge.angle
        .sumExact

    def getPath(from: Vertex, to: Vertex): List[HalfEdge] =
      val startEdgeOpt =
        halfEdges.find: halfEdge =>
          halfEdge.origin == from

      startEdgeOpt match
        case Some(startEdge) =>
          val holeEdgesList = mutable.ListBuffer[HalfEdge]()
          var currentEdge   = startEdge

          while (currentEdge.destinationUnsafe != to && !holeEdgesList.contains(currentEdge))
            holeEdgesList += currentEdge
            currentEdge = currentEdge.next.get

          if currentEdge.destinationUnsafe == to then
            holeEdgesList += currentEdge

          holeEdgesList.toList
        case None            => List.empty

    /** Returns the path from the origin vertex to the destination vertex if it exists.
      *
      * @return
      */
    def maybePath: Option[List[HalfEdge]] =
      if halfEdges.isEmpty then return Some(Nil)

      val degrees = mutable.Map.empty[Vertex, Int].withDefaultValue(0)
      for
        edge <- halfEdges
      do
        degrees(edge.origin) += 1
        degrees(edge.destinationUnsafe) -= 1

      val balanced =
        degrees.filter: (_, degree) =>
          degree != 0

      val startVertexOpt =
        if balanced.isEmpty then
          halfEdges.headOption.map: halfEdge =>
            halfEdge.origin
        else if balanced.size == 2 then
          balanced
            .find: (_, degree) =>
              degree == 1
            .map: (vertex, _) =>
              vertex
        else
          None

      startVertexOpt.flatMap: startVertex =>
        val edgesByOrigin = mutable.Map
          .from(
            halfEdges
              .groupBy: halfEdge =>
                halfEdge.origin
              .view
              .mapValues: edges =>
                mutable.Queue.from(edges)
          )
        val path          = mutable.ListBuffer.empty[HalfEdge]

        def findPath(u: Vertex): Unit =
          while (
            edgesByOrigin.get(u).exists: edges =>
              edges.nonEmpty
          )
            val edge = edgesByOrigin(u).dequeue()
            findPath(edge.destinationUnsafe)
            path.prepend(edge)

        findPath(startVertex)

        if path.size == halfEdges.size then Some(path.toList)
        else None // Graph is not connected

    def orderBoundary: List[HalfEdge] =
      if halfEdges.isEmpty then return Nil
      val remaining = mutable.HashSet.from(halfEdges)
      val ordered   = mutable.ListBuffer[HalfEdge]()
      var current   = halfEdges.head
      ordered += current
      remaining -= current
      while remaining.nonEmpty do
        val nextOpt = remaining.find: halfEdge =>
          current.destination.contains(halfEdge.origin)
        nextOpt match
          case Some(next) =>
            ordered += next
            remaining -= next
            current = next
          case None       =>
            return ordered.toList
      ordered.toList
