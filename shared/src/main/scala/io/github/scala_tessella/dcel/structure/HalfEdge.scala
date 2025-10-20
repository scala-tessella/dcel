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
    s"HalfEdge ${origin.id} -> ${destination.map(_.id).getOrElse("?")}${validate().swap.map(error =>
        s" [${error.message}]"
      ).getOrElse("")}"

  def destination: Option[Vertex] =
    twin.map(_.origin)

  def endpointsAsVertices: Option[(Vertex, Vertex)] =
    destination.map(dest => (origin, dest))

  def key: Option[(VertexId, VertexId)] =
    endpointsAsVertices.map((orig, dest) => (orig.id, dest.id))

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
    traverseUnsafe[T](_.twin.flatMap(_.next))(f)

  def vertexTraversal[T](f: HalfEdge => T = identity): Either[TilingError, List[T]] =
    traverse[T](_.twin.flatMap(_.next))(f)

  private def traverse[T](direction: HalfEdge => Option[HalfEdge])(f: HalfEdge => T =
    identity): Either[TilingError, List[T]] =
    val startEdge = this
    val visited   = mutable.Set[HalfEdge]()

    @tailrec
    def collectEdges(current: HalfEdge, acc: List[T]): Either[TilingError, List[T]] =
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
    traverseUnsafe[T](_.next)(f)

  def faceTraversal[T](f: HalfEdge => T = identity): Either[TilingError, List[T]] =
    traverse[T](_.next)(f)

  def hasIncidentFace(face: Face): Boolean =
    incidentFace.contains(face)

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
      f(halfEdges).foreach {
        case e1 :: e2 :: Nil => e1.linkWith(e2)
        case _               => ()
      }

    // Helper function to link edges in a cycle
    private[dcel] def linkInCycle(): Unit =
      linkIn(_.slidingO(2))

    // Helper function to link edges in a sequence
    private[dcel] def linkInSequence(): Unit =
      linkIn(_.sliding(2))

    private[dcel] def setIncidentFace(face: Face): Unit =
      halfEdges.foreach:
        _.incidentFace = Some(face)

    private[dcel] def setAngle(angle: AngleDegree): Unit =
      halfEdges.foreach:
        _.angle = Some(angle)

    private[dcel] def linkFace(face: Face, angle: AngleDegree): Unit =
      halfEdges.linkInCycle()
      halfEdges.setIncidentFace(face)
      face.outerComponent = halfEdges.headOption
      halfEdges.setAngle(angle)

    def interiorAnglesSum(outerFace: Face): AngleDegree =
      halfEdges
        .filterNot(_.hasIncidentFace(outerFace))
        .flatMap(_.angle)
        .sumExact

    def getPath(from: Vertex, to: Vertex): List[HalfEdge] =
      val startEdgeOpt = halfEdges.find(_.origin == from)

      startEdgeOpt match
        case Some(startEdge) =>
          val holeEdgesList = mutable.ListBuffer[HalfEdge]()
          var currentEdge   = startEdge

          while (currentEdge.destination.get != to && !holeEdgesList.contains(currentEdge))
            holeEdgesList += currentEdge
            currentEdge = currentEdge.next.get

          if currentEdge.destination.get == to then
            holeEdgesList += currentEdge

          holeEdgesList.toList
        case None            => List.empty

    /** Returns the path from the origin vertex to the destination vertex if it exists.
      *
      * @return
      */
    def maybePath: Option[List[HalfEdge]] =
      if halfEdges.isEmpty then return Some(Nil)
      if halfEdges.exists(_.destination.isEmpty) then return None

      val outDegrees = halfEdges.groupBy(_.origin).view.mapValues(_.size)
      val inDegrees  = halfEdges.groupMap(_.destination.get)(_ => 1).view.mapValues(_.sum)
      val vertices   = (outDegrees.keySet ++ inDegrees.keySet).toList

      val startNodeCandidates =
        vertices.filter(v => outDegrees.getOrElse(v, 0) - inDegrees.getOrElse(v, 0) == 1)
      val endNodeCandidates   =
        vertices.filter(v => inDegrees.getOrElse(v, 0) - outDegrees.getOrElse(v, 0) == 1)

      val startVertexOpt =
        if startNodeCandidates.size == 1 && endNodeCandidates.size == 1 then
          startNodeCandidates.headOption
        else if startNodeCandidates.isEmpty && endNodeCandidates.isEmpty then
          // This must be a cycle (or disjoint cycles), so all vertices must be balanced
          if vertices.forall(v => outDegrees.getOrElse(v, 0) == inDegrees.getOrElse(v, 0)) then
            halfEdges.headOption.map(_.origin)
          else
            None
        else
          None

      startVertexOpt.flatMap { startVertex =>
        val edgesByOrigin = mutable.Map.from(halfEdges.groupBy(_.origin).view.mapValues(mutable.Queue.from))
        val path          = mutable.ListBuffer.empty[HalfEdge]

        def findPath(u: Vertex): Unit =
          while (edgesByOrigin.get(u).exists(_.nonEmpty))
            val edge = edgesByOrigin(u).dequeue()
            findPath(edge.destination.get)
            path.prepend(edge)

        findPath(startVertex)

        if path.size == halfEdges.size then Some(path.toList)
        else None // Graph is not connected
      }

    def orderBoundary: List[HalfEdge] =
      if halfEdges.isEmpty then return Nil
      val remaining = mutable.HashSet.from(halfEdges)
      val ordered   = mutable.ListBuffer[HalfEdge]()
      var current   = halfEdges.head
      ordered += current
      remaining -= current
      while remaining.nonEmpty do
        val nextOpt = remaining.find(e => current.destination.contains(e.origin))
        nextOpt match
          case Some(next) =>
            ordered += next
            remaining -= next
            current = next
          case None       =>
            return ordered.toList
      ordered.toList
