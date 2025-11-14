package io.github.scala_tessella.dcel.torus

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.structure.*
import io.github.scala_tessella.dcel.torus.TilingTorusValidation.validate
import io.github.scala_tessella.dcel.{NotFoundError, TilingDCEL, TilingError, TopologyError}
import io.github.scala_tessella.ring_seq.RingSeq.sliceO
import spire.implicits.*

/** Represents the entire tiling structure as a container for its components.
  *
  * @param vertices
  *   List of all vertices in the tiling.
  * @param halfEdges
  *   List of all half-edges in the tiling.
  * @param faces
  *   List of the tiling's interior faces.
  */
final case class TilingTorusDCEL private (
    vertices: List[Vertex],
    halfEdges: List[HalfEdge],
    faces: List[Face]
):

  def isEmpty: Boolean =
    vertices.isEmpty

  def coordinates: Map[VertexId, BigPoint] =
    vertices.map(vertex => vertex.id -> vertex.coords).toMap

  private[dcel] def findVertexUnsafe(vertexId: VertexId): Option[Vertex] =
    vertices.find(_.id == vertexId)

  def findVertex(vertexId: VertexId): Either[NotFoundError, Vertex] =
    findVertexUnsafe(vertexId).toRight(NotFoundError("Vertex", vertexId.value))

  def findFace(faceId: FaceId): Either[NotFoundError, Face] =
    faces.find(_.id == faceId).toRight(NotFoundError("Inner face", faceId.value))

  /** Finds the edge between the two given vertices.
    *
    * @param vertexId1
    *   id of the first vertex
    * @param vertexId2
    *   id the second vertex
    */
  def findVerticesAndEdgeBetween(
      vertexId1: VertexId,
      vertexId2: VertexId
  ): Either[NotFoundError, (Vertex, Vertex, HalfEdge)] =
    for
      v1   <- findVertex(vertexId1)
      v2   <- findVertex(vertexId2)
      edge <-
        v1.findEdgeBetweenUnsafe(v2).toRight(NotFoundError("Edge", s"between $vertexId1 and $vertexId2"))
    yield (v1, v2, edge)

  def facesVertices: List[(FaceId, List[Vertex])] =
    faces.map(face => (face.id, face.getVerticesUnsafe))

  def findFaceVertices(faceId: FaceId): Either[NotFoundError, List[Vertex]] =
    for
      face <- findFace(faceId)
    yield face.getVerticesUnsafe

  private[dcel] def getAnglesAtVertexUnsafe(vertexId: VertexId): List[AngleDegree] =
    val vertex = findVertexUnsafe(vertexId).get
    val edges  = vertex.incidentEdgesUnsafe
    edges.map(_.angle.get)

  /** Returns the ordered angles at the given vertex.
    *
    * @param vertexId
    *   id of the vertex
    */
  def getAnglesAtVertex(vertexId: VertexId): Either[NotFoundError, List[AngleDegree]] =
    for
      vertex <- findVertex(vertexId)
    yield getAnglesAtVertexUnsafe(vertexId)

  private[dcel] def getInnerAnglesAtVertexUnsafe(vertexId: VertexId): List[AngleDegree] =
    val vertex = findVertexUnsafe(vertexId).get
    val edges  = vertex.incidentEdgesUnsafe
    edges.map(_.angle.get)

  /** Returns the ordered inner angles at the given vertex.
    *
    * @param vertexId
    *   id of the vertex
    */
  def getInnerAnglesAtVertex(vertexId: VertexId): Either[NotFoundError, List[AngleDegree]] =
    for
      vertex <- findVertex(vertexId)
    yield getInnerAnglesAtVertexUnsafe(vertexId)

  /** Finds boundary of the tiling.
    *
    * @return
    *   A List of Vertices forming the perimeter
    */
  def unorderedBoundaryVertices: List[Vertex] =
    // filter the vertices that has distance > 1 with at least one adjacent vertex
    vertices.filter { vertex =>

      vertex.adjacentVerticesUnsafe.exists { adjacent =>

        (vertex.coords.distanceTo(adjacent.coords) - 1.0).abs > ACCURACY
      }
    } match
      case Nil      => vertices
      case boundary => boundary

  def toTilingDCEL: Either[TilingError, TilingDCEL] =
    // keep only unit-length edges (and their twins)
    val unitPairs: Set[(Vertex, Vertex)] =
      halfEdges.flatMap(_.endpointsAsVertices).collect {
        case (a, b) if spire.math.abs(a.coords.distanceTo(b.coords) - 1.0) <= ACCURACY => (a, b)
      }.toSet

    // Build new half-edges only for allowed pairs, preserving incidence, next/prev and angles by copying structure.
    // We need fresh HalfEdge instances so we can wire a planar DCEL (with an outer face).
    val newEdgeByOld = scala.collection.mutable.Map[HalfEdge, HalfEdge]()

    def allow(e: HalfEdge): Boolean =
      e.endpointsAsVertices.exists { (a, b) =>

        unitPairs.contains((a, b))
      }

    // Create fresh edges for allowed ones, keep same origin to reuse vertices as-is
    halfEdges.foreach { e =>

      if allow(e) then
        newEdgeByOld(e) = HalfEdge(e.origin)
    }

    // Re-wire twins, next, prev, incident face, angle for the retained edges, but only if the referenced edge was retained too.
    newEdgeByOld.foreach { case (oldE, newE) =>
      // twin
      oldE.twin.foreach { t =>

        newEdgeByOld.get(t).foreach { nt =>

          if newE.twin.isEmpty && nt.twin.isEmpty then newE.twinWith(nt)
        }
      }
      // next/prev (keep only if target exists)
      oldE.next.flatMap(newEdgeByOld.get).foreach(ne => newE.linkWith(ne))
      oldE.prev.flatMap(newEdgeByOld.get).foreach(pe => pe.linkWith(newE))

      // incident face and angle (inner faces only for now)
      newE.angle = oldE.angle
    }

    // Build inner faces from retained edges by walking each face cycle that survived fully.
    // We keep faces whose outerComponent (or any edge) has been retained and forms a closed cycle in the new graph.
    val visited       = scala.collection.mutable.Set[HalfEdge]()
    val newInnerFaces = scala.collection.mutable.ListBuffer[Face]()
    val allNewEdges   = newEdgeByOld.values.toList

    def tryBuildFace(start: HalfEdge): Option[Face] =
      // follow next pointers; ensure we come back to start without gaps
      var curr = start
      val ring = scala.collection.mutable.ListBuffer[HalfEdge]()
      val seen = scala.collection.mutable.Set[HalfEdge]()
      while !seen.contains(curr) && curr.next.isDefined do
        ring += curr
        seen += curr
        curr = curr.next.get
      if curr eq start then
        val f = Face(FaceId(java.util.UUID.randomUUID().toString))
        ring.foreach { e =>

          e.incidentFace = Some(f)
        }
        f.outerComponent = Some(start)
        Some(f)
      else None

    allNewEdges.foreach { e =>

      if !visited.contains(e) then
        val cycle = e.faceTraversalUnsafe()
        cycle.foreach(visited.add)
        // Confirm closure
        if cycle.nonEmpty && cycle.last.next.contains(cycle.head) then
          // Ensure every edge in cycle currently has no face assigned
          if cycle.forall(_.incidentFace.isEmpty) then
            tryBuildFace(e).foreach(newInnerFaces += _)
    }

    // Now build boundary (outer face) by taking all half-edges not assigned to any inner face.
    val boundaryEdges = allNewEdges.filter(_.incidentFace.isEmpty)

    val outer = Face.outer

    if boundaryEdges.nonEmpty then
      // order boundary as a path/cycle and link it; there might be multiple components, connect each
      val remaining         = scala.collection.mutable.Set.from(boundaryEdges)
      var outerComponentSet = false

      while remaining.nonEmpty do
        val seed      = remaining.head
        // try to order this boundary component
        val component = {
          val ordered = remaining.toList.orderBoundary
          if ordered.nonEmpty then ordered else List(seed)
        }
        // link the component in cycle if possible
        component.linkInCycle()
        component.foreach { e =>

          e.incidentFace = Some(outer)
          // boundary edges do not have interior angle; reuse any existing angle or leave None
        }
        if !outerComponentSet && component.nonEmpty then
          outer.outerComponent = Some(component.head)
          outerComponentSet = true
        remaining --= component

    // assign a leaving edge per vertex if missing
    vertices.foreach { v =>

      if v.leaving.isEmpty then
        v.leaving = allNewEdges.find(_.origin eq v)
    }

    TilingDCEL.fromUntrusted(
      vertices = vertices,
      halfEdges = allNewEdges,
      innerFaces = newInnerFaces.toList,
      outerFace = outer
    )

  // Render the tiling edges in 3D torus as SVG paths
  def toSVG3D(
      options: TilingTorusDCEL.TorusSvg3DOptions = TilingTorusDCEL.TorusSvg3DOptions()
  ): String =
    val opt = options

    // Precompute projected positions for vertices
    val vProj = vertices.map { v =>
      val p2 = TilingTorusDCEL.projectOnTorus(v.coords, opt)
      v -> p2
    }.toMap

    // Helper: unwrap delta in [0,1) to shortest signed step in (-0.5, 0.5]
    def wrapDelta(d: Double): Double =
      var x = d - Math.floor(d) // [0,1)
      if x > 0.5 then x -= 1.0
      x

    // Sample a torus-surface path between two points along the chosen wrap (du,dv).
    // If samples < 8 we clamp to 8 to keep it smooth.
    def sampleEdgeCurveWithDelta(
        p0: BigPoint,
        p1: BigPoint,
        du: Double,
        dv: Double,
        samples: Int
    ): Seq[(Double, Double)] =
      val (u0, v0) = TilingTorusDCEL.toUV(p0, opt.uScale, opt.vScale)
      val n        = samples max 8
      (0 to n).map { i =>
        val t = i.toDouble / n
        val u = u0 + du * t
        val v = v0 + dv * t
        val P = TilingTorusDCEL.torusParam(u, v, opt.majorRadius, opt.minorRadius)
        TilingTorusDCEL.rotateAndProject(
          P,
          opt.yawDeg,
          opt.pitchDeg,
          opt.rollDeg,
          opt.camDist,
          opt.imgWidth,
          opt.imgHeight
        )
      }

    // Convenience: shortest wrap deltas between two (u,v) points
    def shortestWrapDelta(p0: BigPoint, p1: BigPoint): (Double, Double) =
      val (u0, v0) = TilingTorusDCEL.toUV(p0, opt.uScale, opt.vScale)
      val (u1, v1) = TilingTorusDCEL.toUV(p1, opt.uScale, opt.vScale)
      (wrapDelta(u1 - u0), wrapDelta(v1 - v0))

    // Edge set: draw each undirected edge once; use origin id ordering to deduplicate
    val undirectedEdges = halfEdges
      .flatMap(_.endpointsAsVertices)
      .map { case (a, b) =>
        if a.id.value <= b.id.value then (a, b) else (b, a)
      }
      .distinct

    // Straight chords (existing look)
    val chordLines = undirectedEdges.map { case (va, vb) =>
      val (x1, y1) = vProj(va)
      val (x2, y2) = vProj(vb)
      s"""<line x1="$x1" y1="$y1" x2="$x2" y2="$y2" stroke="${opt.stroke}" stroke-width="${opt.strokeWidth}"/>"""
    }.mkString("\n        ")

    // Surface-following curves (sampled paths on the torus)
    val curveStroke     = "#1e90ff"
    val curveStrokeW    = Math.max(1.0, opt.strokeWidth - 0.3)
    val samplesPerCurve = 64

    // For pairs that appear twice (two distinct undirected edges between same vertices),
    // draw both complementary wraps: the shortest and its exact complement so the two cover the full ellipse.
    val edgeMultiplicity: Map[(Vertex, Vertex), Int] =
      halfEdges
        .flatMap(_.endpointsAsVertices)
        .map { case (a, b) =>
          if a.id.value <= b.id.value then (a, b) else (b, a)
        }
        .groupBy(identity)
        .view.mapValues(_.size).toMap

    // Helper: exact complementary delta to travel the other half of the torus ellipse
    inline def complementDelta(d: Double): Double =
      if d > 0 then d - 1.0
      else if d < 0 then d + 1.0
      else 0.0

    // Helper: draw a full ring (circle) at fixed u or fixed v passing through (u0,v0)
    def ringAt(u0: Double, v0: Double, constantIsU: Boolean, samples: Int): String =
      val n   = samples max 32
      val pts =
        (0 until n).map { i =>
          val t = i.toDouble / n
          val u = if constantIsU then u0 else t
          val v = if constantIsU then t else v0
          val P = TilingTorusDCEL.torusParam(u, v, opt.majorRadius, opt.minorRadius)
          TilingTorusDCEL.rotateAndProject(
            P,
            opt.yawDeg,
            opt.pitchDeg,
            opt.rollDeg,
            opt.camDist,
            opt.imgWidth,
            opt.imgHeight
          )
        } :+ {
          val u = if constantIsU then u0 else 0.0
          val v = if constantIsU then 0.0 else v0
          val P = TilingTorusDCEL.torusParam(u, v, opt.majorRadius, opt.minorRadius)
          TilingTorusDCEL.rotateAndProject(
            P,
            opt.yawDeg,
            opt.pitchDeg,
            opt.rollDeg,
            opt.camDist,
            opt.imgWidth,
            opt.imgHeight
          )
        }
      val d   = pts.map { case (x, y) =>
        s"$x,$y"
      }.mkString(" ")
      s"""<polyline points="$d" fill="none" stroke="$curveStroke" stroke-width="$curveStrokeW"/>"""

    val surfacePaths = undirectedEdges.flatMap { case (va, vb) =>
      val mult = edgeMultiplicity.getOrElse((va, vb), 1)

      if va eq vb then
        // Self-loop(s): draw great circles on torus parameter axes through the vertex (u0,v0).
        val (u0, v0) = TilingTorusDCEL.toUV(va.coords, opt.uScale, opt.vScale)
        val ringU    = ringAt(u0, v0, constantIsU = true, samplesPerCurve)
        if mult >= 2 then
          val ringV = ringAt(u0, v0, constantIsU = false, samplesPerCurve)
          Seq(ringU, ringV)
        else
          Seq(ringU)
      else
        // Normal pair of distinct vertices
        val (duS, dvS) = shortestWrapDelta(va.coords, vb.coords)
        val pts1       = sampleEdgeCurveWithDelta(va.coords, vb.coords, duS, dvS, samplesPerCurve)
        val d1         = pts1.map { case (x, y) =>
          s"$x,$y"
        }.mkString(" ")
        val first      =
          s"""<polyline points="$d1" fill="none" stroke="$curveStroke" stroke-width="$curveStrokeW"/>"""
        if mult >= 2 then
          val duC  = complementDelta(duS)
          val dvC  = complementDelta(dvS)
          val pts2 = sampleEdgeCurveWithDelta(va.coords, vb.coords, duC, dvC, samplesPerCurve)
          val d2   = pts2.map { case (x, y) =>
            s"$x,$y"
          }.mkString(" ")
          Seq(
            first,
            s"""<polyline points="$d2" fill="none" stroke="$curveStroke" stroke-width="$curveStrokeW"/>"""
          )
        else
          Seq(first)
    }.mkString("\n        ")

    // Face outlines as polygons (optional)
    val facePolys = faces.flatMap { f =>

      f.getVertices.toOption.map { ring =>
        val pts = ring.map(v => vProj(v))
        val d   = pts.map { case (x, y) =>
          s"$x,$y"
        }.mkString(" ")
        s"""<polygon points="$d" fill="${opt.faceFill}" stroke="${opt.faceStroke}" stroke-width="${opt.faceStrokeWidth}"/>"""
      }
    }.mkString("\n        ")

    // --- Torus wireframe ---
    def ringPath(points: Seq[(Double, Double)], stroke: String, width: Double, dash: String = "2,3"): String =
      if points.isEmpty then ""
      else
        val d = points.map { case (x, y) =>
          s"$x,$y"
        }.mkString(" ")
        s"""<polyline points="$d" fill="none" stroke="$stroke" stroke-width="$width" stroke-dasharray="$dash"/>"""

    // Sample N points along a constant-(u or v) ring
    def sampleRing(constUV: Double, constantIsU: Boolean, samples: Int): Seq[(Double, Double)] =
      val n = samples max 8
      (0 until n).map { i =>
        val t     = i.toDouble / n
        val u     = if constantIsU then constUV else t
        val v     = if constantIsU then t else constUV
        val pos3d = TilingTorusDCEL.torusParam(u, v, opt.majorRadius, opt.minorRadius)
        TilingTorusDCEL.rotateAndProject(
          pos3d,
          opt.yawDeg,
          opt.pitchDeg,
          opt.rollDeg,
          opt.camDist,
          opt.imgWidth,
          opt.imgHeight
        )
      } :+ // close the loop
        {
          val u     = if constantIsU then constUV else 0.0
          val v     = if constantIsU then 0.0 else constUV
          val pos3d = TilingTorusDCEL.torusParam(u, v, opt.majorRadius, opt.minorRadius)
          TilingTorusDCEL.rotateAndProject(
            pos3d,
            opt.yawDeg,
            opt.pitchDeg,
            opt.rollDeg,
            opt.camDist,
            opt.imgWidth,
            opt.imgHeight
          )
        }

    // Choose a few rings
    val samplesPerRing = 128
    val wireStroke     = "red"
    val wireWidth      = 0.8

    // Extreme latitude rings (v = 0 outer, v = 0.5 inner)
    val outerRing = ringPath(sampleRing(0.0, constantIsU = false, samplesPerRing), wireStroke, wireWidth)
    val innerRing = ringPath(sampleRing(0.5, constantIsU = false, samplesPerRing), wireStroke, wireWidth)

    // Additional latitude rings (e.g., v = 0.25, 0.75)
    val latRing1 = ringPath(sampleRing(0.25, constantIsU = false, samplesPerRing), wireStroke, wireWidth)
    val latRing2 = ringPath(sampleRing(0.75, constantIsU = false, samplesPerRing), wireStroke, wireWidth)

    // A few longitude rings (constant u)
    val lonRing0 = ringPath(sampleRing(0.00, constantIsU = true, samplesPerRing), wireStroke, wireWidth)
    val lonRing1 = ringPath(sampleRing(0.25, constantIsU = true, samplesPerRing), wireStroke, wireWidth)
    val lonRing2 = ringPath(sampleRing(0.50, constantIsU = true, samplesPerRing), wireStroke, wireWidth)
    val lonRing3 = ringPath(sampleRing(0.75, constantIsU = true, samplesPerRing), wireStroke, wireWidth)

    val wireframe =
      s"""$outerRing
         |$innerRing
         |$latRing1
         |$latRing2
         |$lonRing0
         |$lonRing1
         |$lonRing2
         |$lonRing3""".stripMargin

    // Vertex ID labels (optional)
    val vertexLabels =
      if opt.showVertexIds then
        vertices.map { v =>
          val (x, y) = vProj(v)
          // slight offset for readability
          val dx     = 6.0
          val dy     = -6.0
          s"""<text x="${x + dx}" y="${y + dy}" font-size="10" fill="darkblue">${v.id.value}</text>"""
        }.mkString("\n           |")
      else ""

    s"""<svg xmlns="http://www.w3.org/2000/svg" width="${opt.imgWidth}" height="${opt.imgHeight}" viewBox="0 0 ${opt.imgWidth} ${opt.imgHeight}">
       |<g>
       |<!-- Wireframe -->
       |$wireframe
       |<!-- Face polygons -->
       |$facePolys
       |<!-- Surface paths -->
       |$surfacePaths
       |<!-- Chord lines -->
       |$chordLines
       |<!-- Vertex labels -->
       |$vertexLabels
       |</g>
       |</svg>""".stripMargin

object TilingTorusDCEL:

  // Private internal constructor that bypasses validation
  private[dcel] def apply(
      vertices: List[Vertex],
      halfEdges: List[HalfEdge],
      faces: List[Face]
  ): TilingTorusDCEL =
    new TilingTorusDCEL(vertices, halfEdges, faces)

  // Smart constructor for untrusted sources
  def fromUntrusted(
      vertices: List[Vertex],
      halfEdges: List[HalfEdge],
      faces: List[Face]
  ): Either[TilingError, TilingTorusDCEL] =
    val candidateTiling = apply(vertices, halfEdges, faces)
    validate(candidateTiling).map(_ => candidateTiling)

  def empty: TilingTorusDCEL =
    TilingTorusDCEL(
      vertices = List.empty,
      halfEdges = List.empty,
      faces = List.empty
    )

  def fromTilingDCELalt(tilingDCEL: TilingDCEL): Either[TilingError, TilingTorusDCEL] =
    tilingDCEL.boundarySimplePolygon.parallelogonIndices match
      case None => Left(TopologyError("TilingDCEL does not have a parallelogram boundary"))
      case Some((i0, i1, i2, i3)) =>
        val boundaryVertices = tilingDCEL.boundaryVertices
//        println(boundaryVertices(i0))
//        println(boundaryVertices(i1))
//        println(boundaryVertices(i2))
//        println(boundaryVertices(i3))
        val boundaryVertexIds = boundaryVertices.map(_.id)

        // Ids of the vertices that will not be part of the TilingTorusDCEL
        val deletableVertexIds = boundaryVertexIds.sliceO(i2, i0 + boundaryVertexIds.size + 1)
        println(s"deletableVertexIds: $deletableVertexIds")
        // Vertices that will be part of the TilingTorusDCEL
        val remainingVertices = tilingDCEL.vertices.filterNot(vertex => deletableVertexIds.contains(vertex.id))
        println(s"remainingVertices: ${remainingVertices.size} $remainingVertices")
        println(s"remainingVertices leaving edges: ${remainingVertices.map(_.leaving)}")
        // if a vertex has a leaving edge pointing to a deletable vertex, we need to correct it
        val newVertices = remainingVertices.map(vertex =>
          if deletableVertexIds.contains(vertex.leaving.get.destination.get.id) then
            Vertex(
              vertex.id,
              vertex.coords,
              leaving = tilingDCEL.halfEdges.find(edge => edge.origin.id == vertex.id && remainingVertices.contains(edge.twin.get.origin))
            )
          else
            vertex
        )
        println(s"newVertices: ${newVertices.size} $newVertices")
        println(s"newVertices leaving edges: ${newVertices.map(_.leaving)}")

        // convert vertexId to new vertex
        val toNewVertex: VertexId => Vertex = vertexId => newVertices.find(_.id == vertexId).get


        val double = boundaryVertexIds ++ boundaryVertexIds
        val len = i1 - i0
        val start = boundaryVertexIds(i3) -> toNewVertex(boundaryVertexIds(len))
        val firstPass = (1 to len).foldLeft(List(start))((l, index) =>
          boundaryVertexIds(i3 - index) -> toNewVertex(boundaryVertexIds(i0 + index)) :: l
        )
        val len2 = i2 - i1

        // Map from deletable vertex ID to old vertex that substitutes it
        val substitutionMap = (0 until len2).foldLeft(firstPass)((l, index) =>
          double(i3 + len2 - index) -> toNewVertex(boundaryVertexIds(i1 + index)) :: l
        ).toMap
        println(s"substitution map: $substitutionMap")


        // boundary edges that must be deleted together with their twins
        val deletableBoundaryEdges = tilingDCEL.boundaryEdges.sliceO(i2, i0 + tilingDCEL.boundaryEdges.size)
        val deletableBoundaryEdgesTwins = deletableBoundaryEdges.map(_.twin.get)
        println(s"boundaryVertexIds: $boundaryVertexIds")
        println(s"i0: $i0, i2: $i2")
        println(s"original total edges: ${tilingDCEL.halfEdges.size}")
        println(s"deletableBoundaryEdges: ${deletableBoundaryEdges.size} $deletableBoundaryEdges")
        println(s"deletableBoundaryEdgesTwins: ${deletableBoundaryEdgesTwins.size} $deletableBoundaryEdgesTwins")

        val remainingHalfEdges = tilingDCEL.halfEdges.diff(deletableBoundaryEdges ++ deletableBoundaryEdgesTwins)
        println(s"remainingHalfEdges: ${remainingHalfEdges.size} $remainingHalfEdges")

        // split between edges that need a substitution and those that don't
        val (toBeChanged, unchanged) = remainingHalfEdges.partition(e =>
          deletableVertexIds.contains(e.origin.id) || deletableVertexIds.contains(e.twin.get.origin.id)
        )
        println(s"toBeChanged: ${toBeChanged.size} $toBeChanged")
        println(s"unchanged: ${unchanged.size} $unchanged")

        // create new edges plus twins with substitution
        val changed = toBeChanged.collect {
          case e if deletableVertexIds.contains(e.origin.id) =>
            val he =
              HalfEdge(
                origin = substitutionMap(e.origin.id),
                twin = e.twin,
                incidentFace = e.incidentFace,
                next = e.next,
                prev = e.prev,
                angle = e.angle
              )
            List(
              he,
              HalfEdge(
                origin = toNewVertex(e.twin.map(_.origin.id).get),
                twin = Some(he),
                incidentFace = e.twin.flatMap(_.incidentFace),
                next = e.twin.flatMap(_.next),
                prev = e.twin.flatMap(_.prev),
                angle = e.twin.flatMap(_.angle)
              )
            )
        }.flatten
        println(s"changed: ${changed.size} $changed")
        val newHalfEdges = unchanged ++ changed
        println(s"newHalfEdges: ${newHalfEdges.size} $newHalfEdges")

        fromUntrusted(
          vertices = newVertices,
          halfEdges = newHalfEdges,
          faces = tilingDCEL.innerFaces
        )

  def fromTilingDCEL(tilingDCEL: TilingDCEL): Either[TilingError, TilingTorusDCEL] =
    tilingDCEL.boundarySimplePolygon.parallelogonIndices match
      case None =>
        Left(TopologyError("TilingDCEL does not have a parallelogram boundary"))

      case Some((i0, i1, i2, i3)) =>
        // ---- 1. Boundary vertices and indexed sides ----
        val boundaryVertices = tilingDCEL.boundaryVertices
        val boundaryEdges = tilingDCEL.boundaryEdges

        if boundaryVertices.isEmpty || boundaryEdges.isEmpty then
          return Left(TopologyError("TilingDCEL has empty boundary"))

        val n = boundaryVertices.size
        if n != boundaryEdges.size then
          return Left(TopologyError("Boundary vertices/edges size mismatch"))

        inline def wrap(idx: Int): Int = (idx % n + n) % n

        // Index ring [0..n)
        val idxRing = (0 until n).toVector

        // Inclusive circular slice: [s .. e] with wrap-around via e possibly >= n.
        def inclusiveSide(s: Int, e: Int): Vector[Int] =
          val open = idxRing.sliceO(s, e) // includes s, excludes e (with circular semantics)
          open :+ wrap(e) // add endpoint

        // Four parallelogon sides as inclusive arcs
        val sideA = inclusiveSide(i0, i1) // i0 .. i1
        val sideB = inclusiveSide(i1, i2) // i1 .. i2
        val sideC = inclusiveSide(i2, i3) // i2 .. i3
        val sideD = inclusiveSide(i3, i0 + n) // i3 .. i0 (wrapped)

        if sideA.size != sideC.size || sideB.size != sideD.size then
          return Left(TopologyError("Parallelogram sides do not have matching lengths"))

        // ---- 2. Union–find over boundary vertex IDs using index pairing ----
        import scala.collection.mutable

        val parent = mutable.HashMap.empty[VertexId, VertexId]

        // Make-set for any vertex we touch
        def find(v: VertexId): VertexId =
          parent.get(v) match
            case None =>
              parent(v) = v
              v
            case Some(p) if p == v =>
              v
            case Some(p) =>
              val r = find(p)
              parent(v) = r
              r

        def union(a: VertexId, b: VertexId): Unit =
          val ra = find(a)
          val rb = find(b)
          if ra != rb then parent(ra) = rb

        // A <-> reversed C
        val sideCRev = sideC.reverse
        sideA.zip(sideCRev).foreach { case (ia, ic) =>
          val va = boundaryVertices(ia).id
          val vc = boundaryVertices(ic).id
          union(va, vc)
        }

        // B <-> reversed D
        val sideDRev = sideD.reverse
        sideB.zip(sideDRev).foreach { case (ib, id) =>
          val vb = boundaryVertices(ib).id
          val vd = boundaryVertices(id).id
          union(vb, vd)
        }

        // ---- 3. Build new vertices, one per equivalence class ----
        val oldVertices = tilingDCEL.vertices

        // Representative -> new vertex
        val repToVertex = mutable.HashMap.empty[VertexId, Vertex]

        def repOf(id: VertexId): VertexId =
          // If id was never in parent map, it is its own rep
          parent.get(id).map(find).getOrElse(id)

        // For each original vertex, map its ID to the representative vertex
        oldVertices.foreach { v =>
          val r = repOf(v.id)
          if !repToVertex.contains(r) then
            // Reuse coordinates of the first vertex in the class
            repToVertex(r) = Vertex(r, v.coords)
        }

        val newVertexOfId: Map[VertexId, Vertex] =
          oldVertices.map { v =>
            v.id -> repToVertex(repOf(v.id))
          }.toMap

        val newVertices: List[Vertex] =
          repToVertex.values.toList

        // ---- 4. Clone inner half-edges (drop outer-face half-edges) ----
        val oldInnerEdges = tilingDCEL.halfEdges.filterNot(tilingDCEL.isBoundaryEdge)
        val edgeMap = mutable.HashMap.empty[HalfEdge, HalfEdge]
        val oldInnerEdgesS = oldInnerEdges.toSet

        // First pass: create new edges with correct origin, copy angle only
        oldInnerEdges.foreach { e =>
          val newOrigin = newVertexOfId(e.origin.id)
          val ne = HalfEdge(newOrigin)
          ne.angle = e.angle
          edgeMap(e) = ne
        }

        // ---- 5. Clone inner faces ----
        val oldInnerFaces = tilingDCEL.innerFaces
        val faceMap = mutable.HashMap.empty[Face, Face]

        oldInnerFaces.foreach { f =>
          faceMap(f) = Face(f.id)
        }

        // ---- 6. Wire next/prev and incidentFace using the edge/face maps ----
        oldInnerEdges.foreach { oe =>
          val ne = edgeMap(oe)

          // next
          oe.next.filter(oldInnerEdgesS.contains).foreach { on =>
            ne.next = Some(edgeMap(on))
          }
          // prev
          oe.prev.filter(oldInnerEdgesS.contains).foreach { op =>
            ne.prev = Some(edgeMap(op))
          }
          // incident face
          oe.incidentFace.foreach { of =>
            val nf = faceMap(of)
            ne.incidentFace = Some(nf)
          }
        }

        // Set outerComponent for faces
        oldInnerFaces.foreach { of =>
          val nf = faceMap(of)
          of.outerComponent.foreach { startOld =>
            if oldInnerEdgesS.contains(startOld) then
              nf.outerComponent = Some(edgeMap(startOld))
          }
        }

        // ---- 7. Wire twins for inner–inner edges (non-boundary) ----
        oldInnerEdges.foreach { oe =>
          val ne = edgeMap(oe)
          oe.twin.foreach { ot =>
            if oldInnerEdgesS.contains(ot) then
              val nt = edgeMap(ot)
              if ne.twin.isEmpty && nt.twin.isEmpty then
                ne.twinWith(nt)
          }
        }

        // ---- 8. Rewire boundary-inner edges between opposite sides ----
        val boundaryInnerEdges = boundaryEdges.map(_.twin.get)

        // Helper: get corresponding inner edge and its clone for a boundary index
        def innerOldAt(idx: Int): HalfEdge =
          boundaryInnerEdges(idx)

        def innerNewAt(idx: Int): HalfEdge =
          edgeMap(innerOldAt(idx))

        val innerA = sideA.map(innerNewAt)
        val innerB = sideB.map(innerNewAt)
        val innerC = sideC.map(innerNewAt)
        val innerD = sideD.map(innerNewAt)

        // A <-> C (note: we already reversed C for vertex equivalence, but edges are oriented; we still pair by index)
        innerA.zip(innerC).foreach { case (ea, ec) =>
          // Overwrite any existing twin relation (should be none at this point)
          ea.twin = None
          ec.twin = None
          ea.twinWith(ec)
        }

        // B <-> D
        innerB.zip(innerD).foreach { case (eb, ed) =>
          eb.twin = None
          ed.twin = None
          eb.twinWith(ed)
        }

        // ---- 9. Ensure each new vertex has a leaving edge ----
        val allNewEdges = edgeMap.values.toList
        newVertices.foreach { v =>
          if v.leaving.isEmpty then
            v.leaving = allNewEdges.find(_.origin eq v)
        }

        // ---- 10. Build the torus DCEL (only inner faces, no outer face) ----
        val newFaces = faceMap.values.toList

        println(s"newFaces: ${newFaces.size} $newFaces")
        println(s"newVertices: ${newVertices.size} $newVertices")
        println(s"allNewEdges: ${allNewEdges.size} $allNewEdges")
        fromUntrusted(
          vertices = newVertices,
          halfEdges = allNewEdges,
          faces = newFaces
        )

  // 3D SVG options
  final case class TorusSvg3DOptions(
      majorRadius: Double = 150.0, // R
      minorRadius: Double = 60.0,  // r
      imgWidth: Int = 600,
      imgHeight: Int = 600,
      stroke: String = "#333",
      strokeWidth: Double = 1.5,
      faceStroke: String = "#888",
      faceStrokeWidth: Double = 1.0,
      faceFill: String = "none",
      showVertexIds: Boolean = false,
      // camera
      camDist: Double = 600.0,     // distance of camera from origin along -Z
      yawDeg: Double = -30.0,      // rotation around Z
      pitchDeg: Double = 25.0,     // rotation around X
      rollDeg: Double = 0.0,       // rotation around Y
      // how to interpret 2D coords as torus parameters
      // coords are assumed to be in a unit square tile grid, we map x,y to u,v in [0,1] via scaling
      uScale: Double = 1.0,
      vScale: Double = 1.0
  )

  // Convert (u,v) in [0,1]x[0,1] to torus (x,y,z) with radii (R,r)
  private def torusParam(u: Double, v: Double, R: Double, r: Double): (Double, Double, Double) =
    val U = 2.0 * Math.PI * u
    val V = 2.0 * Math.PI * v
    val x = (R + r * Math.cos(V)) * Math.cos(U)
    val y = (R + r * Math.cos(V)) * Math.sin(U)
    val z = r * Math.sin(V)
    (x, y, z)

  // Basic 3D rotations (yaw Z, pitch X, roll Y) then simple perspective projection
  private def rotateAndProject(
      p: (Double, Double, Double),
      yaw: Double,
      pitch: Double,
      roll: Double,
      camDist: Double,
      width: Int,
      height: Int
  ): (Double, Double) =
    val (x0, y0, z0) = p

    // Angles in radians
    val ya = Math.toRadians(yaw)
    val pa = Math.toRadians(pitch)
    val ra = Math.toRadians(roll)

    // Rot Z (yaw)
    val x1 = x0 * Math.cos(ya) - y0 * Math.sin(ya)
    val y1 = x0 * Math.sin(ya) + y0 * Math.cos(ya)
    val z1 = z0

    // Rot X (pitch)
    val x2 = x1
    val y2 = y1 * Math.cos(pa) - z1 * Math.sin(pa)
    val z2 = y1 * Math.sin(pa) + z1 * Math.cos(pa)

    // Rot Y (roll)
    val x3 = x2 * Math.cos(ra) + z2 * Math.sin(ra)
    val y3 = y2
    val z3 = -x2 * Math.sin(ra) + z2 * Math.cos(ra)

    // Perspective: camera at (0,0,camDist), looking toward origin
    val denom = (camDist - z3) max 1e-6
    val px    = (x3 * camDist) / denom
    val py    = (y3 * camDist) / denom

    // Shift to image center
    (px + width / 2.0, py + height / 2.0)

  // Map vertex coords (x,y) to normalized (u,v) in [0,1]. If your coords are already 0..1, keep uScale/vScale=1
  private def toUV(p: BigPoint, uScale: Double, vScale: Double): (Double, Double) =
    ((p.x.toDouble * uScale) % 1.0 + 1.0) % 1.0 -> (((p.y.toDouble * vScale) % 1.0 + 1.0) % 1.0) match
      case (u, v) => (u, v)

  // Compute 2D projected point from BigPoint coords on torus
  private def projectOnTorus(
      p: BigPoint,
      opt: TorusSvg3DOptions
  ): (Double, Double) =
    val (u, v) = toUV(p, opt.uScale, opt.vScale)
    val pos3d  = torusParam(u, v, opt.majorRadius, opt.minorRadius)
    rotateAndProject(pos3d, opt.yawDeg, opt.pitchDeg, opt.rollDeg, opt.camDist, opt.imgWidth, opt.imgHeight)
