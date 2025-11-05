package io.github.scala_tessella.dcel.torus

import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.structure.*
import io.github.scala_tessella.dcel.torus.TilingTorusValidation.validate
import io.github.scala_tessella.dcel.{NotFoundError, TilingError}

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
      val n = samples max 8
      (0 to n).map { i =>
        val t = i.toDouble / n
        val u = u0 + du * t
        val v = v0 + dv * t
        val P = TilingTorusDCEL.torusParam(u, v, opt.majorRadius, opt.minorRadius)
        TilingTorusDCEL.rotateAndProject(P, opt.yawDeg, opt.pitchDeg, opt.rollDeg, opt.camDist, opt.imgWidth, opt.imgHeight)
      }

    // Convenience: shortest wrap deltas between two (u,v) points
    def shortestWrapDelta(p0: BigPoint, p1: BigPoint): (Double, Double) =
      val (u0, v0) = TilingTorusDCEL.toUV(p0, opt.uScale, opt.vScale)
      val (u1, v1) = TilingTorusDCEL.toUV(p1, opt.uScale, opt.vScale)
      (wrapDelta(u1 - u0), wrapDelta(v1 - v0))

    // Edge set: draw each undirected edge once; use origin id ordering to deduplicate
    val undirectedEdges = halfEdges
      .flatMap(_.endpointsAsVertices)
      .map { case (a, b) => if a.id.value <= b.id.value then (a, b) else (b, a) }
      .distinct

    // Straight chords (existing look)
    val chordLines = undirectedEdges.map { case (va, vb) =>
      val (x1, y1) = vProj(va)
      val (x2, y2) = vProj(vb)
      s"""<line x1="$x1" y1="$y1" x2="$x2" y2="$y2" stroke="${opt.stroke}" stroke-width="${opt.strokeWidth}"/>"""
    }.mkString("\n        ")

    // Surface-following curves (sampled paths on the torus)
    val curveStroke = "#1e90ff"
    val curveStrokeW = Math.max(1.0, opt.strokeWidth - 0.3)
    val samplesPerCurve = 64

    // For pairs that appear twice (two distinct undirected edges between same vertices),
    // draw both complementary wraps: the shortest and its exact complement so the two cover the full ellipse.
    val edgeMultiplicity: Map[(Vertex, Vertex), Int] =
      halfEdges
        .flatMap(_.endpointsAsVertices)
        .map { case (a, b) => if a.id.value <= b.id.value then (a, b) else (b, a) }
        .groupBy(identity)
        .view.mapValues(_.size).toMap

    // Helper: exact complementary delta to travel the other half of the torus ellipse
    inline def complementDelta(d: Double): Double =
      if d > 0 then d - 1.0
      else if d < 0 then d + 1.0
      else 0.0

    // Helper: draw a full ring (circle) at fixed u or fixed v passing through (u0,v0)
    def ringAt(u0: Double, v0: Double, constantIsU: Boolean, samples: Int): String =
      val n = samples max 32
      val pts =
        (0 until n).map { i =>
          val t = i.toDouble / n
          val u = if constantIsU then u0 else t
          val v = if constantIsU then t else v0
          val P = TilingTorusDCEL.torusParam(u, v, opt.majorRadius, opt.minorRadius)
          TilingTorusDCEL.rotateAndProject(P, opt.yawDeg, opt.pitchDeg, opt.rollDeg, opt.camDist, opt.imgWidth, opt.imgHeight)
        } :+ {
          val u = if constantIsU then u0 else 0.0
          val v = if constantIsU then 0.0 else v0
          val P = TilingTorusDCEL.torusParam(u, v, opt.majorRadius, opt.minorRadius)
          TilingTorusDCEL.rotateAndProject(P, opt.yawDeg, opt.pitchDeg, opt.rollDeg, opt.camDist, opt.imgWidth, opt.imgHeight)
        }
      val d = pts.map { case (x, y) => s"$x,$y" }.mkString(" ")
      s"""<polyline points="$d" fill="none" stroke="$curveStroke" stroke-width="$curveStrokeW"/>"""

    val surfacePaths = undirectedEdges.flatMap { case (va, vb) =>
      val mult = edgeMultiplicity.getOrElse((va, vb), 1)

      if va eq vb then
        // Self-loop(s): draw great circles on torus parameter axes through the vertex (u0,v0).
        val (u0, v0) = TilingTorusDCEL.toUV(va.coords, opt.uScale, opt.vScale)
        val ringU = ringAt(u0, v0, constantIsU = true, samplesPerCurve)
        if mult >= 2 then
          val ringV = ringAt(u0, v0, constantIsU = false, samplesPerCurve)
          Seq(ringU, ringV)
        else
          Seq(ringU)
      else
        // Normal pair of distinct vertices
        val (duS, dvS) = shortestWrapDelta(va.coords, vb.coords)
        val pts1 = sampleEdgeCurveWithDelta(va.coords, vb.coords, duS, dvS, samplesPerCurve)
        val d1 = pts1.map { case (x, y) => s"$x,$y" }.mkString(" ")
        val first = s"""<polyline points="$d1" fill="none" stroke="$curveStroke" stroke-width="$curveStrokeW"/>"""
        if mult >= 2 then
          val duC = complementDelta(duS)
          val dvC = complementDelta(dvS)
          val pts2 = sampleEdgeCurveWithDelta(va.coords, vb.coords, duC, dvC, samplesPerCurve)
          val d2 = pts2.map { case (x, y) => s"$x,$y" }.mkString(" ")
          Seq(first, s"""<polyline points="$d2" fill="none" stroke="$curveStroke" stroke-width="$curveStrokeW"/>""")
        else
          Seq(first)
    }.mkString("\n        ")

    // Face outlines as polygons (optional)
    val facePolys = faces.flatMap { f =>
      f.getVertices.toOption.map { ring =>
        val pts = ring.map(v => vProj(v))
        val d = pts.map { case (x, y) => s"$x,$y" }.mkString(" ")
        s"""<polygon points="$d" fill="${opt.faceFill}" stroke="${opt.faceStroke}" stroke-width="${opt.faceStrokeWidth}"/>"""
      }
    }.mkString("\n        ")

    // --- Torus wireframe ---
    def ringPath(points: Seq[(Double, Double)], stroke: String, width: Double, dash: String = "2,3"): String =
      if points.isEmpty then ""
      else
        val d = points.map { case (x, y) => s"$x,$y" }.mkString(" ")
        s"""<polyline points="$d" fill="none" stroke="$stroke" stroke-width="$width" stroke-dasharray="$dash"/>"""

    // Sample N points along a constant-(u or v) ring
    def sampleRing(constUV: Double, constantIsU: Boolean, samples: Int): Seq[(Double, Double)] =
      val n = samples max 8
      (0 until n).map { i =>
        val t = i.toDouble / n
        val u = if constantIsU then constUV else t
        val v = if constantIsU then t else constUV
        val pos3d = TilingTorusDCEL.torusParam(u, v, opt.majorRadius, opt.minorRadius)
        TilingTorusDCEL.rotateAndProject(pos3d, opt.yawDeg, opt.pitchDeg, opt.rollDeg, opt.camDist, opt.imgWidth, opt.imgHeight)
      } :+ // close the loop
        {
          val u = if constantIsU then constUV else 0.0
          val v = if constantIsU then 0.0 else constUV
          val pos3d = TilingTorusDCEL.torusParam(u, v, opt.majorRadius, opt.minorRadius)
          TilingTorusDCEL.rotateAndProject(pos3d, opt.yawDeg, opt.pitchDeg, opt.rollDeg, opt.camDist, opt.imgWidth, opt.imgHeight)
        }

    // Choose a few rings
    val samplesPerRing = 128
    val wireStroke = "red"
    val wireWidth = 0.8

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
          val dx = 6.0
          val dy = -6.0
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

  // Build a 1x1 square tiling on a torus: 1 face, 1 vertex, 4 half-edges (two parallel undirected loops)
  def build1x1Square(): TilingTorusDCEL =
    // Single vertex
    val v1       = Vertex(VertexId("V1"), BigPoint(0, 0))
    val vertices = List(v1)

    // Single face
    val f1 = Face(FaceId("F1"))

    // Four half-edges, all originating at V1
    // Interpret them as the boundary CCW cycle of the single square face:
    // e0: V1->V1 (east), e1: V1->V1 (north), e2: V1->V1 (west), e3: V1->V1 (south)
    val e0 = HalfEdge(v1)
    val e1 = HalfEdge(v1)
    val e2 = HalfEdge(v1)
    val e3 = HalfEdge(v1)

    // Link the face cycle: e0 -> e1 -> e2 -> e3 -> e0
    e0.linkWith(e1)
    e1.linkWith(e2)
    e2.linkWith(e3)
    e3.linkWith(e0)

    // Set incident face and outer component
    e0.incidentFace = Some(f1)
    e1.incidentFace = Some(f1)
    e2.incidentFace = Some(f1)
    e3.incidentFace = Some(f1)
    f1.outerComponent = Some(e0)

    // Set interior angles (square corners)
    val rightAngle = AngleDegree(90)
    e0.angle = Some(rightAngle)
    e1.angle = Some(rightAngle)
    e2.angle = Some(rightAngle)
    e3.angle = Some(rightAngle)

    // Twins: two undirected parallel edges at V1
    // Pair (e0 <-> e2) and (e1 <-> e3), giving two distinct V1↔V1 edge classes
    e0.twinWith(e2)
    e1.twinWith(e3)

    // Leaving edge for the vertex
    v1.leaving = Some(e0)

    val faces     = List(f1)
    val halfEdges = List(e0, e1, e2, e3)

    // Construct without extra validation logic here
    TilingTorusDCEL(vertices, halfEdges, faces)

  // Build a 2x2 square tiling on a torus: 4 faces, 4 vertices, 16 half-edges
  def build2x2Squares(): TilingTorusDCEL =
    // Vertices
    val v1       = Vertex(VertexId("V1"), BigPoint(0, 0))
    val v2       = Vertex(VertexId("V2"), BigPoint(1, 0))
    val v3       = Vertex(VertexId("V3"), BigPoint(0, 1))
    val v4       = Vertex(VertexId("V4"), BigPoint(1, 1))
    val vertices = List(v1, v2, v3, v4)

    // Faces
    val f1 = Face(FaceId("F1"))
    val f2 = Face(FaceId("F2"))
    val f3 = Face(FaceId("F3"))
    val f4 = Face(FaceId("F4"))

    // Create all half-edges (only origin is required in constructor)
    val he = Array(
      HalfEdge(v1),
      HalfEdge(v2),
      HalfEdge(v4),
      HalfEdge(v3), // F1 cycle (e1..e4)
      HalfEdge(v2),
      HalfEdge(v1),
      HalfEdge(v3),
      HalfEdge(v4), // F2 cycle (e5..e8)
      HalfEdge(v3),
      HalfEdge(v4),
      HalfEdge(v2),
      HalfEdge(v1), // F3 cycle (e9..e12)
      HalfEdge(v4),
      HalfEdge(v3),
      HalfEdge(v1),
      HalfEdge(v2) // F4 cycle (e13..e16)
    )

    // Helper to set next/prev within a cycle
    def linkCycle(i0: Int, i1: Int, i2: Int, i3: Int): Unit =
      he(i0).linkWith(he(i1))
      he(i1).linkWith(he(i2))
      he(i2).linkWith(he(i3))
      he(i3).linkWith(he(i0))

    // Link per-face cycles (counter-clockwise)
    linkCycle(0, 1, 2, 3)     // F1: v1->v2->v4->v3
    linkCycle(4, 5, 6, 7)     // F2: v2->v1->v3->v4
    linkCycle(8, 9, 10, 11)   // F3: v3->v4->v2->v1
    linkCycle(12, 13, 14, 15) // F4: v4->v3->v1->v2

    // Assign incident faces and set face outerComponent
    def setFace(face: Face, indices: (Int, Int, Int, Int)): Unit =
      val (i0, i1, i2, i3) = indices
      he(i0).incidentFace = Some(face)
      he(i1).incidentFace = Some(face)
      he(i2).incidentFace = Some(face)
      he(i3).incidentFace = Some(face)
      face.outerComponent = Some(he(i0))

    setFace(f1, (0, 1, 2, 3))
    setFace(f2, (4, 5, 6, 7))
    setFace(f3, (8, 9, 10, 11))
    setFace(f4, (12, 13, 14, 15))

    // Set corner angles (all squares: 90 degrees)
    val rightAngle = AngleDegree(90)
    he.foreach(_.angle = Some(rightAngle))

    // Twins: pair opposite-directed copies across diagonal faces (F1↔F3, F2↔F4)
    // v1<->v2
    he(0).twinWith(he(10)) // F1 v1->v2  ↔ F3 v2->v1
    he(14).twinWith(he(4)) // F4 v1->v2  ↔ F2 v2->v1

    // v2<->v4
    he(1).twinWith(he(9))  // F1 v2->v4  ↔ F3 v4->v2
    he(15).twinWith(he(7)) // F4 v4->v2  ↔ F2 v2->v4

    // v4<->v3
    he(2).twinWith(he(8))  // F1 v4->v3  ↔ F3 v3->v4
    he(12).twinWith(he(6)) // F4 v4->v3  ↔ F2 v3->v4

    // v3<->v1
    he(3).twinWith(he(11)) // F1 v3->v1  ↔ F3 v1->v3
    he(13).twinWith(he(5)) // F4 v3->v1  ↔ F2 v1->v3

    // Leaving edges: one per vertex, originating at that vertex
    v1.leaving = Some(he(0)) // v1->v2
    v2.leaving = Some(he(1)) // v2->v4
    v3.leaving = Some(he(3)) // v3->v1
    v4.leaving = Some(he(2)) // v4->v3

    val faces     = List(f1, f2, f3, f4)
    val halfEdges = he.toList

    // Construct without extra validation logic here
    TilingTorusDCEL(vertices, halfEdges, faces)

  def buildSquareNet(width: Int, height: Int): TilingTorusDCEL =
    // width = number of squares along U direction
    // height = number of squares along V direction
    if width <= 0 || height <= 0 then
      return TilingTorusDCEL.empty

    // Vertices on torus are identified modulo the grid, so only width*height distinct vertices
    // Place them on [0,1]x[0,1] lattice
    val verts =
      Array.tabulate(height, width) { (j, i) =>
        val id = VertexId(s"V${j * width + i + 1}")
        val pos = BigPoint(BigDecimal(i) / width, BigDecimal(j) / height)
        Vertex(id, pos)
      }

    val vertices = verts.flatten.toList

    // Faces: one per cell (width*height)
    val faces: Array[Array[Face]] =
      Array.tabulate(height, width) { (j, i) =>
        Face(FaceId(s"F${j * width + i + 1}"))
      }

    // Helpers to wrap indices on torus
    inline def wrapX(i: Int): Int = (i % width + width) % width

    inline def wrapY(j: Int): Int = (j % height + height) % height

    // For each face create 4 half-edges CCW:
    // e0: (i,j)   -> (i+1,j)
    // e1: (i+1,j) -> (i+1,j+1)
    // e2: (i+1,j+1)->(i,j+1)
    // e3: (i,j+1) -> (i,j)
    // We store the directed edges in arrays to wire twins afterwards.
    val e0 = Array.ofDim[HalfEdge](height, width)
    val e1 = Array.ofDim[HalfEdge](height, width)
    val e2 = Array.ofDim[HalfEdge](height, width)
    val e3 = Array.ofDim[HalfEdge](height, width)

    // Create and assign incident faces + angles
    val rightAngle = AngleDegree(90)
    for j <- 0 until height; i <- 0 until width do
      val v00 = verts(j)(i)
      val v10 = verts(j)(wrapX(i + 1))
      val v11 = verts(wrapY(j + 1))(wrapX(i + 1))
      val v01 = verts(wrapY(j + 1))(i)

      e0(j)(i) = HalfEdge(v00)
      e1(j)(i) = HalfEdge(v10)
      e2(j)(i) = HalfEdge(v11)
      e3(j)(i) = HalfEdge(v01)

      // Link CCW
      e0(j)(i).linkWith(e1(j)(i))
      e1(j)(i).linkWith(e2(j)(i))
      e2(j)(i).linkWith(e3(j)(i))
      e3(j)(i).linkWith(e0(j)(i))

      // Incident face and angle
      val f = faces(j)(i)
      e0(j)(i).incidentFace = Some(f)
      e1(j)(i).incidentFace = Some(f)
      e2(j)(i).incidentFace = Some(f)
      e3(j)(i).incidentFace = Some(f)
      faces(j)(i).outerComponent = Some(e0(j)(i))

      e0(j)(i).angle = Some(rightAngle)
      e1(j)(i).angle = Some(rightAngle)
      e2(j)(i).angle = Some(rightAngle)
      e3(j)(i).angle = Some(rightAngle)

    // Twin wiring
    // Horizontal undirected edges are shared between (i,j) right-edge e0 and (i+1,j) left-edge e2 of the left neighbor.
    // Vertical undirected edges are shared between (i,j) top-edge e1 and (i,j+1) bottom-edge e3 of the bottom neighbor.
    for j <- 0 until height; i <- 0 until width do
      // Horizontal pair: current e0 (i,j) with left-neighbor e2 (i-1,j)
      val iL = wrapX(i - 1)
      e0(j)(i).twinWith(e2(j)(iL))

      // Vertical pair: current e1 (i,j) with bottom-neighbor e3 (i,j-1)
      val jB = wrapY(j - 1)
      e1(j)(i).twinWith(e3(jB)(i))

    // Set leaving edge on each vertex: pick one incident (prefer e0 starting at that vertex if exists)
    // Each vertex (i,j) is origin of edges e0(i,j) and e3(i-1,j) after wrapping;
    // choose e0(i,j) as leaving for convenience.
    for j <- 0 until height; i <- 0 until width do
      verts(j)(i).leaving = Some(e0(j)(i))

    val halfEdges =
      (for j <- 0 until height; i <- 0 until width yield
        List(e0(j)(i), e1(j)(i), e2(j)(i), e3(j)(i))
        ).flatten.toList

    TilingTorusDCEL(vertices, halfEdges, faces.flatten.toList)

  def buildTriangleNet(width: Int, height: Int): TilingTorusDCEL =
    // width, height define the rhombus grid; each rhombus is split into two equilateral triangles.
    if width <= 0 || height <= 0 || height % 2 == 1then
      return TilingTorusDCEL.empty

    // Geometry for equilateral grid in [0,1]x[0,1] (torus wraps):
    // Horizontal step dx and vertical step dy so that each small cell is a unit rhombus
    // with 60° angles when seen as two glued equilateral triangles.
    val dx = BigDecimal(1.0) / width
    val dy = (BigDecimal(Math.sqrt(3)) / 2.0) / height

    // Lattice vertices arranged in a "staggered" hex/triangular grid:
    // row j has a horizontal offset of (j parity)*dx/2
    val verts =
      Array.tabulate(height, width) { (j, i) =>
        val offset = if (j & 1) == 1 then dx / 2 else BigDecimal(0)
        val x = (BigDecimal(i) * dx + offset) % 1
        val y = (BigDecimal(j) * dy) % 1
        val id = VertexId(s"V${j * width + i + 1}")
        Vertex(id, BigPoint(x, y))
      }

    val vertices = verts.flatten.toList

    // Each rhombus (cell) will be split into two equilateral triangles.
    // For a cell (i,j), define its 4 corners:
    // A = (i,   j)
    // B = (i+1, j)
    // C = (i,   j+1)
    // D = (i+1, j+1)
    // Even rows use diagonal A->D, odd rows use diagonal B->C.
    inline def wrapX(i: Int): Int = (i % width + width) % width

    inline def wrapY(j: Int): Int = (j % height + height) % height

    // Faces: 2 per cell
    val faces =
      Array.tabulate(height, width, 2) { (j, i, k) =>
        val idx = j * width * 2 + i * 2 + k + 1
        Face(FaceId(s"F$idx"))
      }

    // Angles at equilateral triangle corners
    val sixty = AngleDegree(60)

    // For each cell we create 6 half-edges (two triangles, 3 edges each), link cycles CCW, set faces and angles.
    // We only set next/prev and incidentFace now; twins will be wired after across adjacency.
    case class TriHE(a: HalfEdge, b: HalfEdge, c: HalfEdge)
    val triHEs = Array.ofDim[TriHE](height, width, 2)

    def makeFace(v0: Vertex, v1: Vertex, v2: Vertex, face: Face): TriHE =
      val e0 = HalfEdge(v0)
      val e1 = HalfEdge(v1)
      val e2 = HalfEdge(v2)
      e0.linkWith(e1)
      e1.linkWith(e2)
      e2.linkWith(e0)
      e0.incidentFace = Some(face)
      e1.incidentFace = Some(face)
      e2.incidentFace = Some(face)
      face.outerComponent = Some(e0)
      e0.angle = Some(sixty)
      e1.angle = Some(sixty)
      e2.angle = Some(sixty)
      TriHE(e0, e1, e2)

    for j <- 0 until height; i <- 0 until width do
      val A = verts(j)(i)
      val B = verts(j)(wrapX(i + 1))
      val C = verts(wrapY(j + 1))(i)
      val D = verts(wrapY(j + 1))(wrapX(i + 1))

      val evenRow = (j & 1) == 0

      // Two triangles per cell, CCW orientation
      if evenRow then
        // Diagonal A->D: triangles (A,B,D) and (A,D,C)
        triHEs(j)(i)(0) = makeFace(A, B, D, faces(j)(i)(0)) // ABD
        triHEs(j)(i)(1) = makeFace(A, D, C, faces(j)(i)(1)) // ADC
      else
        // Diagonal B->C: triangles (A,B,C) and (B,D,C)
        triHEs(j)(i)(0) = makeFace(A, B, C, faces(j)(i)(0)) // ABC
        triHEs(j)(i)(1) = makeFace(B, D, C, faces(j)(i)(1)) // BDC

    // Collect all half-edges for twin wiring
    val allHE: List[HalfEdge] =
      (for j <- 0 until height; i <- 0 until width; k <- 0 until 2 yield
        val t = triHEs(j)(i)(k)
        List(t.a, t.b, t.c)
        ).flatten.toList

    // Build map from directed edge (originId,destId) -> list of half-edges
    // Endpoints inferred from 'next' relation: destination of e is e.next.origin
    def destOf(e: HalfEdge): Vertex = e.next.get.origin

    val buckets =
      allHE.groupBy(e => (e.origin.id.value, destOf(e).id.value))

    // Twins: for each directed pair, the twin is the opposite directed edge if present
    def keyOpp(k: (String, String)) = (k._2, k._1)

    for
      (k, list) <- buckets
      opp <- buckets.get(keyOpp(k))
    do
      // pair elements one-by-one (handles wrapping: there should be same multiplicity)
      val n = Math.min(list.size, opp.size)
      var idx = 0
      while idx < n do
        val e1 = list(idx)
        val e2 = opp(idx)
        if e1.twin.isEmpty && e2.twin.isEmpty then e1.twinWith(e2)
        idx += 1

    // Leaving edge per vertex: pick any incident we created; prefer the first seen
    val byVertex = allHE.groupBy(_.origin)
    byVertex.foreach { case (v, es) => if v.leaving.isEmpty then v.leaving = Some(es.head) }

    // Return structure
    TilingTorusDCEL(vertices, allHE, faces.iterator.flatMap(_.iterator.flatMap(_.iterator)).toList)
    
  // Build a 4x1 triangle tiling on a torus:
  // - 2 vertices (V1,V2)
  // - 4 faces (F1..F4)
  // - 12 half-edges
  // Interpretation: on the plane, 4 equilateral triangles around a single apex,
  // wrapped so opposite sides/vertices identify to two vertices on the torus.
  def build4x1Triangles(): TilingTorusDCEL =
    // Vertices
    val v1       = Vertex(VertexId("V1"), BigPoint(0, 0))
    val v2       = Vertex(VertexId("V2"), BigPoint(1, 0))
    val vertices = List(v1, v2)

    // Faces (four triangles around the torus)
    val f1 = Face(FaceId("F1"))
    val f2 = Face(FaceId("F2"))
    val f3 = Face(FaceId("F3"))
    val f4 = Face(FaceId("F4"))

    // For each face, create a 3-cycle with origins alternating between v1 and v2.
    // Each oriented edge is v1->v2 or v2->v1 (since only two vertices exist after identification).
    val e = Array(
      // F1 cycle
      HalfEdge(v1),
      HalfEdge(v2),
      HalfEdge(v1),
      // F2 cycle
      HalfEdge(v2),
      HalfEdge(v1),
      HalfEdge(v2),
      // F3 cycle
      HalfEdge(v1),
      HalfEdge(v2),
      HalfEdge(v1),
      // F4 cycle
      HalfEdge(v2),
      HalfEdge(v1),
      HalfEdge(v2)
    )

    // Helper to link a triangle cycle i, i+1, i+2
    def linkTri(i: Int): Unit =
      e(i + 0).linkWith(e(i + 1))
      e(i + 1).linkWith(e(i + 2))
      e(i + 2).linkWith(e(i + 0))

    // Link face cycles (each face has 3 half-edges)
    linkTri(0) // F1: e0,e1,e2
    linkTri(3) // F2: e3,e4,e5
    linkTri(6) // F3: e6,e7,e8
    linkTri(9) // F4: e9,e10,e11

    // Assign incident faces and set outerComponent
    List(0, 1, 2).foreach(i => e(i).incidentFace = Some(f1))
    List(3, 4, 5).foreach(i => e(i).incidentFace = Some(f2))
    List(6, 7, 8).foreach(i => e(i).incidentFace = Some(f3))
    List(9, 10, 11).foreach(i => e(i).incidentFace = Some(f4))
    f1.outerComponent = Some(e(0))
    f2.outerComponent = Some(e(3))
    f3.outerComponent = Some(e(6))
    f4.outerComponent = Some(e(9))

    // Set interior angles (equilateral triangle corners: 60°)
    val sixty = AngleDegree(60)
    e.foreach(_.angle = Some(sixty))

    // Twins: pair opposite-directed copies so each undirected class appears exactly twice.
    // We pair F1 with F3 and F2 with F4, index-wise, to keep symmetry and ensure closed vertex orbits.
    // F1 (0,1,2) ↔ F3 (6,7,8)
    e(0).twinWith(e(7))  // v1->v2 ↔ v2->v1
    e(1).twinWith(e(6))  // v2->v1 ↔ v1->v2
    e(2).twinWith(e(8))  // v1->v2 ↔ v1->v2 (parallel class across faces; directions differ via next/prev)
    // F2 (3,4,5) ↔ F4 (9,10,11)
    e(3).twinWith(e(10)) // v2->v1 ↔ v1->v2
    e(4).twinWith(e(9))  // v1->v2 ↔ v2->v1
    e(5).twinWith(e(11)) // v2->v1 ↔ v2->v1 (parallel class across faces)

    // Leaving edges (must originate at the vertex)
    v1.leaving = Some(e(0)) // origin v1
    v2.leaving = Some(e(1)) // origin v2

    val faces     = List(f1, f2, f3, f4)
    val halfEdges = e.toList

    TilingTorusDCEL(vertices, halfEdges, faces)

  // Build a 2x1 hexagon tiling on a torus:
  // - 4 vertices (V1..V4)
  // - 2 faces (F1, F2)
  // - 12 half-edges (each hexagon boundary has 6 half-edges)
  // The two hexagons are identified on the torus so that opposite sides match,
  // yielding exactly two faces sharing parallel edge classes.
  def build2x1Hexagons(): TilingTorusDCEL =
    // Vertices (wrapped identifications yield only 4 distinct vertices)
    val v1       = Vertex(VertexId("V1"), BigPoint(0, 0))
    val v2       = Vertex(VertexId("V2"), BigPoint(1, 0))
    val v3       = Vertex(VertexId("V3"), BigPoint(0, 1))
    val v4       = Vertex(VertexId("V4"), BigPoint(1, 1))
    val vertices = List(v1, v2, v3, v4)

    // Faces (two hexagons)
    val f1 = Face(FaceId("F1"))
    val f2 = Face(FaceId("F2"))

    // Define two hexagon boundary cycles using only V1..V4 with wrap-around identifications.
    // We alternate origins v1/v2/v3/v4 along the perimeter so each face has 6 directed half-edges.
    // Face F1 cycle (6 half-edges)
    val h0 = HalfEdge(v1) // conceptual V1->V2
    val h1 = HalfEdge(v2) // conceptual V2->V3
    val h2 = HalfEdge(v3) // conceptual V3->V4
    val h3 = HalfEdge(v4) // conceptual V4->V1
    val h4 = HalfEdge(v1) // conceptual V1->V3 (wrapped)
    val h5 = HalfEdge(v3) // conceptual V3->V1 (wrapped)

    // Face F2 cycle (6 half-edges), complementary directions to F1
    val k0 = HalfEdge(v2) // conceptual V2->V1
    val k1 = HalfEdge(v1) // conceptual V1->V4
    val k2 = HalfEdge(v4) // conceptual V4->V3
    val k3 = HalfEdge(v3) // conceptual V3->V2
    val k4 = HalfEdge(v2) // conceptual V2->V4 (wrapped)
    val k5 = HalfEdge(v4) // conceptual V4->V2 (wrapped)

    // Link both hexagon cycles (next/prev around each face)
    def link6(e0: HalfEdge, e1: HalfEdge, e2: HalfEdge, e3: HalfEdge, e4: HalfEdge, e5: HalfEdge): Unit =
      e0.linkWith(e1)
      e1.linkWith(e2)
      e2.linkWith(e3)
      e3.linkWith(e4)
      e4.linkWith(e5)
      e5.linkWith(e0)

    link6(h0, h1, h2, h3, h4, h5) // F1 boundary
    link6(k0, k1, k2, k3, k4, k5) // F2 boundary

    // Set incident faces and outer components
    val f1Edges = List(h0, h1, h2, h3, h4, h5)
    val f2Edges = List(k0, k1, k2, k3, k4, k5)
    f1Edges.foreach(_.incidentFace = Some(f1))
    f2Edges.foreach(_.incidentFace = Some(f2))
    f1.outerComponent = Some(h0)
    f2.outerComponent = Some(k0)

    // Angles for a regular hexagon corner are 120°
    val oneTwenty = AngleDegree(120)
    (f1Edges ++ f2Edges).foreach(_.angle = Some(oneTwenty))

    // Twin wiring: pair each F1 edge with an opposite-directed F2 edge to form the torus identifications.
    // We choose a consistent, alternating pairing that yields valid vertex-orbits.
    // Pairings (F1 hi ↔ F2 kj):
    h0.twinWith(k0) // v1->? with v2->? across identified edge class
    h1.twinWith(k3)
    h2.twinWith(k2)
    h3.twinWith(k1)
    h4.twinWith(k4)
    h5.twinWith(k5)

    // Leaving edges (originate at each vertex)
    v1.leaving = Some(h0) // origin v1
    v2.leaving = Some(h1) // origin v2
    v3.leaving = Some(h2) // origin v3
    v4.leaving = Some(h3) // origin v4

    val faces     = List(f1, f2)
    val halfEdges = f1Edges ++ f2Edges

    TilingTorusDCEL(vertices, halfEdges, faces)

  