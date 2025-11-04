package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.structure.*
import io.github.scala_tessella.dcel.TilingTorusValidation.validate

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

  // Build a 4x1 triangle tiling on a torus:
  // - 2 vertices (V1,V2)
  // - 4 faces (F1..F4)
  // - 12 half-edges
  // Interpretation: on the plane, 4 equilateral triangles around a single apex,
  // wrapped so opposite sides/vertices identify to two vertices on the torus.
  def build4x1Triangles(): TilingTorusDCEL =
    // Vertices
    val v1 = Vertex(VertexId("V1"), BigPoint(0, 0))
    val v2 = Vertex(VertexId("V2"), BigPoint(1, 0))
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
      HalfEdge(v1), HalfEdge(v2), HalfEdge(v1),
      // F2 cycle
      HalfEdge(v2), HalfEdge(v1), HalfEdge(v2),
      // F3 cycle
      HalfEdge(v1), HalfEdge(v2), HalfEdge(v1),
      // F4 cycle
      HalfEdge(v2), HalfEdge(v1), HalfEdge(v2)
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
    e(0).twinWith(e(7)) // v1->v2 ↔ v2->v1
    e(1).twinWith(e(6)) // v2->v1 ↔ v1->v2
    e(2).twinWith(e(8)) // v1->v2 ↔ v1->v2 (parallel class across faces; directions differ via next/prev)
    // F2 (3,4,5) ↔ F4 (9,10,11)
    e(3).twinWith(e(10)) // v2->v1 ↔ v1->v2
    e(4).twinWith(e(9)) // v1->v2 ↔ v2->v1
    e(5).twinWith(e(11)) // v2->v1 ↔ v2->v1 (parallel class across faces)

    // Leaving edges (must originate at the vertex)
    v1.leaving = Some(e(0)) // origin v1
    v2.leaving = Some(e(1)) // origin v2

    val faces = List(f1, f2, f3, f4)
    val halfEdges = e.toList

    TilingTorusDCEL(vertices, halfEdges, faces)
