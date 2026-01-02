package io.github.scala_tessella.dcel.torus

import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}

import spire.implicits.*

object TilingTorusBuilder:

  // Small shared helpers
  private inline def wrap(idx: Int, size: Int): Int = (idx % size + size) % size

  private def linkCycleSetFace(edges: Seq[HalfEdge], face: Face, angle: AngleDegree): Unit =
    val n = edges.length
    var k = 0
    while k < n do
      edges(k).linkWith(edges((k + 1) % n))
      edges(k).incidentFace = Some(face)
      edges(k).angle = Some(angle)
      k += 1
    face.outerComponent = Some(edges.head)

  private def setLeavingEdges(vertices: Iterable[Vertex], halfEdges: Iterable[HalfEdge]): Unit =
    val byV = halfEdges.groupBy(_.origin)
    byV.foreach { case (v, es) =>
      if v.leaving.isEmpty then v.leaving = Some(es.head)
    }

  /** Build a toroidal DCEL of an axis-aligned square grid of size width × height.
    *
    * Topology:
    *   - Faces: width*height quads, each with 4 half-edges oriented CCW.
    *   - Edges: opposite sides of the rectangle are glued (torus). Twins are wired accordingly.
    *   - Vertices: placed on an integer lattice; indices wrap modulo width/height.
    *
    * @param width
    *   number of squares along the U (x) direction; must be > 0
    * @param height
    *   number of squares along the V (y) direction; must be > 0
    *
    * @return
    *   - A TilingTorusDCEL with right angles (90°) on each corner.
    *   - TilingTorusDCEL.empty if the input is invalid.
    */
  def createSquareNet(width: Int, height: Int): TilingTorusDCEL =
    // width = number of squares along U direction
    // height = number of squares along V direction
    if width <= 0 || height <= 0 then
      return TilingTorusDCEL.empty

    // Vertices on torus are identified modulo the grid, so only width*height distinct vertices
    // Place them on [0,1]x[0,1] lattice
    val verts =
      Array.tabulate(height, width) { (j, i) =>
        val id  = VertexId(j * width + i + 1)
        val pos = BigPoint(BigDecimal(i), BigDecimal(j))
        Vertex(id, pos)
      }

    val vertices = verts.flatten.toList

    // Faces: one per cell (width*height)
    val faces: Array[Array[Face]] =
      Array.tabulate(height, width): (j, i) =>
        Face(FaceId(j * width + i + 1))

    // Helpers to wrap indices on torus
    inline def wrapX(i: Int): Int = wrap(i, width)
    inline def wrapY(j: Int): Int = wrap(j, height)

    // For each face create 4 half-edges CCW
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

      // Link cycle + set face/angles
      val f = faces(j)(i)
      linkCycleSetFace(Seq(e0(j)(i), e1(j)(i), e2(j)(i), e3(j)(i)), f, rightAngle)

    // Twin wiring
    for j <- 0 until height; i <- 0 until width do
      val jB = wrapY(j - 1)
      e0(j)(i).twinWith(e2(jB)(i))
      val iR = wrapX(i + 1)
      e1(j)(i).twinWith(e3(j)(iR))

    // Leaving edge on each vertex: choose e0(i,j)
    for j <- 0 until height; i <- 0 until width do
      verts(j)(i).leaving = Some(e0(j)(i))

    val halfEdges =
      (for j <- 0 until height; i <- 0 until width
      yield List(e0(j)(i), e1(j)(i), e2(j)(i), e3(j)(i))).flatten.toList

    TilingTorusDCEL(vertices, halfEdges, faces.flatten.toList)

  /** Build a toroidal DCEL of an equilateral triangular tiling derived from a staggered rhombus grid.
    *
    * Topology and geometry:
    *   - Cells: each rectangular cell (in the staggered grid) is split into two CCW triangles.
    *   - Row staggering: odd rows are horizontally offset by dx/2; indices wrap on both axes.
    *   - Twins: computed by pairing opposite directed edges using origin/destination buckets.
    *
    * @param width
    *   number of rhombi per row; must be > 0
    * @param height
    *   number of rows; must be > 0 and even (to close the torus with staggering)
    *
    * @return
    *   - A TilingTorusDCEL with 60° angles on triangle corners.
    *   - TilingTorusDCEL.empty if the input is invalid.
    */
  def createTriangleNet(width: Int, height: Int): TilingTorusDCEL =
    // width, height define the rhombus grid; each rhombus is split into two equilateral triangles.
    if width <= 0 || height <= 0 || height % 2 == 1 then
      return TilingTorusDCEL.empty

    // Geometry for equilateral grid in [0,1]x[0,1] (torus wraps):
    // Horizontal step dx and vertical step dy so that each small cell is a unit rhombus
    // with 60° angles when seen as two glued equilateral triangles.
    val dx = BigDecimal(1.0)
    val dy = BigDecimal(Math.sqrt(3)) / 2.0

    // Lattice vertices arranged in a "staggered" hex/triangular grid:
    // row j has a horizontal offset of (j parity)*dx/2
    val verts =
      Array.tabulate(height, width) { (j, i) =>
        val offset = if (j & 1) == 1 then dx / 2 else BigDecimal(0)
        val x      = BigDecimal(i) * dx + offset
        val y      = BigDecimal(j) * dy
        val id     = VertexId(j * width + i + 1)
        Vertex(id, BigPoint(x, y))
      }

    val vertices = verts.flatten.toList

    inline def wrapX(i: Int): Int = wrap(i, width)
    inline def wrapY(j: Int): Int = wrap(j, height)

    val faces =
      Array.tabulate(height, width, 2) { (j, i, k) =>
        val idx = j * width * 2 + i * 2 + k + 1
        Face(FaceId(idx))
      }

    // Angles at equilateral triangle corners
    val sixty = AngleDegree(60)

    // For each cell we create 6 half-edges (two triangles, 3 edges each), link cycles CCW, set faces and angles.
    // We only set next/prev and incidentFace now; twins will be wired after across adjacency.
    case class TriHE(a: HalfEdge, b: HalfEdge, c: HalfEdge)
    val triHEs = Array.ofDim[TriHE](height, width, 2)

    def makeTriFace(v0: Vertex, v1: Vertex, v2: Vertex, face: Face): TriHE =
      val e0 = HalfEdge(v0)
      val e1 = HalfEdge(v1)
      val e2 = HalfEdge(v2)
      linkCycleSetFace(Seq(e0, e1, e2), face, sixty)
      TriHE(e0, e1, e2)

    for j <- 0 until height; i <- 0 until width do
      val evenRow = (j & 1) == 0

      val A = verts(j)(i)
      val B = verts(j)(wrapX(i + 1))

      val jNext       = wrapY(j + 1)
      val nextRowEven = (jNext & 1) == 0

      val iTop      =
        if evenRow && !nextRowEven then wrapX(i - 1)
        else if !evenRow && nextRowEven then wrapX(i + 1)
        else i
      val iTopRight = wrapX(iTop + 1)

      val C = verts(jNext)(iTop)
      val D = verts(jNext)(iTopRight)

      if evenRow then
        triHEs(j)(i)(0) = makeTriFace(A, B, D, faces(j)(i)(0)) // ABD
        triHEs(j)(i)(1) = makeTriFace(A, D, C, faces(j)(i)(1)) // ADC
      else
        triHEs(j)(i)(0) = makeTriFace(A, B, C, faces(j)(i)(0)) // ABC
        triHEs(j)(i)(1) = makeTriFace(B, D, C, faces(j)(i)(1)) // BDC

    val allHE: List[HalfEdge] =
      (for j <- 0 until height; i <- 0 until width; k <- 0 until 2 yield
        val t = triHEs(j)(i)(k)
        List(t.a, t.b, t.c)
      ).flatten.toList

    // Build map from directed edge (originId,destId) -> list of half-edges
    def destOf(e: HalfEdge): Vertex = e.next.get.origin

    val buckets =
      allHE.groupBy(e => (e.origin.id.toPrefixedString, destOf(e).id.toPrefixedString))

    def keyOpp(k: (String, String)) = (k._2, k._1)

    for
      (k, list) <- buckets
      opp       <- buckets.get(keyOpp(k))
    do
      val n   = Math.min(list.size, opp.size)
      var idx = 0
      while idx < n do
        val e1 = list(idx)
        val e2 = opp(idx)
        if e1.twin.isEmpty && e2.twin.isEmpty then e1.twinWith(e2)
        idx += 1

    // Leaving edge per vertex
    setLeavingEdges(vertices, allHE)

    TilingTorusDCEL(vertices, allHE, faces.iterator.flatMap(_.iterator.flatMap(_.iterator)).toList)

  /** Build a toroidal DCEL of a regular hexagonal tiling (each face is a hexagon).
    *
    * Construction:
    *   - Uses axial-like triple coordinates aligned with global directions 0°, 60°, 120°.
    *   - Vertices are canonicalized into a fundamental domain via lattice periods to enforce torus wrapping.
    *   - Directed half-edges are deduplicated per (origin, destination) and automatically twinned.
    *
    * @param width
    *   number of hexagons along U direction; must be > 0
    * @param height
    *   number of hexagons along V direction; must be > 0 and even (for consistent wrapping)
    *
    * @return
    *   - A TilingTorusDCEL with 120° angles at each hex corner.
    *   - TilingTorusDCEL.empty if the input is invalid.
    */
  def createHexagonNet(width: Int, height: Int): TilingTorusDCEL =
    // width = number of hexagons along U direction
    // height = number of hexagons along V direction
    if width <= 0 || height <= 0 || height % 2 == 1 then
      return TilingTorusDCEL.empty

    import scala.collection.mutable
    val oneTwenty = AngleDegree(120)

    val h0  = BigDecimal(0)
    val h1  = BigDecimal(Math.PI) / 3
    val h2  = BigDecimal(2) * BigDecimal(Math.PI) / 3
    val g0x = spire.math.cos(h0); val g0y = spire.math.sin(h0)
    val g1x = spire.math.cos(h1); val g1y = spire.math.sin(h1)
    val g2x = spire.math.cos(h2); val g2y = spire.math.sin(h2)

    val cornerTriples: Array[(Int, Int, Int)] =
      Array((0, 0, 0), (1, 0, 0), (1, 1, 0), (1, 1, 1), (0, 1, 1), (0, 0, 1))

    inline def baseTriple(i: Int, j: Int): (Int, Int, Int)  = (i, i + j, j)
    inline def addT(a: (Int, Int, Int), b: (Int, Int, Int)) = (a._1 + b._1, a._2 + b._2, a._3 + b._3)

    inline def floorDiv(a: Int, b: Int): Int =
      val q = a / b
      if a % b < 0 then q - 1 else q

    inline def modTripleToFundamental(n0: Int, n1: Int, n2: Int): (Int, Int, Int) =
      val k1 = floorDiv(n0, width)
      val r0 = n0 - k1 * width
      val r1 = n1 - k1 * width
      val r2 = n2
      val k2 = floorDiv(r2, height)
      val s0 = r0
      val s1 = r1 - k2 * height
      val s2 = r2 - k2 * height
      (s0, s1, s2)

    def tripleToPoint(n0: Int, n1: Int, n2: Int): BigPoint =
      val x = BigDecimal(n0) * g0x + BigDecimal(n1) * g1x + BigDecimal(n2) * g2x
      val y = BigDecimal(n0) * g0y + BigDecimal(n1) * g1y + BigDecimal(n2) * g2y
      BigPoint(x, y)

    val vertexByTriple = mutable.Map[(Int, Int, Int), Vertex]()
    var vCount         = 1

    def vOfRaw(key: (Int, Int, Int)): Vertex =
      vertexByTriple.getOrElseUpdate(
        key, {
          val (a, b, c) = key
          val v         = Vertex(VertexId(vCount), tripleToPoint(a, b, c))
          vCount += 1
          v
        }
      )

    def vOf(n0: Int, n1: Int, n2: Int): Vertex =
      vOfRaw(modTripleToFundamental(n0, n1, n2))

    val halfEdgeByDir = mutable.Map[(Vertex, Vertex), HalfEdge]()

    def he(v1: Vertex, v2: Vertex): HalfEdge =
      halfEdgeByDir.getOrElseUpdate(
        (v1, v2), {
          val e1 = HalfEdge(v1)
          val e2 = HalfEdge(v2)
          e1.twinWith(e2)
          halfEdgeByDir((v2, v1)) = e2
          e1
        }
      )

    val faces = Array.tabulate(height, width): (j, i) =>
      Face(FaceId(j * width + i + 1))

    for j <- 0 until height; i <- 0 until width do
      val f    = faces(j)(i)
      val b    = baseTriple(i, j)
      val keys = (0 until 6).map(k => addT(b, cornerTriples(k)))
      val vs   = keys.map { case (a, b, c) =>
        vOf(a, b, c)
      }.toArray
      val ring = (0 until 6).toList.map(k => he(vs(k), vs((k + 1) % 6)))

      // Link cycle + set face/angles
      linkCycleSetFace(ring, f, oneTwenty)

    val allVertices  = vertexByTriple.values.toList
    val allHalfEdges = halfEdgeByDir.values.toList

    setLeavingEdges(allVertices, allHalfEdges)

    TilingTorusDCEL(allVertices, allHalfEdges, faces.flatten.toList)
