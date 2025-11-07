package io.github.scala_tessella.dcel.torus

import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}

import spire.implicits.*

object TilingTorusBuilder:

  def createSquareNet(width: Int, height: Int): TilingTorusDCEL =
    // width = number of squares along U direction
    // height = number of squares along V direction
    if width <= 0 || height <= 0 then
      return TilingTorusDCEL.empty

    // Vertices on torus are identified modulo the grid, so only width*height distinct vertices
    // Place them on [0,1]x[0,1] lattice
    val verts =
      Array.tabulate(height, width) { (j, i) =>
        val id  = VertexId(s"V${j * width + i + 1}")
        val pos = BigPoint(BigDecimal(i), BigDecimal(j))
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
    // Bottom edge e0 of cell (j,i) twins with top edge e2 of cell below (j-1,i)
    // Right edge e1 of cell (j,i) twins with left edge e3 of cell to the right (j,i+1)
    for j <- 0 until height; i <- 0 until width do
      // Bottom edge: e0(j,i) twins with e2(j-1,i)
      val jB = wrapY(j - 1)
      e0(j)(i).twinWith(e2(jB)(i))

      // Right edge: e1(j,i) twins with e3(j,i+1)
      val iR = wrapX(i + 1)
      e1(j)(i).twinWith(e3(j)(iR))

    // Set leaving edge on each vertex: pick one incident (prefer e0 starting at that vertex if exists)
    // Each vertex (i,j) is origin of edges e0(i,j) and e3(i-1,j) after wrapping;
    // choose e0(i,j) as leaving for convenience.
    for j <- 0 until height; i <- 0 until width do
      verts(j)(i).leaving = Some(e0(j)(i))

    val halfEdges =
      (for j <- 0 until height; i <- 0 until width
      yield List(e0(j)(i), e1(j)(i), e2(j)(i), e3(j)(i))).flatten.toList

    TilingTorusDCEL(vertices, halfEdges, faces.flatten.toList)

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
        val id     = VertexId(s"V${j * width + i + 1}")
        Vertex(id, BigPoint(x, y))
      }

    val vertices = verts.flatten.toList

    // Each rhombus (cell) will be split into two equilateral triangles.
    // For a cell (j,i), the 4 corners depend on whether the row is even or odd.
    // Because of staggering, the rhombus corners shift differently.
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
      val evenRow = (j & 1) == 0

      val A = verts(j)(i)
      val B = verts(j)(wrapX(i + 1))

      val jNext = wrapY(j + 1)
      val nextRowEven = (jNext & 1) == 0

      // Adjust horizontal index for top vertices based on stagger pattern
      // When going from even to odd row: vertices shift right by 0.5
      //   Most cells use same column i, but at left edge (i=0) we wrap to i-1
      // When going from odd to even row: vertices shift left by 0.5, so we want next column i+1
      val iTop = if evenRow && !nextRowEven then
        wrapX(i - 1) // Even to odd: shift left by one to account for the stagger
      else if !evenRow && nextRowEven then
        wrapX(i + 1) // Odd to even: shift right by one
      else
        i
      val iTopRight = wrapX(iTop + 1)

      val C = verts(jNext)(iTop)
      val D = verts(jNext)(iTopRight)

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
      opp       <- buckets.get(keyOpp(k))
    do
      // pair elements one-by-one (handles wrapping: there should be same multiplicity)
      val n   = Math.min(list.size, opp.size)
      var idx = 0
      while idx < n do
        val e1 = list(idx)
        val e2 = opp(idx)
        if e1.twin.isEmpty && e2.twin.isEmpty then e1.twinWith(e2)
        idx += 1

    // Leaving edge per vertex: pick any incident we created; prefer the first seen
    val byVertex = allHE.groupBy(_.origin)
    byVertex.foreach { case (v, es) =>
      if v.leaving.isEmpty then v.leaving = Some(es.head)
    }

    // Return structure
    TilingTorusDCEL(vertices, allHE, faces.iterator.flatMap(_.iterator.flatMap(_.iterator)).toList)

  def createHexagonNet(width: Int, height: Int): TilingTorusDCEL =
    // width = number of hexagons along U direction
    // height = number of hexagons along V direction
    if width <= 0 || height <= 0 || height % 2 == 1 then
      return TilingTorusDCEL.empty

    import scala.collection.mutable
    val oneTwenty = AngleDegree(120)

    // Global directions (0°, 60°, 120°)
    val h0  = BigDecimal(0)
    val h1  = BigDecimal(Math.PI) / 3
    val h2  = BigDecimal(2) * BigDecimal(Math.PI) / 3
    val g0x = spire.math.cos(h0);
    val g0y = spire.math.sin(h0)
    val g1x = spire.math.cos(h1);
    val g1y = spire.math.sin(h1)
    val g2x = spire.math.cos(h2);
    val g2y = spire.math.sin(h2)

    // Hex corner triples (CCW): 0, g0, g0+g1, g0+g1+g2, g1+g2, g2
    val cornerTriples: Array[(Int, Int, Int)] =
      Array((0, 0, 0), (1, 0, 0), (1, 1, 0), (1, 1, 1), (0, 1, 1), (0, 0, 1))

    // Cell base in triple coords: i*(g0+g1) + j*(g1+g2) = (i, i+j, j)
    inline def baseTriple(i: Int, j: Int): (Int, Int, Int) = (i, i + j, j)

    inline def addT(a: (Int, Int, Int), b: (Int, Int, Int)) = (a._1 + b._1, a._2 + b._2, a._3 + b._3)

    // Torus lattice periods in triple basis:
    // P1 = width*(g0+g1) = (width, width, 0)
    // P2 = height*(g1+g2) = (0, height, height)
    inline def floorDiv(a: Int, b: Int): Int =
      val q = a / b
      if a % b < 0 then q - 1 else q

    inline def modTripleToFundamental(n0: Int, n1: Int, n2: Int): (Int, Int, Int) =
      // Reduce along P1 to bring n0 into [0,width)
      val k1 = floorDiv(n0, width)
      val r0 = n0 - k1 * width
      val r1 = n1 - k1 * width
      val r2 = n2
      // Reduce along P2 to bring r2 into [0,height)
      val k2 = floorDiv(r2, height)
      val s0 = r0
      val s1 = r1 - k2 * height
      val s2 = r2 - k2 * height
      (s0, s1, s2)

    // Triple → coordinates
    def tripleToPoint(n0: Int, n1: Int, n2: Int): BigPoint =
      val x = BigDecimal(n0) * g0x + BigDecimal(n1) * g1x + BigDecimal(n2) * g2x
      val y = BigDecimal(n0) * g0y + BigDecimal(n1) * g1y + BigDecimal(n2) * g2y
      BigPoint(x, y)

    // Unique vertices by canonicalized triple
    val vertexByTriple = mutable.Map[(Int, Int, Int), Vertex]()
    var vCount         = 1

    def vOfRaw(key: (Int, Int, Int)): Vertex =
      vertexByTriple.getOrElseUpdate(
        key, {
          val (a, b, c) = key
          val v         = Vertex(VertexId(s"V$vCount"), tripleToPoint(a, b, c))
          vCount += 1
          v
        }
      )

    def vOf(n0: Int, n1: Int, n2: Int): Vertex =
      vOfRaw(modTripleToFundamental(n0, n1, n2))

    // Unique directed half-edges with automatic twin creation
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

    // Faces
    val faces = Array.tabulate(height, width) { (j, i) =>

      Face(FaceId(s"F${j * width + i + 1}"))
    }

    // Build faces; edges deduped; vertices canonicalized to torus fundamental domain
    for j <- 0 until height; i <- 0 until width do
      val f    = faces(j)(i)
      val b    = baseTriple(i, j)
      val keys = (0 until 6).map(k => addT(b, cornerTriples(k)))
      val vs   = keys.map { case (a, b, c) =>
        vOf(a, b, c)
      }.toArray
      val ring = (0 until 6).toList.map(k => he(vs(k), vs((k + 1) % 6)))

      // Link cycle and set angles
      ring.indices.foreach(k => ring(k).linkWith(ring((k + 1) % 6)))
      ring.foreach { e =>
        e.incidentFace = Some(f)
        e.angle = Some(oneTwenty)
      }
      f.outerComponent = Some(ring.head)

    // Leaving edge per vertex
    val allVertices  = vertexByTriple.values.toList
    val allHalfEdges = halfEdgeByDir.values.toList
    allVertices.foreach(v => v.leaving = allHalfEdges.find(_.origin eq v))

    // Expect: vertices = 2*width*height, faces = width*height, half-edges = 6*faces
    TilingTorusDCEL(allVertices, allHalfEdges, faces.flatten.toList)
