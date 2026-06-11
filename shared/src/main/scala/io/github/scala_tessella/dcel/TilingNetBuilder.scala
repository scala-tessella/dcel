package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint, BigRadian}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}

import scala.collection.mutable

/** Lattice-construction plumbing behind [[TilingBuilder]]'s net constructors: the rectangular vertex/edge
  * grids shared by the triangle and rhombus nets, the triple-coordinate honeycomb build, and the boundary
  * finalisation they all end with.
  *
  * Inputs are NOT validated here — [[TilingBuilder]] refines dimensions and angles before delegating, which
  * is why every entry point is `…Unsafe` (ADR-0003).
  */
private[dcel] object TilingNetBuilder:

  final private case class RectNetEdges(
      vertices: Array[Array[Vertex]],
      horizontal: Array[Array[(HalfEdge, HalfEdge)]],
      vSlope: Array[Array[(HalfEdge, HalfEdge)]],
      outerFace: Face
  )

  private def buildRectNetEdges(
      height: Int,
      width: Int,
      angle: AngleDegree
  ): RectNetEdges =
    val (_, vertices)        = pointsVertices(height, width, angle)
    val (horizontal, vSlope) = horizontalAndVSlope(height, width, vertices)
    setLeavingEdges(height, width, vertices, horizontal, vSlope)
    RectNetEdges(vertices, horizontal, vSlope, Face.outer)

  private def finalizeOuterBoundary(
      height: Int,
      width: Int,
      horizontal: Array[Array[(HalfEdge, HalfEdge)]],
      vSlope: Array[Array[(HalfEdge, HalfEdge)]],
      outerFace: Face,
      allHalfEdges: List[HalfEdge]
  ): Unit =
    val outerBoundaryCW = linkOuterFace(height, width, horizontal, vSlope, outerFace)
    allHalfEdges.setOuterEdgeAngles(outerBoundaryCW, outerFace)

  /** Generates a grid of vertices for tessellation patterns.
    *
    * @param height
    *   The number of rows in the grid
    * @param width
    *   The number of columns in the grid
    * @param angle
    *   The angle for vertex positioning
    */
  private def pointsVertices(
      height: Int,
      width: Int,
      angle: AngleDegree
  ): (Array[Array[BigPoint]], Array[Array[Vertex]]) =
    // ADR-0009 candidate A: Math trig on Double.
    val rad     = angle.toBigRadian.toDouble
    val v_vec_x = BigDecimal(Math.cos(rad))
    val v_vec_y = BigDecimal(Math.sin(rad))

    val points =
      Array.tabulate(height + 1, width + 1): (j, i) =>
        BigPoint(BigDecimal(i) + v_vec_x * j, v_vec_y * j)

    val vertices =
      Array.tabulate(height + 1, width + 1): (j, i) =>
        val id = j * (width + 1) + i + 1
        Vertex(VertexId(id), points(j)(i))

    (points, vertices)

  private def createTwinPair(v1: Vertex, v2: Vertex): (HalfEdge, HalfEdge) =
    val e1 = HalfEdge(v1)
    val e2 = HalfEdge(v2)
    e1.twinWith(e2)
    (e1, e2)

  private def horizontalAndVSlope(
      height: Int,
      width: Int,
      vertices: Array[Array[Vertex]]
  ): (Array[Array[(HalfEdge, HalfEdge)]], Array[Array[(HalfEdge, HalfEdge)]]) =
    val horizontal =
      Array.tabulate(height + 1, width): (j, i) =>
        createTwinPair(vertices(j)(i), vertices(j)(i + 1))
    val vSlope     =
      Array.tabulate(height, width + 1): (j, i) =>
        createTwinPair(vertices(j)(i), vertices(j + 1)(i))

    (horizontal, vSlope)

  // Set leaving edges for vertices
  private def setLeavingEdges(
      height: Int,
      width: Int,
      vertices: Array[Array[Vertex]],
      horizontal: Array[Array[(HalfEdge, HalfEdge)]],
      vSlope: Array[Array[(HalfEdge, HalfEdge)]]
  ): Unit =
    for j <- 0 to height; i <- 0 to width do
      val v = vertices(j)(i)
      if i < width then v.leaving = Some(horizontal(j)(i)._1)
      else if j < height then v.leaving = Some(vSlope(j)(i)._1)
      else if i > 0 then v.leaving = Some(horizontal(j)(i - 1)._2)
      else if j > 0 then v.leaving = Some(vSlope(j - 1)(i)._2)

  private def netFaces(height: Int, width: Int): Array[Array[Face]] =
    Array.tabulate(height, width): (j, i) =>
      Face(FaceId(j * width + i + 1))

  // Link outer face boundary
  private def linkOuterFace(
      height: Int,
      width: Int,
      horizontal: Array[Array[(HalfEdge, HalfEdge)]],
      vSlope: Array[Array[(HalfEdge, HalfEdge)]],
      fOuter: Face
  ): List[HalfEdge] =
    val innerBoundaryEdgesCCW = new mutable.ListBuffer[HalfEdge]()
    // Bottom boundary
    for i <- 0 until width do
      innerBoundaryEdgesCCW += horizontal(0)(i)._1
    // Right boundary
    for j <- 0 until height do
      innerBoundaryEdgesCCW += vSlope(j)(width)._1
    // Top boundary
    for i <- (0 until width).reverse do
      innerBoundaryEdgesCCW += horizontal(height)(i)._2
    // Left boundary
    for j <- (0 until height).reverse do
      innerBoundaryEdgesCCW += vSlope(j)(0)._2

    val outerBoundaryCW =
      innerBoundaryEdgesCCW.toList
        .map: halfEdge =>
          halfEdge.twin.get
        .reverse
    outerBoundaryCW.linkInCycle()
    outerBoundaryCW.foreach: halfEdge =>
      halfEdge.incidentFace = Some(fOuter)
    fOuter.outerComponent = outerBoundaryCW.headOption
    outerBoundaryCW

  private def toHalfEdges(pairs: Array[Array[(HalfEdge, HalfEdge)]]): List[HalfEdge] =
    pairs
      .flatMap: row =>
        row.flatMap: (halfEdge1, halfEdge2) =>
          List(halfEdge1, halfEdge2)
      .toList

  private def netHalfEdges(
      horizontal: Array[Array[(HalfEdge, HalfEdge)]],
      vSlope: Array[Array[(HalfEdge, HalfEdge)]],
      i: Int,
      j: Int
  ): (HalfEdge, HalfEdge, HalfEdge, HalfEdge) =
    val e1 = horizontal(j)(i)._1     // v_ji -> v_ji1
    val e2 = vSlope(j)(i + 1)._1     // v_ji1 -> v_j1i1
    val e3 = horizontal(j + 1)(i)._2 // v_j1i1 -> v_j1i
    val e4 = vSlope(j)(i)._2         // v_j1i -> v_ji
    (e1, e2, e3, e4)

  /** Triangle net of `width` x `height` rhombus cells, each split into two equilateral triangles.
    * Precondition (validated by the caller): both dimensions positive.
    */
  private[dcel] def createTriangleNetUnsafe(
      width: Int,
      height: Int
  ): TilingDCEL =
    val w: Int = width
    val h: Int = height

    val triangleAngle = AngleDegree(60)

    val RectNetEdges(vertices, horizontal, vSlope, fOuter) =
      buildRectNetEdges(h, w, triangleAngle)

    // Two triangular faces per rhombus cell
    val faces     =
      Array.tabulate(h, w, 2): (j, i, k) =>
        Face(FaceId((j * w + i) * 2 + k + 1))
    // These diagonals split each rhombus into two equilateral triangles
    val diagonals =
      Array.tabulate(h, w): (j, i) =>
        createTwinPair(vertices(j)(i + 1), vertices(j + 1)(i))

    // Link inner faces
    for j <- 0 until h; i <- 0 until w do
      val face1 = faces(j)(i)(0) // Triangle (v_ji, v_ji1, v_j1i)
      val face2 = faces(j)(i)(1) // Triangle (v_ji1, v_j1i1, v_j1i)

      val (e1, e2, e3, e4) = netHalfEdges(horizontal, vSlope, i, j)
      val e_diag           = diagonals(j)(i)._1 // v_ji1 -> v_j1i
      val e_diag_rev       = diagonals(j)(i)._2 // v_j1i -> v_ji1

      // Link face1
      List(e1, e_diag, e4).linkFace(face1, triangleAngle)

      // Link face2
      List(e2, e3, e_diag_rev).linkFace(face2, triangleAngle)

    val allHalfEdges =
      toHalfEdges(horizontal) ++ toHalfEdges(vSlope) ++ toHalfEdges(diagonals)

    finalizeOuterBoundary(h, w, horizontal, vSlope, fOuter, allHalfEdges)

    TilingDCEL(
      vertices = vertices.flatten.toList,
      halfEdges = allHalfEdges,
      innerFaces = faces.flatten.flatten.toList,
      outerFace = fOuter
    )

  /** Rhombus net of `width` x `height` identical rhombi. Preconditions (validated by the caller): both
    * dimensions positive, `angle` strictly between 0 and 180 degrees.
    */
  private[dcel] def createRhombusNetUnsafe(
      width: Int,
      height: Int,
      angle: AngleDegree
  ): TilingDCEL =
    val w: Int = width
    val h: Int = height

    val alpha1 = angle
    val alpha2 = angle.supplement

    val RectNetEdges(vertices, horizontal, vSlope, fOuter) =
      buildRectNetEdges(h, w, angle)
    val faces                                              = netFaces(h, w)

    // Link inner faces
    for j <- 0 until h; i <- 0 until w do
      val face             = faces(j)(i)
      val (e1, e2, e3, e4) = netHalfEdges(horizontal, vSlope, i, j)

      List(e1, e2, e3, e4).linkFace(face, alpha1)
      e2.angle = Some(alpha2)
      e4.angle = Some(alpha2)

    val allHalfEdges =
      toHalfEdges(horizontal) ++ toHalfEdges(vSlope)

    finalizeOuterBoundary(h, w, horizontal, vSlope, fOuter, allHalfEdges)

    TilingDCEL(
      vertices = vertices.flatten.toList,
      halfEdges = allHalfEdges,
      innerFaces = faces.flatten.toList,
      outerFace = fOuter
    )

  /** Hexagon net of `width` x `height` identical hexagons, built on integer triples in a three-direction
    * basis. Preconditions (validated by the caller): both dimensions positive, `angle` strictly between 0 and
    * 180 degrees.
    */
  private[dcel] def createHexagonNetUnsafe(
      width: Int,
      height: Int,
      angle: AngleDegree
  ): TilingDCEL =
    val w: Int             = width
    val h: Int             = height
    val alpha: AngleDegree = angle

    val beta: AngleDegree = (alpha / 2).supplement

    // Interior angles per vertex in CCW order: [alpha, beta, beta, alpha, beta, beta]
    // Exterior turns at vertices: exterior(k) = 180 - interior(k)
    val exteriorAngles: Array[AngleDegree] =
      Array(alpha, beta, beta, alpha, beta, beta).map: angleDegree =>
        angleDegree.supplement

    // IMPORTANT: The exterior turn exterior(k) is applied at vertex k to go from edge k to edge (k+1).
    // Edge headings h(k) are the directions of edges (k) from vertex k to k+1.
    // Hence, h1 = h0 + exterior(1), h2 = h1 + exterior(2). (exterior(0) turns from edge5 to edge0.)
    val h0 = BigRadian(0)
    val h1 = h0 + exteriorAngles(1).toBigRadian
    val h2 = h1 + exteriorAngles(2).toBigRadian

    // Three global unit directions g0, g1, g2 (others are their opposites)
    // ADR-0009 candidate A: Math trig on Double.
    val g0x = BigDecimal(Math.cos(h0.toDouble))
    val g0y = BigDecimal(Math.sin(h0.toDouble))
    val g1x = BigDecimal(Math.cos(h1.toDouble))
    val g1y = BigDecimal(Math.sin(h1.toDouble))
    val g2x = BigDecimal(Math.cos(h2.toDouble))
    val g2y = BigDecimal(Math.sin(h2.toDouble))

    // Corner offsets as integer triples in the (g0,g1,g2) basis:
    // v0=0, v1=g0, v2=g0+g1, v3=g0+g1+g2, v4=g1+g2, v5=g2
    val cornerTriples: Array[(Int, Int, Int)] =
      Array(
        (0, 0, 0), // k=0
        (1, 0, 0), // k=1
        (1, 1, 0), // k=2
        (1, 1, 1), // k=3
        (0, 1, 1), // k=4
        (0, 0, 1)  // k=5
      )

    inline def baseTriple(i: Int, j: Int): (Int, Int, Int) =
      // T1 = g0+g1, T2 = g1+g2 => base = i*T1 + j*T2 = (i, i+j, j)
      (i, i + j, j)

    inline def addTriples(a: (Int, Int, Int), b: (Int, Int, Int)): (Int, Int, Int) =
      (a._1 + b._1, a._2 + b._2, a._3 + b._3)

    val vertexByTriple = mutable.Map[(Int, Int, Int), Vertex]()
    var vertexCounter  = 1

    def tripleToPoint(n0: Int, n1: Int, n2: Int): BigPoint =
      val x = BigDecimal(n0) * g0x + BigDecimal(n1) * g1x + BigDecimal(n2) * g2x
      val y = BigDecimal(n0) * g0y + BigDecimal(n1) * g1y + BigDecimal(n2) * g2y
      BigPoint(x, y)

    def getOrCreateVertexByTriple(key: (Int, Int, Int)): Vertex =
      vertexByTriple.getOrElseUpdate(
        key, {
          val (n0, n1, n2) = key
          val v            = Vertex(VertexId(vertexCounter), tripleToPoint(n0, n1, n2))
          vertexCounter += 1
          v
        }
      )

    val halfEdgeByDir = mutable.Map[(Vertex, Vertex), HalfEdge]()

    def getOrCreateHalfEdge(v1: Vertex, v2: Vertex): HalfEdge =
      halfEdgeByDir.getOrElseUpdate(
        (v1, v2), {
          val e1 = HalfEdge(v1)
          val e2 = HalfEdge(v2)
          e1.twinWith(e2)
          halfEdgeByDir((v2, v1)) = e2
          e1
        }
      )

    val faces  = netFaces(h, w)
    val fOuter = Face.outer

    for
      j <- 0 until h
      i <- 0 until w
    do
      val face       = faces(j)(i)
      val b          = baseTriple(i, j)
      val cornerKeys =
        (0 until 6)
          .map: k =>
            addTriples(b, cornerTriples(k))
          .toList
      val corners    = cornerKeys.map: (i, j, k) =>
        getOrCreateVertexByTriple(i, j, k)

      val cornerAngles: Array[AngleDegree] =
        Array(alpha, beta, beta, alpha, beta, beta)

      val edgesCCW =
        (0 until 6).toList.map: k =>
          val v1 = corners(k)
          val v2 = corners((k + 1) % 6)
          getOrCreateHalfEdge(v1, v2)

      edgesCCW.linkInCycle()
      edgesCCW.zipWithIndex.foreach: (e, k) =>
        e.incidentFace = Some(face)
        e.angle = Some(cornerAngles(k))
      face.outerComponent = edgesCCW.headOption

    val allVertices  = vertexByTriple.values.toList
    val allHalfEdges = halfEdgeByDir.values.toList

    allVertices.foreach: vertex =>
      vertex.leaving =
        allHalfEdges.find: halfEdge =>
          halfEdge.origin eq vertex

    val boundaryEdgesCW =
      allHalfEdges.filter: halfEdge =>
        halfEdge.incidentFace.isEmpty

    val boundaryOrdered = boundaryEdgesCW.orderBoundary

    if boundaryOrdered.nonEmpty then
      boundaryOrdered.linkInCycle()
      boundaryOrdered.foreach: halfEdge =>
        halfEdge.incidentFace = Some(fOuter)
      fOuter.outerComponent = boundaryOrdered.headOption

      allHalfEdges.setOuterEdgeAngles(boundaryOrdered, fOuter)

    TilingDCEL(
      vertices = allVertices,
      halfEdges = allHalfEdges,
      innerFaces = faces.flatten.toList,
      outerFace = fOuter
    )
