package io.github.scala_tessella
package dcel

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.*

/**
 * Represents the entire tiling structure as a container for its components.
 *
 * @param vertices   List of all vertices in the tiling.
 * @param halfEdges  List of all half-edges in the tiling.
 * @param innerFaces List of the tiling's interior faces.
 * @param outerFace  The single, unbounded outer face of the tiling.
 */
case class TilingDCEL(
  vertices: List[Vertex],
   halfEdges: List[HalfEdge],
    innerFaces: List[Face],
     outerFace: Face
):

  /** @return a list of all faces, both inner and outer */
  def faces: List[Face] =
    outerFace :: innerFaces

  /**
   * Finds the outer boundary of the tiling.
   *
   * The traversal follows the half-edges of the outer face, which are linked in
   * a clockwise order around the perimeter.
   *
   * @return A Vector of Vertices forming the perimeter, in clockwise order.
   *         Returns an empty Vector if the outer face has no boundary component.
   */
  def boundary: Vector[Vertex] =
    outerFace.outerComponent match
      case None => Vector.empty
      case Some(startEdge) =>
        val builder = Vector.newBuilder[Vertex]

        @tailrec
        def collectVertices(edge: HalfEdge): Unit =
          builder += edge.origin
          edge.next match
            case Some(nextEdge) if nextEdge ne startEdge => collectVertices(nextEdge)
            case _ => // Traversal complete or malformed loop

        collectVertices(startEdge)
        builder.result()

  def boundarySafe: Vector[Vertex] =
    outerFace.outerComponent match
      case None => Vector.empty
      case Some(startEdge) =>
        val builder = Vector.newBuilder[Vertex]
        val visited = scala.collection.mutable.Set.empty[HalfEdge]

        @tailrec
        def collectVertices(edge: HalfEdge): Unit =
          if visited.contains(edge) then
            if edge eq startEdge then
              () // completed loop: that's fine
            else
              throw new Error("Malformed loop detected") // repeated non-start: broken!
          else
            builder += edge.origin
            visited += edge
            edge.next match
              case Some(nextEdge) => collectVertices(nextEdge)
              case None => throw new Error("Open chain")

        collectVertices(startEdge)
        builder.result()

  /**
   * Helper method to get all half-edges forming the outer boundary loop.
   */
  private def getBoundaryEdges: List[HalfEdge] =
    outerFace.outerComponent.map { start =>
      @scala.annotation.tailrec
      def loop(current: HalfEdge, acc: List[HalfEdge]): List[HalfEdge] =
        val next = current.next.getOrElse(throw new IllegalStateException("Boundary loop is not closed."))
        // Prepend the current edge and continue with the next, until we are back at the start.
        if next eq start then (current :: acc).reverse
        else loop(next, current :: acc)
      // Start the traversal from the beginning.
      loop(start, Nil)
    }.getOrElse(Nil)

  /**
   * Finds a boundary half-edge that originates at the vertex with the given ID.
   *
   * @param vertexId The ID of the origin vertex.
   * @return An Option containing the HalfEdge if found, otherwise None.
   */
  private def findBoundaryEdge(vertexId: String): Option[HalfEdge] =
    getBoundaryEdges.find(_.origin.id == vertexId)

  /**
   * Calculates the coordinates of the new vertices for a regular polygon built upon an existing edge.
   *
   * @param v_start The first vertex of the base edge.
   * @param v_end   The second vertex of the base edge.
   * @param sides   The number of sides for the new regular polygon.
   * @return A list of (Double, Double) tuples for the new vertex coordinates.
   */
  private def calculateNewVertices(v_start: Vertex, v_end: Vertex, sides: Int): List[(Double, Double)] = {
    val interiorAngle = (sides - 2) * 180.0 / sides
    val turnAngle = 180.0 - interiorAngle

    val p_start = (v_start.x, v_start.y)
    val p_end = (v_end.x, v_end.y)

    val dx = p_end._1 - p_start._1
    val dy = p_end._2 - p_start._2
    var heading = atan2(dy, dx)
    var currentPoint = p_end
    val newPoints = List.newBuilder[(Double, Double)]

    // We need to add (sides - 2) new vertices.
    for (_ <- 1 until sides - 1)
      heading += toRadians(turnAngle)
      val nextPoint = (currentPoint._1 + cos(heading), currentPoint._2 + sin(heading))
      newPoints += nextPoint
      currentPoint = nextPoint
    newPoints.result()
  }

  /**
   * Adds a new regular polygon to a specified boundary edge of the tiling.
   *
   * This method does not check for self-intersections.
   *
   * @param sides The number of sides of the regular polygon to add.
   * @param onEdgeStartingWithVertexId The ID of the vertex where the boundary edge starts.
   * @return Either an error message or a new TilingDCEL with the polygon added.
   */
  def maybeAddRegularPolygon(sides: Int, onEdgeStartingWithVertexId: String): Either[String, TilingDCEL] =
    if sides < 3 then return Left(s"A polygon must have at least 3 sides, but $sides were specified.")

    findBoundaryEdge(onEdgeStartingWithVertexId) match
      case None =>
        Left(s"No boundary edge found starting with vertex ID $onEdgeStartingWithVertexId")
      case Some(baseEdge) =>
        val v_start = baseEdge.origin
        val v_end = baseEdge.twin.get.origin // Destination of baseEdge

        // 1. Calculate new vertex positions and create Vertex objects
        val newVertexCoords = calculateNewVertices(v_start, v_end, sides)
        val maxVertexNum = this.vertices.map(_.id.filter(_.isDigit).toInt).maxOption.getOrElse(-1)
        val newVertices = newVertexCoords.zipWithIndex.map { case ((x, y), i) =>
          Vertex(s"V${maxVertexNum + 1 + i}", x, y)
        }

        // 2. Create the new face and half-edges
        val newFace = Face(s"F_Poly_${innerFaces.size}")
        val polyVertices = List(v_start, v_end) ++ newVertices
        val newInnerEdges = mutable.ListBuffer.empty[HalfEdge]
        val newOuterEdges = mutable.ListBuffer.empty[HalfEdge]

        // Create sides-1 new pairs of half-edges
        for (i <- 1 until sides)
          val p1 = polyVertices(i)
          val p2 = polyVertices((i + 1) % sides)
          val inner = HalfEdge(p1, incidentFace = Some(newFace))
          val outer = HalfEdge(p2, incidentFace = Some(outerFace))
          inner.twin = Some(outer)
          outer.twin = Some(inner)
          newInnerEdges.addOne(inner)
          newOuterEdges.addOne(outer)

        // 3. Stitch the new elements into the DCEL graph
        // Update the original base edge to be part of the new inner face
        val oldPrev = baseEdge.prev.get
        val oldNext = baseEdge.next.get
        baseEdge.incidentFace = Some(newFace)

        // Link the inner loop for the new face
        val allInnerEdges = baseEdge +: newInnerEdges.toList
        for (i <- 0 until sides)
          val current = allInnerEdges(i)
          val next = allInnerEdges((i + 1) % sides)
          current.next = Some(next)
          next.prev = Some(current)
          // Set leaving edge for new vertices
          if i >= 1 then // polyVertices(1) is v_end, polyVertices(2) is the first new vertex
            polyVertices(i).leaving = Some(current)
        newFace.outerComponent = Some(baseEdge)

        // Link the new outer boundary edges
        val outerChain = newOuterEdges.reverse.toList
        oldPrev.next = Some(outerChain.head)
        outerChain.head.prev = Some(oldPrev)
        outerChain.last.next = Some(oldNext)
        oldNext.prev = Some(outerChain.last)

        for (i <- 0 until outerChain.size - 1)
          outerChain(i).next = Some(outerChain(i + 1))
          outerChain(i + 1).prev = Some(outerChain(i))

        Right(this.copy(
          vertices = this.vertices ++ newVertices,
          halfEdges = this.halfEdges ++ newInnerEdges ++ newOuterEdges,
          innerFaces = this.innerFaces :+ newFace
        ))