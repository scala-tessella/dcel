package io.github.scala_tessella
package dcel

import scala.annotation.tailrec
import scala.math.{atan2, cos, sin, toDegrees, toRadians}

/**
 * Represents a vertex in the tiling.
 *
 * @param id A unique identifier for the vertex.
 * @param x  The x-coordinate of the vertex.
 * @param y  The y-coordinate of the vertex.
 */
case class Vertex(id: String, x: Double, y: Double):
  var leaving: Option[HalfEdge] = None
  override def toString: String = s"Vertex($id)"

/**
 * Represents a directed edge segment.
 *
 * @param origin The vertex where this half-edge starts.
 */
case class HalfEdge(origin: Vertex):
  var twin: Option[HalfEdge] = None
  var incidentFace: Option[Face] = None
  var next: Option[HalfEdge] = None
  var prev: Option[HalfEdge] = None
  var angle: Double = 0.0

  override def toString: String =
    // Safely get the destination vertex's ID from the optional twin
    val destId = twin.map(_.origin.id).getOrElse("?")
    s"HalfEdge(${origin.id} -> $destId)"

/**
 * Represents a face (a polygon) in the tiling.
 *
 * @param id A unique identifier for the face.
 * @param outerComponent An optional half-edge on the boundary of the face.
 */
case class Face(id: String):
  var outerComponent: Option[HalfEdge] = None
  override def toString: String = s"Face($id)"

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

    // Find the starting edge on the outer boundary
    val boundaryEdge: Option[HalfEdge] =
      @tailrec
      def findEdge(current: HalfEdge, start: HalfEdge): Option[HalfEdge] =
        if (current.origin.id == onEdgeStartingWithVertexId) Some(current)
        else current.next match
          case Some(next) if next ne start => findEdge(next, start)
          case _ => None
      outerFace.outerComponent.flatMap(start => findEdge(start, start))

    boundaryEdge match
      case None => Left(s"No boundary edge found starting with vertex ID '$onEdgeStartingWithVertexId'")
      case Some(edgeToBuildOn) =>
        val v1 = edgeToBuildOn.origin
        val v2 = edgeToBuildOn.twin.get.origin

        // 1. Calculate new vertex coordinates
        val newVertexCount = this.vertices.size
        val angle = (sides - 2) * 180.0 / sides
        val turnAngle = 180.0 - angle
        val newVertices = List.newBuilder[Vertex]
        var currentPoint = v2
        var heading = toDegrees(atan2(v2.y - v1.y, v2.x - v1.x))

        for (i <- 0 until sides - 2)
          heading += turnAngle
          val newX = currentPoint.x + cos(toRadians(heading))
          val newY = currentPoint.y + sin(toRadians(heading))
          val newVertex = Vertex(s"V${newVertexCount + i}", newX, newY)
          newVertices += newVertex
          currentPoint = newVertex

        val newVerticesList = newVertices.result()
        val newPolygonVertices = List(v1, v2) ++ newVerticesList

        // 2. Create new DCEL components
        val newFace = Face(s"F${this.innerFaces.size + 1}")
        val newEdgePairs = (1 until sides).map { i =>
          val p1 = newPolygonVertices(i)
          val p2 = newPolygonVertices((i + 1) % sides)
          val inner = HalfEdge(p1)
          val outer = HalfEdge(p2)
          inner.twin = Some(outer)
          outer.twin = Some(inner)
          (inner, outer)
        }
        val newInnerEdges = newEdgePairs.map(_._1)
        val newOuterEdges = newEdgePairs.map(_._2).reverse

        // 3. Link the new polygon's inner cycle (CCW)
        val allInnerEdges = edgeToBuildOn :: newInnerEdges.toList
        for (i <- allInnerEdges.indices)
          val current = allInnerEdges(i)
          val next = allInnerEdges((i + 1) % sides)
          current.next = Some(next); next.prev = Some(current)
          current.incidentFace = Some(newFace)

        newFace.outerComponent = Some(edgeToBuildOn)

        // 4. Link the new outer boundary cycle (CW)
        val oldPrevOuter = edgeToBuildOn.prev.get
        val oldNextOuter = edgeToBuildOn.next.get

        // Connect the new outer edges to the existing boundary
        oldPrevOuter.next = Some(newOuterEdges.head)
        newOuterEdges.head.prev = Some(oldPrevOuter)
        newOuterEdges.last.next = Some(oldNextOuter)
        oldNextOuter.prev = Some(newOuterEdges.last)

        // Link the new outer edges together
        for (i <- newOuterEdges.indices)
          newOuterEdges(i).incidentFace = Some(this.outerFace)
          if i < newOuterEdges.length - 1 then
            newOuterEdges(i).next = Some(newOuterEdges(i + 1))
            newOuterEdges(i + 1).prev = Some(newOuterEdges(i))

        // Update the outer face's component reference if necessary
        if outerFace.outerComponent.contains(edgeToBuildOn) then
          outerFace.outerComponent = Some(newOuterEdges.head)

        // 5. Assemble and return the new TilingDCEL
        Right(TilingDCEL(
          vertices = this.vertices ++ newVerticesList,
          halfEdges = this.halfEdges ++ newInnerEdges ++ newOuterEdges.reverse,
          innerFaces = this.innerFaces :+ newFace,
          outerFace = this.outerFace
        ))