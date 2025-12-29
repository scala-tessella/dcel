package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.{IntersectionDetection, Orientation}
import spire.implicits.*

import scala.collection.mutable

opaque type BigLineSegment = (p1: BigPoint, p2: BigPoint)

object BigLineSegment:

  inline def apply(p1: BigPoint, p2: BigPoint): BigLineSegment =
    (p1, p2)

  extension (segment: BigLineSegment)

    inline def p1: BigPoint =
      segment.p1

    inline def p2: BigPoint =
      segment.p2

    /** The length of the line segment. */
    def length: BigDecimal =
      spire.math.sqrt((segment.p2.x - segment.p1.x).pow(2) + (segment.p2.y - segment.p1.y).pow(2))

    def midPoint: BigPoint =
      BigPoint((segment.p1.x + segment.p2.x) / 2, (segment.p1.y + segment.p2.y) / 2)

    /** The horizontal angle of the line segment. */
    def horizontalAngle: BigRadian =
      BigRadian(spire.math.atan2(segment.p2.y - segment.p1.y, segment.p2.x - segment.p1.x))

    private def orientations(that: BigLineSegment): (Orientation, Orientation, Orientation, Orientation) =
      val o1 = BigPoint.orientation(segment.p1, segment.p2, that.p1)
      val o2 = BigPoint.orientation(segment.p1, segment.p2, that.p2)
      val o3 = BigPoint.orientation(that.p1, that.p2, segment.p1)
      val o4 = BigPoint.orientation(that.p1, that.p2, segment.p2)
      (o1, o2, o3, o4)

    /** Checks if this bounding box intersects with another one. */
    def intersects(that: BigLineSegment): Boolean =
      val (o1, o2, o3, o4) = orientations(that)

      // General case: segments cross each other
      if o1 != Orientation.Collinear
        && o2 != Orientation.Collinear
        && o3 != Orientation.Collinear
        && o4 != Orientation.Collinear
      then
        o1 != o2 && o3 != o4
      // Special Cases for collinear points
      else
        (o1 == Orientation.Collinear && BigPoint.onSegment(segment.p1, that.p1, segment.p2))
        || (o2 == Orientation.Collinear && BigPoint.onSegment(segment.p1, that.p2, segment.p2))
        || (o3 == Orientation.Collinear && BigPoint.onSegment(that.p1, segment.p1, that.p2))
        || (o4 == Orientation.Collinear && BigPoint.onSegment(that.p1, segment.p2, that.p2))

    def properlyIntersects(that: BigLineSegment): Boolean =
      // If segments share an endpoint, it's not a proper intersection
      // Using almostEquals directly on endpoints is much faster than creating Sets
      if segment.p1.almostEquals(that.p1) || segment.p1.almostEquals(that.p2) ||
        segment.p2.almostEquals(that.p1) || segment.p2.almostEquals(that.p2)
      then
        false
      else
        val (o1, o2, o3, o4) = orientations(that)
        // General case: segments cross each other in their interiors
        o1 != o2 && o3 != o4

    /** Computes a list of points forming a polygonal path unit lenght segments based on the segment initial
      * angle and orientation.
      *
      * @param angles
      *   a vector of AngleDegree, where each angle represents the interior angle of the polygon at each
      *   vertex
      * @return
      *   a list of BigPoint representing the vertices of the computed path
      */
    def unitPath(angles: Vector[AngleDegree]): List[BigPoint] =
      val n            = angles.length
      // Start with V0 at the origin and V1
      val points       = mutable.ListBuffer(p1, p2)
      var currentPoint = p2
      var heading      = p1.angleTo(p2)
      // Calculate the positions of V2 through V(n-1)
      for (i <- 1 until n - 1)
        val interiorAngle = angles(i)
        val turnAngle     = interiorAngle.supplement
        heading += turnAngle.toBigRadian
        currentPoint = currentPoint + BigPoint.fromPolar(1, heading)
        points.append(currentPoint)

      points.toList

  extension (segments: Seq[BigLineSegment])

    def toPoints: Seq[BigPoint] =
      segments.flatMap: segment =>
        List(segment.p1, segment.p2)

    /** Checks if this list of segments has any proper intersections with another list. Uses spatial
      * partitioning for better performance.
      *
      * @param other
      *   another list of segments
      * @param cellSize
      *   Size of each grid cell for spatial partitioning, defaulted to 2 that is double of the unit segment
      */
    def hasProperIntersections(
        other: Vector[BigLineSegment],
        cellSize: Option[BigDecimal] = Some(2)
    ): Boolean =
      IntersectionDetection.hasProperIntersection(segments, other, cellSize)

    def properIntersections(
        other: Vector[BigLineSegment],
        cellSize: Option[BigDecimal] = Some(2)
    ): List[(BigLineSegment, BigLineSegment)] =
      IntersectionDetection.properIntersections(segments, other, cellSize)

    def totalLength: BigDecimal =
      segments
        .map: segment =>
          segment.length
        .sum
