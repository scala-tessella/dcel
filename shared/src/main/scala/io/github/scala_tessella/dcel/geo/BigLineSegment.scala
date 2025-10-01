package io.github.scala_tessella.dcel.geo

import io.github.scala_tessella.dcel.geo.BigDecimalGeometry.{IntersectionDetection, Orientation}
import spire.implicits.*

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
      val thisPoints = Set(segment.p1, segment.p2)
      val thatPoints = Set(that.p1, that.p2)

      if thisPoints.exists(p1 => thatPoints.exists(p2 => p1.almostEquals(p2))) then
        false
      else
        val (o1, o2, o3, o4) = orientations(that)
        // General case: segments cross each other in their interiors
        o1 != o2 && o3 != o4

  extension (segments: List[BigLineSegment])

    /** Checks if this list of segments has any proper intersections with another list. Uses spatial
      * partitioning for better performance.
      *
      * @param other
      *   another list of segments
      * @param cellSize
      *   Size of each grid cell for spatial partitioning, defaulted to 2 that is double of unit segment
      */
    def hasProperIntersections(other: List[BigLineSegment], cellSize: Option[BigDecimal] = Some(2)): Boolean =
      IntersectionDetection.hasProperIntersection(segments, other, cellSize)
