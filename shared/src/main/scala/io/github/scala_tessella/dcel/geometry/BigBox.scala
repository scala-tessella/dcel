package io.github.scala_tessella.dcel.geometry

case class BigBox(min: BigPoint, max: BigPoint):

  def contains(point: BigPoint): Boolean =
    point.x >= min.x && point.x <= max.x &&
      point.y >= min.y && point.y <= max.y

  def width: BigDecimal = max.x - min.x

  def height: BigDecimal = max.y - min.y

  /** Checks if this bounding box intersects with another one. */
  def intersects(that: BigBox): Boolean =
    !(that.min.x > this.max.x || that.max.x < this.min.x || that.min.y > this.max.y ||
      that.max.y < this.min.y)

  def enlargeMinMax(by: BigPoint): BigBox =
    BigBox(min - by, max + by)

  /** Expands the bounding box by a given amount in all directions. */
  def expand(by: BigDecimal): BigBox =
    enlargeMinMax(BigPoint(by, by))

object BigBox:
  /** Creates a BoundingBox that encloses a collection of points. */
  def fromPoints(points: Iterable[BigPoint]): BigBox =
    if points.isEmpty then BigBox(BigPoint.origin, BigPoint.origin)
    else
      val xs = points.map(_.x)
      val ys = points.map(_.y)
      BigBox(BigPoint(xs.min, ys.min), BigPoint(xs.max, ys.max))

  /** Creates a BoundingBox for a single line segment. */
  def fromSegment(segment: BigLineSegment): BigBox =
    fromPoints(List(segment.p1, segment.p2))
