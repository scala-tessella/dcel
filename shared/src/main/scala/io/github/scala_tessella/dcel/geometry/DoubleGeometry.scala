package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.ACCURACY

import scala.collection.mutable
import scala.util.boundary

/** Parallel `Double`-precision helpers for `SimplePolygon.fromUntrusted` and downstream validation callers —
  * the validation-only fast path explored as candidate B of ADR-0009.
  *
  * The `BigDecimal` / Spire-trig path in `BigPoint` / `BigLineSegment` / `unitPath` is preserved unchanged
  * elsewhere. This file is not a general geometry layer — every primitive here is tailored to the three
  * predicates the validator evaluates on reconstructed unit-side polygons: self-intersection, closure length,
  * and vertex-coincidence. All are epsilon-tolerance checks.
  *
  * ADR finding 1 shows `AngleDegree.toBigRadian` already goes through `.toDouble`, so the pipeline's
  * exactness is illusory at the input; this file just stops paying the Spire-`BigDecimal` tax on the trig
  * output.
  */
private[dcel] object DoubleGeometry:

  /** Cartesian point with `Double` coordinates. Tuple keeps allocation cheap and lets Scala 3 inline the
    * accessors.
    */
  private[dcel] type Point2D = (Double, Double)

  private[dcel] inline def x(p: Point2D): Double = p._1
  private[dcel] inline def y(p: Point2D): Double = p._2

  /** `(rho·cos θ, rho·sin θ)` using `java.lang.Math` — the core optimisation. */
  private[dcel] def fromPolar(rho: Double, theta: Double): Point2D =
    (rho * Math.cos(theta), rho * Math.sin(theta))

  /** Rebuilds the polygon's vertex sequence from its interior angles on unit-length sides, mirroring
    * `BigLineSegment.unitPath` for the `p1 = origin`, `p2 = (1, 0)` case used by
    * `SimplePolygon.fromUntrusted`. Emits `n` points (`V0 .. V(n-1)`); the closing edge back to `V0` is
    * implicit.
    */
  private[dcel] def unitPath(angles: Vector[AngleDegree]): Array[Point2D] =
    val n       = angles.length
    val out     = new Array[Point2D](n)
    out(0) = (0.0, 0.0)
    out(1) = (1.0, 0.0)
    var heading = 0.0 // angleTo((1,0)) from origin
    var cx      = 1.0
    var cy      = 0.0
    var i       = 1
    while i < n - 1 do
      val turnRad = angles(i).supplement.toDoubleRadian
      heading += turnRad
      cx += Math.cos(heading)
      cy += Math.sin(heading)
      out(i + 1) = (cx, cy)
      i += 1
    out

  /** Closure error — distance from the last reconstructed vertex back to the first. For a valid unit-sided
    * polygon this equals 1.0.
    */
  private[dcel] def closingEdgeLength(points: Array[Point2D]): Double =
    val n  = points.length
    val dx = x(points(0)) - x(points(n - 1))
    val dy = y(points(0)) - y(points(n - 1))
    Math.sqrt(dx * dx + dy * dy)

  private inline val Collinear        = 0
  private inline val Clockwise        = 1
  private inline val Counterclockwise = 2

  /** Orientation of the triple (p, q, r). Mirrors `BigPoint.orientation`, with the same `ACCURACY` band for
    * collinearity.
    */
  private def orientation(
      px: Double,
      py: Double,
      qx: Double,
      qy: Double,
      rx: Double,
      ry: Double
  ): Int =
    val v = (qy - py) * (rx - qx) - (qx - px) * (ry - qy)
    if Math.abs(v) < ACCURACY then Collinear
    else if v > 0 then Clockwise
    else Counterclockwise

  /** Interior-only intersection test, matching `BigLineSegment.interiorIntersects`. Used for the non-adjacent
    * segment pairs in the boundary sweep.
    */
  private def interiorIntersects(
      a1x: Double,
      a1y: Double,
      a2x: Double,
      a2y: Double,
      b1x: Double,
      b1y: Double,
      b2x: Double,
      b2y: Double
  ): Boolean =
    val o1 = orientation(a1x, a1y, a2x, a2y, b1x, b1y)
    val o2 = orientation(a1x, a1y, a2x, a2y, b2x, b2y)
    val o3 = orientation(b1x, b1y, b2x, b2y, a1x, a1y)
    val o4 = orientation(b1x, b1y, b2x, b2y, a2x, a2y)
    o1 != o2 && o3 != o4

  private def almostEquals(a: Point2D, b: Point2D): Boolean =
    val acc = ACCURACY
    Math.abs(x(a) - x(b)) < acc && Math.abs(y(a) - y(b)) < acc

  /** Mirrors `BigPoint.hasNoAlmostEqualPoints` using a spatial grid on `Double` coordinates. Used as the
    * vertex-coincidence precondition for simplicity.
    */
  private def hasNoAlmostEqualPoints(points: Array[Point2D]): Boolean =
    val n        = points.length
    if n < 2 then return true
    val cellSize = ACCURACY
    val grid     = mutable.HashMap.empty[(Long, Long), mutable.ArrayBuffer[Point2D]]
    boundary:
      var k = 0
      while k < n do
        val p  = points(k)
        val cx = Math.floor(x(p) / cellSize).toLong
        val cy = Math.floor(y(p) / cellSize).toLong
        var i  = -1
        while i <= 1 do
          var j = -1
          while j <= 1 do
            grid.get((cx + i, cy + j)) match
              case Some(bucket) =>
                var b = 0
                while b < bucket.length do
                  if almostEquals(bucket(b), p) then boundary.break(false)
                  b += 1
              case None         => ()
            j += 1
          i += 1
        grid.getOrElseUpdate((cx, cy), mutable.ArrayBuffer.empty).append(p): Unit
        k += 1
      true

  /** Self-intersection test, mirroring `BigPoint.isSimplePolygon`. O(n²) pair loop; same shape as the
    * BigDecimal version so numerical semantics match.
    */
  private[dcel] def isSimplePolygon(points: Array[Point2D]): Boolean =
    val n = points.length
    if n < 4 then return true
    if !hasNoAlmostEqualPoints(points) then return false

    // Segment i = points(i) -> points((i+1) % n); unpack into flat arrays
    // to avoid allocating segment tuples per-pair.
    val ax = new Array[Double](n)
    val ay = new Array[Double](n)
    val bx = new Array[Double](n)
    val by = new Array[Double](n)
    var k  = 0
    while k < n do
      val p = points(k)
      val q = points((k + 1) % n)
      ax(k) = x(p); ay(k) = y(p)
      bx(k) = x(q); by(k) = y(q)
      k += 1

    boundary:
      var i = 0
      while i < n do
        var j = i + 1
        while j < n do
          if i != (j + 1) % n && j != (i + 1) % n && !(i == 0 && j == n - 1) then
            if interiorIntersects(ax(i), ay(i), bx(i), by(i), ax(j), ay(j), bx(j), by(j)) then
              boundary.break(false)
          j += 1
        i += 1
      true
