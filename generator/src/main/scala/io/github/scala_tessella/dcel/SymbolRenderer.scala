package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.DelaneySymbols.DSymbol

import scala.collection.mutable

/** Draw a euclidean regular-polygon tiling directly from its (minimal) Delaney–Dress symbol, by BARYCENTRIC
  * DEVELOPMENT: each chamber is the flag triangle (V = vertex, E = edge midpoint, F = face centre) — for a
  * p-gon face a right triangle with |VE| = 1/2, the right angle at E, angle π/p at F. Starting from one
  * placed chamber, BFS across σ₀/σ₁/σ₂ constructs the neighbour's triangle:
  *
  *   - σ₀ (other vertex, same edge+face): V reflected across line EF;
  *   - σ₁ (other edge, same vertex+face): E reflected across line VF;
  *   - σ₂ (other face, same vertex+edge): F re-placed on the far side of VE at the NEIGHBOUR's apothem (its
  *     face size may differ).
  *
  * Developing the QUOTIENT symbol unfolds the orbifold onto the plane, so v-values > 1 and chains (σ-fixed
  * chambers — crossing a mirror lands in the same chamber, mirrored) need no special handling; dedup is by
  * (chamber, rounded geometry). Pure doubles — no ℤ[ζ₁₂] restriction, so 4.8² renders like everything else.
  * Faces are reconstructed by grouping developed corners around each face centre and emitted as SVG.
  */
object SymbolRenderer:

  type Pt = (Double, Double)

  def apothem(p: Int): Double      = 0.5 / math.tan(math.Pi / p)
  def circumradius(p: Int): Double = 0.5 / math.sin(math.Pi / p)

  /** Reflection of `pt` across the line through `a` and `b`. */
  def reflect(pt: Pt, a: Pt, b: Pt): Pt =
    val (dx, dy) = (b._1 - a._1, b._2 - a._2)
    val len2     = dx * dx + dy * dy
    val (vx, vy) = (pt._1 - a._1, pt._2 - a._2)
    val t        = (vx * dx + vy * dy) / len2
    val (px, py) = (t * dx, t * dy) // v∥
    (a._1 + 2 * px - vx, a._2 + 2 * py - vy)

  final private case class Cham(c: Int, v: Pt, e: Pt, f: Pt)

  private def round6(x: Double): Long   = math.round(x * 1e6)
  private def key(pt: Pt): (Long, Long) = (round6(pt._1), round6(pt._2))

  /** Develop the symbol out to face centres within `radius` of the origin. Returns the COMPLETE faces:
    * `(p, corners sorted around the centre)`.
    */
  def develop(ds: DSymbol, radius: Double): Vector[(Int, Vector[Pt])] =
    val p1                = ds.m(0, 1, 1)
    val start             = Cham(1, (0.0, 0.0), (0.5, 0.0), (0.5, apothem(p1)))
    val seen              = mutable.Set.empty[(Int, (Long, Long), (Long, Long), (Long, Long))]
    val queue             = mutable.Queue(start)
    // face centre -> (p, corner set)
    val corners           =
      mutable.Map.empty[(Long, Long), (Int, mutable.Set[(Long, Long)], mutable.Map[(Long, Long), Pt])]
    def chamKey(ch: Cham) = (ch.c, key(ch.v), key(ch.e), key(ch.f))
    def dist(pt: Pt)      = math.hypot(pt._1, pt._2)
    while queue.nonEmpty do
      val ch = queue.dequeue()
      if !seen(chamKey(ch)) then
        seen += chamKey(ch)
        val p             = ds.m(0, 1, ch.c)
        val (_, cs, reps) =
          corners.getOrElseUpdate(key(ch.f), (p, mutable.Set.empty, mutable.Map.empty))
        cs += key(ch.v)
        reps.getOrElseUpdate(key(ch.v), ch.v)
        // σ₀: other vertex across the edge
        val n0            = Cham(ds.get(0, ch.c), reflect(ch.v, ch.e, ch.f), ch.e, ch.f)
        // σ₁: other edge at the vertex, same face
        val n1            = Cham(ds.get(1, ch.c), ch.v, reflect(ch.e, ch.v, ch.f), ch.f)
        // σ₂: other face across the edge — re-place F at the neighbour's apothem on the far side of VE
        val c2            = ds.get(2, ch.c)
        val a2            = apothem(ds.m(0, 1, c2))
        val aHere         = apothem(p)
        val f2            = (ch.e._1 - a2 / aHere * (ch.f._1 - ch.e._1), ch.e._2 - a2 / aHere * (ch.f._2 - ch.e._2))
        val n2            = Cham(c2, ch.v, ch.e, f2)
        for n <- List(n0, n1, n2) if dist(n.f) <= radius && !seen(chamKey(n)) do queue.enqueue(n)
    corners.toVector.collect:
      case (fk, (p, cs, reps)) if cs.size == p =>
        val centre = (fk._1 / 1e6, fk._2 / 1e6)
        val pts    = reps.values.toVector.sortBy(v => math.atan2(v._2 - centre._2, v._1 - centre._1))
        (p, pts)

  private val fillOf = Map(
    3  -> "#f4d35e",
    4  -> "#ee6c4d",
    6  -> "#7fb069",
    8  -> "#9b5de5",
    12 -> "#3d84a8"
  ).withDefaultValue("#cccccc")

  /** Render developed faces as a self-contained SVG with a banner line (types, level) baked in. The viewBox
    * is in tiling units; explicit pixel `width`/`height` (~80 px per unit edge) make viewers open it at a
    * sensible size instead of unit-scale (~13 px).
    */
  def toSvg(faces: Vector[(Int, Vector[Pt])], banner: String): String =
    val all              = faces.flatMap(_._2)
    val (xs, ys)         = (all.map(_._1), all.map(_._2))
    val (x0, y0, x1, y1) = (xs.min - 0.2, ys.min - 0.2, xs.max + 0.2, ys.max + 0.2)
    val bannerH          = (y1 - y0) * 0.06
    val pxPerUnit        = 80.0
    val (wPx, hPx)       = ((x1 - x0) * pxPerUnit, (y1 - y0 + bannerH) * pxPerUnit)
    val sb               = new StringBuilder
    sb ++=
      s"""<svg xmlns="http://www.w3.org/2000/svg" width="${wPx.round}" height="${hPx.round}" viewBox="$x0 ${-y1 -
          bannerH} ${x1 - x0} ${y1 - y0 + bannerH}">\n"""
    sb ++= s"""<text x="${x0 + 0.1}" y="${-y1 - bannerH * 0.25}" font-size="${bannerH *
        0.6}" font-family="monospace">$banner</text>\n"""
    for (p, pts) <- faces do
      val d = pts.map((x, y) => f"$x%.5f,${-y}%.5f").mkString(" ")
      sb ++= s"""<polygon points="$d" fill="${fillOf(p)}" stroke="#222" stroke-width="0.03"/>\n"""
    sb ++= "</svg>\n"
    sb.toString

  /** Develop + write one tiling's SVG. */
  def render(ds: DSymbol, path: java.nio.file.Path, banner: String, radius: Double = 6.0): Unit =
    java.nio.file.Files.createDirectories(path.getParent)
    java.nio.file.Files.writeString(path, toSvg(develop(ds, radius), banner))
