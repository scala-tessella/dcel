package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.KrotenheerdtTorusMapSearch.FaceZ
import io.github.scala_tessella.dcel.VertexTypes.{isCompleteVertex, isExtendableFan}

/** The GENUINE strip-stacking abstraction for the banded family (ADR-0037).
  *
  * A *band* is one layer of unit regular `{3,4,6,12}`-gons between two **profiles**. A *profile* is a
  * periodic polyline of unit edges (any 30°-multiple turns — straight runs, 120°/60° zig-zags) together with,
  * at every vertex on it, the partial vertex-fan the owning band consumes on its side. A profile vertex is
  * described by its **band-side arc** in degrees: a straight vertex opens 180°, a 120°/60° zig-zag opens 120°
  * or 240°.
  *
  * Bands are built by **filling above a bottom polyline** ([[fillAbove]]): at each polyline vertex the band
  * fills the open arc on its side with a fan of polygon corners; the polygons' far corners form the **top
  * profile**. A band is therefore a two-sided object — e.g. the 1-hexagon-3-triangle band is straight on one
  * boundary and a `120,120,240,240` zig-zag on the other. Two bands stack iff the lower band's top profile
  * equals the upper band's bottom profile as polylines AND at each shared vertex the two band-side fans sum
  * to a valid 360° vertex (the matching graph — the next ADR-0037 step — closes cyclic stacks into torus
  * cells keyed via [[KrotenheerdtTorusMapSearch.verifyCell]]). This file is the band layer + its catalogue
  * only.
  *
  * Geometry is exact ℤ[ζ₁₂] ([[ZetaPoint]]); faces are [[KrotenheerdtTorusMapSearch.FaceZ]] so a closed stack
  * can be handed straight to `verifyCell`.
  */
object StripBand:

  /** The polygons of the 30°-edge world (octagon excluded — its 45° edges need ℤ[ζ₂₄], out of scope here). */
  private val sides: List[Int] = List(3, 4, 6, 12)

  /** Interior angle of an `m`-gon in 30° slots: triangle 2, square 3, hexagon 4, dodecagon 5. */
  val gSlots: Map[Int, Int] = Map(3 -> 2, 4 -> 3, 6 -> 4, 12 -> 5)

  /** CCW exterior-turn between consecutive edges of an `m`-gon, in 30° slots: `δ = 6 − g`. */
  private def delta(m: Int): Int = 6 - gSlots(m)

  private val slotOfUnit: Map[ZetaPoint, Int] = (0 until 12).map(s => ZetaPoint.unit(s) -> s).toMap

  /** The CCW corners of a unit `m`-gon rooted at `p` whose first edge leaves `p` along 30°-slot `s` — so the
    * polygon lies to the LEFT of the directed edge `p → p+step(s)` (above it, for a rightward edge).
    */
  def polygon(p: ZetaPoint, s: Int, m: Int): Vector[ZetaPoint] =
    val buf = Vector.newBuilder[ZetaPoint]
    var cur = p
    var dir = s
    var i   = 0
    while i < m do
      buf += cur
      cur = cur + ZetaPoint.step(dir)
      dir += delta(m)
      i += 1
    buf.result()

  /** A horizontally-periodic single layer of polygons — a band.
    *
    *   - `period` — the horizontal translation period (the in-band glide), a ℤ[ζ₁₂] vector.
    *   - `bottomEdges` — the bottom profile's polyline, one period of unit-edge 30°-slots, left to right.
    *   - `bottomFans` — the band-side (above) fan at each bottom vertex, CCW polygon sizes.
    *   - `faces` — the band's polygons over one period.
    */
  final case class Band(
      period: ZetaPoint,
      bottomEdges: Vector[Int],
      bottomFans: Vector[List[Int]],
      faces: List[FaceZ]
  ):
    /** The band's polygon multiset (one period), as a sorted size list. */
    def faceSizes: List[Int] = faces.map(_.size).sorted

    /** Width of the horizontal period (in unit edges). */
    def widthX: Double = period.toBigPoint.x.toDouble

    /** Vertices of one period of the bottom polyline (left to right), `v(0) = origin`. */
    def bottomVertices: Vector[ZetaPoint] =
      bottomEdges.scanLeft(ZetaPoint.origin)((p, s) => p + ZetaPoint.step(s)).init

    /** The band-side arc (in degrees) the bottom band-side fan fills at each bottom vertex. */
    def bottomArcs: Vector[Int] = bottomFans.map(_.map(gSlots).sum * 30)

    /** The reconstructed top-profile vertices with their band-side (below) fans — the polygons' upper
      * corners, read from the developed faces (so it is the genuine other boundary of the layer).
      */
    def topVertexFans: List[(ZetaPoint, List[Int])] =
      val bottomSet = bottomVertices.toSet
      windowVertexFans(this).collect:
        case (p, fan) if !bottomSet.contains(p) => (p, fan.sortBy(_._1).map(_._2))
      .toList.sortBy(p => p._1.toBigPoint.x.toDouble)

    /** The band-side arc (in degrees) the band fills below each top vertex. */
    def topArcs: List[Int] = topVertexFans.map(_._2.map(gSlots).sum * 30)

    /** A flip-invariant band-type signature: polygon multiset + the unordered pair of boundary arc-multisets
      * (vertically flipping a band swaps its two profiles, so the pair is unordered).
      */
    def typeKey: (List[Int], Set[List[Int]]) =
      (faceSizes, Set(bottomArcs.sorted.toList, topArcs.sorted))

    /** A compact human label for the catalogue. */
    def label: String =
      val poly = faceSizes.groupBy(identity).toList.sortBy(_._1).map((m, g) => s"$m^${g.size}").mkString(".")
      s"$poly  bottom=${bottomArcs.sorted.mkString("/")}  top=${topArcs.sorted.mkString("/")}"

  // ---- filling above a profile -------------------------------------------------------------------------

  /** The open arc the band fills above bottom vertex `i`: it starts at the outgoing edge slot and runs CCW to
    * the reversed incoming edge. `(start, width)` in 30° slots; width in `1..11` (a straight vertex gives 6,
    * a 120°/60° zig-zag gives 4/8, …). Width 0 (a U-turn) is degenerate and never a band vertex.
    */
  private def openArc(edges: Vector[Int], i: Int): (Int, Int) =
    val L     = edges.length
    val sOut  = edges(i)
    val sIn   = edges((i - 1 + L) % L)
    val width = (((sIn + 6 - sOut) % 12) + 12) % 12
    (sOut, width)

  /** Every fan of `{3,4,6,12}`-gon corners that exactly tiles an arc of `width` 30°-slots and is a valid
    * extendable partial vertex fan (so SOME band can complete it on the other side), read CCW from the arc
    * start. The fan's first element sits on the outgoing edge, its last on the incoming edge.
    */
  def fanOptions(width: Int): List[List[Int]] =
    def rec(remaining: Int, acc: List[Int]): List[List[Int]] =
      if remaining == 0 then
        val fan = acc.reverse
        if isExtendableFan(fan) || isCompleteVertex(fan) then List(fan) else Nil
      else
        sides.flatMap: m =>
          val g = gSlots(m)
          if g > remaining then Nil
          else
            val pref = (m :: acc).reverse
            val ok   =
              if remaining - g == 0 then isExtendableFan(pref) || isCompleteVertex(pref)
              else isExtendableFan(pref)
            if ok then rec(remaining - g, m :: acc) else Nil
    rec(width, Nil)

  /** All bands whose bottom profile is the polyline `edges`: choose, per vertex, a fan that tiles its open
    * arc such that adjacent vertices agree on the polygon sitting on the shared edge (the cyclic edge-match
    * `fan(i).head == fan(i+1).last`), then develop the faces. Returns the geometrically-consistent ones.
    */
  def fillAbove(edges: Vector[Int]): List[Band] =
    val L = edges.length
    if L == 0 then Nil
    else
      val period    = edges.foldLeft(ZetaPoint.origin)((p, s) => p + ZetaPoint.step(s))
      val verts     = edges.scanLeft(ZetaPoint.origin)((p, s) => p + ZetaPoint.step(s)).init
      val arcs      = (0 until L).map(openArc(edges, _)).toVector
      val perVertex = arcs.map((_, w) => fanOptions(w))
      if perVertex.exists(_.isEmpty) then Nil
      else
        def choose(i: Int, chosen: Vector[List[Int]]): List[Vector[List[Int]]] =
          if i == L then
            if chosen(L - 1).head == chosen(0).last then List(chosen) else Nil
          else
            perVertex(i).flatMap: fan =>
              if i == 0 || chosen(i - 1).head == fan.last then choose(i + 1, chosen :+ fan) else Nil
        choose(0, Vector.empty).flatMap: fans =>
          val faces = (0 until L).toList.flatMap: i =>
            // drop the LAST fan element (the incoming-edge polygon, owned by the previous vertex as its first)
            fan2faces(verts(i), arcs(i)._1, fans(i).dropRight(1))
          val band  = Band(period, edges, fans.toVector, faces)
          Option.when(bandValid(band))(band)

  /** Develop `sizes` as a contiguous fan rooted at `p`, the first polygon's first edge along `startSlot`. */
  private def fan2faces(p: ZetaPoint, startSlot: Int, sizes: List[Int]): List[FaceZ] =
    var slot = startSlot
    sizes.map: m =>
      val f = FaceZ(m, polygon(p, slot, m))
      slot += gSlots(m)
      f

  // ---- planar reconstruction & validity ----------------------------------------------------------------

  /** The outgoing 30°-slot of face `f` at corner `p`. */
  private def outSlot(f: FaceZ, p: ZetaPoint): Int =
    val i = f.corners.indexOf(p)
    slotOfUnit(f.corners((i + 1) % f.size) - p)

  /** Per-vertex incident fans for the vertices in one period window `x ∈ [0, widthX)`, reconstructed from the
    * faces developed over `±1` period (so a period-boundary vertex gets its full fan). Each fan is the
    * incident `(outSlot, size)` pairs sorted by slot.
    */
  private def windowVertexFans(band: Band): Map[ZetaPoint, List[(Int, Int)]] =
    val w   = band.widthX
    val rep = (-1 to 1).flatMap: k =>
      val shift = mul(band.period, k)
      band.faces.map(f => FaceZ(f.size, f.corners.map(_ + shift)))
    rep
      .flatMap(f => f.corners.map(p => (p, f)))
      .groupBy(_._1)
      .filter { (p, _) =>
        val x = p.toBigPoint.x.toDouble; x > -1e-6 && x < w - 1e-6
      }
      .map((p, incident) => p -> incident.map((_, f) => (outSlot(f, p), f.size)).sortBy(_._1).toList)

  /** A band is geometrically a valid single layer iff: its developed polygons are edge-to-edge with no
    * overlap (at every vertex the incident faces occupy a conflict-free set of 30°-slots), AND no vertex is
    * fully surrounded by band faces alone (a complete vertex would mean the layer is ≥ 2 polygons thick there
    * — a real single band's vertices are all on its two open profiles).
    */
  def bandValid(band: Band): Boolean =
    val w         = band.widthX
    val rep       = (-1 to 1).flatMap: k =>
      val shift = mul(band.period, k)
      band.faces.map(f => FaceZ(f.size, f.corners.map(_ + shift)))
    val coverage  = scala.collection.mutable.Map.empty[ZetaPoint, scala.collection.mutable.Map[Int, Int]]
    val noOverlap =
      rep.forall: f =>
        f.corners.forall: p =>
          val owned = coverage.getOrElseUpdate(p, scala.collection.mutable.Map.empty)
          val start = outSlot(f, p)
          (0 until gSlots(f.size)).forall: kk =>
            val slot = (start + kk) % 12
            owned.get(slot) match
              case Some(s) if s != start => false
              case _                     => owned(slot) = start; true
    noOverlap &&
    // no fully-surrounded vertex in the fundamental window
    coverage.forall: (p, owned) =>
      val x = p.toBigPoint.x.toDouble
      !(x > -1e-6 && x < w - 1e-6) || owned.size < 12

  private def mul(p: ZetaPoint, k: Int): ZetaPoint =
    ZetaPoint(p.a0 * k, p.a1 * k, p.a2 * k, p.a3 * k)

  // ---- the catalogue -----------------------------------------------------------------------------------

  /** The bottom-polyline shapes to try: periodic unit-edge sequences over gentle rightward turns whose period
    * is purely horizontal (so the band stacks), `1..maxLen` edges, deduplicated up to cyclic rotation and
    * kept only if PRIMITIVE (not a repetition of a shorter polyline — the period-2 "0,0" triangle row is the
    * same band as "0").
    */
  private def candidatePolylines(maxLen: Int): List[Vector[Int]] =
    val alphabet                               = List(0, 1, 2, 10, 11)
    val seen                                   = scala.collection.mutable.HashSet.empty[Vector[Int]]
    val out                                    = scala.collection.mutable.ListBuffer.empty[Vector[Int]]
    def rots(v: Vector[Int]): Set[Vector[Int]] = (0 until v.length).map(i => v.drop(i) ++ v.take(i)).toSet
    def primitive(v: Vector[Int]): Boolean     = (1 until v.length).filter(v.length % _ == 0).forall(d =>
      v.grouped(d).toSet.sizeIs > 1
    )
    for
      len <- 1 to maxLen
      seq <- cartesian(alphabet, len)
    do
      val v        = seq.toVector
      val period   = v.foldLeft(ZetaPoint.origin)((p, s) => p + ZetaPoint.step(s))
      val (px, py) = (period.toBigPoint.x.toDouble, period.toBigPoint.y.toDouble)
      if math.abs(py) < 1e-9 && px > 1e-9 && primitive(v) && rots(v).forall(r => !seen.contains(r)) then
        seen ++= rots(v)
        out += v
    out.toList

  private def cartesian(alphabet: List[Int], len: Int): List[List[Int]] =
    if len == 0 then List(Nil)
    else for h <- alphabet; t <- cartesian(alphabet, len - 1) yield h :: t

  /** ALL bands over the candidate polylines, UNdeduplicated — every fill of every polyline. Used as the
    * profile-automaton SEED source: the deduped [[catalogue]] keeps only the smallest band per type and so
    * discards the longer-period / phase-variant band tops that are cuts of the harder banded cells.
    */
  def allBands(maxLen: Int = 4): List[Band] =
    candidatePolylines(maxLen).flatMap(fillAbove)

  /** The band-type catalogue: every distinct band over the candidate polylines, deduplicated by
    * flip-invariant band type ([[Band.typeKey]]) so the two profile-views of one band collapse, smallest
    * first. The genuine answer-blind enumeration of the banded layers — square rows, triangle rows, hexagon
    * zig-zag rows, 3.6.3.6 straight hex+triangle rows, the `120,120,240,240` hexagon and hex+triangle bands,
    * and any others the fan algebra admits.
    */
  def catalogue(maxLen: Int = 4): List[Band] =
    import scala.math.Ordering.Implicits.seqOrdering
    candidatePolylines(maxLen)
      .flatMap(fillAbove)
      .groupBy(_.typeKey)
      .values
      .map(_.minBy(b => (b.faces.size, b.bottomEdges.length)))
      .toList
      .sortBy(b => (b.faceSizes.sum, b.faceSizes, b.bottomEdges.length))
