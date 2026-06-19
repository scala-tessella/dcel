package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.*
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}

import scala.collection.mutable

/** Exact-coordinate fixed-Λ torus engine — the ADR-0019 successor prototype (Option B).
  *
  * ADR-0019 profiled the DCEL fixed-Λ engine ([[KrotenheerdtLatticeSearch]]) as ~45 % canonical-congruence
  * key + ~37 % immutable-DCEL deep-copy per state, and argued both are irreducible *within that model*. This
  * engine removes both by changing the representation: a patch is a list of unit polygons whose corners are
  * exact integer [[ZetaPoint]]s in ℤ[ζ₁₂], grown directly on the fixed torus.
  *
  *   - No deep-copy: state is a cheap immutable `List[FaceZ]`; growth prepends a polygon built by exact
  *     integer ζ-steps.
  *   - No congruence key: Λ is fixed and oriented, so two growth orders that reach the same patch reach the
  *     *same integer point set*. They are deduped by an exact, cheap face-set key (sorted integer centroids)
  *     — not the expensive reflection-invariant congruence key the DCEL engine needs because its symmetric
  *     seed sweeps orientation. Off-lattice scatter never appears: every placement is gated by the
  *     Λ-consistency slot oracle (here on exact coordinates), which is also what recovers the geometric
  *     correctness the DCEL gave for free (coincident vertices = equal residue; "overlap" = a slot already
  *     owned).
  *
  * The completed-cell verification reuses the DCEL engine's proven tail verbatim
  * ([[KrotenheerdtLatticeSearch.verifyContent]]): primitive-lattice re-keying, the EXACT vertex-orbit count
  * (not 1-WL), and the up-to-all-isometries canonical key. So this engine is sound by the same argument; it
  * only changes how candidate cells are *generated*.
  *
  * STATUS: prototype (Option B of ADR-0019's "Performance" conclusion). Targets the pure `{3,4,6,12}` world;
  * the octagon's `4.8.8` (45° edges, ℤ[ζ₂₄]) is out of scope here exactly as it is past the DCEL engine's
  * current `k`. Validated against [[KrotenheerdtLatticeSearch]] on n = 1 (see `KrotenheerdtTorusSearchSpec`).
  */
object KrotenheerdtTorusSearch:

  /** Polygons of the 30°-edge world (octagon excluded — see class doc). */
  private val sides: List[Int] = List(3, 4, 6, 12)

  /** Interior angle of an `m`-gon in 30° slots (triangle 2, square 3, hexagon 4, dodecagon 5). */
  private val gSlots: Map[Int, Int] =
    sides.map(m => m -> (interiorAngle(m).toRational.toDouble / 30.0).toInt).toMap

  /** CCW exterior-turn between consecutive edges of an `m`-gon, in 30° slots: `δ = 6 − g`. */
  private def delta(m: Int): Int = 6 - gSlots(m)

  private val area: Map[Int, BigDecimal] =
    sides.map(m => m -> BigDecimal(m / (4.0 * math.tan(math.Pi / m)))).toMap

  private val slotOfUnit: Map[ZetaPoint, Int] = (0 until 12).map(s => ZetaPoint.unit(s) -> s).toMap

  /** The valid vertex types over `{3,4,6,12}`, each as a concrete cyclic order plus its mirror — the seeds
    * for the propagation engine. Seeding a whole vertex (a full corona) instead of one polygon constrains the
    * corona's outer vertices immediately, so propagation branches far less; a 1-uniform cell often verifies
    * at the seed itself (its corona's outer vertices are lattice translates of the centre).
    */
  private val seedTypes: List[List[Int]] =
    validSignatures.filterNot(_.contains(8)).flatMap(sig => List(sig, sig.reverse)).toList.distinct

  /** The full corona of a vertex type: each polygon placed at its cumulative 30°-slot around the origin. */
  private def coronaFaces(typeSizes: List[Int]): List[FaceZ] =
    var slot = 0
    typeSizes.map: m =>
      val f = FaceZ(m, polygon(ZetaPoint.origin, slot, m))
      slot += gSlots(m)
      f

  /** The CCW corners of a unit `m`-gon rooted at `p` whose first edge leaves `p` along 30°-slot `s`. */
  private def polygon(p: ZetaPoint, s: Int, m: Int): Vector[ZetaPoint] =
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

  final private case class FaceZ(size: Int, corners: Vector[ZetaPoint]):
    def centroid: BigPoint = corners.map(_.toBigPoint).toList.centroid

    /** Double centroid, cached — the per-state hot path groups faces by `tkeyD` of this, which needs only
      * ~1e-9 precision (Double is accurate to ~1e-14, so the grouping is identical to the BigDecimal one).
      * The exact `centroid` is used only at the verify horizon.
      */
    lazy val cD: (Double, Double) =
      var sx = 0.0
      var sy = 0.0
      corners.foreach: z =>
        val (x, y) = dxy(z)
        sx += x
        sy += y
      (sx / size, sy / size)

  final case class Outcome(
      tilings: List[(Set[VertexSignature], String)],
      basesTried: Int,
      statesExplored: Long
  )

  // ---- candidate lattices, as exact ℤ[ζ₁₂] integer bases -----------------------------------------------

  private def cross(a: BigPoint, b: BigPoint): BigDecimal = a.x * b.y - a.y * b.x

  private val minCovolume = 0.4
  private val sqrt3d      = math.sqrt(3.0)

  /** Fast Double embedding of a ZetaPoint into the plane (candidate enumeration only — the search/verify
    * recompute exactly at SCALE 9; ~1e-12 accuracy is ample for covolume filtering and basis dedup).
    */
  private def dxy(z: ZetaPoint): (Double, Double) =
    ((2 * z.a0 + z.a2 + z.a1 * sqrt3d) / 2.0, (2 * z.a3 + z.a1 + z.a2 * sqrt3d) / 2.0)

  /** Covolumes reachable as a non-negative integer combination of unit-polygon areas, as rounded keys for an
    * O(1) membership test in the candidate filter.
    */
  private def achievableCovolumeKeys(maxCovolume: Double): Set[Long] =
    val reached  = mutable.Set(0.0)
    var frontier = List(0.0)
    val areas    = sides.map(m => m / (4.0 * math.tan(math.Pi / m)))
    while frontier.nonEmpty do
      frontier =
        frontier.flatMap(c => areas.map(c + _).filter(s => s <= maxCovolume + 1e-6 && reached.add(s)))
    reached.filter(_ >= minCovolume - 1e-6).map(a => math.round(a * 1e6)).toSet

  /** Integer Lagrange–Gauss reduction tracking exact ZetaPoints; the multiplier is decided by the real
    * (Double) dot products (so the reduced basis is the two successive minima) but `b ← b − m·a` stays exact
    * integer.
    */
  private def gaussReduceZeta(v0: ZetaPoint, w0: ZetaPoint): (ZetaPoint, ZetaPoint) =
    def dot(p: ZetaPoint, q: ZetaPoint): Double =
      val (px, py) = dxy(p); val (qx, qy) = dxy(q); px * qx + py * qy
    var a                                       = v0
    var b                                       = w0
    if dot(a, a) > dot(b, b) then { val t = a; a = b; b = t }
    var iter                                    = 0
    while iter < 1000 && math.abs(dot(a, b)) * 2 > dot(a, a) + 1e-9 do
      val m = math.round(dot(a, b) / dot(a, a))
      b = ZetaPoint(b.a0 - m * a.a0, b.a1 - m * a.a1, b.a2 - m * a.a2, b.a3 - m * a.a3)
      if dot(a, a) > dot(b, b) then { val t = a; a = b; b = t }
      iter += 1
    (a, b)

  /** Candidate primitive bases as exact integer ℤ[ζ₁₂] pairs: module points within the L1 step budget
    * `Σ|aᵢ| ≤ k` with covolume in `[minCovolume, maxCovolume]` and achievable (an integer combination of
    * unit-polygon areas), Gauss-reduced, sign-canonicalised, deduped, smallest cell first. The whole
    * O(points²) filter runs in Double (the exact analogue of [[KrotenheerdtLatticeSearch.candidateBases]]);
    * orientation is swept by the candidate set's rotated copies, so it need not be rotation-closed.
    */
  def candidateBasesZeta(k: Int, maxCovolume: Double): List[(ZetaPoint, ZetaPoint)] =
    val achievable                                 = achievableCovolumeKeys(maxCovolume)
    def isAchievable(cov: Double): Boolean         =
      val r = math.round(cov * 1e6)
      achievable.contains(r) || achievable.contains(r - 1) || achievable.contains(r + 1)
    val points: Array[(ZetaPoint, Double, Double)] =
      (for
        a0 <- -k to k
        a1 <- -(k - a0.abs) to (k - a0.abs)
        a2 <- -(k - a0.abs - a1.abs) to (k - a0.abs - a1.abs)
        a3 <- -(k - a0.abs - a1.abs - a2.abs) to (k - a0.abs - a1.abs - a2.abs)
        z   = ZetaPoint(a0, a1, a2, a3)
        if !z.isOrigin
      yield { val (x, y) = dxy(z); (z, x, y) }).toArray
    val byKey                                      = mutable.HashMap.empty[((Long, Long), (Long, Long)), (ZetaPoint, ZetaPoint)]
    def snap(x: Double, y: Double): (Long, Long)   = (math.round(x * 1e6), math.round(y * 1e6))
    // Sign-canonicalise to the upper half-plane (a 180° flip is covered by polygon symmetry) and order the pair,
    // exactly as the DCEL `candidateBases` does, so the lattice — not its ± / swap variants — is the dedup unit.
    def canon(z: ZetaPoint): ZetaPoint             =
      val (x, y) = dxy(z)
      if y > 1e-9 || (math.abs(y) <= 1e-9 && x > 0) then z else -z
    var i                                          = 0
    while i < points.length do
      val (zi, xi, yi) = points(i)
      var j            = i + 1
      while j < points.length do
        val (zj, xj, yj) = points(j)
        val cov          = math.abs(xi * yj - yi * xj)
        if cov >= minCovolume && cov <= maxCovolume + 1e-9 && isAchievable(cov) then
          val (a0, b0)   = gaussReduceZeta(zi, zj)
          val (ca, cb)   = (canon(a0), canon(b0))
          val (cax, cay) = dxy(ca)
          val (cbx, cby) = dxy(cb)
          val (la, lb)   = (cax * cax + cay * cay, cbx * cbx + cby * cby)
          val (ka, kb)   = (snap(cax, cay), snap(cbx, cby))
          // shorter vector first; tie-break lexicographically by snapped coordinates
          val aFirst     =
            if math.abs(la - lb) > 1e-9 then la < lb
            else if ka._1 != kb._1 then ka._1 < kb._1
            else ka._2 <= kb._2
          val ordered    = if aFirst then (ca, cb) else (cb, ca)
          val (ox, oy)   = dxy(ordered._1)
          val (px, py)   = dxy(ordered._2)
          byKey.getOrElseUpdate((snap(ox, oy), snap(px, py)), ordered)
        j += 1
      i += 1
    byKey.values.toList.sortBy { (v, w) =>
      val (vx, vy) = dxy(v); val (wx, wy) = dxy(w); math.abs(vx * wy - vy * wx)
    }

  // ---- per-lattice exact flood fill -------------------------------------------------------------------

  /** Enumerate Krotenheerdt n-uniform tilings of the `{3,4,6,12}` world via exact-coordinate fixed-Λ growth.
    */
  def enumerate(
      n: Int,
      k: Int,
      maxCovolume: Double,
      parallelism: Int = 1,
      completion: Boolean = true,
      log: String => Unit = _ => ()
  ): Outcome =
    import java.util.concurrent.ConcurrentHashMap
    import java.util.concurrent.atomic.AtomicLong
    import scala.jdk.CollectionConverters.*
    val bases                                                          = candidateBasesZeta(k, maxCovolume)
    val found                                                          = new ConcurrentHashMap[String, Set[VertexSignature]]()
    val states                                                         = new AtomicLong(0)
    val done                                                           = new AtomicLong(0)
    val capped                                                         = new AtomicLong(0)
    val faceCap                                                        = sys.props.get("krot.facecap").map(_.toInt).getOrElse(64)
    val perCap                                                         = sys.props.get("krot.percap").map(_.toLong).getOrElse(100000L)
    // Vertex-completion constraint propagation (ADR-0020 next step) by default; one-polygon growth for
    // cross-checking. Both sound and complete; propagation explores far fewer states. Closes over n so the
    // soundness prune can drop a branch the moment it shows more than n distinct vertex types.
    val grower: (List[FaceZ], BigPoint, BigPoint) => List[List[FaceZ]] =
      if completion then (f, v, w) => growByCompletion(f, v, w, n) else (f, v, w) => grow(f, v, w, n)
    log(s"n=$n k=$k maxCovol=$maxCovolume: ${bases.size} candidate lattices")
    def runOne(vz: ZetaPoint, wz: ZetaPoint): Unit                     =
      val (count, wasCapped) =
        runLattice(
          n,
          vz,
          wz,
          faceCap,
          perCap,
          grower,
          completion,
          (t, key) => found.putIfAbsent(key, t): Unit
        )
      states.addAndGet(count)
      if wasCapped then capped.incrementAndGet()
      val d                  = done.incrementAndGet()
      if d % 200 == 0 then
        log(s"  lattices $d/${bases.size}, states=${states.get}, found=${found.size}, capped=${capped.get}")
    if parallelism <= 1 then bases.foreach((vz, wz) => runOne(vz, wz))
    else
      val pool = java.util.concurrent.Executors.newFixedThreadPool(parallelism)
      try bases.foreach((vz, wz) => pool.submit(new Runnable { def run(): Unit = runOne(vz, wz) }))
      finally
        pool.shutdown()
        pool.awaitTermination(7, java.util.concurrent.TimeUnit.DAYS)
    if capped.get > 0 then
      log(
        s"  WARNING: ${capped.get} lattice(s) hit the ${perCap}-state cap (krot.percap) — completeness caveat"
      )
    Outcome(found.asScala.toList.map((key, t) => (t, key)).sortBy(_._2), bases.size, states.get)

  /** Grow every Λ-consistent patch from each seed orientation; verify completed cells. Returns the state
    * count.
    */
  private def runLattice(
      n: Int,
      vz: ZetaPoint,
      wz: ZetaPoint,
      faceCap: Int,
      perCap: Long,
      grower: (List[FaceZ], BigPoint, BigPoint) => List[List[FaceZ]],
      coronaSeed: Boolean,
      emit: (Set[VertexSignature], String) => Unit
  ): (Long, Boolean) =
    val vB          = vz.toBigPoint
    val wB          = wz.toBigPoint
    val originB     = BigPoint.origin
    val covol       = cross(vB, wB).abs
    val autos       = latticeAutos(vz, wz)
    // Grow a branch only until its distinct content plus a thin verification corona is placed: an n-uniform
    // cell verifies once each of its ~n vertex orbits has a reconstructable fan (~n+2 cells of total area).
    // The generous old bound (×6) let high-covolume *spurious* (near-miss) lattices grow large before dying.
    val growthCells = sys.props.get("krot.growcells").map(_.toDouble).getOrElse((n + 2).toDouble)

    val visited = mutable.HashSet.empty[String]
    val stack   = mutable.Stack.empty[List[FaceZ]]
    var count   = 0L

    // Seeds at the fixed slot-0 orientation (orientation is swept by the candidate lattice set's rotated
    // copies, NOT by rotating the seed — seeding all 12 slots would double-count and explode the state count).
    // Propagation seeds a whole vertex (corona, both chiralities); one-polygon growth seeds a single polygon.
    val seeds =
      if coronaSeed then seedTypes.map(coronaFaces)
      else sides.map(m => List(FaceZ(m, polygon(ZetaPoint.origin, 0, m))))
    for seed <- seeds do if isConsistent(seed, vB, wB) then stack.push(seed)

    // Per-lattice state cap: a real cell resolves in few states, but a pathological near-miss lattice can
    // explore millions (each adding a key to `visited`), so an uncapped parallel run can exhaust memory and
    // crash the host. Abort such a lattice and report it (a capped lattice is a completeness caveat, like the
    // (k, maxCovolume) bound). Tunable via `krot.percap`.
    while stack.nonEmpty && count < perCap do
      val faces    = stack.pop()
      count += 1
      val distinct = distinctArea(faces, vB, wB)
      if distinct > covol + BigDecimal("1e-6") then () // off-lattice: distinct content exceeds one Λ-cell
      else
        val total       = faces.map(f => area(f.size)).sum
        // Classify at the patch's PRIMITIVE period (not Λ): emit if it is an n-uniform tiling, prune if it is a
        // finished sub-tiling of the wrong count, grow otherwise. classify runs the O(faces²) `primitiveBasis`,
        // so gate it cheaply, firing it only when there is reason to (else it is per-state overhead):
        //   • distinct content already fills the Λ-cell (a real primitive cell is ready), or
        //   • one polygon size dominates (≥8) — a 1-type sublattice *field*, or
        //   • two same-type completed vertices sit closer than the Λ basis — a *multi-type* sublattice (a real
        //     small cell repeated), which the field test misses and which is the dominant high-covolume cost at
        //     n ≥ 3. Both sublattice signals let classify emit/prune at the small primitive period instead of
        //     rebuilding the whole coarse Λ-cell.
        val likelyField = faces.groupBy(_.size).valuesIterator.exists(_.sizeIs >= 8)
        val verdict     =
          if distinct >= covol - BigDecimal("1e-6") || likelyField || hasShortSubPeriod(faces, vB, wB) then
            classify(faces, vB, wB, originB, n)
          else Verdict.Grow
        verdict match
          case Verdict.Emit(t, key) => emit(t, key)
          case Verdict.Prune        => ()
          case Verdict.Grow         =>
            if faces.sizeIs < faceCap && total < covol * growthCells then
              grower(faces, vB, wB).foreach: child =>
                if visited.add(canonicalKey(child, autos)) then stack.push(child)
    (count, stack.nonEmpty) // capped iff work remained when the cap was hit

  /** The 24 isometries of the module ℤ[ζ₁₂] (dihedral group of order 24): rotations `ζ^k` and their
    * reflections (conjugation `ζ→ζ⁻¹` then rotation), as exact integer maps on ZetaPoints.
    */
  private val groupMaps: List[ZetaPoint => ZetaPoint] =
    for
      reflect <- List(false, true)
      k       <- (0 until 12).toList
    yield (z: ZetaPoint) =>
      var p = if reflect then z.conjugate else z
      var i = 0
      while i < k do { p = p.timesZeta; i += 1 }
      p

  /** The point group of Λ = (v, w): the module isometries that map Λ to itself (`g(v), g(w) ∈ Λ`, tested by
    * exact integer congruence). These — and translations — are the only congruences that preserve Λ's growth
    * continuations, so canonicalising over them is SOUND (it never merges patches with different futures),
    * unlike canonicalising over the full module group. Always contains the identity.
    */
  private def latticeAutos(vz: ZetaPoint, wz: ZetaPoint): List[ZetaPoint => ZetaPoint] =
    groupMaps.filter(g =>
      g(vz).congruentMod(ZetaPoint.origin, vz, wz) && g(wz).congruentMod(ZetaPoint.origin, vz, wz)
    )

  /** Exact dedup key invariant under Λ's point group + translation — the trig-free replacement for the DCEL
    * engine's expensive `congruenceKey` (ADR-0019 §Performance: 45 %, irreducible). For each lattice
    * automorphism `g`, transform the corners, translation-anchor on the lexicographically minimal corner,
    * sort faces and corners, and take the min serialization over all `g`. Collapses the symmetric-image
    * duplicates that an absolute key leaves un-merged (the ~2.5× state inflation), at the cost of
    * small-integer work only.
    */
  private def canonicalKey(faces: List[FaceZ], autos: List[ZetaPoint => ZetaPoint]): String =
    autos.iterator.map: g =>
      val tf     = faces.map(f => (f.size, f.corners.map(g)))
      val anchor = tf.iterator.flatMap(_._2).min
      tf.map((s, cs) =>
        s"$s:" + cs.map(_ - anchor).sorted.iterator.map(z => s"${z.a0},${z.a1},${z.a2},${z.a3}").mkString(";")
      )
        .sorted
        .mkString("|")
    .min

  // ---- torus bookkeeping (shared conventions with verifyTorus) ----------------------------------------

  private def frac9(x: BigDecimal): BigDecimal =
    val r = x.setScale(9, BigDecimal.RoundingMode.HALF_UP)
    val f = r - r.setScale(0, BigDecimal.RoundingMode.FLOOR)
    if f >= BigDecimal(1) then BigDecimal(0).setScale(9) else f.setScale(9, BigDecimal.RoundingMode.HALF_UP)

  /** Torus-vertex key: position mod Λ, snapped to 1e5 with wrapping (the convention `verifyTorus.tkey` uses,
    * folding the ≈0/≈1 boundary so cell-corner instances group together).
    */
  private def tkey(p: BigPoint, vB: BigPoint, wB: BigPoint, originB: BigPoint): (Long, Long) =
    val det                       = vB.x * wB.y - vB.y * wB.x
    val d                         = p - originB
    def snap(t: BigDecimal): Long =
      val r = (t * 100000).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
      (((r % 100000) + 100000) % 100000).toLong
    (snap((d.x * wB.y - wB.x * d.y) / det), snap((vB.x * d.y - d.x * vB.y) / det))

  /** Double residue key (origin at 0) — the Double twin of [[tkey]] for the per-state hot path. Same 1e5
    * wrapping snap; the coordinates are O(k)·√3-irrational, so Double's ~1e-14 accuracy snaps identically to
    * the BigDecimal version for every real vertex (exact half-way snap points are measure-zero).
    */
  private def tkeyD(
      x: Double,
      y: Double,
      vx: Double,
      vy: Double,
      wx: Double,
      wy: Double,
      det: Double
  ): (Long, Long) =
    def snap(t: Double): Long =
      val r = math.round(t * 100000.0)
      ((r % 100000) + 100000) % 100000
    (snap((x * wy - wx * y) / det), snap((vx * y - x * vy) / det))

  /** Distinct-torus-face area: each face counted once per `(size, centroid mod Λ)`. Equals one covolume when
    * a full cell of distinct faces is covered. Grouping is in Double ([[tkeyD]]); the summed areas stay
    * exact.
    */
  private def distinctArea(faces: List[FaceZ], vB: BigPoint, wB: BigPoint): BigDecimal =
    val vx  = vB.x.toDouble; val vy = vB.y.toDouble; val wx = wB.x.toDouble; val wy = wB.y.toDouble
    val det = vx * wy - vy * wx
    faces
      .map(f => (f.size, tkeyD(f.cD._1, f.cD._2, vx, vy, wx, wy, det)))
      .distinct
      .map((m, _) => area(m))
      .sum

  /** The outgoing 30°-slot of face `f` at corner `p` (the edge `p → next` in CCW order). */
  private def outSlot(f: FaceZ, p: ZetaPoint): Int =
    val i = f.corners.indexOf(p)
    slotOfUnit(f.corners((i + 1) % f.size) - p)

  /** Λ-consistency: every torus vertex's incident faces occupy a conflict-free set of 30° slots. Residues are
    * grouped in Double ([[tkeyD]]); the slot bookkeeping is exact integer.
    */
  private def isConsistent(faces: List[FaceZ], vB: BigPoint, wB: BigPoint): Boolean =
    val vx       = vB.x.toDouble; val vy = vB.y.toDouble; val wx = wB.x.toDouble; val wy = wB.y.toDouble
    val det      = vx * wy - vy * wx
    val coverage = mutable.Map.empty[(Long, Long), mutable.Map[Int, (Int, Int)]]
    faces.forall: f =>
      f.corners.forall: p =>
        val (px, py) = dxy(p)
        val slotMap  = coverage.getOrElseUpdate(tkeyD(px, py, vx, vy, wx, wy, det), mutable.Map.empty)
        val start    = outSlot(f, p)
        (0 until gSlots(f.size)).forall: kk =>
          val slot = (start + kk) % 12
          slotMap.get(slot) match
            case Some(owner) if owner != ((start, f.size)) => false
            case _                                         => slotMap(slot) = (start, f.size); true

  // ---- growth -----------------------------------------------------------------------------------------

  /** Planar fan at vertex `p`: incident faces' `(startSlot, size)`, by ascending start slot. */
  private def planarFan(faces: List[FaceZ], p: ZetaPoint): List[(Int, Int)] =
    faces.filter(_.corners.contains(p)).map(f => (outSlot(f, p), f.size)).sortBy(_._1)

  private def coveredSlots(fan: List[(Int, Int)]): Set[Int] =
    fan.flatMap((start, m) => (0 until gSlots(m)).map(kk => (start + kk) % 12)).toSet

  /** Push every Λ-consistent, sound continuation that fills the centroid-nearest boundary gap (mirrors the
    * DCEL engine's deterministic single-vertex growth; completeness rests on the same argument).
    */
  private def grow(faces: List[FaceZ], vB: BigPoint, wB: BigPoint, n: Int): List[List[FaceZ]] =
    val verts    = faces.flatMap(_.corners).distinct
    val centroid = verts.map(_.toBigPoint).centroid
    val boundary = verts.flatMap: p =>
      val covered = coveredSlots(planarFan(faces, p))
      Option.when(covered.sizeIs < 12)((p, covered))
    boundary.minByOption((p, covered) => (p.toBigPoint.distanceTo(centroid), 12 - covered.size, p.hashCode))
      .toList
      .flatMap: (p, covered) =>
        // CCW boundary edge: first free slot whose predecessor is owned.
        val b = (0 until 12).find(s => covered((s + 11) % 12) && !covered(s)).getOrElse(0)
        sides.flatMap: m =>
          val claim = (0 until gSlots(m)).map(kk => (b + kk) % 12)
          if claim.exists(covered) then None
          else
            val next = FaceZ(m, polygon(p, b, m)) :: faces
            Option.when(isConsistent(next, vB, wB) && isSound(next, n))(next)

  /** A cheap heuristic that the patch already repeats with a period finer than Λ: two *completed* vertices of
    * the same type sit closer than the shortest Λ basis vector (so their difference is a sub-period vector).
    * It only *gates* the sound `classify`, so a false positive merely costs one `classify` call; a primitive
    * cell's same-type vertices are ≥ a basis vector apart, so it does not fire there.
    */
  private def hasShortSubPeriod(faces: List[FaceZ], vB: BigPoint, wB: BigPoint): Boolean =
    val minBasis2 = math.min(vB.dot(vB).toDouble, wB.dot(wB).toDouble)
    val seen      = mutable.Map.empty[VertexSignature, mutable.ListBuffer[ZetaPoint]]
    faces.flatMap(f => f.corners.map(p => (p, f))).groupBy(_._1).exists: (p, incident) =>
      val fan = incident.map((_, f) => (outSlot(f, p), f.size)).sortBy(_._1)
      coveredSlots(fan).sizeIs == 12 && {
        val t   = VertexTypes.normalize(fan.map(_._2))
        val buf = seen.getOrElseUpdate(t, mutable.ListBuffer.empty)
        val hit = buf.exists: q =>
          val d        = p - q
          val (dx, dy) = dxy(d)
          !d.isOrigin && dx * dx + dy * dy < minBasis2 - 1e-9
        buf += p
        hit
      }

  /** Sound iff every planar vertex's fan is a valid completed vertex or an extendable partial fan, AND the
    * patch's *completed* vertices show at most `n` distinct types — an n-uniform tiling can never contain
    * more, so this drops a spurious branch the moment an (n+1)-th type closes, long before the verify horizon
    * (the main prune that keeps near-miss high-covolume lattices from growing large).
    */
  private def isSound(faces: List[FaceZ], n: Int): Boolean =
    val completeTypes = mutable.Set.empty[VertexSignature]
    faces.flatMap(_.corners).distinct.forall: p =>
      val fan     = planarFan(faces, p)
      val covered = coveredSlots(fan)
      if covered.sizeIs == 12 then
        isCompleteVertex(fan.map(_._2)) && {
          completeTypes += VertexTypes.normalize(fan.map(_._2)); completeTypes.sizeIs <= n
        }
      else isExtendableFan(fan.map(_._2))

  /** Every way to fill a vertex's remaining `gap` (in 30° slots) so that `fan ++ completion`, read CCW, is a
    * valid complete vertex type. The bounded recursion prunes through the partial-fan table at each step, so
    * dead and forced (single-completion) vertices are recognised immediately — the basis of the MRV ordering.
    */
  private def completions(fan: List[Int], gap: Int): List[List[Int]] =
    if gap == 0 then if isCompleteVertex(fan) then List(Nil) else Nil
    else
      sides.flatMap: m =>
        val g = gSlots(m)
        if g > gap then Nil
        else
          val extended = fan :+ m
          val ok       = if g == gap then isCompleteVertex(extended) else isExtendableFan(extended)
          if !ok then Nil else completions(extended, gap - g).map(m :: _)

  /** Constraint-propagation growth: commit the WHOLE most-constrained (fewest-completions, MRV) incomplete
    * vertex at once, branching only over its valid completions, instead of adding one polygon at a time. A
    * vertex with a single completion is committed deterministically (no branch); one with none kills the
    * branch. Completeness holds by the same argument as one-polygon growth — the chosen vertex must be
    * completed by one of its valid vertex types — but the search tree is far smaller (most of a cell is
    * forced, not branched). The ADR-0020 next step.
    */
  private def growByCompletion(faces: List[FaceZ], vB: BigPoint, wB: BigPoint, n: Int): List[List[FaceZ]] =
    val incomplete = faces.flatMap(_.corners).distinct.flatMap: p =>
      val fan     = planarFan(faces, p)
      val covered = coveredSlots(fan)
      Option.when(covered.sizeIs < 12):
        val free     = 12 - covered.size
        val b        = (0 until 12).find(s => covered((s + 11) % 12) && !covered(s)).getOrElse(0)
        val arcStart = (b + free) % 12
        val ordered  = fan.sortBy((start, _) => (start - arcStart + 12) % 12).map(_._2)
        (p, b, completions(ordered, free))
    if incomplete.isEmpty then Nil
    else
      // MRV: the vertex with the fewest completions, tie-broken canonically by position for determinism.
      val (p, b, comps) = incomplete.minBy((p, _, cs) => (cs.size, p.a0, p.a1, p.a2, p.a3))
      comps.flatMap: comp =>
        var slot     = b
        val newFaces = comp.map: m =>
          val f = FaceZ(m, polygon(p, slot, m))
          slot += gSlots(m)
          f
        val next     = newFaces ++ faces
        Option.when(isConsistent(next, vB, wB) && isSound(next, n))(next)

  // ---- verification (reuses the DCEL engine's proven tail) --------------------------------------------

  /** Outcome of classifying a grown patch against its own primitive period. */
  private enum Verdict:
    case Emit(types: Set[VertexSignature], key: String)
    case Prune
    case Grow

  /** Reconstruct each torus vertex's full fan (mod the basis aB, bB) by unioning the incident corners of all
    * its planar instances, keyed by angle — exactly `verifyTorus`'s union reconstruction, on ZetaPoints.
    */
  private def reconstructFans(
      faces: List[FaceZ],
      aB: BigPoint,
      bB: BigPoint,
      originB: BigPoint
  ): (Map[(Long, Long), List[((Long, Long), ZetaPoint, FaceZ)]], Map[(Long, Long), Map[Long, Int]]) =
    val byTorus = faces
      .flatMap(f => f.corners.map(p => (tkey(p.toBigPoint, aB, bB, originB), p, f)))
      .groupBy(_._1)
    val fans    = byTorus.view.mapValues: incident =>
      incident.map: (_, p, f) =>
        val c = f.centroid
        (
          math.round(math.atan2((c.y - p.toBigPoint.y).toDouble, (c.x - p.toBigPoint.x).toDouble) * 10000),
          f.size
        )
      .toMap
    .toMap
    (byTorus, fans)

  private def fanComplete(fan: Map[Long, Int]): Boolean =
    fan.values.map(interiorAngle).foldLeft(AngleDegree(0))(_ + _) == AngleDegree(360)

  /** Classify a grown patch **at its own primitive period**, not at the candidate Λ. This is the lever that
    * tames the high-covolume cost: a near-miss / sublattice patch (e.g. a 3⁶ field grown under a coarse Λ) is
    * recognised as a *complete tiling under a finer period* and pruned — instead of growing to `faceCap`
    * because its distinct content never fills the coarse Λ-cell. Soundness rests on requiring **every** torus
    * fan to be complete under that primitive period: a half-built big cell has incomplete boundary fans and
    * so returns `Grow` (never pruned), while a genuinely finished sub-tiling has all fans complete and is
    * either emitted (if n-uniform — it is also reachable at its primitive-lattice candidate, so dedup handles
    * it) or pruned (wrong type/orbit count). The primitive period is read from the face content via the
    * proven `primitiveBasis` (empty verts), so chirality/sublattice keying stays identical to the DCEL
    * engine.
    */
  private def classify(faces: List[FaceZ], vB: BigPoint, wB: BigPoint, originB: BigPoint, n: Int): Verdict =
    val distinctFaces = faces
      .map(f => (f.size, f.centroid))
      .distinctBy((s, c) => (s, tkey(c, vB, wB, originB)))
    val (pv, pw)      = KrotenheerdtLatticeSearch.primitiveBasis(vB, wB, originB, distinctFaces, Nil)
    val pcov          = cross(pv, pw).abs
    if distinctArea(faces, pv, pw) < pcov - BigDecimal("1e-6") then Verdict.Grow
    else
      val (byTorus, fans) = reconstructFans(faces, pv, pw, originB)
      if fans.exists((_, f) => !fanComplete(f)) then Verdict.Grow
      else
        val sigs  = fans.view.mapValues(f => VertexTypes.normalize(f.toList.sortBy(_._1).map(_._2))).toMap
        val types = sigs.values.toSet
        if types.sizeIs != n then Verdict.Prune
        else
          val faceList =
            faces.map(f => (f.size, f.centroid)).distinctBy((s, c) => (s, tkey(c, pv, pw, originB)))
          val verts    = sigs.toList.map((tk, sig) => (sig.mkString("."), byTorus(tk).head._2.toBigPoint))
          KrotenheerdtLatticeSearch.verifyContent(pv, pw, originB, faceList, verts, types, n) match
            case Some((t, key)) => Verdict.Emit(t, key)
            case None           => Verdict.Prune
