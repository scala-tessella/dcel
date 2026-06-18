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

  final case class Outcome(
      tilings: List[(Set[VertexSignature], String)],
      basesTried: Int,
      statesExplored: Long
  )

  // ---- candidate lattices, as exact ℤ[ζ₁₂] integer bases -----------------------------------------------

  private def cross(a: BigPoint, b: BigPoint): BigDecimal = a.x * b.y - a.y * b.x

  private val minCovolume = BigDecimal("0.4")

  private def achievableCovolumes(maxCovolume: Double): Set[Double] =
    val reached  = mutable.Set(0.0)
    var frontier = List(0.0)
    val areas    = sides.map(m => m / (4.0 * math.tan(math.Pi / m)))
    while frontier.nonEmpty do
      frontier =
        frontier.flatMap(c => areas.map(c + _).filter(s => s <= maxCovolume + 1e-6 && reached.add(s)))
    reached.filter(_ >= minCovolume.toDouble - 1e-6).toSet

  /** Integer Lagrange–Gauss reduction tracking exact ZetaPoints; the multiplier is decided by the real dot
    * products (so the reduced basis is the two successive minima) but `b ← b − m·a` stays exact.
    */
  private def gaussReduceZeta(v0: ZetaPoint, w0: ZetaPoint): (ZetaPoint, ZetaPoint) =
    def dot(p: ZetaPoint, q: ZetaPoint): BigDecimal =
      val (a, b) = (p.toBigPoint, q.toBigPoint); a.x * b.x + a.y * b.y
    var a                                           = v0
    var b                                           = w0
    if dot(a, a) > dot(b, b) then { val t = a; a = b; b = t }
    var iter                                        = 0
    while iter < 1000 && (dot(a, b).abs * 2) > dot(a, a) + BigDecimal("1e-9") do
      val m  = (dot(a, b) / dot(a, a)).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
      val mb = ZetaPoint(m * a.a0, m * a.a1, m * a.a2, m * a.a3)
      b = b - mb
      if dot(a, a) > dot(b, b) then { val t = a; a = b; b = t }
      iter += 1
    (a, b)

  /** Candidate primitive bases as exact integer ℤ[ζ₁₂] pairs: module points in the integer box `|aᵢ| ≤ k`
    * with covolume in `[minCovolume, maxCovolume]` and achievable (an integer combination of unit-polygon
    * areas), Gauss-reduced and deduped, smallest cell first. The integer-box enumeration is the exact
    * analogue of [[KrotenheerdtLatticeSearch.candidateBases]] (which enumerates the same module in Double);
    * orientation is swept by seeding all edge directions, so the candidate set need not be rotation-closed.
    */
  def candidateBasesZeta(k: Int, maxCovolume: Double): List[(ZetaPoint, ZetaPoint)] =
    val achievable                            = achievableCovolumes(maxCovolume)
    // Module points within the L1 step budget Σ|aᵢ| ≤ k (the exact analogue of the DCEL engine's
    // `modulePoints` over generators ζ⁰..ζ³); BigPoint embeddings are cached so the O(points²) pairing stays
    // in cheap Double-magnitude BigDecimal, not in repeated √3 multiplies.
    val points: Vector[(ZetaPoint, BigPoint)] =
      (for
        a0 <- -k to k
        a1 <- -(k - a0.abs) to (k - a0.abs)
        a2 <- -(k - a0.abs - a1.abs) to (k - a0.abs - a1.abs)
        a3 <- -(k - a0.abs - a1.abs - a2.abs) to (k - a0.abs - a1.abs - a2.abs)
        p   = ZetaPoint(a0, a1, a2, a3)
        if !p.isOrigin
      yield (p, p.toBigPoint)).toVector
    val byKey                                 = mutable.LinkedHashMap.empty[((Long, Long), (Long, Long)), (ZetaPoint, ZetaPoint)]
    val maxCovB                               = BigDecimal(maxCovolume)
    def snap(p: BigPoint): (Long, Long)       =
      (
        (p.x * 1000000).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong,
        (p.y * 1000000).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
      )
    // Sign-canonicalise to the upper half-plane and order the pair, exactly as the DCEL `candidateBases` does,
    // so the lattice (not its ± / swap variants) is the dedup unit — a 180° sign flip is covered by polygon
    // symmetry, so this removes redundancy without dropping any orientation the slot-0 seed needs.
    def canon(z: ZetaPoint): ZetaPoint =
      val p = z.toBigPoint
      if p.y > BigDecimal("1e-9") || (p.y.abs <= BigDecimal("1e-9") && p.x > 0) then z else -z
    for
      i   <- points.indices
      j   <- (i + 1) until points.size
      covB = cross(points(i)._2, points(j)._2).abs
      if covB >= minCovolume && covB <= maxCovB + BigDecimal("1e-9")
      if achievable.exists(a => (BigDecimal(a) - covB).abs < BigDecimal("1e-6"))
    do
      val (a0, b0) = gaussReduceZeta(points(i)._1, points(j)._1)
      val (ca, cb) = (canon(a0), canon(b0))
      val (caB, cbB) = (ca.toBigPoint, cb.toBigPoint)
      val ordered  =
        if (caB.dot(caB), snap(caB)) <= (cbB.dot(cbB), snap(cbB)) then (ca, cb) else (cb, ca)
      byKey.getOrElseUpdate((snap(ordered._1.toBigPoint), snap(ordered._2.toBigPoint)), ordered)
    byKey.values.toList.sortBy((v, w) => cross(v.toBigPoint, w.toBigPoint).abs)

  // ---- per-lattice exact flood fill -------------------------------------------------------------------

  /** Enumerate Krotenheerdt n-uniform tilings of the `{3,4,6,12}` world via exact-coordinate fixed-Λ growth.
    */
  def enumerate(
      n: Int,
      k: Int,
      maxCovolume: Double,
      parallelism: Int = 1,
      log: String => Unit = _ => ()
  ): Outcome =
    import java.util.concurrent.ConcurrentHashMap
    import java.util.concurrent.atomic.AtomicLong
    import scala.jdk.CollectionConverters.*
    val bases   = candidateBasesZeta(k, maxCovolume)
    val found   = new ConcurrentHashMap[String, Set[VertexSignature]]()
    val states  = new AtomicLong(0)
    val done    = new AtomicLong(0)
    val faceCap = sys.props.get("krot.facecap").map(_.toInt).getOrElse(64)
    log(s"n=$n k=$k maxCovol=$maxCovolume: ${bases.size} candidate lattices")
    def runOne(vz: ZetaPoint, wz: ZetaPoint): Unit =
      states.addAndGet(runLattice(n, vz, wz, faceCap, (t, key) => found.putIfAbsent(key, t): Unit))
      val d = done.incrementAndGet()
      if d % 200 == 0 then log(s"  lattices $d/${bases.size}, states=${states.get}, found=${found.size}")
    if parallelism <= 1 then bases.foreach((vz, wz) => runOne(vz, wz))
    else
      val pool = java.util.concurrent.Executors.newFixedThreadPool(parallelism)
      try bases.foreach((vz, wz) => pool.submit(new Runnable { def run(): Unit = runOne(vz, wz) }))
      finally
        pool.shutdown()
        pool.awaitTermination(7, java.util.concurrent.TimeUnit.DAYS)
    Outcome(found.asScala.toList.map((key, t) => (t, key)).sortBy(_._2), bases.size, states.get)

  /** Grow every Λ-consistent patch from each seed orientation; verify completed cells. Returns the state
    * count.
    */
  private def runLattice(
      n: Int,
      vz: ZetaPoint,
      wz: ZetaPoint,
      faceCap: Int,
      emit: (Set[VertexSignature], String) => Unit
  ): Long =
    val vB      = vz.toBigPoint
    val wB      = wz.toBigPoint
    val originB = BigPoint.origin
    val covol   = cross(vB, wB).abs
    val autos   = latticeAutos(vz, wz)

    val visited = mutable.HashSet.empty[String]
    val stack   = mutable.Stack.empty[List[FaceZ]]
    var count   = 0L

    // Seed each polygon once, at the fixed slot-0 orientation. Orientation is swept by the candidate lattice
    // set (which holds the rotated copies of each lattice), NOT by rotating the seed — seeding all 12 slots as
    // well double-counts and, with the absolute face-set dedup, explodes the state count with congruent copies
    // that never merge (the ADR-0019 "absolute key triples states" effect, ×12). Same orientation-sweep
    // convention as the DCEL engine, so the completeness profile in `k` is identical.
    for m <- sides do
      val face = FaceZ(m, polygon(ZetaPoint.origin, 0, m))
      if isConsistent(List(face), vB, wB) then stack.push(List(face))

    while stack.nonEmpty do
      val faces    = stack.pop()
      count += 1
      val distinct = distinctArea(faces, vB, wB)
      if distinct > covol + BigDecimal("1e-6") then () // scatter — should not occur past the gate
      else
        val total     = faces.map(f => area(f.size)).sum
        val tryVerify =
          if total >= covol - BigDecimal("1e-6") && distinct >= covol - BigDecimal("1e-6") then
            verify(faces, vB, wB, originB, n)
          else None
        tryVerify match
          case Some((t, key)) => emit(t, key)
          case None           =>
            if faces.sizeIs < faceCap && total < covol * 4 then
              grow(faces, vB, wB).foreach: child =>
                if visited.add(canonicalKey(child, autos)) then stack.push(child)
    count

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
    * automorphism `g`, transform the corners, translation-anchor on the lexicographically minimal corner, sort
    * faces and corners, and take the min serialization over all `g`. Collapses the symmetric-image duplicates
    * that an absolute key leaves un-merged (the ~2.5× state inflation), at the cost of small-integer work only.
    */
  private def canonicalKey(faces: List[FaceZ], autos: List[ZetaPoint => ZetaPoint]): String =
    autos.iterator.map: g =>
      val tf     = faces.map(f => (f.size, f.corners.map(g)))
      val anchor = tf.iterator.flatMap(_._2).min
      tf.map((s, cs) => s"$s:" + cs.map(_ - anchor).sorted.iterator.map(z => s"${z.a0},${z.a1},${z.a2},${z.a3}").mkString(";"))
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

  /** Distinct-torus-face area: each face counted once per `(size, centroid mod Λ)`. Equals one covolume when
    * a full cell of distinct faces is covered.
    */
  private def distinctArea(faces: List[FaceZ], vB: BigPoint, wB: BigPoint): BigDecimal =
    faces
      .map(f => (f.size, tkey(f.centroid, vB, wB, BigPoint.origin)))
      .distinct
      .map((m, _) => area(m))
      .sum

  /** The outgoing 30°-slot of face `f` at corner `p` (the edge `p → next` in CCW order). */
  private def outSlot(f: FaceZ, p: ZetaPoint): Int =
    val i = f.corners.indexOf(p)
    slotOfUnit(f.corners((i + 1) % f.size) - p)

  /** Λ-consistency: every torus vertex's incident faces occupy a conflict-free set of 30° slots. */
  private def isConsistent(faces: List[FaceZ], vB: BigPoint, wB: BigPoint): Boolean =
    val coverage = mutable.Map.empty[(Long, Long), mutable.Map[Int, (Int, Int)]]
    faces.forall: f =>
      f.corners.forall: p =>
        val slotMap = coverage.getOrElseUpdate(tkey(p.toBigPoint, vB, wB, BigPoint.origin), mutable.Map.empty)
        val start   = outSlot(f, p)
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
  private def grow(faces: List[FaceZ], vB: BigPoint, wB: BigPoint): List[List[FaceZ]] =
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
            Option.when(isConsistent(next, vB, wB) && isSound(next))(next)

  /** Sound iff every planar vertex's fan is a valid completed vertex or an extendable partial fan. */
  private def isSound(faces: List[FaceZ]): Boolean =
    faces.flatMap(_.corners).distinct.forall: p =>
      val fan     = planarFan(faces, p)
      val covered = coveredSlots(fan)
      if covered.sizeIs == 12 then isCompleteVertex(fan.map(_._2))
      else isExtendableFan(fan.map(_._2))

  // ---- verification (reuses the DCEL engine's proven tail) --------------------------------------------

  private def verify(
      faces: List[FaceZ],
      vB: BigPoint,
      wB: BigPoint,
      originB: BigPoint,
      n: Int
  ): Option[(Set[VertexSignature], String)] =
    // Reconstruct each torus vertex's full fan by unioning the incident corners of all its planar instances,
    // keyed by angle (translation-invariant) — exactly verifyTorus's union reconstruction, on ZetaPoints.
    val byTorus                                     = faces
      .flatMap(f => f.corners.map(p => (tkey(p.toBigPoint, vB, wB, originB), p, f)))
      .groupBy(_._1)
    val torusFan: Map[(Long, Long), Map[Long, Int]] = byTorus.view.mapValues: incident =>
      incident.map: (_, p, f) =>
        val c = f.centroid
        (
          math.round(math.atan2((c.y - p.toBigPoint.y).toDouble, (c.x - p.toBigPoint.x).toDouble) * 10000),
          f.size
        )
      .toMap
    .toMap

    def fanComplete(fan: Map[Long, Int]): Boolean    =
      fan.values.map(interiorAngle).foldLeft(AngleDegree(0))(_ + _) == AngleDegree(360)
    def fanSig(fan: Map[Long, Int]): VertexSignature =
      VertexTypes.normalize(fan.toList.sortBy(_._1).map(_._2))

    if torusFan.exists((_, f) => !fanComplete(f)) then None
    else
      val sigs  = torusFan.view.mapValues(fanSig).toMap
      val types = sigs.values.toSet
      if types.sizeIs != n then None
      else
        val faceList = faces
          .map(f => (f.size, f.centroid))
          .distinctBy((s, c) => (s, tkey(c, vB, wB, originB)))
        val verts    = sigs.toList.map((tk, sig) => (sig.mkString("."), byTorus(tk).head._2.toBigPoint))
        KrotenheerdtLatticeSearch.verifyContent(vB, wB, originB, faceList, verts, types, n)
