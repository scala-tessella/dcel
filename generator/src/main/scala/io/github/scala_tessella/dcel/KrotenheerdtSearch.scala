package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingCertifier.{Certified, RejectReason, certify, innerVertexTypes}
import io.github.scala_tessella.dcel.VertexTypes.*
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon}
import io.github.scala_tessella.dcel.structure.Vertex
import io.github.scala_tessella.ring_seq.RingSeq.startAt

import scala.collection.mutable

/** Exhaustive canonical-growth search for the Krotenheerdt tilings (OEIS A068600): n-uniform edge-to-edge
  * regular-polygon tilings with exactly n distinct vertex types.
  *
  * Completeness argument: at every step the search fills THE deterministically chosen boundary vertex (the
  * smallest positive angle gap, tie-broken by vertex id) in all geometrically possible ways — any infinite
  * tiling containing the current patch must fill that vertex's slot with one of the branched polygons, so
  * some branch stays inside it. Pruning is sound: geometric rejections cannot occur for a sub-patch of a
  * valid tiling, a boundary fan outside [[VertexTypes.validPartialFans]] can never complete, more than n
  * inner vertex types can never shrink, and an irregular auto-filled hole face can never belong to a
  * regular-polygon tiling. Search states are merged only when congruent ([[PatchCanonical.congruenceKey]]),
  * which preserves all continuations. Horizon patches go through [[TilingCertifier.certify]]; distinct
  * infinite tilings are collected by torus key.
  */
object KrotenheerdtSearch:

  final case class Outcome(
      certified: List[Certified],
      rejections: Map[RejectReason, Int],
      statesExplored: Long
  )

  def enumerate(
      n: Int,
      maxVertices: Int,
      hardCapFactor: Double = 3.5,
      earlyTypeGate: Int = 60,
      typeBallRadius: Int = 5,
      parallelism: Int = 1,
      log: String => Unit = _ => ()
  ): Outcome =
    import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedDeque}
    import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
    import scala.jdk.CollectionConverters.*

    // Parallel workers are safe because every state is an independent object graph: expansion
    // operations deep-copy their input (the sequential search already relies on parent immutability
    // when trying sibling polygons), each congruence class is claimed exactly once through the
    // atomic visited-set add, and a claimed patch is processed by a single worker.
    //
    // The visited-set stores a 128-bit digest of the congruence key, not the key itself: a full key
    // is a list of hundreds of rounded coordinates (~10-20 KB) and millions of visited states would
    // exhaust any heap (seen: GC collapse at 1.3M states on a 20 GB heap). A digest collision would
    // silently merge two distinct classes; at 10^8 states the probability is ~10^-22 — far below
    // any other source of error in the pipeline.
    val visited                                       = ConcurrentHashMap.newKeySet[java.math.BigInteger]()
    def digestOf(patch: Tiling): java.math.BigInteger =
      val md = java.security.MessageDigest.getInstance("MD5")
      java.math.BigInteger(1, md.digest(PatchCanonical.congruenceKey(patch).toString.getBytes("UTF-8")))
    val found                                         = new ConcurrentHashMap[String, Certified]()
    val foundCount                                    = new AtomicLong(0)
    val rejections                                    = new ConcurrentHashMap[RejectReason, AtomicLong]()
    val retries                                       = new ConcurrentHashMap[RejectReason, AtomicLong]()
    val states                                        = new AtomicLong(0)
    val sample                                        = new AtomicReference[Int](0)

    def bump(map: ConcurrentHashMap[RejectReason, AtomicLong], reason: RejectReason): Unit  =
      map.computeIfAbsent(reason, _ => new AtomicLong(0)).incrementAndGet(): Unit
    def snapshot(map: ConcurrentHashMap[RejectReason, AtomicLong]): Map[RejectReason, Long] =
      map.asScala.map((k, v) => (k, v.get)).toMap

    val certifyStep = 10
    // Work-stealing deques: each worker pushes and pops at the head of its own deque (depth-first,
    // memory bounded like the sequential stack) and steals from another worker's tail when its own
    // is empty. A single shared deque makes all workers collectively burrow into whichever subtree
    // owns the global front — seen pinning an entire 12-worker run inside one decoration-scatter
    // forest for a million states while other seed families held all the findings. `pending` counts
    // pushed-but-unfinished tasks — zero means every deque is empty AND no worker can still push,
    // the only sound termination signal.
    val workers     = math.max(1, parallelism)
    val deques      = Array.fill(workers)(new ConcurrentLinkedDeque[(Tiling, Int)]())
    val pending     = new AtomicLong(0)

    def push(worker: Int, task: (Tiling, Int)): Unit =
      pending.incrementAndGet()
      deques(worker).addFirst(task)

    polygonSides.reverse.zipWithIndex.foreach: (sides, i) =>
      val seed = TilingBuilder.createRegularPolygon(RegularPolygon(sides))
      if visited.add(digestOf(seed)) then push(i % workers, (seed, maxVertices))

    // Rejections that just mean "not enough patch yet": keep growing (bounded) instead of dropping.
    val transient =
      Set[RejectReason](
        RejectReason.NoLattice,
        RejectReason.BlockTooSmall,
        RejectReason.NoWitnessCell,
        RejectReason.TooShallow,
        RejectReason.CellsDiffer
      )
    val hardCap   = (maxVertices * hardCapFactor).toInt

    def process(worker: Int, patch: Tiling, certifyAt: Int): Unit =
      val state = states.incrementAndGet()
      sample.set(patch.vertices.size)
      if state % 5000 == 0 then
        log(
          s"heartbeat: states=$state pending=${pending.get} visited=${visited.size} " +
            s"found=${foundCount.get} v=${sample.get} retries=${snapshot(retries)} finals=${snapshot(rejections)}"
        )

      def grow(nextCertifyAt: Int): Unit =
        // children are pushed largest-polygon-first so the DFS pops small-polygon branches first:
        // dodecagon-rich families have the deepest lineages (huge cells) and would otherwise
        // monopolize the search front for hours before any other family completes.
        expansions(patch, n).reverse.foreach: next =>
          if visited.add(digestOf(next)) then push(worker, (next, nextCertifyAt))

      // Growth gates past the early-gate size (all validated by the published-count cross-checks):
      // type density — all n types showing, each deep vertex seeing every type within the ball
      // radius — kills mono-type cores acquiring fringe decorations. A witnessed corona split
      // (tooManyWitnessedOrbits) was also tried here as a growth gate, but the adversarial
      // decoration families keep their inconsistencies just outside the truncation-guarded
      // witnessed core until the horizon, so it fired ~once per 6000 states while costing
      // local-DCEL construction on every state — the certify-time verdicts do the real work. It
      // remains the veto on transient certify retries.
      def gateReason: Option[RejectReason] =
        if innerVertexTypes(patch).size < n || !typesLocallyComplete(patch, n, typeBallRadius) then
          Some(RejectReason.WrongTypeCount)
        else None

      if patch.vertices.sizeIs < certifyAt then
        if patch.vertices.sizeIs >= earlyTypeGate then
          gateReason match
            case Some(reason) => bump(rejections, reason)
            case None         => grow(certifyAt)
        else grow(certifyAt)
      else if gateReason.isDefined then bump(rejections, gateReason.get)
      else
        def finalReject(reason: RejectReason): Unit =
          bump(rejections, reason)
          if sys.props.get("krot.dump").isDefined then
            val dir = java.nio.file.Paths.get("/tmp/krot-rejects")
            java.nio.file.Files.createDirectories(dir)
            java.nio.file.Files.writeString(
              dir.resolve(s"reject-$state.xml"), {
                import io.github.scala_tessella.dcel.conversion.TilingSVG.toMetadataXml
                patch.toMetadataXml
              }
            ): Unit
          log(
            s"reject $reason: types ${innerVertexTypes(patch).map(_.mkString(".")).toList.sorted.mkString("; ")} (v=${patch.vertices.size})"
          )

        certify(patch, n) match
          case Right(certified)                                                     =>
            if found.putIfAbsent(certified.torusKey, certified) == null then
              log(
                s"found #${foundCount.incrementAndGet()}: types ${certified.vertexTypes.map(_.mkString(".")).toList.sorted.mkString("; ")}"
              )
          case Left(reason) if transient(reason) && patch.vertices.sizeIs < hardCap =>
            // Retry growth is granted only if the patch could still be n-uniform: the witnessed
            // corona refinement lower-bounds the orbit count of any continuation, so > n classes is
            // a sound final verdict that stops the stacking-aperiodic families at their first
            // certification instead of letting them retry-branch to the hard cap.
            if TilingCertifier.tooManyWitnessedOrbits(patch, n) then
              finalReject(RejectReason.WrongClassCount)
            else
              bump(retries, reason)
              grow(patch.vertices.size + certifyStep)
          case Left(reason)                                                         =>
            finalReject(reason)

    def workerLoop(worker: Int): Unit =
      while pending.get() > 0 do
        val task =
          deques(worker).pollFirst() match
            case null => // steal from the tail of the first non-empty victim, round-robin from us
              (1 until workers).iterator
                .map(k => deques((worker + k) % workers).pollLast())
                .find(_ != null)
                .orNull
            case own  => own
        if task == null then Thread.sleep(1)
        else
          try process(worker, task._1, task._2)
          finally pending.decrementAndGet(): Unit

    if workers <= 1 then workerLoop(0)
    else
      val threads = (0 until workers).map(i => Thread.ofPlatform.name(s"krot-$i").start(() => workerLoop(i)))
      threads.foreach(_.join())

    Outcome(
      found.values.asScala.toList.sortBy(_.torusKey),
      snapshot(rejections).map((k, v) => (k, v.toInt)),
      states.get
    )

  /** Every inner vertex whose `radius`-ball is fully interior must see all n vertex types within it.
    *
    * A fully interior ball is final content — no continuation can change it — so a mono-type ball can only
    * tend to a tiling whose fundamental cell outgrows this horizon: the certifier requires a 2x2 cell block
    * inside the patch, and a cell containing all n types cannot be smaller than the type-complete
    * neighbourhoods it is made of. Without this prune the search front drowns in sparse-decoration families
    * (a triangle field with isolated hexagons shows exactly the types of `3.3.3.3.3.3 + 3.3.3.3.6` while
    * being aperiodic for every hexagon spacing — an exponential family that survives every other gate all the
    * way to the hard cap). Like the type-density gate, the radius is validated by the published-count
    * cross-checks at n <= 3.
    */
  private def typesLocallyComplete(patch: Tiling, n: Int, radius: Int): Boolean =
    val boundaryIds = patch.boundaryVerticesUnsafe.map(_.id).toSet
    val typeOf      = patch.innerVertices.map(v => v.id -> TilingCertifier.vertexTypeOf(patch, v)).toMap
    patch.innerVertices.forall: v =>
      val ball = v.bfsVertices(radius)
      ball.exists(u => boundaryIds.contains(u.id))
      || ball.flatMap(u => typeOf.get(u.id)).sizeIs == n

  /** All sound continuations of the patch: every polygon that fits the slot at the canonical target vertex,
    * filtered by the soundness prunes described in the object scaladoc.
    */
  private def expansions(patch: Tiling, n: Int): List[Tiling] =
    val gaps = patch.boundaryVerticesUnsafe.flatMap: vertex =>
      val gap = AngleDegree(360) - vertex.currentInteriorAngleSumUnsafe(patch.outerFace)
      Option.when(gap.toRational > 0)((vertex, gap))

    // Fill the boundary vertex nearest the patch centroid first (gap size and id as tie-breaks):
    // smallest-gap-first chains sideways on row-structured tilings (e.g. the elongated triangular),
    // growing 1-cell-tall ribbons whose lattice block never reaches 2x2. Centroid-first growth keeps
    // patches compact in every direction; the choice is still a deterministic function of the patch,
    // preserving the completeness argument.
    val centroid = patch.vertices.map(_.coords).centroid
    gaps.minByOption((vertex, gap) =>
      (vertex.coords.distanceTo(centroid), gap.toRational, vertex.id.value)
    ) match
      case None                =>
        Nil
      case Some((target, gap)) =>
        val previousInnerIds = patch.innerVertices.map(_.id).toSet
        polygonSides.flatMap: sides =>
          if interiorAngle(sides).toRational <= gap.toRational then
            patch
              .maybeAddRegularPolygonToBoundary(target.id, RegularPolygon(sides))
              .toOption
              .filter(isSoundContinuation(_, previousInnerIds, n))
          else None

  private def isSoundContinuation(next: Tiling, previousInnerIds: Set[structure.VertexId], n: Int): Boolean =
    // Every newly completed vertex must be a valid vertex type.
    val newInnerValid =
      next.innerVertices
        .filterNot(v => previousInnerIds.contains(v.id))
        .forall(v => isCompleteVertex(TilingCertifier.vertexTypeOf(next, v)))
    // Every boundary fan must still be completable to a valid type.
    val fansValid     =
      next.boundaryVerticesUnsafe.forall: vertex =>
        boundaryFan(next, vertex) match
          case Nil => true
          case fan => isExtendableFan(fan)
    // hasUnitRegularPolygonsOnly: an auto-filled hole face must be a unit regular polygon; the
    // type-count bound: a Krotenheerdt tiling for this n never shows more than n inner types.
    next.hasUnitRegularPolygonsOnly && newInnerValid && fansValid && innerVertexTypes(next).size <= n

  /** The contiguous run of inner polygons around a boundary vertex, in rotational order (the outer gap
    * rotated to the cut position).
    */
  private def boundaryFan(tiling: TilingDCEL, vertex: Vertex): List[Int] =
    val edges      = vertex.incidentEdgesUnsafe
    val outerIndex = edges.indexWhere(_.incidentFace.exists(_ eq tiling.outerFace))
    val ordered    = if outerIndex < 0 then edges else edges.startAt(outerIndex).tail
    ordered
      .flatMap(_.incidentFace)
      .filter(_ != tiling.outerFace)
      .map(_.halfEdgesUnsafe.size)
