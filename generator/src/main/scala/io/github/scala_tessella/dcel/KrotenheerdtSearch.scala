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
      log: String => Unit = _ => ()
  ): Outcome =
    val visited    = mutable.HashSet[List[(Int, BigDecimal, BigDecimal)]]()
    val found      = mutable.LinkedHashMap[String, Certified]()
    val rejections = mutable.Map[RejectReason, Int]().withDefaultValue(0)
    var states     = 0L

    // (patch, nextCertifyAt): certification is expensive (lattice + block + corona refinement), so
    // past the horizon it runs on a vertex-count cadence rather than at every state.
    val certifyStep = 20
    val stack       = mutable.Stack[(Tiling, Int)]()
    polygonSides.reverse.foreach: sides =>
      val seed = TilingBuilder.createRegularPolygon(RegularPolygon(sides))
      if visited.add(PatchCanonical.congruenceKey(seed)) then stack.push((seed, maxVertices))

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

    while stack.nonEmpty do
      val (patch, certifyAt) = stack.pop()
      states += 1

      def grow(nextCertifyAt: Int): Unit =
        // children are pushed largest-polygon-first so the DFS pops small-polygon branches first:
        // dodecagon-rich families have the deepest lineages (huge cells) and would otherwise
        // monopolize the search front for hours before any other family completes.
        expansions(patch, n).reverse.foreach: next =>
          val key = PatchCanonical.congruenceKey(next)
          if visited.add(key) then stack.push((next, nextCertifyAt))

      if patch.vertices.sizeIs < certifyAt then
        // Type-density gate: in every published Krotenheerdt tiling all n types appear within any
        // ~60-vertex ball (fundamental cells are small), so a patch still type-deficient past the
        // gate is a mono-type core acquiring fringe decorations — an exponential family of doomed
        // congruence classes if allowed to grow to the horizon. Validated by the published-count
        // cross-checks (a too-aggressive gate would surface as a missing tiling at n <= 3).
        if patch.vertices.sizeIs >= earlyTypeGate && innerVertexTypes(patch).size < n then
          rejections(RejectReason.WrongTypeCount) += 1
        else grow(certifyAt)
      else if innerVertexTypes(patch).size < n then
        // Horizon assumption (validated by the published-count cross-checks): every ball of horizon
        // size in a true Krotenheerdt-n tiling shows all n vertex types — the fundamental cells are
        // far smaller than the horizon. A patch still type-deficient here is a mono-type core whose
        // retry tree would dominate the search for nothing.
        rejections(RejectReason.WrongTypeCount) += 1
      else
        certify(patch, n) match
          case Right(certified)                                                     =>
            if !found.contains(certified.torusKey) then
              found(certified.torusKey) = certified
              log(
                s"found #${found.size}: types ${certified.vertexTypes.map(_.mkString(".")).toList.sorted.mkString("; ")}"
              )
          case Left(reason) if transient(reason) && patch.vertices.sizeIs < hardCap =>
            grow(patch.vertices.size + certifyStep)
          case Left(reason)                                                         =>
            rejections(reason) += 1
            if sys.props.get("krot.dump").isDefined then
              val dir = java.nio.file.Paths.get("/tmp/krot-rejects")
              java.nio.file.Files.createDirectories(dir)
              java.nio.file.Files.writeString(
                dir.resolve(s"reject-$states.xml"), {
                  import io.github.scala_tessella.dcel.conversion.TilingSVG.toMetadataXml
                  patch.toMetadataXml
                }
              ): Unit
            log(
              s"reject $reason: types ${innerVertexTypes(patch).map(_.mkString(".")).toList.sorted.mkString("; ")} (v=${patch.vertices.size})"
            )

    Outcome(found.values.toList, rejections.toMap, states)

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
