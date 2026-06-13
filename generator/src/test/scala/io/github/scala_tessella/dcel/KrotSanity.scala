package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingCertifier.{RejectReason, certify, innerVertexTypes, vertexTypeOf}
import io.github.scala_tessella.dcel.VertexTypes.*
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon}
import io.github.scala_tessella.ring_seq.RingSeq.startAt

import scala.collection.mutable

/** Sanity probe: grow patches restricted to a target vertex-type set and report the first certification.
  * Proves end-to-end that the pipeline can certify a given (published) tiling.
  *
  * Usage: KrotSanity <maxVertices> <type> <type> ... e.g. `KrotSanity 100 3.3.3.4.4 4.4.4.4`
  */
object KrotSanity:

  /** Distinct certified torus keys of all tilings whose vertex types lie in `targets`. Grows patches
    * restricted to the target type set (collapsing the branching to one composition) and certifies at the
    * horizon. With `stopAtFirst` it returns as soon as one key is found (existence check); otherwise it
    * exhausts the restricted space (multiplicity check). `onKey` receives the patch behind each new key.
    */
  def distinctKeys(
      targets: Set[VertexSignature],
      maxV: Int,
      hardCapFactor: Double = 4.5,
      stopAtFirst: Boolean = false,
      onKey: (String, Tiling) => Unit = (_, _) => ()
  ): Set[String] =
    val n         = targets.size
    val hardCap   = (maxV * hardCapFactor).toInt
    val visited   = mutable.HashSet[List[(Int, BigDecimal, BigDecimal)]]()
    val stack     = mutable.Stack[(Tiling, Int)]()
    val keys      = mutable.LinkedHashSet[String]()
    val transient = Set[RejectReason](
      RejectReason.NoLattice,
      RejectReason.BlockTooSmall,
      RejectReason.NoWitnessCell,
      RejectReason.TooShallow,
      RejectReason.CellsDiffer
    )

    polygonSides.reverse.foreach: sides =>
      val seed = TilingBuilder.createRegularPolygon(RegularPolygon(sides))
      if visited.add(PatchCanonical.congruenceKey(seed)) then stack.push((seed, maxV))

    var done = false
    while stack.nonEmpty && !done do
      val (patch, certifyAt) = stack.pop()

      def grow(next: Int): Unit =
        expansions(patch, targets).reverse.foreach: child =>
          if visited.add(PatchCanonical.congruenceKey(child)) then stack.push((child, next))

      if patch.vertices.sizeIs < certifyAt then grow(certifyAt)
      else
        certify(patch, n) match
          case Right(certified)                                                     =>
            if keys.add(certified.torusKey) then onKey(certified.torusKey, patch)
            if stopAtFirst then done = true
          case Left(reason) if transient(reason) && patch.vertices.sizeIs < hardCap =>
            grow(patch.vertices.size + 10)
          case Left(_)                                                              => ()

    keys.toSet

  def main(args: Array[String]): Unit =
    val maxV                          = args.head.toInt
    val targets: Set[VertexSignature] =
      args.tail.map(s => normalize(s.split('.').map(_.toInt).toList)).toSet
    println(s"targets (${targets.size}): ${targets.map(_.mkString(".")).mkString("; ")}, horizon $maxV")

    val collect = sys.props.get("krot.collect").isDefined
    val keys    = distinctKeys(
      targets,
      maxV,
      stopAtFirst = !collect,
      onKey = (key, patch) =>
        if collect then
          val dir = java.nio.file.Paths.get("/tmp/krot-collect")
          java.nio.file.Files.createDirectories(dir)
          import io.github.scala_tessella.dcel.conversion.TilingSVG.toMetadataXml
          java.nio.file.Files.writeString(
            dir.resolve(s"key-${key.hashCode}-v${patch.vertices.size}.xml"),
            patch.toMetadataXml
          ): Unit
          println(s"DISTINCT KEY at v=${patch.vertices.size}:\n  $key")
        else println(s"CERTIFIED at v=${patch.vertices.size}: $key")
    )
    println(if collect then s"EXHAUSTED: distinctKeys=${keys.size}" else s"FOUND ${keys.size}")

  private def expansions(patch: Tiling, targets: Set[VertexSignature]): List[Tiling] =
    val gaps     = patch.boundaryVerticesUnsafe.flatMap: vertex =>
      val gap = AngleDegree(360) - vertex.currentInteriorAngleSumUnsafe(patch.outerFace)
      Option.when(gap.toRational > 0)((vertex, gap))
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
              .filter(isSound(_, previousInnerIds, targets))
          else None

  private def isSound(
      next: Tiling,
      previousInnerIds: Set[structure.VertexId],
      targets: Set[VertexSignature]
  ): Boolean =
    val newInnerValid =
      next.innerVertices
        .filterNot(v => previousInnerIds.contains(v.id))
        .forall(v => targets.contains(vertexTypeOf(next, v)))
    val fansValid     =
      next.boundaryVerticesUnsafe.forall: vertex =>
        boundaryFan(next, vertex) match
          case Nil => true
          case fan => isExtendableFan(fan)
    next.hasUnitRegularPolygonsOnly && newInnerValid && fansValid

  private def boundaryFan(tiling: TilingDCEL, vertex: structure.Vertex): List[Int] =
    val edges      = vertex.incidentEdgesUnsafe
    val outerIndex = edges.indexWhere(_.incidentFace.exists(_ eq tiling.outerFace))
    val ordered    = if outerIndex < 0 then edges else edges.startAt(outerIndex).tail
    ordered
      .flatMap(_.incidentFace)
      .filter(_ != tiling.outerFace)
      .map(_.halfEdgesUnsafe.size)
