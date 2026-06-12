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
  def main(args: Array[String]): Unit =
    val maxV                          = args.head.toInt
    val targets: Set[VertexSignature] =
      args.tail.map(s => normalize(s.split('.').map(_.toInt).toList)).toSet
    val n                             = targets.size
    val hardCap                       = (maxV * 4.5).toInt
    println(s"targets ($n): ${targets.map(_.mkString(".")).mkString("; ")}, horizon $maxV, cap $hardCap")

    val visited = mutable.HashSet[List[(Int, BigDecimal, BigDecimal)]]()
    val stack   = mutable.Stack[(Tiling, Int)]()
    val finals  = mutable.Map[RejectReason, Int]().withDefaultValue(0)
    var states  = 0L

    polygonSides.reverse.foreach: sides =>
      val seed = TilingBuilder.createRegularPolygon(RegularPolygon(sides))
      if visited.add(PatchCanonical.congruenceKey(seed)) then stack.push((seed, maxV))

    val transient = Set[RejectReason](
      RejectReason.NoLattice,
      RejectReason.BlockTooSmall,
      RejectReason.NoWitnessCell,
      RejectReason.TooShallow,
      RejectReason.CellsDiffer
    )

    while stack.nonEmpty do
      val (patch, certifyAt) = stack.pop()
      states += 1

      def grow(next: Int): Unit =
        expansions(patch, targets).reverse.foreach: child =>
          val key = PatchCanonical.congruenceKey(child)
          if visited.add(key) then stack.push((child, next))

      if patch.vertices.sizeIs < certifyAt then grow(certifyAt)
      else
        certify(patch, n) match
          case Right(certified)                                                     =>
            println(
              s"CERTIFIED at v=${patch.vertices.size} states=$states: " +
                s"types ${certified.vertexTypes.map(_.mkString(".")).toList.sorted.mkString("; ")} " +
                s"basis=${certified.basis} key=${certified.torusKey.take(80)}"
            )
            return
          case Left(reason) if transient(reason) && patch.vertices.sizeIs < hardCap =>
            grow(patch.vertices.size + 10)
          case Left(reason)                                                         =>
            finals(reason) += 1
            if finals.values.sum % 50 == 0 then
              println(s"  ...finals so far: ${finals.toMap} (states=$states)")

    println(s"EXHAUSTED with no certification: finals=${finals.toMap} states=$states")

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
