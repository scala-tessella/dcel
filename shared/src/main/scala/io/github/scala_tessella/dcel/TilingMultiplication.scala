package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingEquivalency.*
import io.github.scala_tessella.dcel.TilingMerge.mergeTilings
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint, BigRadian}
import io.github.scala_tessella.dcel.structure.{FaceId, HalfEdge, Vertex, VertexId}

import scala.annotation.tailrec

/** Copy-based growth of a tiling: fresh-id doubling, isometric/reflected copies, fans and translational
  * repeats. All operations funnel through [[TilingMerge.mergeTilings]]; the polygon-by-polygon growth
  * pipeline lives in [[TilingAddition]].
  */
private[dcel] object TilingMultiplication:

  extension (tiling: TilingDCEL)

    private def freshIdTranslations(base: TilingDCEL): (VertexId => VertexId, FaceId => FaceId) =
      // Translate vertices: give completely fresh vertex ids
      val vertexIds                                 =
        tiling.vertices.map: vertex =>
          vertex.id
      val maxVertexId                               =
        base.vertices.map(_.id.value).maxOption.getOrElse(0)
      val vertexIdTranslation: VertexId => VertexId =
        vertexIds.indices
          .map: index =>
            val oldId = vertexIds(index)
            oldId -> VertexId(maxVertexId + index + 1)
          .toMap

      // Translate faces: keep outer face id, shift inner ones
      val faceIds                             =
        tiling.faces.map: face =>
          face.id
      val maxFaceId                           =
        base.faces.map(_.id.value).maxOption.getOrElse(0)
      val faceIdTranslation: FaceId => FaceId =
        faceIds.indices
          .map: index =>
            faceIds(index) match
              case faceId if faceId == FaceId.outerId => faceId -> faceId // outer face
              case faceId                             => faceId -> FaceId(maxFaceId + index)
          .toMap

      (vertexIdTranslation, faceIdTranslation)

    private[dcel] def rawDouble(origin: Vertex, repeat: Vertex, withInversion: Boolean = false): TilingDCEL =
      // Compute the translation vector from origin to repeat
      val modifier: BigPoint => BigPoint           = if withInversion then _.scaled(-1.0) else identity
      val delta                                    = repeat.coords - modifier(origin.coords)
      val coordsTranslation: BigPoint => BigPoint  = modifier(_) + delta
      val (vertexIdTranslation, faceIdTranslation) = freshIdTranslations(tiling)

      // Second copy, translated in space and with fresh ids
      val translated: TilingDCEL =
        tiling.translatedDouble(
          coordsTranslation,
          vertexIdTranslation,
          faceIdTranslation
        )

      mergeTilings(tiling, translated)

    /** Builds a fresh-id copy of the tiling (via `buildCopy`, given fresh vertex/face id maps), merges it
      * back via [[TilingMerge.mergeTilings]] — which unifies coincident vertices, collapses shared edges and
      * deduplicates fully-overlapping faces — and validates the result in full. The shared engine behind the
      * three isometric copy operations (mirror / translate / rotate).
      *
      * @return
      *   The grown tiling, or a [[TilingError]] if the copy conflicts with the existing composition (partial
      *   overlap, crossing edges, over-full vertex angles, ...) and the merge fails validation.
      */
    private def addMergedCopy(
        buildCopy: (VertexId => VertexId, FaceId => FaceId) => TilingDCEL
    ): Either[TilingError, TilingDCEL] =
      if tiling.isEmpty then
        Right(tiling)
      else
        val (vertexIdTranslation, faceIdTranslation) = freshIdTranslations(tiling)
        val copied: TilingDCEL                       =
          buildCopy(vertexIdTranslation, faceIdTranslation)
        val merged: TilingDCEL                       =
          mergeTilings(tiling, copied)
        TilingValidation.validate(merged).map: _ =>
          merged

    /** Adds an orientation-preserving isometric copy (a translation or rotation, det +1, so the DCEL wiring
      * is reused verbatim) under `coordsTransformer`.
      */
    private[dcel] def addIsometricCopy(
        coordsTransformer: BigPoint => BigPoint
    ): Either[TilingError, TilingDCEL] =
      addMergedCopy: (vertexIdTransformer, faceIdTransformer) =>
        tiling.translatedDouble(coordsTransformer, vertexIdTransformer, faceIdTransformer)

    /** Adds an orientation-reversing reflected copy (det -1) under `coordsTransformer`: the copy's half-edge
      * wiring is rebuilt with reversed orientation via [[TilingEquivalency.reflectedDouble]] so the
      * face-on-the-left invariant survives the mirror.
      */
    private[dcel] def addReflectedCopy(
        coordsTransformer: BigPoint => BigPoint
    ): Either[TilingError, TilingDCEL] =
      addMergedCopy: (vertexIdTransformer, faceIdTransformer) =>
        tiling.reflectedDouble(coordsTransformer, vertexIdTransformer, faceIdTransformer)

    /** Accumulates `count - 1` orientation-preserving copies of the *original* onto it: for `k = 1 until
      * count` the original is transformed by `coordsTransformerForK(k)` (a fresh transform of the original,
      * so trig error never accumulates) and merged via [[TilingMerge.mergeTilings]]. Shared engine of
      * [[rawFanAround]] (rotational) and [[rawRepeatAlong]] (translational).
      *
      * Validation runs once, on the final patch, rather than after every merge — for a clean fan/repeat the
      * intermediate sectors are themselves valid, so per-step validation only re-paid the expensive
      * geometry/spatial pipeline on every prefix (quadratic over a large fan). A conflicting copy is still
      * rejected: it surfaces as a self-intersection or over-full vertex in the final `validate`. The merge
      * keeps the inner faces of both inputs structurally intact (it only ever recomputes the boundary), so an
      * intermediate that is geometrically invalid is still safe to merge again.
      */
    private def accumulateCopies(
        count: Int
    )(coordsTransformerForK: Int => BigPoint => BigPoint): Either[TilingError, TilingDCEL] =
      @tailrec
      def loop(acc: TilingDCEL, k: Int): TilingDCEL =
        if k >= count then
          acc
        else
          val (vertexIdTranslation, faceIdTranslation) = freshIdTranslations(acc)
          val copied: TilingDCEL                       =
            tiling.translatedDouble(coordsTransformerForK(k), vertexIdTranslation, faceIdTranslation)
          loop(mergeTilings(acc, copied), k + 1)

      val merged: TilingDCEL = loop(tiling, 1)
      TilingValidation.validate(merged).map: _ =>
        merged

    /** Fans `order` rotated copies of the tiling around an arbitrary `center` point, each rotated by a full
      * `360 / order` slice, merging them into one rotationally-symmetric patch (strict full ring).
      *
      * Unlike [[rawFan]] — which fills the angular gap at a boundary *vertex* with as many wedges as fit —
      * the centre here is any point (a face centroid, an edge midpoint, ...) and the ring always spans the
      * full 360°. The merge deduplicates a centre face that maps onto itself (e.g. a regular polygon rotated
      * about its own centroid). Strict: a wedge that conflicts makes the completed ring fail validation.
      *
      * @return
      *   The fanned tiling, or a [[TilingError]] if `order < 2` or any wedge conflicts with the composition.
      */
    private[dcel] def rawFanAround(center: BigPoint, order: Int): Either[TilingError, TilingDCEL] =
      if tiling.isEmpty then
        Right(tiling)
      else if order < 2 then
        Left(ValidationError(s"A fan order must be at least 2, was $order."))
      else
        accumulateCopies(order): k =>
          _.rotatedAround(center, BigRadian(2 * Math.PI * k / order))

    /** Repeats the tiling in a strip: `count` copies translated by successive multiples of `step`. Merged and
      * validated once when complete; exactly-overlapping copies are deduplicated. `count == 1` is the
      * identity.
      *
      * @return
      *   The strip, or a [[TilingError]] if `count < 1` or a copy conflicts with the composition.
      */
    private[dcel] def rawRepeatAlong(step: BigPoint, count: Int): Either[TilingError, TilingDCEL] =
      if tiling.isEmpty then
        Right(tiling)
      else if count < 1 then
        Left(ValidationError(s"A repeat count must be at least 1, was $count."))
      else
        accumulateCopies(count): k =>
          _ + step.scaled(k)

    /** Multiply the tiling as a fan centered at the given vertex.
      *
      * @param origin
      * @return
      */
    private[dcel] def rawFan(origin: Vertex): Either[TilingError, TilingDCEL] =
      if tiling.isEmpty then
        Right(tiling)
      else

        // 1. check if origin is the id of an existing boundary vertex, otherwise return a validation error
        val originId = origin.id

        if !tiling.boundaryVerticesUnsafe.exists(_.id == originId) then
          return Left(ValidationError(s"Vertex ${origin.id.toPrefixedString} is not on the boundary."))

        // 2. calculate max multiplication factor, is the integer part of 360° divided by the sum interior angles, minus 1
        val anglesSum = origin.currentInteriorAngleSumUnsafe(tiling.outerFace)
        val factor    =
          math.floor(AngleDegree(360).toRational.toDouble / anglesSum.toRational.toDouble).toInt - 1

        // Validate a single merged copy before applying all symmetric copies.
        def cannotExpand: Left[ValidationError, TilingDCEL] =
          Left(ValidationError(s"Cannot be expanded around boundary Vertex ${originId.toPrefixedString}."))

        // 3. if it is 0 (the interior angles more than 180°) return the tiling itself
        if factor <= 0 then
          return cannotExpand

        def boundaryEdgesAtOrigin(target: TilingDCEL): Either[TilingError, (HalfEdge, HalfEdge)] =
          val boundaryEdges = target.boundaryEdgesUnsafe
          val edgeOutOpt    =
            boundaryEdges.find: halfEdge =>
              halfEdge.origin.id == originId
          val edgeInOpt     =
            boundaryEdges.find: halfEdge =>
              halfEdge.destination.exists(_.id == originId)
          (edgeInOpt, edgeOutOpt) match
            case (Some(edgeIn), Some(edgeOut)) => Right((edgeIn, edgeOut))
            case _                             =>
              Left(ValidationError(s"Adjacent boundary edges not found for ${originId.toPrefixedString}."))

        // 4. calculate the two perimeter edges adjacent to the origin vertex, and call A the first and Z the second
        val zEdge =
          boundaryEdgesAtOrigin(tiling) match
            case Left(_)             =>
              return Left(ValidationError(
                s"Adjacent boundary edges not found for ${originId.toPrefixedString}."
              ))
            case Right((_, edgeOut)) => edgeOut

        def translatedCopy(base: TilingDCEL, rotation: BigRadian, center: BigPoint): TilingDCEL =
          val (vertexIdTranslation, faceIdTranslation) = freshIdTranslations(base)

          val coordsTransformer: BigPoint => BigPoint =
            _.rotatedAround(center, rotation)

          tiling.translatedDouble(coordsTransformer, vertexIdTranslation, faceIdTranslation)

        def firstCopy(current: TilingDCEL): Option[TilingDCEL] =
          boundaryEdgesAtOrigin(current) match
            case Left(_)           =>
              None
            case Right((bEdge, _)) =>
              current.vertices.find(_.id == originId).map: currentOrigin =>
                val zAngle = currentOrigin.coords.angleTo(zEdge.destinationUnsafe.coords)
                val bAngle = currentOrigin.coords.angleTo(bEdge.origin.coords)
                val delta  = bAngle - zAngle
                translatedCopy(current, delta, currentOrigin.coords)

        val seedCopy   = firstCopy(tiling)
        if seedCopy.isEmpty then return cannotExpand
        val seedMerged = mergeTilings(tiling, seedCopy.get)
        if TilingValidation.validate(seedMerged).isLeft then
          return cannotExpand

        // 5. for the times of the multiplication factor, repeat this process:
        @tailrec
        def loop(current: TilingDCEL, remaining: Int): Either[TilingError, TilingDCEL] =
          if remaining <= 0 then Right(current)
          else
            firstCopy(current) match
              case None         =>
                cannotExpand
              case Some(copied) =>
                val grown = mergeTilings(current, copied)
                loop(grown, remaining - 1)

        // 6. return the grown tiling (or the original one if even the first growth attempt fails)
        loop(seedMerged, factor - 1)
