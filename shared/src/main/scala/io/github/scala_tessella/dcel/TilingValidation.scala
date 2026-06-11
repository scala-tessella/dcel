package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.IntersectionDetection
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigLineSegment, BigPoint, SimplePolygon}
import io.github.scala_tessella.dcel.structure.HalfEdge

import scala.collection.mutable

/** Validation entry point for [[TilingDCEL]] instances. The public [[validate]] method runs the full
  * pipeline; the per-stage helpers ([[validateCompleteness]], [[validateTopologically]],
  * [[validateGeometrically]], [[validateSpatially]]) are internal and called in sequence by [[validate]] but
  * exposed here for library-internal reuse.
  */
object TilingValidation:

  /** Runs `body` with a fresh error buffer, then folds the collected messages into a single
    * `Either[TilingError, Unit]` of the given category. Returns `Right(())` when `body` records no errors.
    * Internal — shared shape for the four per-stage checks.
    */
  private def collect(category: ErrorCategory)(
      body: mutable.ListBuffer[String] => Unit
  ): Either[TilingError, Unit] =
    val errors = mutable.ListBuffer[String]()
    body(errors)
    if errors.isEmpty then Right(())
    else Left(TilingError.combineErrors(errors.toList, category))

  /** Per-entity structural integrity: every vertex, half-edge and face must self-validate. A failure here
    * indicates internal state was never fully populated (e.g. a vertex without a leaving edge); [[validate]]
    * returns immediately with an [[IncompleteError]] and skips the later stages.
    */
  private[dcel] def validateCompleteness(tiling: TilingDCEL): Either[TilingError, Unit] =
    collect(ErrorCategory.Incomplete): errors =>
      tiling.vertices.foreach: vertex =>
        if vertex.validate().isLeft then
          errors += vertex.toString
      tiling.halfEdges.foreach: halfEdge =>
        if halfEdge.validate().isLeft then
          errors += halfEdge.toString
      (tiling.outerFace :: tiling.innerFaces).foreach: face =>
        if face.validate().isLeft then
          errors += face.toString

  /** Half-edge linkage and face cycle consistency: twin / next / prev relationships are symmetric and point
    * into the tiling, every edge's incident face owns it back, the outer face is reachable from its outer
    * component, and no inner face contains holes (since holes are themselves represented as inner polygons in
    * this model). Returns a [[TopologyError]] on failure.
    */
  private[dcel] def validateTopologically(tiling: TilingDCEL): Either[TilingError, Unit] =
    collect(ErrorCategory.Topology): errors =>
      // Precompute sets for membership checks
      val vertexSet    = tiling.vertices.toSet
      val edgeSet      = tiling.halfEdges.toSet
      val innerFaceSet = tiling.innerFaces.toSet

      // Check vertex consistency
      tiling.vertices.foreach: vertex =>
        vertex.leaving match
          case None       =>
            errors += s"Vertex ${vertex.id} has no leaving edge"
          case Some(edge) =>
            if edge.origin ne vertex then
              errors += s"Vertex ${vertex.id} leaving edge doesn't originate from it"
            if !edgeSet.contains(edge) then
              errors += s"Vertex ${vertex.id} leaving edge is not part of this tiling"

      // Check half-edge consistency
      tiling.halfEdges.foreach: edge =>
        if !vertexSet.contains(edge.origin) then
          errors += s"Edge has origin not part of this tiling: ${edge.origin.id}"

        edge.twin match
          case None       =>
            errors += s"Edge from ${edge.origin.id} has no twin edge"
          case Some(twin) =>
            if !edgeSet.contains(twin) then
              errors += s"Edge from ${edge.origin.id} twin is not part of this tiling"
            else
              if twin eq edge then
                errors += s"Edge from ${edge.origin.id} has itself as twin"
              if !twin.twin.contains(edge) then
                errors += s"Edge from ${edge.origin.id} twin relationship is not symmetric"

        edge.next match
          case None       =>
            errors += s"Edge from ${edge.origin.id} has no next edge"
          case Some(next) =>
            if !edgeSet.contains(next) then
              errors += s"Edge from ${edge.origin.id} next edge is not part of this tiling"
            if !next.prev.contains(edge) then
              errors +=
                s"Next/prev relationship broken: $edge has next edge $next which has prev edge ${next.prev}"

        edge.prev.foreach: prev =>
          if !edgeSet.contains(prev) then
            errors += s"Edge from ${edge.origin.id} prev edge is not part of this tiling"
          else if !prev.next.contains(edge) then
            errors +=
              s"Next/prev relationship broken: $edge has prev edge $prev which has next edge ${prev.next}"

        edge.incidentFace.foreach: face =>
          if !((face eq tiling.outerFace) || innerFaceSet.contains(face)) then
            errors +=
              s"Edge from ${edge.origin.id} references incident face not part of this tiling: ${face.id}"
          else
            // Ensure the face actually "owns" this edge
            val isReachable =
              face.halfEdges match
                case Left(_)          => false
                case Right(halfEdges) => halfEdges.contains(edge)
            if !isReachable then
              errors +=
                s"Edge from ${edge.origin.id} claims to be in face ${face.id}, but is not reachable from face components"

      // Check face consistency
      tiling.faces.foreach: face =>
        face.halfEdges match
          case Left(error)  => errors += s"Face ${face.id} has a broken edge cycle: $error"
          case Right(edges) =>
            edges.foreach: edge =>
              if !edge.incidentFace.contains(face) then
                errors +=
                  s"Face consistency error: $face contains $edge which references back to another incident ${edge.incidentFace}"
              if !edgeSet.contains(edge) then
                errors += s"Face ${face.id} cycle includes an edge not part of this tiling"

      // Ensure all edges that claim to be on the outer face are covered by the outer face traversal
      val outerEdgesClaimed        =
        tiling.halfEdges
          .filter: halfEdge =>
            halfEdge.hasIncidentFace(tiling.outerFace)
          .toSet
      val maybeOuterTraversalEdges =
        tiling.outerFace.outerComponent.map: face =>
          face.faceTraversal()
      maybeOuterTraversalEdges match
        case None if outerEdgesClaimed.nonEmpty =>
          errors += "Outer face traversal failed: no traversal found"
        case None                               => ()
        case Some(Left(outerTraversalError))    =>
          errors += "Outer face traversal failed: " + outerTraversalError
        case Some(Right(outerTraversalEdges))   =>
          if outerEdgesClaimed.diff(outerTraversalEdges.toSet).nonEmpty then
            errors += "Outer face has edges not reachable from its outer component"

      // This is specific to the tessellation we want, without holes, because holes are just other inner polygons
      if tiling.innerFaces.exists: face =>
          face.hasHoles
      then
        errors += "Face with inner holes"

  /** Angle constraints: no half-edge carries a full-circle interior angle; every inner face's angle vector
    * closes to a simple polygon; the tiling boundary closes in both the interior and exterior angle views;
    * every interior vertex's incident angles sum to a full circle. Returns a [[GeometryError]] on failure.
    *
    * Topology errors on inner-face / boundary cycles are ignored here — they are surfaced by
    * [[validateTopologically]] and are the caller's responsibility to resolve first.
    */
  private[dcel] def validateGeometrically(tiling: TilingDCEL): Either[TilingError, Unit] =
    collect(ErrorCategory.Geometry): errors =>
      // Disallow full-circle or invalid angles on any half-edge
      tiling.halfEdges.foreach: halfEdge =>
        halfEdge.angle.foreach: angleDegree =>
          if angleDegree.isFullCircle then
            errors +=
              s"Edge from ${halfEdge.origin.id} cannot have full circles as interior angles: $angleDegree"

      // Check angles' sum for each inner face
      tiling.innerFaces.foreach: face =>
        face.halfEdges match
          case Right(edges) =>
            val angles =
              edges.flatMap: halfEdge =>
                halfEdge.angle
            SimplePolygon.fromUntrusted(angles.toVector) match
              case Left(error) =>
                errors += s"Face ${face.id.toPrefixedString} has an invalid polygon: ${error.message}"
              case Right(_)    => ()
          case Left(_)      => // NOTE: topological error, handled in validateTopologically

      // Check angles' sum for the tiling boundary (interior view)
      tiling.boundaryVertices match
        case Right(boundaryVertices) if boundaryVertices.length >= 3 =>
          val boundaryAngles             =
            boundaryVertices
              .map: vertex =>
                vertex.currentInteriorAngleSum(tiling.outerFace)
              .toList
          val (angleErrors, angleValues) = boundaryAngles.partitionMap(identity)
          if angleErrors.nonEmpty then
            angleErrors.foreach: error =>
              errors += s"Boundary angles calculation failed: $error"
          else
            SimplePolygon.fromUntrusted(angleValues.toVector) match
              case Left(error) => errors += s"Boundary angles sum is incorrect: ${error.message}"
              case Right(_)    => ()
        case Left(_)                                                 => // NOTE: topological error
        case _                                                       => // Not enough vertices to form a polygon

      // Check angles' sum for the tiling boundary (exterior view)
      tiling.boundaryEdges match
        case Right(boundaryEdges) if boundaryEdges.length >= 3 =>
          val boundaryAngles =
            boundaryEdges.flatMap: halfEdge =>
              halfEdge.angle
          if boundaryAngles.exists: angleDegree =>
              angleDegree.isFullCircle
          then
            errors += s"Full circle boundary angles are invalid: ${boundaryAngles.mkString("; ")}"
          else
            SimplePolygon.fromUntrusted(boundaryAngles.map(_.conjugate).toVector) match
              case Left(error) => errors += s"Boundary angles sum is incorrect: ${error.message}"
              case Right(_)    => ()
        case Left(_)                                           => // NOTE: topological error
        case _                                                 => // Not enough edges

      // Check angles' sum for each interior vertex
      val boundaryVertices = tiling.boundaryVerticesUnsafe.toSet
      val interiorVertices = tiling.vertices.filterNot(boundaryVertices.contains)
      interiorVertices.foreach: vertex =>
        tiling.getAnglesAtVertex(vertex.id) match
          case Right(angles) =>
            val sum = angles.sumExact
            if !sum.isFullCircle then
              errors += s"Angles around interior vertex ${vertex.id} do not sum to a full circle: $sum."
          case Left(error)   =>
            errors += s"Could not validate angles for interior vertex ${vertex.id} due to: $error"

  /** Coordinate-level checks: the boundary forms a simple polygon (sweep-line simplicity test); no two
    * vertices share a position (within `ACCURACY`); every edge has unit length (within `1e-9`); no two edges
    * properly intersect. Returns a [[SpatialError]] on failure.
    */
  private[dcel] def validateSpatially(tiling: TilingDCEL): Either[TilingError, Unit] =
    collect(ErrorCategory.Spatial): errors =>
      SimplePolygon.fromUntrusted(tiling.boundarySimplePolygonUnsafe.toAngles) match
        case Left(SpatialError(message)) => errors += s"Coordinates: boundary not a simple polygon. $message"
        case _                           => ()

      if !tiling.vertices.map(_.coords).hasNoAlmostEqualPoints() then
        errors += "Coordinates: vertices in the same position"

      // Optional: unit-length sides check (edge-to-twin origin distance)
      // Only check when both endpoints are available
      tiling.halfEdges.foreach: halfEdge =>
        val edgeOrigin      = halfEdge.origin.coords
        val maybeTwinOrigin =
          halfEdge.twin.map: twinHalfEdge =>
            twinHalfEdge.origin.coords
        maybeTwinOrigin.foreach: twinOrigin =>
          val d = edgeOrigin.distanceTo(twinOrigin)
          // Allow small tolerance
          if (d - 1.0).abs > 1e-9 then
            errors += s"Edge from ${halfEdge.origin.id} does not have unit length: $d"

      // NEW: Check for self-intersections (overlapping faces)
      // We only need to check one half-edge per twin pair to detect overlaps
      val uniqueEdges = mutable.Set.empty[HalfEdge]
      val segments    =
        tiling.halfEdges
          .flatMap: halfEdge =>
            if uniqueEdges.contains(halfEdge) then
              None
            else
              halfEdge.twin.foreach: halfEdge =>
                uniqueEdges.add(halfEdge)
              Some(BigLineSegment(halfEdge.origin.coords, halfEdge.destinationUnsafe.coords))
          .toVector

      if IntersectionDetection.hasSelfIntersection(segments) then
        errors += "Spatial intersection: some edges properly intersect, suggesting overlapping faces"

  /** Run the full validation pipeline against `tiling`.
    *
    *   1. [[validateCompleteness]] runs first; on failure, returns immediately.
    *   2. [[validateTopologically]], [[validateGeometrically]] and [[validateSpatially]] then run in
    *      sequence; their failures are collected.
    *
    * @return
    *   `Right(())` if all stages pass; otherwise `Left`:
    *   - [[IncompleteError]] if step 1 fails (later steps are skipped)
    *   - The category-specific error ([[TopologyError]] / [[GeometryError]] / [[SpatialError]]) when exactly
    *     one later stage fails
    *   - [[ValidationError]] wrapping a "Multiple validation errors: …" message that lists every underlying
    *     failure when more than one later stage fails
    */
  def validate(tiling: TilingDCEL): Either[TilingError, Unit] =
    validateCompleteness(tiling).flatMap: _ =>
      val topoErrors  = validateTopologically(tiling).left.toOption.map(_.message)
      val geoErrors   = validateGeometrically(tiling).left.toOption.map(_.message)
      val spaceErrors = validateSpatially(tiling).left.toOption.map(_.message)
      val allErrors   = topoErrors.toList ::: geoErrors.toList ::: spaceErrors.toList
      if allErrors.isEmpty then Right(())
      else Left(TilingError.combineErrors(allErrors, ErrorCategory.Validation))
