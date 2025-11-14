package io.github.scala_tessella.dcel.torus

import io.github.scala_tessella.dcel.TilingError
import io.github.scala_tessella.dcel.geometry.{AngleDegree, SimplePolygon}

import scala.collection.mutable

object TilingTorusValidation:

  def validateCompleteness(tiling: TilingTorusDCEL): Either[TilingError, Unit] =
    val errors = mutable.ListBuffer[String]()
    tiling.vertices.foreach { vertex =>

      if vertex.validate().isLeft then
        errors += vertex.toString
    }
    tiling.halfEdges.foreach { halfEdge =>

      if halfEdge.validate().isLeft then
        errors += halfEdge.toString
    }
    tiling.faces.foreach { face =>

      if face.validate().isLeft then
        errors += face.toString
    }
    if errors.isEmpty then Right(())
    else Left(TilingError.combineErrors(errors.toList, TilingError.incomplete))

  private def safeGet[A](opt: Option[A], msg: => String, acc: mutable.ListBuffer[String]): Option[A] =
    opt.orElse {
      acc += msg
      None
    }

  def validateTopologically(tiling: TilingTorusDCEL): Either[TilingError, Unit] =
    val errors = mutable.ListBuffer[String]()

    val vertexSet = tiling.vertices.toSet
    val edgeSet   = tiling.halfEdges.toSet
    val faceSet   = tiling.faces.toSet

    // Check vertex consistency (no unsafe .get)
    tiling.vertices.foreach { vertex =>

      vertex.leaving match
        case None       =>
          errors += s"Vertex ${vertex.id} missing leaving edge"
        case Some(edge) =>
          if edge.origin ne vertex then
            errors += s"Vertex ${vertex.id} leaving edge doesn't originate from it"
          if !edgeSet.contains(edge) then
            errors += s"Vertex ${vertex.id} leaving edge is not part of this tiling"
    }

    // Check half-edge consistency with guards
    tiling.halfEdges.foreach { edge =>
      if !vertexSet.contains(edge.origin) then
        errors += s"Edge has origin not part of this tiling: ${edge.origin.id}"

      val twinOpt = safeGet(edge.twin, s"Edge from ${edge.origin.id} missing twin", errors)
      twinOpt.foreach { twin =>

        if !edgeSet.contains(twin) then
          errors += s"Edge from ${edge.origin.id} twin is not part of this tiling"
        else
          if !safeGet(
              twin.twin,
              s"Twin of edge from ${edge.origin.id} missing back-reference",
              errors
            ).contains(edge)
          then
            errors += s"Edge from ${edge.origin.id} twin relationship is not symmetric"
      }

      val nextOpt = safeGet(edge.next, s"Edge from ${edge.origin.id} missing next", errors)
      nextOpt.foreach { next =>
        if !edgeSet.contains(next) then
          errors += s"Edge from ${edge.origin.id} next edge is not part of this tiling"
        if !safeGet(next.prev, s"Next edge from ${edge.origin.id} missing prev", errors).contains(edge) then
          errors += s"Next/prev relationship broken: $edge has next $next whose prev is ${next.prev}"
      }

      edge.prev.foreach { prev =>

        if !edgeSet.contains(prev) then
          errors += s"Edge from ${edge.origin.id} prev edge is not part of this tiling"
        else if !safeGet(prev.next, s"Prev edge into ${edge.origin.id} missing next", errors).contains(edge)
        then
          errors += s"Next/prev relationship broken: $edge has prev $prev whose next is ${prev.next}"
      }

      edge.incidentFace.foreach { f =>

        if !faceSet.contains(f) then
          errors += s"Edge from ${edge.origin.id} references incident face not part of this tiling: ${f.id}"
      }
    }

    // Check face consistency with safe traversal
    tiling.faces.foreach { face =>

      face.halfEdges match
        case Left(error)  => errors += s"Face ${face.id} has a broken edge cycle: $error"
        case Right(edges) =>
          // Detect repeated edges in face cycle (guard against infinite loops elsewhere)
          if edges.distinct.length != edges.length then
            errors += s"Face ${face.id} cycle repeats edges (possible malformed links)"
          edges.foreach { edge =>
            if !edge.incidentFace.contains(face) then
              errors += s"Face consistency error: $face contains $edge which references ${edge.incidentFace}"
            if !edgeSet.contains(edge) then
              errors += s"Face ${face.id} cycle includes an edge not part of this tiling"
          }
    }

    if errors.isEmpty then Right(())
    else Left(TilingError.combineErrors(errors.toList, TilingError.topology))

  def validateGeometrically(tiling: TilingTorusDCEL): Either[TilingError, Unit] =
    val errors = mutable.ListBuffer[String]()

    // Disallow full-circle or invalid angles on any half-edge
    tiling.halfEdges.foreach { e =>

      e.angle.foreach { a =>

        if a.isFullCircle then
          errors += s"Edge from ${e.origin.id} cannot have full circles as interior angles: $a"
      }
    }

    // Check face polygons
    tiling.faces.foreach { face =>

      face.halfEdges match
        case Right(edges) =>
          val angles = edges.flatMap(_.angle)
          if angles.length == edges.length && angles.length >= 3 then
            try
              val _ = SimplePolygon(angles.toVector)
            catch
              case e: IllegalArgumentException =>
                errors += s"Face ${face.id} has an invalid polygon: ${e.getMessage}"
        case Left(_)      => // topological errors already collected
    }

    // Angle sum at each vertex: collect all incident edges by origin (supports parallel edges; no traversal)
    tiling.vertices.foreach { vertex =>
      val incident = tiling.halfEdges.filter(_.origin eq vertex)
      val angles   = incident.flatMap(_.angle)
      if angles.size != incident.size then
        errors += s"Missing angle(s) at vertex ${vertex.id}"
      else
        val sum = angles.sumExact
        if !sum.isFullCircle then
          errors += s"Angles around interior vertex ${vertex.id} do not sum to a full circle: $sum."
    }

    if errors.isEmpty then Right(()) else Left(TilingError.combineErrors(errors.toList, TilingError.geometry))

  def validateSpatially(tiling: TilingTorusDCEL): Either[TilingError, Unit] =
    val errors = mutable.ListBuffer[String]()
    if errors.isEmpty then Right(())
    else Left(TilingError.combineErrors(errors.toList, TilingError.spatial))

  def validate(tiling: TilingTorusDCEL): Either[TilingError, Unit] =
    validateCompleteness(tiling) match
      case Left(value) => Left(value)
      case Right(_)    =>
        val topoErrors  = validateTopologically(tiling).left.toOption.map(_.message)
        val geoErrors   = validateGeometrically(tiling).left.toOption.map(_.message)
        val spaceErrors = validateSpatially(tiling).left.toOption.map(_.message)
        val allErrors   = topoErrors.toList ++ geoErrors.toList ++ spaceErrors.toList
        if allErrors.isEmpty then Right(())
        else Left(TilingError.combineErrors(allErrors, TilingError.validation))
