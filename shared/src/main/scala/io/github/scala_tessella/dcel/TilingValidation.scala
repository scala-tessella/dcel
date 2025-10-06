package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint, SimplePolygon}

import scala.collection.mutable

object TilingValidation:

  def validateTopologically(tiling: TilingDCEL): Either[TilingError, Unit] =
    val errors = mutable.ListBuffer[String]()

    // Check vertex consistency
    tiling.vertices.foreach { vertex =>

      vertex.leaving match
        case None       => errors += s"Vertex ${vertex.id} has no leaving edge"
        case Some(edge) =>
          if edge.origin ne vertex then
            errors += s"Vertex ${vertex.id} leaving edge doesn't originate from it"
    }

    // Check half-edge consistency
    tiling.halfEdges.foreach { edge =>
      edge.twin match
        case None       => errors += s"Edge from ${edge.origin.id} has no twin"
        case Some(twin) =>
          if !twin.twin.contains(edge) then
            errors += s"Edge from ${edge.origin.id} twin relationship is not symmetric"

      edge.next match
        case None       => errors += s"Edge from ${edge.origin.id} has no next edge"
        case Some(next) =>
          if !next.prev.contains(edge) then
            errors += s"Next/prev relationship broken: $edge has next edge $next which has prev edge ${next.prev}"
    }

    // Check face consistency
    tiling.faces.foreach { face =>

      face.halfEdges match
        case Left(error)  => errors += s"Face ${face.id} has a broken edge cycle: $error"
        case Right(edges) =>
          edges.foreach { edge =>

            if !edge.incidentFace.contains(face) then
              errors += s"Face consistency error: $face contains $edge which references back to another incident ${edge.incidentFace}"
          }
    }

    if tiling.faces.exists(_.outerComponent.isEmpty) then
      errors += "Face with no outer component edge"

    // This is specific to the tessellation we want, without holes, because holes are just other inner polygons
    if tiling.innerFaces.exists(_.hasHoles) then
      errors += "Face with inner holes"

    if errors.isEmpty then Right(()) else Left(TilingError.combineErrors(errors.toList, TilingError.topology))

  def validateGeometrically(tiling: TilingDCEL): Either[TilingError, Unit] =
    val errors = mutable.ListBuffer[String]()

    // Check if all half-edges have an angle
    if tiling.halfEdges.exists(_.angle.isEmpty) then
      return Left(ValidationError("Tiling has at least one half-edge with no angle defined."))

    // Check angles' sum for each inner face
    tiling.innerFaces.foreach { face =>

      face.halfEdges match
        case Right(edges) =>
          val angles = edges.flatMap(_.angle)
          if angles.length == edges.length && angles.length >= 3 then
            try
              val simple = SimplePolygon(angles.toVector)
            catch
              case e: IllegalArgumentException =>
                errors += s"Face ${face.id} has an invalid polygon: ${e.getMessage}"
        case Left(_)      => // NOTE: topological error, handled in validateTopologically
    }

    // Check angles' sum for the tiling boundary (interior view)
    tiling.boundaryVerticesSafer match
      case Right(boundaryVertices) if boundaryVertices.length >= 3 =>
        val boundaryAngles = boundaryVertices.map(_.currentInteriorAngleSum(tiling.outerFace)).toList
        if boundaryAngles.exists(_.isLeft) then
          boundaryAngles.filter(_.isLeft).map(_.swap.toOption.get).foreach(error =>
            errors += s"Boundary angles calculation failed: $error"
          )
        else
          try
            val simple = SimplePolygon(boundaryAngles.map(_.toOption.get).toVector)
          catch
            case e: IllegalArgumentException => errors += s"Boundary angles sum is incorrect: ${e.getMessage}"
      case Left(_)                                                 => // NOTE: topological error
      case _                                                       => // Not enough vertices to form a polygon

    // Check angles' sum for the tiling boundary (exterior view)
    tiling.boundaryEdgesSafer match
      case Right(boundaryEdges) if boundaryEdges.length >= 3 =>
        val boundaryAngles = boundaryEdges.flatMap(_.angle)
        if boundaryAngles.exists(_.isFullCircle) then
          errors += s"Full circle boundary angles are invalid: ${boundaryAngles.mkString("; ")}"
        else
          try
            val simple = SimplePolygon(boundaryAngles.map(_.conjugate).toVector)
          catch
            case e: IllegalArgumentException => errors += s"Boundary angles sum is incorrect: ${e.getMessage}"
      case Left(_)                                           => // NOTE: topological error
      case _                                                 => // Not enough edges

    // Check angles' sum for each interior vertex
    val boundaryVertices = tiling.boundaryVertices.toSet
    val interiorVertices = tiling.vertices.filterNot(boundaryVertices.contains)
    interiorVertices.foreach { vertex =>

      tiling.getAnglesAtVertex(vertex.id) match
        case Right(angles) =>
          val sum = angles.sumExact
          if !sum.isFullCircle then
            errors += s"Angles around interior vertex ${vertex.id} do not sum to a full circle: $sum."
        case Left(error)   =>
          errors += s"Could not validate angles for interior vertex ${vertex.id} due to: $error"
    }

    if errors.isEmpty then Right(()) else Left(TilingError.combineErrors(errors.toList, TilingError.geometry))

  def validateSpatially(tiling: TilingDCEL): Either[TilingError, Unit] =
    val errors = mutable.ListBuffer[String]()

    tiling.boundaryVerticesSafer match
      case Right(boundaryVertices) =>
        if boundaryVertices.length >= 3 then
          if !boundaryVertices.map(_.coords).toList.hasNoAlmostEqualPoints() then
            errors += "Coordinates: boundary with vertices in the same position"
      case Left(error)             => // NOTE: topological error, handled in validateTopologically

    if errors.isEmpty then Right(()) else Left(TilingError.combineErrors(errors.toList, TilingError.spatial))

  def validate(tiling: TilingDCEL): Either[TilingError, Unit] =
    val topoErrors  = validateTopologically(tiling).left.toOption.map(_.message)
    val geoErrors   = validateGeometrically(tiling).left.toOption.map(_.message)
    val spaceErrors = validateSpatially(tiling).left.toOption.map(_.message)
    val allErrors   = topoErrors.toList ++ geoErrors.toList ++ spaceErrors.toList
    if allErrors.isEmpty then Right(())
    else Left(TilingError.combineErrors(allErrors, TilingError.validation))
