package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.Utils.{associateValues, sequence, traverse}
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.structure.*
import io.github.scala_tessella.dcel.{NotFoundError, TilingDCEL, TilingError, ValidationError}
import spire.math.Rational

import scala.util.Try

object TilingSVGPlatform:

  // Pre-compile Regexes outside the method to avoid re-compilation
  private val TagRe  = """<\s*([\w\-]+)\b([^>]*?)(/?)>""".r
  private val AttrRe = """([A-Za-z_][\w\-]*)\s*=\s*"([^"]*)"""".r

  def fromMetadata(metadata: String): Either[TilingError, TilingDCEL] =

    // Optimized attribute parsing: single pass into a mutable Map
    def parseAttrs(attrStr: String): Map[String, String] =
      val m = Map.newBuilder[String, String]
      AttrRe.findAllMatchIn(attrStr).foreach: mat =>
        m += (mat.group(1) -> mat.group(2))
      m.result()

    def getAttr(attrs: Map[String, String], owner: String, name: String)
        : Either[ValidationError, String] =
      attrs.get(name)
        .filter:
          _.nonEmpty
        .toRight(ValidationError(s"$owner missing '$name'"))

    def attrAs[T](
        attrs: Map[String, String],
        owner: String,
        name: String,
        f: String => T,
        typeName: String
    ): Either[ValidationError, T] =
      for
        s <- getAttr(attrs, owner, name)
        r <- Try(f(s)).toEither.left.map: e =>
               ValidationError(s"Invalid $typeName in $owner attribute '$name': ${e.getMessage}")
      yield r

    // Check presence once
    if !metadata.contains("<vertices") || !metadata.contains("<half-edges") || !metadata.contains("<faces")
    then
      return Left(ValidationError("Required metadata tags (<vertices>, <half-edges>, <faces>) not found"))

    // Single pass to extract all attributes by tag type
    val extracted = TagRe.findAllMatchIn(metadata).foldLeft(Map.empty[
      String,
      List[Map[String, String]]
    ].withDefaultValue(Nil)): (acc, m) =>
      val tagName = m.group(1)
      if tagName == "vertex" || tagName == "half-edge" || tagName == "face" then
        acc.updated(tagName, parseAttrs(m.group(2)) :: acc(tagName))
      else acc

    val vertexAttrs    = extracted("vertex").reverse
    val halfEdgeAttrs0 = extracted("half-edge").reverse
    val faceAttrs      = extracted("face").reverse

    // Build vertices
    for
      vertices <- vertexAttrs
                    .map: attrs =>
                      for
                        id <- attrAs(attrs, "vertex", "id", _.toInt, "Int")
                        x  <- attrAs(attrs, "vertex", "x", BigDecimal.apply, "BigDecimal")
                        y  <- attrAs(attrs, "vertex", "y", BigDecimal.apply, "BigDecimal")
                      yield Vertex(VertexId(id), BigPoint(x, y))
                    .sequence
      vertexMap = vertices.associateValues: vertex =>
                    vertex.id

      // Build half-edges by explicit id, not by order
      heIdAndAttrs <- halfEdgeAttrs0
                        .map: attrs =>
                          for id <- attrAs(attrs, "half-edge", "id", _.toInt, "Int")
                          yield id -> attrs
                        .sequence
      // Allocate HalfEdge instances indexed by id (origin points to an existing vertex)
      heAllocated  <- heIdAndAttrs
                        .map: (id, attrs) =>
                          for
                            originId <- attrAs(attrs, "half-edge", "origin", _.toInt, "Int")
                            origin   <- vertexMap.get(VertexId(originId)).toRight(NotFoundError(
                                          "Vertex for half-edge origin",
                                          originId.toString
                                        ))
                          yield id -> HalfEdge(origin)
                        .sequence
      halfEdgeMap   = heAllocated.toMap
      // Deterministic list to pass to DCEL; order by id
      halfEdges     = heAllocated
                        .sortBy: (id, _) =>
                          id
                        .map: (_, halfEdge) =>
                          halfEdge

      // Faces
      faces  <- faceAttrs
                  .map: attrs =>
                    for
                      id <- attrAs(attrs, "face", "id", _.toInt, "Int")
                    yield Face(FaceId(id))
                  .sequence
      faceMap = faces.associateValues(_.id)

      // Wire vertex.leaving (optional; id references halfEdgeMap)
      _ <- vertexAttrs.zip(vertices)
             .map: (attrs, vertex) =>
               attrs.get("leaving").traverse: leavingIdStr =>
                 for
                   leavingId <- Try(leavingIdStr.toInt).toEither.left.map: _ =>
                                  ValidationError(s"Invalid leaving ID: $leavingIdStr")

                   leavingEdge <- halfEdgeMap.get(leavingId).toRight(NotFoundError(
                                    "Leaving edge",
                                    leavingId.toString
                                  ))
                 yield vertex.leaving = Some(leavingEdge)
             .sequence

      // Wire half-edges using id lookups (twin/next/prev/face/angle)
      _ <- heIdAndAttrs
             .map: (id, attrs) =>
               val he = halfEdgeMap(id)
               for
                 twinId       <- attrAs(attrs, "half-edge", "twin", _.toInt, "Int")
                 twinEdge     <- halfEdgeMap.get(twinId).toRight(NotFoundError("Twin edge", twinId.toString))
                 _             = he.twin = Some(twinEdge)
                 nextId       <- attrAs(attrs, "half-edge", "next", _.toInt, "Int")
                 nextEdge     <- halfEdgeMap.get(nextId).toRight(NotFoundError("Next edge", nextId.toString))
                 _             = he.next = Some(nextEdge)
                 prevId       <- attrAs(attrs, "half-edge", "prev", _.toInt, "Int")
                 prevEdge     <- halfEdgeMap.get(prevId).toRight(NotFoundError("Prev edge", prevId.toString))
                 _             = he.prev = Some(prevEdge)
                 faceId       <- attrAs(attrs, "half-edge", "face", _.toInt, "Int")
                 incidentFace <-
                   faceMap.get(FaceId(faceId)).toRight(NotFoundError("Incident face", faceId.toString))
                 _             = he.incidentFace = Some(incidentFace)
                 angleStr     <- getAttr(attrs, "half-edge", "angle")
                 angle         = AngleDegree(Rational(angleStr))
                 _             = he.angle = Some(angle)
               yield ()
             .sequence

      // Wire faces
      _ <- faceAttrs.zip(faces)
             .map: (attrs, f) =>
               for
                 // optional outer-component
                 _ <- attrs.get("outer-component").traverse: ocIdStr =>
                        for
                          ocId   <- Try(ocIdStr.toInt).toEither.left.map: _ =>
                                      ValidationError(s"Invalid outer-component ID: $ocIdStr")
                          ocEdge <- halfEdgeMap.get(ocId).toRight(NotFoundError(
                                      "Outer component edge",
                                      ocId.toString
                                    ))
                        yield f.outerComponent = Some(ocEdge)
                 // optional inner-components
                 _ <- attrs.get("inner-components")
                        .filter:
                          _.nonEmpty
                        .traverse: icIdsStr =>
                          val idsEither = icIdsStr.split(',').toList
                            .map: idStr =>
                              Try(idStr.trim.toInt).toEither.left.map: _ =>
                                ValidationError(s"Invalid inner-component ID: $idStr")
                            .sequence
                          for
                            ids     <- idsEither
                            icEdges <-
                              ids
                                .map: id =>
                                  halfEdgeMap.get(id).toRight(NotFoundError(
                                    "Inner component edge",
                                    id.toString
                                  ))
                                .sequence
                          yield f.innerComponents =
                            icEdges.map: halfEdge =>
                              Some(halfEdge)
               yield ()
             .sequence

      outerFace <-
        faceMap.get(FaceId.outerId).toRight(ValidationError("Outer face (ID 0) not found in metadata"))
      innerFaces = faces.filterNot:
                     _.id == FaceId.outerId
//        // ---- DEBUG: dump wiring before constructing the DCEL (remove after diagnosing) ----
//        _          = {
//          def heIdOf(h: HalfEdge): Option[Int] =
//            halfEdgeMap.collectFirst { case (id, ref) if ref eq h => id }
//
//          val vDump = vertices.map { v =>
//            val leavingId = v.leaving.flatMap(heIdOf).fold("None")(i => s"$i")
//            s"V(${v.id.value}) -> leaving=$leavingId (origin check: ${v.leaving.map(_.origin.id.value).getOrElse("-")})"
//          }.mkString("\n")
//
//          val heDump = heIdAndAttrs.sortBy(_._1).map { case (id, _) =>
//            val he     = halfEdgeMap(id)
//            val o      = he.origin.id.value
//            val twinId = he.twin.flatMap(heIdOf).fold("None")(i => s"$i")
//            val nextId = he.next.flatMap(heIdOf).fold("None")(i => s"$i")
//            val prevId = he.prev.flatMap(heIdOf).fold("None")(i => s"$i")
//            val faceId = he.incidentFace.map(_.id.value).getOrElse("None")
//            val ang    = he.angle.fold("None")(_ => "Some")
//            s"HE#$id origin=$o twin=$twinId next=$nextId prev=$prevId face=$faceId angle=$ang"
//          }.mkString("\n")
//
//          println(
//            "JS fromMetadata DEBUG:\n" +
//              s"vertices=${vertices.size}, halfEdges=${halfEdges.size}, faces=${faces.size}\n" +
//              "Vertices:\n" + vDump + "\n" +
//              "HalfEdges:\n" + heDump + "\n"
//          )
//        }
//        // ---- END DEBUG ----

      tiling     = TilingDCEL.fromUntrusted(vertices, halfEdges, innerFaces, outerFace)
      validated <-
        if vertices.isEmpty && halfEdges.isEmpty && innerFaces.isEmpty then
          Right(TilingDCEL.empty)
        else
          tiling
    yield validated
