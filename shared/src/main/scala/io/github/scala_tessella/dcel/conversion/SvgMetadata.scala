package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.Utils.{associateValues, sequence, traverse}
import io.github.scala_tessella.dcel.conversion.SvgDsl.{attrs, tag}
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.dcel.{NotFoundError, Tiling, TilingDCEL, TilingError, ValidationError}
import spire.math.Rational

import scala.util.Try

/** Metadata round-trip: serialises a [[TilingDCEL]] into embeddable XML and reconstructs it from the same
  * format. This is an I/O boundary and is kept separate from [[TilingSVG]] (rendering) and [[SvgDsl]] (XML
  * primitives).
  */
private[conversion] object SvgMetadata:

  private[conversion] val TagRe  = """<\s*([\w\-]+)\b([^>]*?)(/?)>""".r
  private[conversion] val AttrRe = """([A-Za-z_][\w\-]*)\s*=\s*"([^"]*)"""".r

  /** Serializes the complete structure of a [[TilingDCEL]] into XML metadata, which can be embedded within an
    * SVG. This metadata includes all vertices, half-edges, and faces, along with their properties and
    * relationships, ensuring that the [[TilingDCEL]] can be fully reconstructed later.
    */
  def toMetadataXml(tiling: TilingDCEL): String =

    val halfEdgeIds: Map[HalfEdge, Int] = tiling.halfEdges.zipWithIndex.toMap

    val vertexNodes  = tiling.vertices.map: vertex =>
      val attrsList = List(
        Some("id" -> vertex.id.toPrefixedString),
        Some("x"  -> vertex.coords.x.toString),
        Some("y"  -> vertex.coords.y.toString),
        vertex.leaving
          .flatMap: halfEdge =>
            halfEdgeIds.get(halfEdge)
          .map: id =>
            "leaving" -> id
      ).flatten
      tag("vertex", attrs(attrsList*), selfClosing = true)
    val verticesElem = tag("vertices", children = vertexNodes)

    val halfEdgeNodes =
      tiling.halfEdges.zipWithIndex
        .map: (halfEdge, id) =>
          val attrsList = List(
            Some("id"     -> id),
            Some("origin" -> halfEdge.origin.id.toPrefixedString),
            halfEdge.twin
              .flatMap: twinHalfEdge =>
                halfEdgeIds.get(twinHalfEdge)
              .map: twinId =>
                "twin" -> twinId,
            halfEdge.next
              .flatMap: nextHalfEdge =>
                halfEdgeIds.get(nextHalfEdge)
              .map: nextId =>
                "next" -> nextId,
            halfEdge.prev
              .flatMap: prevHalfEdge =>
                halfEdgeIds.get(prevHalfEdge)
              .map: prevId =>
                "prev" -> prevId,
            halfEdge.incidentFace.map: face =>
              "face" -> face.id.toPrefixedString,
            halfEdge.angle.map: angleDegree =>
              "angle" -> angleDegree.toRational
          ).flatten
          tag("half-edge", attrs(attrsList*), selfClosing = true)
    val halfEdgesElem = tag("half-edges", children = halfEdgeNodes)

    val faceNodes = tiling.faces.map: f =>
      val attrsList =
        List(
          Some("id" -> f.id.toPrefixedString),
          f.outerComponent
            .flatMap: halfEdge =>
              halfEdgeIds.get(halfEdge)
            .map: id =>
              "outer-component" -> id
        ).flatten
      val innerIds  =
        f.innerComponents
          .flatMap: maybeHalfEdge =>
            maybeHalfEdge.flatMap: halfEdge =>
              halfEdgeIds.get(halfEdge)
      val allAttrs  = if innerIds.nonEmpty then
        attrsList :+ ("inner-components" -> innerIds.mkString(","))
      else
        attrsList
      tag("face", attrs(allAttrs*), selfClosing = true)
    val facesElem = tag("faces", children = faceNodes)

    tag(
      "tessella:tessella-dcel",
      attrs("xmlns:tessella" -> "https://github.com/scala-tessella/tessella"),
      children = Seq(verticesElem, halfEdgesElem, facesElem)
    )

  /** Deserializes the XML metadata that includes all vertices, half-edges, and faces, along with their
    * properties and relationships, and fully reconstructs the complete structure of a [[TilingDCEL]].
    */
  def fromMetadata(metadata: String): Either[TilingError, Tiling] =
    // Optimized attribute parsing: single pass into a mutable Map
    def parseAttrs(attrStr: String): Map[String, String] =
      val m = Map.newBuilder[String, String]
      AttrRe.findAllMatchIn(attrStr).foreach: mat =>
        m += (mat.group(1) -> mat.group(2))
      m.result()

    // Check presence once
    if !metadata.contains("<vertices") || !metadata.contains("<half-edges") || !metadata.contains("<faces")
    then
      return Left(ValidationError("Required metadata tags (<vertices>, <half-edges>, <faces>) not found"))

    // Single pass to extract all attributes by tag type
    val emptyMap  = Map.empty[String, List[Map[String, String]]].withDefaultValue(Nil)
    val extracted =
      TagRe
        .findAllMatchIn(metadata)
        .foldLeft(emptyMap): (acc, m) =>
          val tagName = m.group(1)
          if tagName == "vertex" || tagName == "half-edge" || tagName == "face" then
            acc.updated(tagName, parseAttrs(m.group(2)) :: acc(tagName))
          else acc

    val vertexAttrs   = extracted("vertex").reverse
    val halfEdgeAttrs = extracted("half-edge").reverse
    val faceAttrs     = extracted("face").reverse

    reconstructDCEL(vertexAttrs, halfEdgeAttrs, faceAttrs)

  private def reconstructDCEL(
      vertexAttrs: List[Map[String, String]],
      halfEdgeAttrs: List[Map[String, String]],
      faceAttrs: List[Map[String, String]]
  ): Either[TilingError, Tiling] =
    def getAttr(attrs: Map[String, String], owner: String, name: String): Either[ValidationError, String] =
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

    for
      vertices <- vertexAttrs
                    .map: attrs =>
                      for
                        id <- attrAs(attrs, "vertex", "id", VertexId.fromStringUnsafe, "VertexId")
                        x  <- attrAs(attrs, "vertex", "x", BigDecimal.apply, "BigDecimal")
                        y  <- attrAs(attrs, "vertex", "y", BigDecimal.apply, "BigDecimal")
                      yield Vertex(id, BigPoint(x, y))
                    .sequence
      vertexMap = vertices.associateValues:
                    _.id

      heIdAndAttrs <- halfEdgeAttrs
                        .map: attrs =>
                          for id <- attrAs(attrs, "half-edge", "id", _.toInt, "Int")
                          yield id -> attrs
                        .sequence
      heAllocated  <- heIdAndAttrs
                        .map: (id, attrs) =>
                          for
                            originId <-
                              attrAs(attrs, "half-edge", "origin", VertexId.fromStringUnsafe, "VertexId")
                            origin   <- vertexMap.get(originId).toRight(
                                          NotFoundError("Vertex for half-edge origin", originId.toString)
                                        )
                          yield id -> HalfEdge(origin)
                        .sequence
      halfEdgeMap   = heAllocated.toMap
      halfEdges     = heAllocated
                        .sortBy((id, _) => id)
                        .map((_, halfEdge) => halfEdge)

      faces  <- faceAttrs.map(attrs =>
                  for id <- attrAs(attrs, "face", "id", FaceId.fromStringUnsafe, "FaceId")
                  yield Face(id)
                ).sequence
      faceMap = faces.associateValues:
                  _.id

      _ <- vertexAttrs
             .zip(vertices)
             .map: (attrs, vertex) =>
               attrs.get("leaving").traverse: leavingIdStr =>
                 for
                   leavingId   <- Try(leavingIdStr.toInt).toEither.left.map: _ =>
                                    ValidationError(s"Invalid leaving ID: $leavingIdStr")
                   leavingEdge <- halfEdgeMap.get(leavingId).toRight(
                                    NotFoundError("Leaving edge", leavingId.toString)
                                  )
                 yield vertex.leaving = Some(leavingEdge)
             .sequence

      _ <- heIdAndAttrs.map((id, attrs) =>
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
               faceId       <- attrAs(attrs, "half-edge", "face", FaceId.fromStringUnsafe, "FaceId")
               incidentFace <- faceMap.get(faceId).toRight(
                                 NotFoundError("Incident face", faceId.toString)
                               )
               _             = he.incidentFace = Some(incidentFace)
               angleStr     <- getAttr(attrs, "half-edge", "angle")
               _             = he.angle = Some(AngleDegree(Rational(angleStr)))
             yield ()
           ).sequence

      _ <- faceAttrs.zip(faces)
             .map: (attrs, f) =>
               for
                 _ <- attrs.get("outer-component").traverse(ocIdStr =>
                        for
                          ocId   <- Try(ocIdStr.toInt).toEither.left.map: _ =>
                                      ValidationError(s"Invalid outer-component ID: $ocIdStr")
                          ocEdge <- halfEdgeMap.get(ocId).toRight(
                                      NotFoundError("Outer component edge", ocId.toString)
                                    )
                        yield f.outerComponent = Some(ocEdge)
                      )
                 _ <- attrs.get("inner-components")
                        .filter:
                          _.nonEmpty
                        .traverse: icIdsStr =>
                          for
                            ids     <- icIdsStr.split(',').toList
                                         .map: idStr =>
                                           Try(idStr.trim.toInt).toEither.left.map: _ =>
                                             ValidationError(s"Invalid inner-component ID: $idStr")
                                         .sequence
                            icEdges <- ids
                                         .map: id =>
                                           halfEdgeMap.get(id).toRight(
                                             NotFoundError("Inner component edge", id.toString)
                                           )
                                         .sequence
                          yield f.innerComponents = icEdges.map:
                            Some(_)
               yield ()
             .sequence

      outerFace <- faceMap.get(FaceId.outerId).toRight(
                     ValidationError("Outer face (ID 0) not found in metadata")
                   )
      innerFaces = faces.filterNot:
                     _.id == FaceId.outerId
      tiling     = TilingDCEL.fromUntrusted(vertices, halfEdges, innerFaces, outerFace)
      validated <- if vertices.isEmpty && halfEdges.isEmpty && innerFaces.isEmpty then
        Right(Tiling.empty)
      else tiling
    yield validated
