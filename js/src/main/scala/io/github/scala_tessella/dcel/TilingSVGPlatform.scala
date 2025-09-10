package io.github.scala_tessella.dcel

import spire.math.Rational
import scala.util.Try

object TilingSVGPlatform:

  def fromMetadata(metadata: String): Either[TilingError, TilingDCEL] =

    extension [E, A](eithers: List[Either[E, A]])
      def sequence: Either[E, List[A]] =
        eithers.foldRight(Right(Nil): Either[E, List[A]]) { (e, acc) =>

          for
            xs <- acc
            x  <- e
          yield x :: xs
        }

    extension [A](opt: Option[A])
      def traverse[E, B](f: A => Either[E, B]): Either[E, Option[B]] =
        opt match
          case Some(a) => f(a).map(Some(_))
          case None    => Right(None)

    def openTags(xml: String, tag: String): List[String] =
      val tagRe = s"""<\\s*$tag\\b([^>]*?)(/?)>""".r
      val buf   = scala.collection.mutable.ListBuffer.empty[String]
      tagRe.findAllMatchIn(xml).foreach { m =>
        val attrs = m.group(1)
        val slash = m.group(2) // "/" if self-closing, "" otherwise
        // Count each logical element exactly once; both forms are fine
        buf += attrs
      }
      buf.toList

    def parseAttrs(attrStr: String): Map[String, String] =
      val attrRe = """([A-Za-z_][\w\-]*)\s*=\s*"([^"]*)"""".r
      attrRe.findAllMatchIn(attrStr).map(m => m.group(1) -> m.group(2)).toMap

    def getAttr(attrs: Map[String, String], owner: String, name: String)
        : Either[ValidationError, String] =
      attrs.get(name).filter(_.nonEmpty).toRight(ValidationError(s"$owner missing '$name'"))

    def attrAs[T](
        attrs: Map[String, String],
        owner: String,
        name: String,
        f: String => T,
        typeName: String
    ): Either[ValidationError, T] =
      for
        s <- getAttr(attrs, owner, name)
        r <- Try(f(s)).toEither.left.map(e =>
               ValidationError(s"Invalid $typeName in $owner attribute '$name': ${e.getMessage}")
             )
      yield r

    val vertexAttrStrs   = openTags(metadata, "vertex")
    val halfEdgeAttrStrs = openTags(metadata, "half-edge")
    val faceAttrStrs     = openTags(metadata, "face")

    if !metadata.contains("<vertices") then
      Left(ValidationError("<vertices> not found"))
    else if !metadata.contains("<half-edges") then
      Left(ValidationError("<half-edges> not found"))
    else if !metadata.contains("<faces") then
      Left(ValidationError("<faces> not found"))
    else

      val vertexAttrs    = vertexAttrStrs.map(parseAttrs)
      val halfEdgeAttrs0 = halfEdgeAttrStrs.map(parseAttrs)
      val faceAttrs      = faceAttrStrs.map(parseAttrs)

      // Build vertices
      for
        vertices <- vertexAttrs.map { attrs =>

                      for
                        id <- getAttr(attrs, "vertex", "id")
                        x  <- attrAs(attrs, "vertex", "x", BigDecimal.apply, "BigDecimal")
                        y  <- attrAs(attrs, "vertex", "y", BigDecimal.apply, "BigDecimal")
                      yield Vertex(VertexId(id), BigDecimalGeometry.BigPoint(x, y))
                    }.sequence
        vertexMap = vertices.map(v => v.id -> v).toMap

        // Build half-edges by explicit id, not by order
        heIdAndAttrs <- halfEdgeAttrs0.map { attrs =>

                          for id <- attrAs(attrs, "half-edge", "id", _.toInt, "Int")
                          yield id -> attrs
                        }.sequence
        // Allocate HalfEdge instances indexed by id (origin points to an existing vertex)
        heAllocated <- heIdAndAttrs.map { case (id, attrs) =>
                         for
                           originId <- getAttr(attrs, "half-edge", "origin")
                           origin   <- vertexMap.get(VertexId(originId)).toRight(NotFoundError(
                                         "Vertex for half-edge origin",
                                         originId
                                       ))
                         yield id -> HalfEdge(origin)
                       }.sequence
        halfEdgeMap  = heAllocated.toMap
        // Deterministic list to pass to DCEL; order by id
        halfEdges    = heAllocated.sortBy(_._1).map(_._2)

        // Faces
        faces <- faceAttrs.map { attrs =>

                   getAttr(attrs, "face", "id").map(id => Face(FaceId(id)))
                 }.sequence
        faceMap = faces.map(f => f.id -> f).toMap

        // Wire vertex.leaving (optional; id references halfEdgeMap)
        _ <- vertexAttrs.zip(vertices).map { case (attrs, vertex) =>
               attrs.get("leaving").traverse { leavingIdStr =>

                 for
                   leavingId   <- Try(leavingIdStr.toInt).toEither.left.map(_ =>
                                    ValidationError(s"Invalid leaving ID: $leavingIdStr")
                                  )
                   leavingEdge <- halfEdgeMap.get(leavingId).toRight(NotFoundError(
                                    "Leaving edge",
                                    leavingId.toString
                                  ))
                 yield vertex.leaving = Some(leavingEdge)
               }
             }.sequence

        // Wire half-edges using id lookups (twin/next/prev/face/angle)
        _ <- heIdAndAttrs.map { case (id, attrs) =>
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
                 faceId       <- getAttr(attrs, "half-edge", "face")
                 incidentFace <- faceMap.get(FaceId(faceId)).toRight(NotFoundError("Incident face", faceId))
                 _             = he.incidentFace = Some(incidentFace)
                 angleStr     <- getAttr(attrs, "half-edge", "angle")
                 angle         = BigDecimalGeometry.AngleDegree(Rational(angleStr))
                 _             = he.angle = Some(angle)
               yield ()
             }.sequence

        // Wire faces
        _ <- faceAttrs.zip(faces).map { case (attrs, f) =>
               for
                 // optional outer-component
                 _ <- attrs.get("outer-component").traverse { ocIdStr =>

                        for
                          ocId   <- Try(ocIdStr.toInt).toEither.left.map(_ =>
                                      ValidationError(s"Invalid outer-component ID: $ocIdStr")
                                    )
                          ocEdge <- halfEdgeMap.get(ocId).toRight(NotFoundError(
                                      "Outer component edge",
                                      ocId.toString
                                    ))
                        yield f.outerComponent = Some(ocEdge)
                      }
                 // optional inner-components
                 _ <- attrs.get("inner-components").filter(_.nonEmpty).traverse { icIdsStr =>
                        val idsEither = icIdsStr.split(',').toList.map(idStr =>
                          Try(idStr.trim.toInt).toEither.left.map(_ =>
                            ValidationError(s"Invalid inner-component ID: $idStr")
                          )
                        ).sequence
                        for
                          ids     <- idsEither
                          icEdges <-
                            ids.map(id =>
                              halfEdgeMap.get(id).toRight(NotFoundError("Inner component edge", id.toString))
                            ).sequence
                        yield f.innerComponents = icEdges.map(Some(_))
                      }
               yield ()
             }.sequence

        outerFace <-
          faceMap.get(FaceId.outerId).toRight(ValidationError("Outer face (ID 0) not found in metadata"))
        innerFaces = faces.filterNot(_.id == FaceId.outerId)
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
