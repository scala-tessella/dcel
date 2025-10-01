package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geo.AngleDegree
import io.github.scala_tessella.dcel.BigDecimalGeometry.BigPoint
import spire.math.Rational
import scala.util.Try
import scala.xml.{Node, XML}

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

    def getAttr(node: Node, attr: String): Either[ValidationError, String] =
      node.attribute(attr).map(_.text).toRight(ValidationError(s"${node.label} missing '$attr'"))

    def attrAs[T](node: Node, attr: String, f: String => T, typeName: String): Either[ValidationError, T] =
      for {
        str <- getAttr(node, attr)
        res <- Try(f(str)).toEither.left.map(e =>
                 ValidationError(s"Invalid $typeName in ${node.label} attribute '$attr': ${e.getMessage}")
               )
      } yield res

    for {
      xmlRoot <-
        Try(XML.loadString(metadata)).toEither.left.map(e => ValidationError(s"Invalid XML: ${e.getMessage}"))

      vertexNodes = (xmlRoot \ "vertices" \ "vertex").toList
      vertices   <- vertexNodes.map { vNode =>

                      for {
                        id <- getAttr(vNode, "id")
                        x  <- attrAs(vNode, "x", BigDecimal.apply, "BigDecimal")
                        y  <- attrAs(vNode, "y", BigDecimal.apply, "BigDecimal")
                      } yield Vertex(VertexId(id), BigPoint(x, y))
                    }.sequence
      vertexMap = vertices.map(v => v.id -> v).toMap

      halfEdgeNodes = (xmlRoot \ "half-edges" \ "half-edge").toList
      halfEdges    <- halfEdgeNodes.map { heNode =>

                        for {
                          originId <- getAttr(heNode, "origin")
                          origin   <- vertexMap.get(VertexId(originId)).toRight(NotFoundError(
                                        "Vertex for half-edge origin",
                                        originId
                                      ))
                        } yield HalfEdge(origin)
                      }.sequence
      halfEdgeMap = halfEdges.zipWithIndex.map { case (he, i) =>
                      i -> he
                    }.toMap

      faceNodes = (xmlRoot \ "faces" \ "face").toList
      faces    <- faceNodes.map { fNode =>

                    getAttr(fNode, "id").map(id => Face(FaceId(id)))
                  }.sequence
      faceMap = faces.map(f => f.id -> f).toMap

      _ <- vertexNodes.zip(vertices).map { case (vNode, vertex) =>
             vNode.attribute("leaving").map(_.text).traverse { leavingIdStr =>

               for {
                 leavingId   <- Try(leavingIdStr.toInt).toEither.left.map(_ =>
                                  ValidationError(s"Invalid leaving ID: $leavingIdStr")
                                )
                 leavingEdge <-
                   halfEdgeMap.get(leavingId).toRight(NotFoundError("Leaving edge", leavingId.toString))
               } yield vertex.leaving = Some(leavingEdge)
             }
           }.sequence

      _ <- halfEdgeNodes.zip(halfEdges).map { case (heNode, he) =>
             for {
               twinId       <- attrAs(heNode, "twin", _.toInt, "Int")
               twinEdge     <- halfEdgeMap.get(twinId).toRight(NotFoundError("Twin edge", twinId.toString))
               _             = he.twin = Some(twinEdge)
               nextId       <- attrAs(heNode, "next", _.toInt, "Int")
               nextEdge     <- halfEdgeMap.get(nextId).toRight(NotFoundError("Next edge", nextId.toString))
               _             = he.next = Some(nextEdge)
               prevId       <- attrAs(heNode, "prev", _.toInt, "Int")
               prevEdge     <- halfEdgeMap.get(prevId).toRight(NotFoundError("Prev edge", prevId.toString))
               _             = he.prev = Some(prevEdge)
               faceId       <- getAttr(heNode, "face")
               incidentFace <- faceMap.get(FaceId(faceId)).toRight(NotFoundError("Incident face", faceId))
               _             = he.incidentFace = Some(incidentFace)
               angleStr     <- getAttr(heNode, "angle")
               angle         = AngleDegree(
                                 Rational(angleStr)
                               ) // .toRight(ValidationError(s"Invalid angle format: $angleStr"))
               _             = he.angle = Some(angle)
             } yield ()
           }.sequence

      _ <- faceNodes.zip(faces).map { case (fNode, f) =>
             for
               // optional outer-component
               _ <- fNode.attribute("outer-component").map(_.text).traverse { ocIdStr =>

                      for
                        ocId   <- Try(ocIdStr.toInt).toEither.left.map(_ =>
                                    ValidationError(s"Invalid outer-component ID: $ocIdStr")
                                  )
                        ocEdge <-
                          halfEdgeMap.get(ocId).toRight(NotFoundError("Outer component edge", ocId.toString))
                      yield f.outerComponent = Some(ocEdge)
                    }
               // optional inner-components
               _ <- fNode.attribute("inner-components").map(_.text).filter(_.nonEmpty).traverse { icIdsStr =>
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
      tiling     = TilingDCEL.fromUntrusted(vertices, halfEdges, innerFaces, outerFace)
      validated <-
        if vertices.isEmpty && halfEdges.isEmpty && innerFaces.isEmpty then
          Right(TilingDCEL.empty)
        else
          tiling
    } yield validated
