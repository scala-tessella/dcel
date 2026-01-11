package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.Utils.{associateValues, sequence, traverse}
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.structure._
import io.github.scala_tessella.dcel.{NotFoundError, TilingDCEL, TilingError, ValidationError}
import spire.math.Rational

import scala.util.Try
import scala.xml.{Node, XML}

object TilingSVGPlatform:

  def fromMetadata(metadata: String): Either[TilingError, TilingDCEL] =

    // Helper to extract attributes efficiently from a Node
    def getAttr(node: Node, attr: String): Either[ValidationError, String] =
      val seq = node.attribute(attr)
      if seq.isEmpty then Left(ValidationError(s"${node.label} missing '$attr'"))
      else Right(seq.head.text)

    def attrAs[T](node: Node, attr: String, f: String => T, typeName: String): Either[ValidationError, T] =
      getAttr(node, attr).flatMap: str =>
        Try(f(str)).toEither.left.map: e =>
          ValidationError(s"Invalid $typeName in ${node.label} attribute '$attr': ${e.getMessage}")

    for
      xmlRoot <-
        Try(XML.loadString(metadata)).toEither.left.map: e =>
          ValidationError(s"Invalid XML: ${e.getMessage}")

      // 1. Process Vertices in a single pass
      vertexNodes = (xmlRoot \ "vertices" \ "vertex").toList
      vertices   <-
        vertexNodes
          .map: vNode =>
            for
              id       <- attrAs(vNode, "id", _.toInt, "Int")
              x        <- attrAs(vNode, "x", BigDecimal.apply, "BigDecimal")
              y        <- attrAs(vNode, "y", BigDecimal.apply, "BigDecimal")
            yield Vertex(VertexId(id), BigPoint(x, y))
          .sequence

      vertexMap = vertices.associateValues(_.id)

      // 2. Process Half-Edges efficiently
      // We zip with index immediately to avoid repeated index lookups later
      halfEdgeNodes = (xmlRoot \ "half-edges" \ "half-edge").toList
      halfEdges    <-
        halfEdgeNodes
          .map: heNode =>
            for
              id       <- attrAs(heNode, "origin", _.toInt, "Int")
              origin   <- vertexMap.get(VertexId(id)).toRight(NotFoundError(
                            "Vertex for half-edge origin",
                            id.toString
                          ))
            yield HalfEdge(origin)
          .sequence

      // Use an array-backed lookup if possible, or a direct Map for speed
      halfEdgeMap =
        halfEdges
          .zipWithIndex
          .map: (he, i) =>
            i -> he
          .toMap

      // 3. Process Faces
      faceNodes = (xmlRoot \ "faces" \ "face").toList
      faces    <-
        faceNodes
          .map: fNode =>
            for
              id <- attrAs(fNode, "id", _.toInt, "Int")
            yield Face(FaceId(id))
          .sequence

      faceMap =
        faces.associateValues:
          _.id

      // 4. Wiring: Perform side-effecting wiring in grouped iterations
      _ <- vertexNodes
        .zip(vertices)
        .map: (vNode, vertex) =>
          vNode.attribute("leaving").map(_.text).traverse: leavingIdStr =>
            for
              leavingId   <- Try(leavingIdStr.toInt).toEither.left.map: _ =>
                ValidationError(s"Invalid leaving ID: $leavingIdStr")
              leavingEdge <-
                halfEdgeMap.get(leavingId).toRight(NotFoundError("Leaving edge", leavingId.toString))
            yield vertex.leaving = Some(leavingEdge)
        .sequence

      _ <-
        halfEdgeNodes
          .zip(halfEdges)
          .map: (heNode, he) =>
            for
              twinId       <- attrAs(heNode, "twin", _.toInt, "Int")
              twinEdge     <- halfEdgeMap.get(twinId).toRight(NotFoundError("Twin edge", twinId.toString))
              _             = he.twin = Some(twinEdge)
              nextId       <- attrAs(heNode, "next", _.toInt, "Int")
              nextEdge     <- halfEdgeMap.get(nextId).toRight(NotFoundError("Next edge", nextId.toString))
              _             = he.next = Some(nextEdge)
              prevId       <- attrAs(heNode, "prev", _.toInt, "Int")
              prevEdge     <- halfEdgeMap.get(prevId).toRight(NotFoundError("Prev edge", prevId.toString))
              _             = he.prev = Some(prevEdge)
              id           <- attrAs(heNode, "face", _.toInt, "Int")
              incidentFace <- faceMap.get(FaceId(id)).toRight(NotFoundError("Incident face", id.toString))
              _             = he.incidentFace = Some(incidentFace)
              angleStr     <- getAttr(heNode, "angle")
              _             = he.angle = Some(AngleDegree(Rational(angleStr)))
            yield ()
          .sequence

      _ <-
        faceNodes
          .zip(faces)
          .map: (fNode, f) =>
            for
              // optional outer-component
              _ <-
                fNode
                  .attribute("outer-component")
                  .map:
                    _.text
                  .traverse: ocIdStr =>
                    for
                      ocId   <- Try(ocIdStr.toInt).toEither.left.map: _ =>
                                  ValidationError(s"Invalid outer-component ID: $ocIdStr")
                      ocEdge <-
                        halfEdgeMap.get(ocId).toRight(NotFoundError(
                          "Outer component edge",
                          ocId.toString
                        ))
                    yield f.outerComponent = Some(ocEdge)
               // optional inner-components
              _ <-
                fNode
                  .attribute("inner-components")
                  .map:
                    _.text
                  .filter:
                    _.nonEmpty
                  .traverse: icIdsStr =>
                    val idsEither =
                      icIdsStr
                        .split(',').toList
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
      innerFaces =
        faces.filterNot:
          _.id == FaceId.outerId
      tiling     = TilingDCEL.fromUntrusted(vertices, halfEdges, innerFaces, outerFace)
      validated <-
        if vertices.isEmpty && halfEdges.isEmpty && innerFaces.isEmpty then
          Right(TilingDCEL.empty)
        else
          tiling
    yield validated
