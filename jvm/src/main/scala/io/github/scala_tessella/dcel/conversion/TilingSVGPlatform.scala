package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.{TilingDCEL, TilingError, ValidationError}

import scala.util.Try
import scala.xml.{Node, XML}

object TilingSVGPlatform:

  def fromMetadata(metadata: String): Either[TilingError, TilingDCEL] =
    def nodeToMap(node: Node): Map[String, String] =
      node.attributes.asAttrMap

    for
      xmlRoot       <- Try(XML.loadString(metadata)).toEither.left.map: e =>
                         ValidationError(s"Invalid XML: ${e.getMessage}")
      vertexAttrs    = (xmlRoot \ "vertices" \ "vertex").toList.map:
                         nodeToMap
      halfEdgeAttrs  = (xmlRoot \ "half-edges" \ "half-edge").toList.map:
                         nodeToMap
      faceAttrs      = (xmlRoot \ "faces" \ "face").toList.map:
                         nodeToMap
      reconstructed <- TilingSVG.reconstructDCEL(vertexAttrs, halfEdgeAttrs, faceAttrs)
    yield reconstructed
