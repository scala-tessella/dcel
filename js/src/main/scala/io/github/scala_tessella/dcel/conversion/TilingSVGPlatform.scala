package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.{TilingDCEL, TilingError, ValidationError}

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

    val vertexAttrs   = extracted("vertex").reverse
    val halfEdgeAttrs = extracted("half-edge").reverse
    val faceAttrs     = extracted("face").reverse

    TilingSVG.reconstructDCEL(vertexAttrs, halfEdgeAttrs, faceAttrs)
