package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.conversion.TilingSVG._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingSVGPlatformSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private val TagRe  = """<\s*([\w\-]+)\b([^>]*?)(/?)>""".r
  private val AttrRe = """([A-Za-z_][\w\-]*)\s*=\s*"([^"]*)"""".r

  private def extractAttrs(metadata: String, tagName: String): List[Map[String, String]] =
    def parseAttrs(attrStr: String): Map[String, String] =
      val m = Map.newBuilder[String, String]
      AttrRe.findAllMatchIn(attrStr).foreach: mat =>
        m += (mat.group(1) -> mat.group(2))
      m.result()

    TagRe
      .findAllMatchIn(metadata)
      .collect:
        case m if m.group(1) == tagName => parseAttrs(m.group(2))
      .toList

  behavior of "TilingSVG.toMetadata"

  it should "generate metadata for an empty tiling" in {
    val metadata  = emptyTiling.toMetadata
    val vertices  = extractAttrs(metadata, "vertex")
    val halfEdges = extractAttrs(metadata, "half-edge")
    val faces     = extractAttrs(metadata, "face")

    allAssert(
      metadata should include("""xmlns:tessella="https://github.com/scala-tessella/tessella""""),
      metadata should include("<tessella:tessella-dcel"),
      vertices.isEmpty shouldBe true,
      halfEdges.isEmpty shouldBe true,
      faces.size shouldBe 1,
      faces.head.get("id") shouldBe Some("0"),
      faces.head.get("outer-component") shouldBe None
    )
  }

  it should "generate correct metadata for a single triangle tiling" in {
    val metadata  = triangle.toMetadata
    val vertices  = extractAttrs(metadata, "vertex")
    val halfEdges = extractAttrs(metadata, "half-edge")
    val faces     = extractAttrs(metadata, "face")

    allAssert(
      metadata should include("""xmlns:tessella="https://github.com/scala-tessella/tessella""""),
      metadata should include("<tessella:tessella-dcel"), {
        allAssert(
          vertices.size shouldBe 3, {
            val assertions =
              vertices.map: vertex =>
                allAssert(
                  vertex.get("id") shouldBe defined,
                  vertex.get("x") shouldBe defined,
                  vertex.get("y") shouldBe defined,
                  vertex.get("leaving") shouldBe defined
                )
            allAssert(assertions*)
          }
        )
      }, {
        allAssert(
          halfEdges.size shouldBe 6, {
            val assertions =
              halfEdges.map: halfEdge =>
                allAssert(
                  halfEdge.get("id") shouldBe defined,
                  halfEdge.get("origin") shouldBe defined,
                  halfEdge.get("twin") shouldBe defined,
                  halfEdge.get("next") shouldBe defined,
                  halfEdge.get("prev") shouldBe defined,
                  halfEdge.get("face") shouldBe defined,
                  halfEdge.get("angle") shouldBe defined
                )
            allAssert(assertions*)
          }
        )
      }, {
        allAssert(
          faces.size shouldBe 2, {
            val assertions =
              faces.map: face =>
                face.get("id") shouldBe defined
            allAssert(assertions*)
          },
          // All faces in a complete tiling must have an outer component
          faces.count(_.get("outer-component").isDefined).shouldBe(2)
        )
      }
    )
  }

  it should "generate correct metadata for a single square tiling" in {
    val metadata  = square.toMetadata
    val vertices  = extractAttrs(metadata, "vertex")
    val halfEdges = extractAttrs(metadata, "half-edge")
    val faces     = extractAttrs(metadata, "face")

    allAssert(
      metadata should include("""xmlns:tessella="https://github.com/scala-tessella/tessella""""),
      metadata should include("<tessella:tessella-dcel"), {
        vertices.size shouldBe 4
      }, {
        halfEdges.size shouldBe 8
      }, {
        faces.size shouldBe 2
      }
    )
  }
