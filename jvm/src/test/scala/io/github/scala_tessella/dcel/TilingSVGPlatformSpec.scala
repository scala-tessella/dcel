package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.conversion.TilingSVG._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingSVGPlatformSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingSVG.toMetadata"

  it should "generate metadata for an empty tiling" in {
    val metadataXml = scala.xml.XML.loadString(emptyTiling.toMetadata)
    val vertices    = metadataXml \ "vertices" \ "vertex"
    val halfEdges   = metadataXml \ "half-edges" \ "half-edge"
    val faces       = metadataXml \ "faces" \ "face"

    allAssert(
      metadataXml.scope.getURI("tessella") shouldBe "https://github.com/scala-tessella/tessella",
      metadataXml.label shouldBe "tessella-dcel",
      metadataXml.prefix shouldBe "tessella",
      vertices.isEmpty shouldBe true,
      halfEdges.isEmpty shouldBe true,
      faces.size shouldBe 1,
      (faces.head \@ "id") shouldBe "0",
      faces.head.attribute("outer-component").isEmpty shouldBe true
    )
  }

  it should "generate correct metadata for a single triangle tiling" in {
    val metadataXml = scala.xml.XML.loadString(triangle.toMetadata)

    allAssert(
      metadataXml.scope.getURI("tessella") shouldBe "https://github.com/scala-tessella/tessella",
      metadataXml.label shouldBe "tessella-dcel",
      metadataXml.prefix shouldBe "tessella", {
        val vertices = metadataXml \ "vertices" \ "vertex"
        allAssert(
          vertices.size shouldBe 3, {
            val assertions =
              vertices.map: vertex =>
                allAssert(
                  vertex.attribute("id") shouldBe defined,
                  vertex.attribute("x") shouldBe defined,
                  vertex.attribute("y") shouldBe defined,
                  vertex.attribute("leaving") shouldBe defined
                )
            allAssert(assertions*)
          }
        )
      }, {
        val halfEdges = metadataXml \ "half-edges" \ "half-edge"
        allAssert(
          halfEdges.size shouldBe 6, {
            val assertions =
              halfEdges.map: halfEdge =>
                allAssert(
                  halfEdge.attribute("id") shouldBe defined,
                  halfEdge.attribute("origin") shouldBe defined,
                  halfEdge.attribute("twin") shouldBe defined,
                  halfEdge.attribute("next") shouldBe defined,
                  halfEdge.attribute("prev") shouldBe defined,
                  halfEdge.attribute("face") shouldBe defined,
                  halfEdge.attribute("angle") shouldBe defined
                )
            allAssert(assertions*)
          }
        )
      }, {
        val faces = metadataXml \ "faces" \ "face"
        allAssert(
          faces.size shouldBe 2, {
            val assertions =
              faces.map: face =>
                face.attribute("id") shouldBe defined
            allAssert(assertions*)
          },
          // All faces in a complete tiling must have an outer component
          faces
            .count: face =>
              face.attribute("outer-component").isDefined
            .shouldBe(2)
        )
      }
    )
  }

  it should "generate correct metadata for a single square tiling" in {
    val metadataXml = scala.xml.XML.loadString(square.toMetadata)

    allAssert(
      metadataXml.scope.getURI("tessella") shouldBe "https://github.com/scala-tessella/tessella",
      metadataXml.label shouldBe "tessella-dcel",
      metadataXml.prefix shouldBe "tessella", {
        val vertices = metadataXml \ "vertices" \ "vertex"
        vertices.size shouldBe 4
      }, {
        val halfEdges = metadataXml \ "half-edges" \ "half-edge"
        halfEdges.size shouldBe 8
      }, {
        val faces = metadataXml \ "faces" \ "face"
        faces.size shouldBe 2
      }
    )
  }
