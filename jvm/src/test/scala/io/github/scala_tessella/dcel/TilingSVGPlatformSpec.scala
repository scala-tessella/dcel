package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingSVG.*
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
      (faces.head \@ "id") shouldBe "F0",
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
          vertices.size shouldBe 3,
          allAssert(
            vertices.map { v =>

              allAssert(
                v.attribute("id") shouldBe defined,
                v.attribute("x") shouldBe defined,
                v.attribute("y") shouldBe defined,
                v.attribute("leaving") shouldBe defined
              )
            }*
          )
        )
      }, {
        val halfEdges = metadataXml \ "half-edges" \ "half-edge"
        allAssert(
          halfEdges.size shouldBe 6,
          allAssert(
            halfEdges.map { he =>

              allAssert(
                he.attribute("id") shouldBe defined,
                he.attribute("origin") shouldBe defined,
                he.attribute("twin") shouldBe defined,
                he.attribute("next") shouldBe defined,
                he.attribute("prev") shouldBe defined,
                he.attribute("face") shouldBe defined,
                he.attribute("angle") shouldBe defined
              )
            }*
          )
        )
      }, {
        val faces = metadataXml \ "faces" \ "face"
        allAssert(
          faces.size shouldBe 2,
          allAssert(
            faces.map { f =>

              f.attribute("id") shouldBe defined
            }*
          ),
          // All faces in a complete tiling must have an outer component
          faces.count(_.attribute("outer-component").isDefined) shouldBe 2
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
