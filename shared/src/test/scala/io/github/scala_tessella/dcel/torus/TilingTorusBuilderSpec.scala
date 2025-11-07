package io.github.scala_tessella.dcel.torus

import io.github.scala_tessella.dcel.TilingTestHelpers
import io.github.scala_tessella.dcel.geometry.BigPoint
import io.github.scala_tessella.dcel.structure.VertexId
import io.github.scala_tessella.dcel.torus.TilingTorusBuilder.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingTorusBuilderSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  val torus2x2SquareNet: TilingTorusDCEL =
    createSquareNet(2, 2)

  behavior of "TilingTorusDCEL coordinates"

  it should "return the vertex coords of a 2x2 square net" in {
    torus2x2SquareNet.vertices.map(vertex => vertex.id -> vertex.coords).toMap shouldEqual
      Map(
        V1 -> BigPoint(0, 0),
        V2 -> BigPoint(1, 0),
        V3 -> BigPoint(0, 1),
        V4 -> BigPoint(1, 1)
      )
  }

  it should "return the vertex coords of a 3x3 square net" in {
    createSquareNet(3, 3).vertices.map(vertex => vertex.id -> vertex.coords).toMap shouldEqual
      Map(
        V1   -> BigPoint(0, 0),
        V2   -> BigPoint(1, 0),
        V3   -> BigPoint(2, 0),
        V4   -> BigPoint(0, 1),
        "V5" -> BigPoint(1, 1),
        "V6" -> BigPoint(2, 1),
        "V7" -> BigPoint(0, 2),
        "V8" -> BigPoint(1, 2),
        "V9" -> BigPoint(2, 2)
      )
  }

  it should "return the vertex coords of a 2x2 triangle net" in {
    createTriangleNet(2, 2).vertices.map(vertex => vertex.id -> vertex.coords).toMap shouldEqual
      Map(
        V1 -> BigPoint(0, 0),
        V2 -> BigPoint(1, 0),
        V3 -> BigPoint(0.5, 0.8660254037844386),
        V4 -> (1.5, 0.8660254037844386)
      )
  }

  it should "return the vertex coords of a 2x2 hexagon net" in {
    val expected = Map(
      V1             -> BigPoint(0, 0),
      V2             -> BigPoint(1, 0),
      V3             -> BigPoint(1.5, 0.8660254037844386),
      V4             -> BigPoint(1, 1.7320508075688773),
      VertexId("V5") -> BigPoint(0, 1.7320508075688773),
      VertexId("V6") -> BigPoint(-0.5, 0.8660254037844386),
      VertexId("V7") -> BigPoint(-0.5, -0.8660254037844386),
      VertexId("V8") -> BigPoint(1.5, 2.5980762113533159)
    )
    createHexagonNet(2, 2).vertices.forall(vertex =>
      expected(vertex.id).almostEquals(vertex.coords)
    ) shouldBe true
  }

