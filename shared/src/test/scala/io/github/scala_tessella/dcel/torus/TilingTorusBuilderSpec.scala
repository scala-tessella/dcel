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

  behavior of "TilingTorusBuilder.createSquareNet"

  it should "return the vertex coords of a 2x2 square net" in {
    torus2x2SquareNet.vertices.map(vertex => vertex.id -> vertex.coords).toMap shouldEqual
      Map(
        V1 -> (0, 0),
        V2 -> (1, 0),
        V3 -> (0, 1),
        V4 -> (1, 1)
      )
  }

  it should "return the vertex coords of a 3x3 square net" in {
    createSquareNet(3, 3).vertices.map(vertex => vertex.id -> vertex.coords).toMap shouldEqual
      Map(
        V1 -> (0, 0),
        V2 -> (1, 0),
        V3 -> (2, 0),
        V4 -> (0, 1),
        "V5" -> (1, 1),
        "V6" -> (2, 1),
        "V7" -> (0, 2),
        "V8" -> (1, 2),
        "V9" -> (2, 2)
      )
  }

  it should "compute all adjacent vertices in a torus" in {
    val torus = TilingTorusBuilder.createSquareNet(3, 3)
    torus.vertices.map { v =>

      v.id -> v.adjacentVerticesUnsafe.map(_.id)
    }.toMap shouldEqual
      Map(
        V1 -> List(V2, "V7", V3, V4),
        V2 -> List(V3, "V8", V1, "V5"),
        V3 -> List(V1, "V9", V2, "V6"),
        V4 -> List("V5", V1, "V6", "V7"),
        "V5" -> List("V6", V2, V4, "V8"),
        "V6" -> List(V4, V3, "V5", "V9"),
        "V7" -> List("V8", V4, "V9", V1),
        "V8" -> List("V9", "V5", "V7", V2),
        "V9" -> List("V7", "V6", "V8", V3)
      )
  }

  behavior of "TilingTorusBuilder.createTriangleNet"

  it should "return the vertex coords of a 2x2 triangle net" in {
    createTriangleNet(2, 2).vertices.map(vertex => vertex.id -> vertex.coords).toMap shouldEqual
      Map(
        V1 -> (0, 0),
        V2 -> (1, 0),
        V3 -> (0.5, 0.8660254037844386),
        V4 -> (1.5, 0.8660254037844386)
      )
  }

  it should "return the vertex coords of a 4x4 triangle net" in {
    val expected = Map(
      V1 -> BigPoint(0, 0),
      V2 -> BigPoint(1, 0),
      V3 -> BigPoint(2, 0),
      V4 -> BigPoint(3, 0),
      "V5" -> BigPoint(0.5, 0.8660254037844386),
      "V6" -> BigPoint(1.5, 0.8660254037844386),
      "V7" -> BigPoint(2.5, 0.8660254037844386),
      "V8" -> BigPoint(3.5, 0.8660254037844386),
      "V9" -> BigPoint(0, 1.7320508075688772),
      "V10" -> BigPoint(1, 1.7320508075688772),
      "V11" -> BigPoint(2, 1.7320508075688772),
      "V12" -> BigPoint(3, 1.7320508075688772),
      "V13" -> BigPoint(0.5, 2.5980762113533158),
      "V14" -> BigPoint(1.5, 2.5980762113533158),
      "V15" -> BigPoint(2.5, 2.5980762113533158),
      "V16" -> BigPoint(3.5, 2.5980762113533158)
    )
    createTriangleNet(4, 4).vertices.forall(vertex =>
      expected(vertex.id).almostEquals(vertex.coords)
    ) shouldBe true
  }

  it should "compute all adjacent vertices in a torus" in {
    val torus = TilingTorusBuilder.createTriangleNet(4, 4)
    torus.vertices.map { v =>

      v.id -> v.adjacentVerticesUnsafe.map(_.id)
    }.toMap shouldEqual
      Map(
        "V14" -> List("V10", "V13", V2, V3, "V15", "V11"),
        "V9" -> List("V12", "V16", "V13", "V10", "V5", "V8"),
        "V15" -> List("V11", "V14", V3, V4, "V16", "V12"),
        "V13" -> List("V9", "V16", V1, V2, "V14", "V10"),
        "V10" -> List("V5", "V9", "V13", "V14", "V11", "V6"),
        V3 -> List("V6", "V7", V4, "V15", "V14", V2),
        V2 -> List("V5", "V6", V3, "V14", "V13", V1),
        "V16" -> List("V9", "V12", "V15", V4, V1, "V13"),
        "V7" -> List(V3, "V6", "V11", "V12", "V8", V4),
        "V12" -> List("V11", "V15", "V16", "V9", "V8", "V7"),
        "V8" -> List(V1, V4, "V7", "V12", "V9", "V5"),
        "V5" -> List(V1, "V8", "V9", "V10", "V6", V2),
        "V11" -> List("V10", "V14", "V15", "V12", "V7", "V6"),
        V4 -> List("V7", "V8", V1, "V16", "V15", V3),
        V1 -> List(V2, "V13", "V16", V4, "V8", "V5"),
        "V6" -> List(V2, "V5", "V10", "V11", "V7", V3)
      )
  }




