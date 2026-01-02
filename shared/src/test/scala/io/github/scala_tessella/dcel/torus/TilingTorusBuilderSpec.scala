package io.github.scala_tessella.dcel.torus

import io.github.scala_tessella.dcel.TilingTestHelpers
import io.github.scala_tessella.dcel.geometry.BigPoint
import io.github.scala_tessella.dcel.structure.VertexId
import io.github.scala_tessella.dcel.torus.TilingTorusBuilder.*
//import io.github.scala_tessella.dcel.torus.TilingTorusDCEL.TorusSvg3DOptions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingTorusBuilderSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  val torus2x2SquareNet: TilingTorusDCEL =
    createSquareNet(2, 2)

  behavior of "TilingTorusBuilder.createSquareNet"

  it should "return the vertex coords of a 2x2 square net" in:
    torus2x2SquareNet.vertices.map(vertex => vertex.id -> vertex.coords).toMap shouldEqual
      Map(
        V1 -> BigPoint(0, 0),
        V2 -> BigPoint(1, 0),
        V3 -> BigPoint(0, 1),
        V4 -> BigPoint(1, 1)
      )

  it should "return the vertex coords of a 3x3 square net" in:
    createSquareNet(3, 3).vertices.map(vertex => vertex.id -> vertex.coords).toMap shouldEqual
      Map(
        V1 -> BigPoint(0, 0),
        V2 -> BigPoint(1, 0),
        V3 -> BigPoint(2, 0),
        V4 -> BigPoint(0, 1),
        V5 -> BigPoint(1, 1),
        V6 -> BigPoint(2, 1),
        7  -> BigPoint(0, 2),
        8  -> BigPoint(1, 2),
        9  -> BigPoint(2, 2)
      )

  it should "compute all adjacent vertices in a torus" in:
    val torus = TilingTorusBuilder.createSquareNet(3, 3)
    torus.vertices
      .map: vertex =>
        vertex.id -> vertex.adjacentVerticesUnsafe.map: adjacentVertex =>
          adjacentVertex.id
      .toMap shouldEqual
      Map(
        V1 -> List(V2, 7, V3, V4),
        V2 -> List(V3, 8, V1, V5),
        V3 -> List(V1, 9, V2, V6),
        V4 -> List(V5, V1, V6, 7),
        V5 -> List(V6, V2, V4, 8),
        V6 -> List(V4, V3, V5, 9),
        7  -> List(8, V4, 9, V1),
        8  -> List(9, V5, 7, V2),
        9  -> List(7, V6, 8, V3)
      )

  it should "create a 1x1 square net on a torus" in:
    val torus = createSquareNet(1, 1)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 1,
      torus.faces.size shouldBe 1,
      torus.halfEdges.size shouldBe 4
    )

  it should "create a 1x2 square net on a torus" in:
    val torus = createSquareNet(1, 2)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 2,
      torus.faces.size shouldBe 2,
      torus.halfEdges.size shouldBe 8
    )

  it should "create a 2x1 square net on a torus" in:
    val torus = createSquareNet(2, 1)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 2,
      torus.faces.size shouldBe 2,
      torus.halfEdges.size shouldBe 8
    )

  it should "create a 2x2 square net on a torus" in:
    val torus         = createSquareNet(2, 2)
    val scale: Double = 1.0 / 2.0
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = scale, vScale = scale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 4,
      torus.faces.size shouldBe 4,
      torus.halfEdges.size shouldBe 16
    )

  it should "create a 3x3 square net on a torus" in:
    val torus         = createSquareNet(3, 3)
    val scale: Double = 1.0 / 3.0
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = scale, vScale = scale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 9,
      torus.faces.size shouldBe 9,
      torus.halfEdges.size shouldBe 36
    )

  it should "create a 4x4 square net on a torus" in:
    val torus         = createSquareNet(4, 4)
    val scale: Double = 1.0 / 4.0
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = scale, vScale = scale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 16,
      torus.faces.size shouldBe 16,
      torus.halfEdges.size shouldBe 64
    )

  it should "create a 8x8 square net on a torus" in:
    val torus         = createSquareNet(8, 8)
    val scale: Double = 1.0 / 8.0
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = scale, vScale = scale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 64,
      torus.faces.size shouldBe 64,
      torus.halfEdges.size shouldBe 256
    )

  it should "create a 16x4 square net on a torus" in:
    val torus          = createSquareNet(16, 4)
    val uScale: Double = 1.0 / 16.0
    val vScale: Double = 1.0 / 4.0
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 64,
      torus.faces.size shouldBe 64,
      torus.halfEdges.size shouldBe 256
    )

  it should "create a 4x16 square net on a torus" in:
    val torus          = createSquareNet(4, 16)
    val uScale: Double = 1.0 / 4.0
    val vScale: Double = 1.0 / 16.0
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 64,
      torus.faces.size shouldBe 64,
      torus.halfEdges.size shouldBe 256
    )

  behavior of "TilingTorusBuilder.createTriangleNet"

  it should "return the vertex coords of a 2x2 triangle net" in:
    createTriangleNet(2, 2).vertices.map(vertex => vertex.id -> vertex.coords).toMap shouldEqual
      Map(
        V1 -> BigPoint(0, 0),
        V2 -> BigPoint(1, 0),
        V3 -> BigPoint(0.5, 0.8660254037844386),
        V4 -> BigPoint(1.5, 0.8660254037844386)
      )

  it should "return the vertex coords of a 4x4 triangle net" in:
    val expected = Map(
      V1 -> BigPoint(0, 0),
      V2 -> BigPoint(1, 0),
      V3 -> BigPoint(2, 0),
      V4 -> BigPoint(3, 0),
      V5 -> BigPoint(0.5, 0.8660254037844386),
      V6 -> BigPoint(1.5, 0.8660254037844386),
      7  -> BigPoint(2.5, 0.8660254037844386),
      8  -> BigPoint(3.5, 0.8660254037844386),
      9  -> BigPoint(0, 1.7320508075688772),
      10 -> BigPoint(1, 1.7320508075688772),
      11 -> BigPoint(2, 1.7320508075688772),
      12 -> BigPoint(3, 1.7320508075688772),
      13 -> BigPoint(0.5, 2.5980762113533158),
      14 -> BigPoint(1.5, 2.5980762113533158),
      15 -> BigPoint(2.5, 2.5980762113533158),
      16 -> BigPoint(3.5, 2.5980762113533158)
    )
    createTriangleNet(4, 4).vertices.forall(vertex =>
      expected(vertex.id).almostEquals(vertex.coords)
    ) shouldBe true

  it should "compute all adjacent vertices in a torus" in:
    val torus = TilingTorusBuilder.createTriangleNet(4, 4)
    torus.vertices
      .map: vertex =>
        vertex.id -> vertex.adjacentVerticesUnsafe.map: adjacentVertex =>
          adjacentVertex.id
      .toMap shouldEqual
      Map(
        V1 -> List(V2, 13, 16, V4, 8, V5),
        V2 -> List(V5, V6, V3, 14, 13, V1),
        V3 -> List(V6, 7, V4, 15, 14, V2),
        V4 -> List(7, 8, V1, 16, 15, V3),
        V5 -> List(V1, 8, 9, 10, V6, V2),
        V6 -> List(V2, V5, 10, 11, 7, V3),
        7  -> List(V3, V6, 11, 12, 8, V4),
        8  -> List(V1, V4, 7, 12, 9, V5),
        9  -> List(12, 16, 13, 10, V5, 8),
        10 -> List(V5, 9, 13, 14, 11, V6),
        11 -> List(10, 14, 15, 12, 7, V6),
        12 -> List(11, 15, 16, 9, 8, 7),
        13 -> List(9, 16, V1, V2, 14, 10),
        14 -> List(10, 13, V2, V3, 15, 11),
        15 -> List(11, 14, V3, V4, 16, 12),
        16 -> List(9, 12, 15, V4, V1, 13)
      )

  it should "create a 1x2 triangle net on a torus" in:
    val torus          = createTriangleNet(1, 2)
    val uScale: Double = 1.0
    val vScale: Double = 1.0 / (2.0 * 0.8660254037844386)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 2,
      torus.faces.size shouldBe 4,
      torus.halfEdges.size shouldBe 12
    )

  it should "create a 2x2 triangle net on a torus" in:
    val torus          = createTriangleNet(2, 2)
    val uScale: Double = 1.0 / 2.0
    val vScale: Double = 1.0 / (2.0 * 0.8660254037844386)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 4,
      torus.faces.size shouldBe 8,
      torus.halfEdges.size shouldBe 24
    )

  it should "create a 4x4 triangle net on a torus" in:
    val torus          = createTriangleNet(4, 4)
    val uScale: Double = 1.0 / 4.0
    val vScale: Double = 1.0 / (4.0 * 0.8660254037844386)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 16,
      torus.faces.size shouldBe 32,
      torus.halfEdges.size shouldBe 96
    )

  it should "create a 8x8 triangle net on a torus" in:
    val torus          = createTriangleNet(8, 8)
    val uScale: Double = 1.0 / 8.0
    val vScale: Double = 1.0 / (8.0 * 0.8660254037844386)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 64,
      torus.faces.size shouldBe 128,
      torus.halfEdges.size shouldBe 384
    )

  it should "create a 16x4 triangle net on a torus" in:
    val torus          = createTriangleNet(16, 4)
    val uScale: Double = 1.0 / 16.0
    val vScale: Double = 1.0 / (4.0 * 0.8660254037844386)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 64,
      torus.faces.size shouldBe 128,
      torus.halfEdges.size shouldBe 384
    )

  it should "create a 4x16 triangle net on a torus" in:
    val torus          = createTriangleNet(4, 16)
    val uScale: Double = 1.0 / 4.0
    val vScale: Double = 1.0 / (16.0 * 0.8660254037844386)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 64,
      torus.faces.size shouldBe 128,
      torus.halfEdges.size shouldBe 384
    )

  behavior of "TilingTorusBuilder.createHexagonNet"

  it should "return the vertex coords of a 2x2 hexagon net" in:
    val expected = Map(
      V1          -> BigPoint(0, 0),
      V2          -> BigPoint(1, 0),
      V3          -> BigPoint(1.5, 0.8660254037844386),
      V4          -> BigPoint(1, 1.7320508075688773),
      V5          -> BigPoint(0, 1.7320508075688773),
      V6          -> BigPoint(-0.5, 0.8660254037844386),
      VertexId(7) -> BigPoint(-0.5, -0.8660254037844386),
      VertexId(8) -> BigPoint(1.5, 2.5980762113533159)
    )
    createHexagonNet(2, 2).vertices.forall(vertex =>
      expected(vertex.id).almostEquals(vertex.coords)
    ) shouldBe true

  it should "compute all adjacent vertices in a torus" in:
    val torus = TilingTorusBuilder.createHexagonNet(2, 2)
    torus.vertices
      .map: vertex =>
        vertex.id -> vertex.adjacentVerticesUnsafe.map: adjacentVertex =>
          adjacentVertex.id
      .toMap shouldEqual
      Map(
        V1 -> List(V6, V2, 7),
        V2 -> List(V1, V3, 8),
        V3 -> List(7, V2, V4),
        V4 -> List(V3, V5, 8),
        V5 -> List(7, V4, V6),
        V6 -> List(V5, V1, 8),
        7  -> List(V5, V3, V1),
        8  -> List(V2, V6, V4)
      )

  it should "return the vertex coords of a 4x4 hexagon net" in:
    val expected =
      Map(
        V1 -> BigPoint(0, 0),
        V2 -> BigPoint(1, 0),
        V3 -> BigPoint(1.5, 0.8660254037844386),
        V4 -> BigPoint(1, 1.7320508075688773),
        V5 -> BigPoint(0, 1.7320508075688773),
        V6 -> BigPoint(-0.5, 0.8660254037844386),
        7  -> BigPoint(2.5, 0.8660254037844386),
        8  -> BigPoint(3, 1.7320508075688773),
        9  -> BigPoint(2.5, 2.5980762113533159),
        10 -> BigPoint(1.5, 2.5980762113533159),
        11 -> BigPoint(4, 1.7320508075688773),
        12 -> BigPoint(4.5, 2.5980762113533159),
        13 -> BigPoint(4, 3.464101615137754547311118785798475),
        14 -> BigPoint(3, 3.464101615137754547311118785798475),
        15 -> BigPoint(-0.5, -0.8660254037844386),
        16 -> BigPoint(4.5, 4.330127018922193154331068059338159),
        17 -> BigPoint(1, 3.464101615137754666542440477438219),
        18 -> BigPoint(0, 3.464101615137754666542440477438219),
        19 -> BigPoint(-0.5, 2.5980762113533159),
        20 -> BigPoint(2.5, 4.330127018922193273562389750977902),
        21 -> BigPoint(1.5, 4.330127018922193273562389750977902),
        22 -> BigPoint(4, 5.196152422706631880582339024517586),
        23 -> BigPoint(3, 5.196152422706631880582339024517586),
        24 -> BigPoint(4.5, 6.062177826491070487602288298057269),
        25 -> BigPoint(1, 5.196152422706631999813660716157328),
        26 -> BigPoint(0, 5.196152422706631999813660716157328),
        27 -> BigPoint(-0.5, 4.330127018922193392793711442617645),
        28 -> BigPoint(2.5, 6.062177826491070606833609989697012),
        29 -> BigPoint(1.5, 6.062177826491070606833609989697012),
        30 -> BigPoint(4, 6.928203230275509213853559263236695),
        31 -> BigPoint(3, 6.928203230275509213853559263236695),
        32 -> BigPoint(4.5, 7.794228634059947820873508536776378)
      )
    createHexagonNet(4, 4).vertices.forall(vertex =>
      expected(vertex.id.value).almostEquals(vertex.coords)
    ) shouldBe true

  it should "compute all adjacent vertices of 4x4 hexagon net in a torus" in:
    val torus = TilingTorusBuilder.createHexagonNet(4, 4)
    torus.vertices
      .map: vertex =>
        vertex.id -> vertex.adjacentVerticesUnsafe.map: adjacentVertex =>
          adjacentVertex.id
      .toMap shouldEqual
      Map(
        5  -> List(6, 19, 4),
        10 -> List(9, 4, 17),
        14 -> List(9, 20, 13),
        1  -> List(6, 2, 15),
        6  -> List(16, 5, 1),
        9  -> List(8, 10, 14),
        13 -> List(12, 14, 16),
        2  -> List(3, 29, 1),
        12 -> List(15, 11, 13),
        7  -> List(8, 31, 3),
        3  -> List(7, 2, 4),
        18 -> List(19, 27, 17),
        11 -> List(12, 32, 8),
        8  -> List(7, 9, 11),
        4  -> List(10, 3, 5),
        15 -> List(1, 26, 12),
        24 -> List(22, 30, 19),
        25 -> List(29, 21, 26),
        20 -> List(14, 21, 23),
        29 -> List(28, 25, 2),
        28 -> List(29, 31, 23),
        21 -> List(20, 17, 25),
        32 -> List(27, 30, 11),
        17 -> List(10, 18, 21),
        22 -> List(16, 23, 24),
        27 -> List(32, 26, 18),
        16 -> List(6, 13, 22),
        31 -> List(7, 30, 28),
        26 -> List(27, 15, 25),
        23 -> List(22, 20, 28),
        30 -> List(31, 32, 24),
        19 -> List(18, 5, 24)
      )

  it should "create a 1x2 hexagon net on a torus" in:
    val torus          = createHexagonNet(1, 2)
    val uScale: Double = 1.0 / 1.5
    val vScale: Double = 1.0 / (4 * 0.8660254037844386)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 4,
      torus.faces.size shouldBe 2,
      torus.halfEdges.size shouldBe 8
    )

  it should "create a 2x2 hexagon net on a torus" in:
    val torus          = createHexagonNet(2, 2)
    val uScale: Double = 1.0 / 3.0
    val vScale: Double = 1.0 / (4 * 0.8660254037844386)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 8,
      torus.faces.size shouldBe 4,
      torus.halfEdges.size shouldBe 24
    )

  it should "create a 4x4 hexagon net on a torus" in:
    val torus          = createHexagonNet(4, 4)
    val uScale: Double = 1.0 / 6.0
    val vScale: Double = 1.0 / (8 * 0.8660254037844386)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 32,
      torus.faces.size shouldBe 16,
      torus.halfEdges.size shouldBe 96
    )

  it should "create a 6x6 hexagon net on a torus" in:
    val torus          = createHexagonNet(6, 6)
    val uScale: Double = 1.0 / 9.0
    val vScale: Double = 1.0 / (12 * 0.8660254037844386)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 72,
      torus.faces.size shouldBe 36,
      torus.halfEdges.size shouldBe 216
    )

  it should "create a 12x4 hexagon net on a torus" in:
    val torus          = createHexagonNet(12, 4)
    val uScale: Double = 1.0 / 18.0
    val vScale: Double = 1.0 / (8 * 0.8660254037844386)
//    println(torus.toSVG3D(TorusSvg3DOptions().copy(uScale = uScale, vScale = vScale, showVertexIds = true)))
    allAssert(
      torus.vertices.size shouldBe 96,
      torus.faces.size shouldBe 48,
      torus.halfEdges.size shouldBe 288
    )
