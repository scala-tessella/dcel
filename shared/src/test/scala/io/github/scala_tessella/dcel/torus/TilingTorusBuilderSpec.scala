package io.github.scala_tessella.dcel.torus

import io.github.scala_tessella.dcel.TilingTestHelpers
import io.github.scala_tessella.dcel.geometry.BigPoint
import io.github.scala_tessella.dcel.structure.VertexId
import io.github.scala_tessella.dcel.torus.TilingTorusBuilder.*
import io.github.scala_tessella.dcel.torus.TilingTorusDCEL.TorusSvg3DOptions
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
        V1   -> BigPoint(0, 0),
        V2   -> BigPoint(1, 0),
        V3   -> BigPoint(2, 0),
        V4   -> BigPoint(0, 1),
        V5   -> BigPoint(1, 1),
        V6   -> BigPoint(2, 1),
        "V7" -> BigPoint(0, 2),
        "V8" -> BigPoint(1, 2),
        "V9" -> BigPoint(2, 2)
      )

  it should "compute all adjacent vertices in a torus" in:
    val torus = TilingTorusBuilder.createSquareNet(3, 3)
    torus.vertices
      .map: vertex =>
        vertex.id -> vertex.adjacentVerticesUnsafe.map: adjacentVertex =>
          adjacentVertex.id
      .toMap shouldEqual
      Map(
        V1   -> List(V2, "V7", V3, V4),
        V2   -> List(V3, "V8", V1, V5),
        V3   -> List(V1, "V9", V2, V6),
        V4   -> List(V5, V1, V6, "V7"),
        V5   -> List(V6, V2, V4, "V8"),
        V6   -> List(V4, V3, V5, "V9"),
        "V7" -> List("V8", V4, "V9", V1),
        "V8" -> List("V9", V5, "V7", V2),
        "V9" -> List("V7", V6, "V8", V3)
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
      V1    -> BigPoint(0, 0),
      V2    -> BigPoint(1, 0),
      V3    -> BigPoint(2, 0),
      V4    -> BigPoint(3, 0),
      V5    -> BigPoint(0.5, 0.8660254037844386),
      V6    -> BigPoint(1.5, 0.8660254037844386),
      "V7"  -> BigPoint(2.5, 0.8660254037844386),
      "V8"  -> BigPoint(3.5, 0.8660254037844386),
      "V9"  -> BigPoint(0, 1.7320508075688772),
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

  it should "compute all adjacent vertices in a torus" in:
    val torus = TilingTorusBuilder.createTriangleNet(4, 4)
    torus.vertices
      .map: vertex =>
        vertex.id -> vertex.adjacentVerticesUnsafe.map: adjacentVertex =>
          adjacentVertex.id
      .toMap shouldEqual
      Map(
        V1    -> List(V2, "V13", "V16", V4, "V8", V5),
        V2    -> List(V5, V6, V3, "V14", "V13", V1),
        V3    -> List(V6, "V7", V4, "V15", "V14", V2),
        V4    -> List("V7", "V8", V1, "V16", "V15", V3),
        V5    -> List(V1, "V8", "V9", "V10", V6, V2),
        V6    -> List(V2, V5, "V10", "V11", "V7", V3),
        "V7"  -> List(V3, V6, "V11", "V12", "V8", V4),
        "V8"  -> List(V1, V4, "V7", "V12", "V9", V5),
        "V9"  -> List("V12", "V16", "V13", "V10", V5, "V8"),
        "V10" -> List(V5, "V9", "V13", "V14", "V11", V6),
        "V11" -> List("V10", "V14", "V15", "V12", "V7", V6),
        "V12" -> List("V11", "V15", "V16", "V9", "V8", "V7"),
        "V13" -> List("V9", "V16", V1, V2, "V14", "V10"),
        "V14" -> List("V10", "V13", V2, V3, "V15", "V11"),
        "V15" -> List("V11", "V14", V3, V4, "V16", "V12"),
        "V16" -> List("V9", "V12", "V15", V4, V1, "V13")
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
      V1             -> BigPoint(0, 0),
      V2             -> BigPoint(1, 0),
      V3             -> BigPoint(1.5, 0.8660254037844386),
      V4             -> BigPoint(1, 1.7320508075688773),
      V5             -> BigPoint(0, 1.7320508075688773),
      V6             -> BigPoint(-0.5, 0.8660254037844386),
      VertexId("V7") -> BigPoint(-0.5, -0.8660254037844386),
      VertexId("V8") -> BigPoint(1.5, 2.5980762113533159)
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
        V1   -> List(V2, "V7", V6),
        V2   -> List("V8", V1, V3),
        V3   -> List(V2, V4, "V7"),
        V4   -> List(V3, V5, "V8"),
        V5   -> List(V4, V6, "V7"),
        V6   -> List(V1, "V8", V5),
        "V7" -> List(V3, V1, V5),
        "V8" -> List(V4, V2, V6)
      )

  it should "return the vertex coords of a 4x4 hexagon net" in:
    val expected =
      Map(
        V1    -> BigPoint(0, 0),
        V2    -> BigPoint(1, 0),
        V3    -> BigPoint(1.5, 0.8660254037844386),
        V4    -> BigPoint(1, 1.7320508075688773),
        V5    -> BigPoint(0, 1.7320508075688773),
        V6    -> BigPoint(-0.5, 0.8660254037844386),
        "V7"  -> BigPoint(2.5, 0.8660254037844386),
        "V8"  -> BigPoint(3, 1.7320508075688773),
        "V9"  -> BigPoint(2.5, 2.5980762113533159),
        "V10" -> BigPoint(1.5, 2.5980762113533159),
        "V11" -> BigPoint(4, 1.7320508075688773),
        "V12" -> BigPoint(4.5, 2.5980762113533159),
        "V13" -> BigPoint(4, 3.464101615137754547311118785798475),
        "V14" -> BigPoint(3, 3.464101615137754547311118785798475),
        "V15" -> BigPoint(-0.5, -0.8660254037844386),
        "V16" -> BigPoint(4.5, 4.330127018922193154331068059338159),
        "V17" -> BigPoint(1, 3.464101615137754666542440477438219),
        "V18" -> BigPoint(0, 3.464101615137754666542440477438219),
        "V19" -> BigPoint(-0.5, 2.5980762113533159),
        "V20" -> BigPoint(2.5, 4.330127018922193273562389750977902),
        "V21" -> BigPoint(1.5, 4.330127018922193273562389750977902),
        "V22" -> BigPoint(4, 5.196152422706631880582339024517586),
        "V23" -> BigPoint(3, 5.196152422706631880582339024517586),
        "V24" -> BigPoint(4.5, 6.062177826491070487602288298057269),
        "V25" -> BigPoint(1, 5.196152422706631999813660716157328),
        "V26" -> BigPoint(0, 5.196152422706631999813660716157328),
        "V27" -> BigPoint(-0.5, 4.330127018922193392793711442617645),
        "V28" -> BigPoint(2.5, 6.062177826491070606833609989697012),
        "V29" -> BigPoint(1.5, 6.062177826491070606833609989697012),
        "V30" -> BigPoint(4, 6.928203230275509213853559263236695),
        "V31" -> BigPoint(3, 6.928203230275509213853559263236695),
        "V32" -> BigPoint(4.5, 7.794228634059947820873508536776378)
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
        "V14" -> List("V20", "V13", "V9"),
        "V9"  -> List("V8", "V10", "V14"),
        "V22" -> List("V23", "V24", "V16"),
        "V30" -> List("V32", "V24", "V31"),
        "V32" -> List("V27", "V30", "V11"),
        "V21" -> List("V25", "V20", "V17"),
        "V10" -> List("V17", "V9", V4),
        V2    -> List(V1, V3, "V29"),
        "V16" -> List("V13", "V22", V6),
        "V31" -> List("V30", "V28", "V7"),
        "V11" -> List("V8", "V12", "V32"),
        "V25" -> List("V26", "V29", "V21"),
        V4    -> List("V10", V3, V5),
        "V15" -> List(V1, "V26", "V12"),
        "V29" -> List(V2, "V28", "V25"),
        "V13" -> List("V14", "V16", "V12"),
        "V18" -> List("V17", "V19", "V27"),
        "V26" -> List("V15", "V25", "V27"),
        V3    -> List(V2, V4, "V7"),
        "V7"  -> List(V3, "V8", "V31"),
        "V27" -> List("V26", "V18", "V32"),
        "V12" -> List("V13", "V15", "V11"),
        "V19" -> List("V18", V5, "V24"),
        V1    -> List("V15", V6, V2),
        "V24" -> List("V30", "V19", "V22"),
        V6    -> List("V16", V5, V1),
        "V17" -> List("V21", "V10", "V18"),
        "V20" -> List("V21", "V23", "V14"),
        "V8"  -> List("V11", "V7", "V9"),
        V5    -> List("V19", V4, V6),
        "V28" -> List("V29", "V31", "V23"),
        "V23" -> List("V28", "V22", "V20")
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
