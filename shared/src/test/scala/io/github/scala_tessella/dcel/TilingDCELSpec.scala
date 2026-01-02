package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingDeletion.deleteEdge
import io.github.scala_tessella.dcel.TilingEquivalency.isBoundaryEquivalentTo
import io.github.scala_tessella.dcel.Tree.{Branch, Leaf}
import io.github.scala_tessella.dcel.geometry.{AngleDegree, RegularPolygon}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingDCELSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingDCEL.faces"

  it should "return all faces including outer face" in
    allAssert(
      triangle.faces should have length 2,
      triangle.faces should contain(triangle.outerFace),
      triangle.faces should contain allElementsOf triangle.innerFaces
    )

  it should "return only outer face when no inner faces exist" in:
    val empty = emptyTiling
    allAssert(
      empty.faces should have length 1,
      empty.faces should contain only empty.outerFace
    )

  behavior of "TilingDCEL.findVertex"

  it should "find an existing vertex by id" in
    allAssert(
      triangle.findVertexUnsafe(V1) shouldBe defined,
      triangle.findVertexUnsafe(V2) shouldBe defined,
      triangle.findVertexUnsafe(V3) shouldBe defined
    )

  it should "return None for non-existent vertex id" in
    allAssert(
      triangle.findVertexUnsafe(VertexId("V999")) shouldBe None,
      triangle.findVertexUnsafe(VertexId("NonExistent")) shouldBe None
    )

  it should "find vertices in empty tiling" in:
    emptyTiling.findVertexUnsafe(VertexId("V0")) shouldBe None

  behavior of "TilingDCEL.findFace"

  it should "find an existing face by id" in
    allAssert(
      triangle.findFace(FaceId.outerId).toOption shouldBe defined,
      triangle.findFace(F1).toOption shouldBe defined
    )

  it should "return None for non-existent face id" in
    allAssert(
      triangle.findFace(FaceId(999)).toOption shouldBe None
    )

  behavior of "TilingDCEL.hasConnectedFaces"

  it should "return true for empty tiling" in:
    emptyTiling.hasConnectedFaces shouldBe true

  it should "return true for single polygon tiling" in
    allAssert(
      triangle.hasConnectedFaces shouldBe true,
      square.hasConnectedFaces shouldBe true
    )

  it should "return true for connected multi-polygon tiling" in:
    val twoTriangles = triangle.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
    twoTriangles.hasConnectedFaces shouldBe true

  behavior of "TilingDCEL.boundary"

  it should "return empty vector for empty tiling" in:
    emptyTiling.boundaryVertices shouldBe Vector.empty[Vertex]

  it should "return correct boundary vertices for triangle in clockwise order" in:
    val boundary = triangle.boundaryVertices
    allAssert(
      boundary should have length 3,
      boundary.map(_.id) should contain theSameElementsInOrderAs Vector(V1, V3, V2)
    )

  it should "return correct boundary vertices for square in clockwise order" in:
    val boundary = square.boundaryVertices
    allAssert(
      boundary should have length 4,
      boundary.map(_.id) should contain theSameElementsInOrderAs Vector(V1, V4, V3, V2)
    )

  it should "return correct boundary vertices for hexagon" in:
    val boundary = hexagon.boundaryVertices
    allAssert(
      boundary should have length 6,
      boundary.map(_.id) should contain theSameElementsInOrderAs Vector(V1, V6, V5, V4, V3, V2)
    )

  behavior of "TilingDCEL.boundarySafe"

  it should "return same result as boundary for well-formed tilings" in
    allAssert(
      triangle.boundaryVerticesSafer.value shouldEqual triangle.boundaryVertices,
      square.boundaryVerticesSafer.value shouldEqual square.boundaryVertices
    )

  it should "return empty vector for empty tiling" in:
    emptyTiling.boundaryVerticesSafer shouldBe Right(Vector.empty)

  it should "fail for malformed boundary loop" in:
    val tiling        = triangle
    val boundaryEdges = tiling.boundaryEdgesSafer.value
    val firstEdge     = boundaryEdges.head // This is the startEdge
    val secondEdge    = boundaryEdges(1)

    // Create a malformed loop where a non-start edge is repeated
    firstEdge.next = Some(secondEdge)
    secondEdge.next = Some(secondEdge) // Make second edge point to itself

    tiling.boundaryVerticesSafer.isLeft shouldBe true

  it should "fail for open chain in boundary" in:
    val tiling        = triangle
    val boundaryEdges = tiling.boundaryEdgesSafer.value
    val firstEdge     = boundaryEdges.head
    // Break the chain by setting next to None
    firstEdge.next = None

    tiling.boundaryVerticesSafer.isLeft shouldBe true

  behavior of "TilingDCEL.getBoundaryEdges"

  it should "return empty list for empty tiling" in:
    emptyTiling.boundaryEdgesSafer shouldBe Right(List.empty)

  it should "return boundary edges in correct order" in:
    val boundaryEdges = triangle.boundaryEdgesSafer.value
    allAssert(
      boundaryEdges should have length 3, {
        // Check that edges form a closed loop
        val vertices = boundaryEdges.map(_.origin)
        vertices.map(_.id) should contain theSameElementsInOrderAs Vector(V1, V3, V2)
      }
    )

  it should "fail for malformed boundary with visited edge" in:
    val tiling        = triangle
    val boundaryEdges = tiling.boundaryEdgesSafer.value
    val firstEdge     = boundaryEdges.head // e0 (start)
    val secondEdge    = boundaryEdges(1)   // e1
    val thirdEdge     = boundaryEdges(2)   // e2

    // Modify the structure to create a cycle that doesn't include the start
    // Make e1 -> e2 -> e1 (cycle between e1 and e2)
    secondEdge.next = Some(thirdEdge)
    thirdEdge.next = Some(secondEdge) // This creates the problematic cycle

    tiling.boundaryEdgesSafer.isLeft shouldBe true

  it should "fail for unclosed boundary loop" in:
    val tiling        = triangle
    val boundaryEdges = tiling.boundaryEdgesSafer.value
    // Break the loop by making the last edge not point back to the first
    boundaryEdges.last.next = None
    tiling.boundaryEdgesSafer.isLeft shouldBe true

  behavior of "TilingDCEL.getAnglesAtVertex"

  it should "return the angles for a vertex where all incident edges have an angle" in:
    val v1     = triangle.findVertexUnsafe(V1).get
    v1.incidentEdgesUnsafe.filter(_.hasIncidentFace(triangle.outerFace)).foreach(_.angle =
      Some(AngleDegree(300))
    )
    val result = triangle.getAnglesAtVertex(V1)
    result.value should contain theSameElementsAs List(AngleDegree(60), AngleDegree(300))

  it should "return an error for a non-existent vertex" in:
    val result = triangle.getAnglesAtVertex(VertexId("V999"))
    allAssert(
      result.isLeft shouldBe true,
      result.left.value.message shouldEqual "Vertex with ID 'V999' not found."
    )

  /** @see <img src="file:../../../../../resources/bench.svg"/> */
  def bench: TilingDCEL =
    hexagon
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V3, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(4)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V8"), RegularPolygon(4)).value
      .maybeAddRegularPolygonToBoundary(VertexId("V11"), RegularPolygon(6)).value

  it should "return the angles for the inner vertex of the bench" in:
    val result = bench.getAnglesAtVertex(VertexId("V8"))
    result.value shouldBe List(120, 90, 60, 90).map(AngleDegree(_))

  it should "return the angles for a boundary vertex of the bench" in:
    val result = bench.getAnglesAtVertex(V1)
    result.value shouldBe List(30, 60, 120, 60, 90).map(AngleDegree(_))

  behavior of "TilingDCEL.getInnerAnglesAtVertex"

  it should "return the inner angles for the inner vertex of the bench" in:
    val result = bench.getInnerAnglesAtVertex(VertexId("V8"))
    result.value shouldBe List(120, 90, 60, 90).map(AngleDegree(_))

  it should "return the inner angles for a boundary vertex of the bench" in:
    val result = bench.getInnerAnglesAtVertex(V1)
    result.value shouldBe List(60, 120, 60, 90).map(AngleDegree(_))

  behavior of "TilingDCEL.innerVertices"

  it should "return the inner vertices of the bench" in:
    val result = bench.innerVertices
    result.map(_.id) shouldBe List(VertexId("V8"))

  behavior of "TilingDCEL.empty"

  it should "create empty tiling" in:
    val emptyTiling = TilingDCEL.empty
    allAssert(
      emptyTiling.vertices shouldBe empty,
      emptyTiling.halfEdges shouldBe empty,
      emptyTiling.innerFaces shouldBe empty,
      emptyTiling.outerFace.id shouldBe FaceId.outerId
    )

  behavior of "TilingDCEL.getDcelAtVertex"

  it should "return the DCEL around the inner vertex of the bench" in:

    /** @see <img src="file:../../../../../resources/aroundInnerVertex.svg"/> */
    val result = bench.getDcelAtVertex(VertexId("V8"))
    TilingValidation.validate(result.value).isRight shouldBe true

  it should "return the DCEL around a boundary vertex of the bench" in:

    /** @see <img src="file:../../../../../resources/aroundBoundaryVertex.svg"/> */
    val result = bench.getDcelAtVertex(V1)
    TilingValidation.validate(result.value).isRight shouldBe true

  it should "return the DCEL around another boundary vertex of the bench" in:
    val result = bench.getDcelAtVertex(VertexId("V7"))
    allAssert(
      TilingValidation.validate(result.value).isRight shouldBe true,
      result.value.isBoundaryEquivalentTo(triangle) shouldBe true
    )

  it should "return the DCEL around the inner vertex with varying distances" in:
    val net = TilingBuilder.createRhombusNet(6, 6)
    allAssert(
      net.getDcelAtVertex(VertexId("V25"), 0).value.innerFaces.size shouldBe 4,
      net.getDcelAtVertex(VertexId("V25"), 1).value.innerFaces.size shouldBe 12,
      net.getDcelAtVertex(VertexId("V25"), 2).value.innerFaces.size shouldBe 24
    )

//  behavior of "TilingDCEL.getPolygonVerticesAroundVertex"
//
//  it should "return the DCEL around the inner vertex of the bench" in:
//    val result = bench.getPolygonVerticesAroundVertex(VertexId("V8"))
//    result.value shouldBe List("V13", "V1", "V2", "V10", "V11", "V14", "V15", "V16", "V12")
//  }
//
//  it should "return the DCEL around a boundary vertex of the bench" in:
//    val result = bench.getPolygonVerticesAroundVertex(V1)
//    result.value shouldBe List(V6, V5, "V4", "V3", "V2", "V8", "V12", "V13", "V1", "V7")
//  }
//
//  behavior of "TilingDCEL.groupedInnerVertices"

  def net: TilingDCEL = TilingBuilder.createRhombusNet(3, 6)

  def holeInNet2: TilingDCEL = net.deleteEdge(VertexId("V14"), VertexId("V15")).value

//  it should "group the inner vertices according to their adjacent polygons" in:
//    holeInNet2.groupedInnerVertices.values.toList shouldBe List(
//      List(V6, "V7", "V22", "V23"),
//      List("V10", "V11", "V18", "V19"),
//      List("V14", "V15")
//    )
//  }

  behavior of "TilingDCEL.uniformity"

  it should "calculate the uniformity of a square net" in:
    val net = TilingBuilder.createRhombusNet(6, 6)
    net.uniformityTree.flattenLeaves shouldBe
      List(
        List(
          "V9",
          "V10",
          "V11",
          "V12",
          "V13",
          "V16",
          "V20",
          "V23",
          "V27",
          "V30",
          "V34",
          "V37",
          "V38",
          "V39",
          "V40",
          "V41",
          "V17",
          "V18",
          "V19",
          "V24",
          "V26",
          "V31",
          "V32",
          "V33",
          "V25"
        )
      )

  it should "calculate the uniformity of a holed net" in:
    holeInNet2.uniformityTree.flattenLeaves shouldBe List(
      List(V6, "V7", "V22", "V23"),
      List("V10", "V11", "V18", "V19"),
      List("V14", "V15")
    )

  it should "calculate another" in:
    val result = TilingBuilder.createHoledTriangleNet(12, 12)((i, j) => i % 10 == (j * 8) % 10)
    result.uniformityTree shouldBe
      Branch(
        List(),
        List(
          Branch(
            List(
              "V15",
              "V16",
              "V17",
              "V18",
              "V19",
              "V25",
              "V28",
              "V36",
              "V37",
              "V38",
              "V41",
              "V51",
              "V80",
              "V90",
              "V93",
              "V103",
              "V106",
              "V116",
              "V137",
              "V138",
              "V145",
              "V146",
              "V147",
              "V148",
              "V149",
              "V155"
            ),
            List(
              Leaf(List(
                "V29",
                "V48",
                "V50",
                "V81",
                "V94",
                "V102",
                "V115",
                "V124",
                "V126",
                "V135",
                "V59",
                "V61",
                "V70",
                "V83",
                "V113",
                "V72"
              )),
              Leaf(List(
                "V30",
                "V47",
                "V62",
                "V69",
                "V95",
                "V101",
                "V127",
                "V134",
                "V58",
                "V73",
                "V84",
                "V112",
                "V123"
              )),
              Leaf(List("V49", "V125", "V136", "V60", "V82", "V114", "V71"))
            )
          ),
          Leaf(List(
            "V20",
            "V35",
            "V42",
            "V63",
            "V68",
            "V89",
            "V107",
            "V128",
            "V133",
            "V139",
            "V154",
            "V31",
            "V46",
            "V57",
            "V74",
            "V96",
            "V100",
            "V122",
            "V111",
            "V85"
          )),
          Leaf(List(
            "V21",
            "V34",
            "V43",
            "V56",
            "V75",
            "V88",
            "V108",
            "V121",
            "V140",
            "V153",
            "V32",
            "V45",
            "V86",
            "V97",
            "V99",
            "V110"
          ))
        )
      )

  behavior of "TilingDCEL.doubleArea"

  it should "keep an empty tiling" in:
    TilingDCEL.empty.doubleArea.value.isEmpty shouldBe true

  it should "double a 2x1 square net along the longest segment" in:
    val doubled = TilingBuilder.createRhombusNet(2, 1).doubleArea
    val result  = doubled.value
    allAssert(
      result.halfEdges.size shouldBe 24,
      result.vertices.size shouldBe 9,
      result.innerFaces.size shouldBe 4,
      TilingValidation.validate(result).isRight shouldBe true
    )

  it should "double a 1x2 square net along the longest segment" in:
    val doubled = TilingBuilder.createRhombusNet(1, 2).doubleArea
    val result  = doubled.value
    allAssert(
      result.halfEdges.size shouldBe 24,
      result.vertices.size shouldBe 9,
      result.innerFaces.size shouldBe 4,
      TilingValidation.validate(result).isRight shouldBe true
    )

  it should "double a 3x2 hex net along the longest segment" in:
    val doubled = TilingBuilder.createHexagonNet(3, 2).doubleArea
    val result  = doubled.value
    allAssert(
      result.halfEdges.size shouldBe 98,
      result.vertices.size shouldBe 38,
      result.innerFaces.size shouldBe 12,
      TilingValidation.validate(result).isRight shouldBe true
    )

  it should "double a 2x3 hex net along the longest segment" in:
    val doubled = TilingBuilder.createHexagonNet(2, 3).doubleArea
    val result  = doubled.value
    allAssert(
      result.halfEdges.size shouldBe 98,
      result.vertices.size shouldBe 38,
      result.innerFaces.size shouldBe 12,
      TilingValidation.validate(result).isRight shouldBe true
    )

  it should "double a triangle" in:
    val doubled = triangle.doubleArea
    val result  = doubled.value
    allAssert(
      result.halfEdges.size shouldBe 10,
      result.vertices.size shouldBe 4,
      result.innerFaces.size shouldBe 2,
      TilingValidation.validate(result).isRight shouldBe true
    )

  it should "double a larger triangle" in:
    val doubled =
      TilingBuilder.createSimplePolygon(180, 60, 180, 180, 60, 180, 180, 60, 180).toOption.get.doubleArea
    val result  = doubled.value
//    println(result.toSVG())
    allAssert(
      result.halfEdges.size shouldBe 30,
      result.vertices.size shouldBe 14,
      result.innerFaces.size shouldBe 2,
      TilingValidation.validate(result).isRight shouldBe true
    )

  behavior of "TilingDCEL.hasUnitRegularPolygonsOnly"

  it should "succeed for a net" in:
    net.hasUnitRegularPolygonsOnly shouldBe true

  it should "fail for a holed net" in:
    holeInNet2.hasUnitRegularPolygonsOnly shouldBe false
