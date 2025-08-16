package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, BigPoint}

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingDCELSpec extends AnyFlatSpec with Matchers with EitherValues:

  // Helper methods to create test data
  private def createTriangleTiling(): TilingDCEL =
    TilingBuilder.createRegularPolygon(3).value

  private def createSquareTiling(): TilingDCEL =
    TilingBuilder.createRegularPolygon(4).value

  private def createHexagonTiling(): TilingDCEL =
    TilingBuilder.createRegularPolygon(6).value

  behavior of "TilingDCEL.faces"

  it should "return all faces including outer face" in {
    val triangle = createTriangleTiling()
    triangle.faces should have length 2
    triangle.faces should contain(triangle.outerFace)
    triangle.faces should contain allElementsOf triangle.innerFaces
  }

  it should "return only outer face when no inner faces exist" in {
    val empty = TilingBuilder.empty
    empty.faces should have length 1
    empty.faces should contain only empty.outerFace
  }

  behavior of "TilingDCEL.findVertex"

  it should "find an existing vertex by id" in {
    val triangle = createTriangleTiling()
    triangle.findVertex("V1") shouldBe defined
    triangle.findVertex("V2") shouldBe defined
    triangle.findVertex("V3") shouldBe defined
  }

  it should "return None for non-existent vertex id" in {
    val triangle = createTriangleTiling()
    triangle.findVertex("V999") shouldBe None
    triangle.findVertex("NonExistent") shouldBe None
  }

  it should "find vertices in empty tiling" in {
    val empty = TilingBuilder.empty
    empty.findVertex("V0") shouldBe None
  }

  behavior of "TilingDCEL.findFace"

  it should "find an existing face by id" in {
    val triangle = createTriangleTiling()
    triangle.findFace(Face.outerId) shouldBe defined
    triangle.findFace(Face.firstInnerId) shouldBe defined
  }

  it should "return None for non-existent face id" in {
    val triangle = createTriangleTiling()
    triangle.findFace("F999") shouldBe None
    triangle.findFace("NonExistent") shouldBe None
  }

  behavior of "TilingDCEL.findEdgeBetween"

  it should "find an edge between two connected vertices" in {
    val triangle = createTriangleTiling()
    val v0 = triangle.findVertex("V1").get
    val v1 = triangle.findVertex("V2").get
    val v2 = triangle.findVertex("V3").get

    triangle.findEdgeBetween(v0, v1) shouldBe defined
    triangle.findEdgeBetween(v1, v2) shouldBe defined
    triangle.findEdgeBetween(v2, v0) shouldBe defined
  }

  it should "return None for vertices that are not connected" in {
    val square = createSquareTiling()
    val v0 = square.findVertex("V1").get
    val v2 = square.findVertex("V3").get

    // V0 and V2 are diagonal vertices in a square, not directly connected
    square.findEdgeBetween(v0, v2) shouldBe None
  }

  it should "return None when either vertex has no incident edges" in {
    val isolatedVertex = Vertex("Isolated", BigPoint(10, 10))
    val triangle = createTriangleTiling()
    val v0 = triangle.findVertex("V1").get

    triangle.findEdgeBetween(isolatedVertex, v0) shouldBe None
  }

  behavior of "TilingDCEL.hasConnectedFaces"

  it should "return true for empty tiling" in {
    val empty = TilingBuilder.empty
    empty.hasConnectedFaces shouldBe true
  }

  it should "return true for single polygon tiling" in {
    val triangle = createTriangleTiling()
    triangle.hasConnectedFaces shouldBe true

    val square = createSquareTiling()
    square.hasConnectedFaces shouldBe true
  }

  it should "return true for connected multi-polygon tiling" in {
    val twoTriangles = createTriangleTiling().maybeAddRegularPolygon(3, "V1").value
    twoTriangles.hasConnectedFaces shouldBe true
  }

  behavior of "TilingDCEL.boundary"

  it should "return empty vector for empty tiling" in {
    val empty = TilingBuilder.empty
    empty.boundary shouldBe Vector.empty
  }

  it should "return correct boundary vertices for triangle in clockwise order" in {
    val triangle = createTriangleTiling()
    val boundary = triangle.boundary
    boundary should have length 3
    boundary.map(_.id) should contain theSameElementsInOrderAs Vector("V1", "V3", "V2")
  }

  it should "return correct boundary vertices for square in clockwise order" in {
    val square = createSquareTiling()
    val boundary = square.boundary
    boundary should have length 4
    boundary.map(_.id) should contain theSameElementsInOrderAs Vector("V1", "V4", "V3", "V2")
  }

  it should "return correct boundary vertices for hexagon" in {
    val hexagon = createHexagonTiling()
    val boundary = hexagon.boundary
    boundary should have length 6
    boundary.map(_.id) should contain theSameElementsInOrderAs Vector("V1", "V6", "V5", "V4", "V3", "V2")
  }

  behavior of "TilingDCEL.boundarySafe"

  it should "return same result as boundary for well-formed tilings" in {
    val triangle = createTriangleTiling()
    triangle.boundarySafe.value shouldEqual triangle.boundary

    val square = createSquareTiling()
    square.boundarySafe.value shouldEqual square.boundary
  }

  it should "return empty vector for empty tiling" in {
    val empty = TilingBuilder.empty
    empty.boundarySafe shouldBe Right(Vector.empty)
  }

  it should "fail for malformed boundary loop" in {
    val triangle = createTriangleTiling()
    val boundaryEdges = triangle.getBoundaryEdges.value
    if (boundaryEdges.length >= 2) {
      val firstEdge = boundaryEdges.head // This is the startEdge
      val secondEdge = boundaryEdges(1)

      // Create a malformed loop where a non-start edge is repeated
      firstEdge.next = Some(secondEdge)
      secondEdge.next = Some(secondEdge) // Make second edge point to itself

      triangle.boundarySafe.isLeft shouldBe true
    }
  }

  it should "fail for open chain in boundary" in {
    val triangle = createTriangleTiling()
    val boundaryEdges = triangle.getBoundaryEdges.value
    if (boundaryEdges.nonEmpty) {
      val firstEdge = boundaryEdges.head
      // Break the chain by setting next to None
      firstEdge.next = None

      triangle.boundarySafe.isLeft shouldBe true
    }
  }

  behavior of "TilingDCEL.getBoundaryEdges"

  it should "return empty list for empty tiling" in {
    val empty = TilingBuilder.empty
    empty.getBoundaryEdges shouldBe Right(List.empty)
  }

  it should "return boundary edges in correct order" in {
    val triangle = createTriangleTiling()
    val boundaryEdges = triangle.getBoundaryEdges.value
    boundaryEdges should have length 3

    // Check that edges form a closed loop
    val vertices = boundaryEdges.map(_.origin)
    vertices.map(_.id) should contain theSameElementsInOrderAs Vector("V1", "V3", "V2")
  }

  it should "fail for malformed boundary with visited edge" in {
    val triangle = createTriangleTiling()
    val boundaryEdges = triangle.getBoundaryEdges.value
    if (boundaryEdges.length >= 3) {
      val firstEdge = boundaryEdges.head // e0 (start)
      val secondEdge = boundaryEdges(1) // e1
      val thirdEdge = boundaryEdges(2) // e2

      // Modify the structure to create a cycle that doesn't include the start
      // Make e1 -> e2 -> e1 (cycle between e1 and e2)
      secondEdge.next = Some(thirdEdge)
      thirdEdge.next = Some(secondEdge) // This creates the problematic cycle

      triangle.getBoundaryEdges.isLeft shouldBe true
    }
  }

  it should "fail for unclosed boundary loop" in {
    val triangle = createTriangleTiling()
    val boundaryEdges = triangle.getBoundaryEdges.value
    if (boundaryEdges.nonEmpty) {
      // Break the loop by making the last edge not point back to the first
      boundaryEdges.last.next = None
      triangle.getBoundaryEdges.isLeft shouldBe true
    }
  }

  behavior of "TilingDCEL.getAnglesAtVertex"

  it should "return the angles for a vertex where all incident edges have an angle" in {
    val triangle = createTriangleTiling()
    val v1 = triangle.findVertex("V1").get
    v1.incidentEdges.filter(_.incidentFace.contains(triangle.outerFace)).foreach(_.angle = Some(AngleDegree(300)))
    val result = triangle.getAnglesAtVertex("V1")
    result.value should contain theSameElementsAs List(AngleDegree(60), AngleDegree(300))
  }

  it should "return an error for a non-existent vertex" in {
    val triangle = createTriangleTiling()
    val result = triangle.getAnglesAtVertex("V999")
    result.isLeft shouldBe true
    result.left.value shouldEqual "Vertex with ID V999 not found."
  }

  it should "return an error if an inner incident edge has no angle" in {
    val square = createSquareTiling()
    square.innerFaces.head.halfEdges.toOption.get.head.angle = None
    val result = square.getAnglesAtVertex("V1")
    result.isLeft shouldBe true
    result.left.value shouldEqual "Vertex with ID V1 has at least one edge with no angle."
  }

  it should "fail if the incident edge loop is broken" in {
    val triangle = createTriangleTiling()
    val v1Leaving = triangle.findVertex("V1").get.leaving.get
    v1Leaving.twin = None // Break the chain for vertex traversal
    val result = triangle.getAnglesAtVertex("V1")
    result.isLeft shouldBe true
    result.left.value should include("Broken edge chain")
  }

  behavior of "TilingDCEL.validate"

  it should "succeed for a valid single polygon tiling" in {
    val square = createSquareTiling()
    TilingDCEL.validate(square) shouldBe Right(())
  }

  it should "succeed for a valid multi-polygon tiling" in {
    val twoSquares = createSquareTiling().maybeAddRegularPolygon(4, "V1").value
    TilingDCEL.validate(twoSquares) shouldBe Right(())
  }

  it should "fail if a vertex has no leaving edge" in {
    val square = createSquareTiling()
    square.vertices.head.leaving = None
    val result = TilingDCEL.validate(square)
    result.isLeft shouldBe true
    result.left.value should include("has no leaving edge")
  }

  it should "fail if an edge has no twin" in {
    val square = createSquareTiling()
    square.halfEdges.head.twin = None
    val result = TilingDCEL.validate(square)
    result.isLeft shouldBe true
    result.left.value should include("has no twin")
  }

  it should "fail if an edge's next/prev relationship is broken" in {
    val square = createSquareTiling()
    val edge = square.halfEdges.head
    edge.next.get.prev = None // Break the link
    val result = TilingDCEL.validate(square)
    result.isLeft shouldBe true
    result.left.value should startWith("Next/prev relationship broken")
  }

  it should "fail if an inner face has an incorrect sum of angles" in {
    val square = createSquareTiling()
    // Tamper with an angle
    square.innerFaces.head.outerComponent.get.angle = Some(AngleDegree(89))
    val result = TilingDCEL.validate(square)
    result.isLeft shouldBe true
    result.left.value should include("The sum of interior angles is incorrect")
  }

  it should "fail if an inner face has a full circle angle" in {
    val square = createSquareTiling()
    // Tamper with angles to make one a full circle while keeping the sum correct
    val edges = square.innerFaces.head.halfEdges.value
    edges(0).angle = Some(AngleDegree(360))
    edges(1).angle = Some(AngleDegree(0))
    edges(2).angle = Some(AngleDegree(90))
    edges(3).angle = Some(AngleDegree(-90))
    val result = TilingDCEL.validate(square)
    result.isLeft shouldBe true
    result.left.value should include("cannot have full circles as interior angles")
  }

  it should "fail if a face edge does not point back to the face" in {
    val square = createSquareTiling()
    // Make an inner edge "forget" its face
    square.innerFaces.head.outerComponent.get.incidentFace = None
    val result = TilingDCEL.validate(square)
    result.isLeft shouldBe true
    result.left.value should include("references back")
  }

  it should "fail if the boundary angles do not sum correctly" in {
    val twoSquares = createSquareTiling().maybeAddRegularPolygon(4, "V1").value
    // V2 is on the boundary. The inner edge from V2 belongs to the first square.
    val v2 = twoSquares.findVertex("V2").get
    val innerEdgeFromV2 = v2.incidentEdges.find(_.incidentFace.exists(_.id == Face.firstInnerId)).get

    // Distort the angle, which affects both the face and boundary angle sums
    innerEdgeFromV2.angle = Some(AngleDegree(80))

    val result = TilingDCEL.validate(twoSquares)
    result.isLeft shouldBe true
    val error = result.left.value
    // Check that at least one of the expected errors is present, as iteration order is not guaranteed
    val faceError = "Face F1: The sum of interior angles is incorrect"
    val boundaryError = "Boundary: The sum of interior angles is incorrect"
    (error.contains(faceError) || error.contains(boundaryError)) shouldBe true
  }

  it should "fail if a boundary angle is undefined" in {
    val tiling = createSquareTiling()
    tiling.getBoundaryEdges.value.head.angle = None
    val result = TilingDCEL.validate(tiling)
    result.isLeft shouldBe true
    result.left.value should include("Undefined boundary angles")
  }

  it should "fail if a boundary angle is a full circle (360 degrees)" in {
    val tiling = createSquareTiling()
    tiling.getBoundaryEdges.value.head.angle = Some(AngleDegree(360))
    val result = TilingDCEL.validate(tiling)
    result.isLeft shouldBe true
    result.left.value should include("Full circle boundary angles")
  }

  it should "fail if a boundary angle is a full circle (0 degrees)" in {
    val tiling = createSquareTiling()
    tiling.getBoundaryEdges.value.head.angle = Some(AngleDegree(0))
    val result = TilingDCEL.validate(tiling)
    result.isLeft shouldBe true
    result.left.value should include("Full circle boundary angles")
  }

  behavior of "TilingDCEL.deepCopy"

  it should "create a copy with same structural properties as original" in {
    val original = createTriangleTiling()
    val copy = original.deepCopy

    // Basic structural properties should match
    copy.vertices should have length original.vertices.length
    copy.halfEdges should have length original.halfEdges.length
    copy.innerFaces should have length original.innerFaces.length
    copy.faces should have length original.faces.length

    // Vertex IDs and coordinates should match
    copy.vertices.map(_.id) should contain theSameElementsAs original.vertices.map(_.id)
    copy.vertices.zip(original.vertices).foreach { case (copyV, origV) =>
      copyV.id shouldEqual origV.id
      copyV.coords shouldEqual origV.coords
    }

    // Face IDs should match
    copy.faces.map(_.id) should contain theSameElementsAs original.faces.map(_.id)
  }

  it should "create completely independent objects" in {
    val original = createSquareTiling()
    val copy = original.deepCopy

    // Verify that all objects are different instances
    original.vertices.zip(copy.vertices).foreach { case (origV, copyV) =>
      origV should not be theSameInstanceAs(copyV)
    }

    original.halfEdges.zip(copy.halfEdges).foreach { case (origE, copyE) =>
      origE should not be theSameInstanceAs(copyE)
    }

    original.faces.zip(copy.faces).foreach { case (origF, copyF) =>
      origF should not be theSameInstanceAs(copyF)
    }
  }

  it should "preserve all cross-references correctly" in {
    val original = createHexagonTiling()
    val copy = original.deepCopy

    // Check vertex leaving edges
    copy.vertices.foreach { vertex =>
      vertex.leaving shouldBe defined
      vertex.leaving.get.origin should be theSameInstanceAs vertex
    }

    // Check half-edge relationships
    copy.halfEdges.foreach { edge =>
      // Twin relationships
      edge.twin shouldBe defined
      edge.twin.get.twin should contain(edge)

      // Next/prev relationships
      edge.next shouldBe defined
      edge.prev shouldBe defined
      edge.next.get.prev should contain(edge)
      edge.prev.get.next should contain(edge)

      // Origin relationships - check that the edge is among the vertex's incident edges
      edge.origin.incidentEdges should contain(edge)

      // Incident face relationships
      edge.incidentFace shouldBe defined
    }

    // Check face outer components
    copy.faces.foreach { face =>
      if face.outerComponent.isDefined then
        face.outerComponent.get.incidentFace should contain(face)
    }

    // Additional check: verify that leaving edges are actually incident to their vertices
    copy.vertices.foreach { vertex =>
      vertex.leaving.foreach { leavingEdge =>
        vertex.incidentEdges should contain(leavingEdge)
      }
    }
  }

  it should "maintain DCEL validation after copying" in {
    val original = createTriangleTiling()
    val copy = original.deepCopy

    // Both original and copy should validate successfully
    TilingDCEL.validate(original) shouldBe Right(())
    TilingDCEL.validate(copy) shouldBe Right(())
  }

  it should "not affect original when copy is modified" in {
    val original = createSquareTiling()
    val copy = original.deepCopy

    // Get original boundary before modification
    val originalBoundaryBefore = original.boundary
    val copyBoundaryBefore = copy.boundary

    originalBoundaryBefore shouldEqual copyBoundaryBefore

    // Modify the copy by adding a polygon
    val modifiedCopy = copy.maybeAddRegularPolygon(3, "V1")
    modifiedCopy shouldBe a[Right[_, _]]

    // Original should remain unchanged
    val originalBoundaryAfter = original.boundary
    originalBoundaryBefore shouldEqual originalBoundaryAfter

    // Original structure should still be valid
    TilingDCEL.validate(original) shouldBe Right(())

    // Original should have same number of components as before
    original.vertices should have length 4
    original.innerFaces should have length 1
  }

  it should "not affect copy when original is modified" in {
    val original = createTriangleTiling()
    val copy = original.deepCopy

    // Get copy boundary before modification
    val copyBoundaryBefore = copy.boundary

    // Modify the original by adding a polygon
    val modifiedOriginal = original.maybeAddRegularPolygon(4, "V1")
    modifiedOriginal shouldBe a[Right[_, _]]

    // Copy should remain unchanged
    val copyBoundaryAfter = copy.boundary
    copyBoundaryBefore shouldEqual copyBoundaryAfter

    // Copy structure should still be valid
    TilingDCEL.validate(copy) shouldBe Right(())

    // Copy should have same number of components as before
    copy.vertices should have length 3
    copy.innerFaces should have length 1
  }

  it should "work correctly with empty tiling" in {
    val original = TilingBuilder.empty
    val copy = original.deepCopy

    copy.vertices shouldBe empty
    copy.halfEdges shouldBe empty
    copy.innerFaces shouldBe empty
    copy.outerFace should not be theSameInstanceAs(original.outerFace)
    copy.outerFace.id shouldEqual original.outerFace.id
  }

  it should "preserve boundary traversal functionality" in {
    val original = createHexagonTiling()
    val copy = original.deepCopy

    // Boundary traversal should work the same way
    original.boundary shouldEqual copy.boundary
    original.boundarySafe shouldEqual copy.boundarySafe

    // The actual vertex instances should be different but have same properties
    original.boundary.zip(copy.boundary).foreach { case (origV, copyV) =>
      origV should not be theSameInstanceAs(copyV)
      origV.id shouldEqual copyV.id
      origV.coords shouldEqual copyV.coords
    }
  }

  it should "preserve angle information correctly" in {
    val original = createTriangleTiling()
    val copy = original.deepCopy

    // Check that angles are preserved
    original.vertices.zip(copy.vertices).foreach { case (origV, copyV) =>
      val origAngles = original.getAnglesAtVertex(origV.id)
      val copyAngles = copy.getAnglesAtVertex(copyV.id)

      origAngles shouldEqual copyAngles
    }

    // Check half-edge angles
    original.halfEdges.zip(copy.halfEdges).foreach { case (origE, copyE) =>
      origE.angle shouldEqual copyE.angle
    }
  }

  it should "maintain connectedness property" in {
    val original = createSquareTiling()
    val copy = original.deepCopy

    original.hasConnectedFaces shouldEqual copy.hasConnectedFaces

    // Add polygons to both and check they remain connected
    val expandedOriginal = original.maybeAddRegularPolygon(3, "V1").value
    val expandedCopy = copy.maybeAddRegularPolygon(3, "V1").value

    expandedOriginal.hasConnectedFaces shouldBe true
    expandedCopy.hasConnectedFaces shouldBe true
  }

  it should "work correctly for complex multi-polygon tilings" in {
    val original = createTriangleTiling()
      .maybeAddRegularPolygon(4, "V1").value
      .maybeAddRegularPolygon(3, "V5").value

    val copy = original.deepCopy

    // Verify structure is preserved
    copy.vertices should have length original.vertices.length
    copy.innerFaces should have length original.innerFaces.length

    // Verify both validate correctly
    TilingDCEL.validate(original) shouldBe Right(())
    TilingDCEL.validate(copy) shouldBe Right(())

    // Verify independence by modifying each
    val modifiedOriginal = original.maybeAddRegularPolygon(6, "V2")
    val modifiedCopy = copy.maybeAddRegularPolygon(5, "V3")

    modifiedOriginal shouldBe a[Right[_, _]]
    modifiedCopy shouldBe a[Right[_, _]]

    // They should have different numbers of faces now
    modifiedOriginal.value.innerFaces should have length (original.innerFaces.length + 1)
    modifiedCopy.value.innerFaces should have length (copy.innerFaces.length + 1)
  }
