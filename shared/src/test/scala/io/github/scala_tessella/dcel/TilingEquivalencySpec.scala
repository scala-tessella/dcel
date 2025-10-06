package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingDeletion.*
import io.github.scala_tessella.dcel.TilingEquivalency.*
import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.RegularPolygon
import io.github.scala_tessella.dcel.structure.VertexId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingEquivalencySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingDCEL.deepCopy"

  it should "create a copy with same structural properties as original" in {
    val original = triangle
    val copy     = original.deepCopy

    allAssert(
      // Basic structural properties should match
      copy.vertices should have length original.vertices.length,
      copy.halfEdges should have length original.halfEdges.length,
      copy.innerFaces should have length original.innerFaces.length,
      copy.faces should have length original.faces.length,
      // Vertex IDs and coordinates should match
      copy.vertices.map(_.id) should contain theSameElementsAs original.vertices.map(_.id),
      allAssert(
        copy.vertices.zip(original.vertices).map { case (copyV, origV) =>
          allAssert(
            copyV.id shouldEqual origV.id,
            copyV.coords shouldEqual origV.coords
          )
        }*
      ),
      // Face IDs should match
      copy.faces.map(_.id) should contain theSameElementsAs original.faces.map(_.id)
    )
  }

  it should "create completely independent objects" in {
    val original = square
    val copy     = original.deepCopy

    allAssert(
      allAssert(
        // Verify that all objects are different instances
        original.vertices.zip(copy.vertices).map { case (origV, copyV) =>
          origV should not be theSameInstanceAs(copyV)
        }*
      ),
      allAssert(
        original.halfEdges.zip(copy.halfEdges).map { case (origE, copyE) =>
          origE should not be theSameInstanceAs(copyE)
        }*
      ),
      allAssert(
        original.faces.zip(copy.faces).map { case (origF, copyF) =>
          origF should not be theSameInstanceAs(copyF)
        }*
      )
    )
  }

  it should "preserve all cross-references correctly" in {
    val original = hexagon
    val copy     = original.deepCopy

    allAssert(
      // Check vertex leaving edges
      allAssert(
        copy.vertices.map { vertex =>

          allAssert(
            vertex.leaving shouldBe defined,
            vertex.leaving.get.origin should be theSameInstanceAs vertex
          )
        }*
      ),
      allAssert(
        // Check half-edge relationships
        copy.halfEdges.map { edge =>

          allAssert(
            // Twin relationships
            edge.twin shouldBe defined,
            edge.twin.get.twin should contain(edge),

            // Next/prev relationships
            edge.next shouldBe defined,
            edge.prev shouldBe defined,
            edge.next.get.prev should contain(edge),
            edge.prev.get.next should contain(edge),

            // Origin relationships - check that the edge is among the vertex's incident edges
            edge.origin.incidentEdgesUnsafe should contain(edge),

            // Incident face relationships
            edge.incidentFace shouldBe defined
          )
        }*
      ),
      allAssert(
        // Check face outer components
        copy.faces.map { face =>

          if face.outerComponent.isDefined then
            face.outerComponent.get.incidentFace should contain(face)
          else fail()
        }*
      ),
      allAssert(
        // Additional check: verify that leaving edges are actually incident to their vertices
        copy.vertices.flatMap { vertex =>

          vertex.leaving.map { leavingEdge =>

            vertex.incidentEdgesUnsafe should contain(leavingEdge)
          }
        }*
      )
    )
  }

  it should "maintain DCEL validation after copying" in {
    val original = triangle
    val copy     = original.deepCopy

    // Both original and copy should validate successfully
    allAssert(
      validate(original) shouldBe Right(()),
      validate(copy) shouldBe Right(())
    )
  }

  it should "not affect original when copy is modified" in {
    val original = square
    val copy     = original.deepCopy

    // Get the original boundary before modification
    val originalBoundaryBefore = original.boundaryVertices
    val copyBoundaryBefore     = copy.boundaryVertices

    allAssert(
      originalBoundaryBefore shouldEqual copyBoundaryBefore, {
        // Modify the copy by adding a polygon
        val modifiedCopy = copy.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3))
        modifiedCopy shouldBe a[Right[?, ?]]
      }, {
        // Original should remain unchanged
        val originalBoundaryAfter = original.boundaryVertices
        originalBoundaryBefore shouldEqual originalBoundaryAfter
      },
      // The original structure should still be valid
      validate(original) shouldBe Right(()),
      // Original should have the same number of components as before
      original.vertices should have length 4,
      original.innerFaces should have length 1
    )
  }

  it should "not affect copy when original is modified" in {
    val original = triangle
    val copy     = original.deepCopy

    // Get copy boundary before modification
    val copyBoundaryBefore = copy.boundaryVertices

    // Modify the original by adding a polygon
    val modifiedOriginal = original.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(4))
    allAssert(
      modifiedOriginal shouldBe a[Right[?, ?]], {
        // Copy should remain unchanged
        val copyBoundaryAfter = copy.boundaryVertices
        copyBoundaryBefore shouldEqual copyBoundaryAfter
      },
      // Copy structure should still be valid
      validate(copy) shouldBe Right(()),

      // Copy should have the same number of components as before
      copy.vertices should have length 3,
      copy.innerFaces should have length 1
    )
  }

  it should "work correctly with empty tiling" in {
    val original = emptyTiling
    val copy     = original.deepCopy

    allAssert(
      copy.vertices shouldBe empty,
      copy.halfEdges shouldBe empty,
      copy.innerFaces shouldBe empty,
      copy.outerFace should not be theSameInstanceAs(original.outerFace),
      copy.outerFace.id shouldEqual original.outerFace.id
    )
  }

  it should "preserve boundary traversal functionality" in {
    val original = hexagon
    val copy     = original.deepCopy

    // Boundary traversal should work the same way
    allAssert(
      original.boundaryVertices shouldEqual copy.boundaryVertices,
      original.boundaryVerticesSafer shouldEqual copy.boundaryVerticesSafer,

      // The actual vertex instances should be different but have the same properties
      allAssert(
        original.boundaryVertices.zip(copy.boundaryVertices).map { case (origV, copyV) =>
          allAssert(
            origV should not be theSameInstanceAs(copyV),
            origV.id shouldEqual copyV.id,
            origV.coords shouldEqual copyV.coords
          )
        }*
      )
    )
  }

  it should "preserve angle information correctly" in {
    val original = triangle
    val copy     = original.deepCopy

    allAssert(
      // Check that angles are preserved
      allAssert(
        original.vertices.zip(copy.vertices).map { case (origV, copyV) =>
          val origAngles = original.getAnglesAtVertex(origV.id)
          val copyAngles = copy.getAnglesAtVertex(copyV.id)

          origAngles shouldEqual copyAngles
        }*
      ),
      // Check half-edge angles
      allAssert(
        original.halfEdges.zip(copy.halfEdges).map { case (origE, copyE) =>
          origE.angle shouldEqual copyE.angle
        }*
      )
    )
  }

  it should "maintain connectedness property" in {
    val original = square
    val copy     = original.deepCopy
    allAssert(
      original.hasConnectedFaces shouldEqual copy.hasConnectedFaces, {
        // Add polygons to both and check they remain connected
        val expandedOriginal = original.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
        val expandedCopy     = copy.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
        allAssert(
          expandedOriginal.hasConnectedFaces shouldBe true,
          expandedCopy.hasConnectedFaces shouldBe true
        )
      }
    )
  }

  it should "work correctly for complex multi-polygon tilings" in {
    val original = triangle
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(4)).value
      .maybeAddRegularPolygonToBoundary(V5, RegularPolygon(3)).value

    val copy = original.deepCopy

    allAssert(
      // Verify the structure is preserved
      copy.vertices should have length original.vertices.length,
      copy.innerFaces should have length original.innerFaces.length,

      // Verify both validate correctly
      validate(original) shouldBe Right(()),
      validate(copy) shouldBe Right(()), {
        // Verify independence by modifying each
        val modifiedOriginal = original.maybeAddRegularPolygonToBoundary(V2, RegularPolygon(6))
        val modifiedCopy     = copy.maybeAddRegularPolygonToBoundary(V3, RegularPolygon(5))
        allAssert(
          modifiedOriginal shouldBe a[Right[?, ?]],
          modifiedCopy shouldBe a[Right[?, ?]],

          // They should have different numbers of faces now
          modifiedOriginal.value.innerFaces should have length (original.innerFaces.length + 1),
          modifiedCopy.value.innerFaces should have length (copy.innerFaces.length + 1)
        )
      }
    )
  }

  behavior of "TilingDCEL.isTopologicallyEquivalentTo"

  it should "return true for the same instance" in {
    triangle.isTopologicallyEquivalentTo(triangle) shouldBe true
  }

  it should "return true for a deep copy of a tiling" in {
    val squareCopy = square.deepCopy
    square.isTopologicallyEquivalentTo(squareCopy) shouldBe true
  }

  it should "return true for two identical but separate tilings" in {
    val triangle1 = triangle
    val triangle2 = triangle
    triangle1.isTopologicallyEquivalentTo(triangle2) shouldBe true
  }

  it should "return false for tilings with different numbers of components" in {
    triangle.isTopologicallyEquivalentTo(square) shouldBe false
  }

  it should "return false for tilings with different face signatures" in {
    // Both have 2 faces, 7 vertices, 16 half-edges
    val tiling1 = square.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
    // Both have 2 faces, 8 vertices, 18 half-edges
    val tiling2 = square.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(4)).value
    tiling1.isTopologicallyEquivalentTo(tiling2) shouldBe false
  }

  it should "return true for two complex tilings built differently but structurally identical" in {
    // Tiling A: Add a triangle to V1, then another to V4
    val tilingA = triangle
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V4, RegularPolygon(3)).value
    // Tiling B: Add a triangle to V2, then another to V4
    val tilingB = triangle
      .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(3)).value
      .maybeAddRegularPolygonToBoundary(V4, RegularPolygon(3)).value

    tilingA.isTopologicallyEquivalentTo(tilingB) shouldBe true
  }

  it should "return false for tilings with the same face signatures but different vertex signatures" in {
    // Tiling 1: Four squares in a 2x2 grid
    val gridTiling = TilingBuilder.createRhombusNet(2, 2) // V7 is on the new edge

    // Tiling 2: Four squares in a line
    val lineTiling = TilingBuilder.createRhombusNet(4, 1)

    // Both have 4 square faces, but the arrangement of vertices is different.
    // Grid has a central vertex of degree 4, line does not.
    allAssert(
      gridTiling.innerFaces.map(
        _.halfEdgesUnsafe.size
      ) should contain theSameElementsAs lineTiling.innerFaces.map(_.halfEdgesUnsafe.size),
      gridTiling.isTopologicallyEquivalentTo(lineTiling) shouldBe false
    )
  }

  it should "return false for an empty tiling vs a non-empty one" in {
    emptyTiling.isTopologicallyEquivalentTo(triangle) shouldBe false
  }

  it should "return false for two different rhombuses" in
    allAssert(
      square.isTopologicallyEquivalentTo(rhombus) shouldBe true,
      square.isEquivalentTo(rhombus) shouldBe false
    )

  /** <img src="file:../../../../../resources/shapeL.svg"/> */
  def shapeL: TilingDCEL = square
    .maybeAddRegularPolygonToBoundary(V3, RegularPolygon(4)).value
    .maybeAddRegularPolygonToBoundary(V4, RegularPolygon(4)).value
    .maybeAddRegularPolygonToBoundary(VertexId("V7"), RegularPolygon(4)).value

  /** <img src="file:../../../../../resources/shapeΓ.svg"/> */
  def shapeΓ: TilingDCEL =
    shapeL.verticallyReflectedCopy

  /** <img src="file:../../../../../resources/shapeL2.svg"/> */
  def shapeL2: TilingDCEL = square
    .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(4)).value
    .maybeAddRegularPolygonToBoundary(V2, RegularPolygon(3)).value

  /** <img src="file:../../../../../resources/shapeΓ2.svg"/> */
  def shapeΓ2: TilingDCEL =
    shapeL2.verticallyReflectedCopy

  it should "return true for two reflected shapes" in
    allAssert(
      shapeL.isTopologicallyEquivalentTo(shapeΓ) shouldBe true,
      shapeL.isEquivalentTo(shapeΓ) shouldBe true,
      shapeL2.isTopologicallyEquivalentTo(shapeΓ2) shouldBe true,
      shapeL2.isEquivalentTo(shapeΓ2) shouldBe true
    )

  def net: TilingDCEL = TilingBuilder.createRhombusNet(3, 6)

  /** <img src="file:../../../../../resources/holeInNet1.svg"/> */
  def holeInNet1: TilingDCEL = net.deleteEdge(VertexId("V18"), VertexId("V19")).value

  /** <img src="file:../../../../../resources/holeInNet2.svg"/> */
  def holeInNet2: TilingDCEL = net.deleteEdge(VertexId("V14"), VertexId("V15")).value

  it should "fail for two similar but different tiling" in {
    holeInNet1.isTopologicallyEquivalentTo(holeInNet2) shouldBe false
//    shape1.isEquivalentTo(shape2) shouldBe false
  }

  behavior of "TilingDCEL.reflectedCopy"

  it should "create a valid reflected copy" in {
    val reflected = shapeL.verticallyReflectedCopy
    validate(reflected).isRight shouldBe true
//    println(reflected.toSVG())
  }

  behavior of "TilingDCEL.isReflectionOf"

  it should "return true when comparing two reflected shapes" in
    allAssert(
      shapeL.isReflectionOf(shapeΓ) shouldBe true,
      shapeΓ.isReflectionOf(shapeL) shouldBe true,
      shapeL2.isReflectionOf(shapeΓ2) shouldBe true,
      shapeΓ2.isReflectionOf(shapeL2) shouldBe true
    )

  behavior of "TilingDCEL.isRotationOf"

  it should "return false when comparing two reflected shapes" in
    allAssert(
      shapeL.isRotationOf(shapeΓ) shouldBe false,
      shapeΓ.isRotationOf(shapeL) shouldBe false,
      shapeL2.isRotationOf(shapeΓ2) shouldBe false,
      shapeΓ2.isRotationOf(shapeL2) shouldBe false
    )
