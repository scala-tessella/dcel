package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingEquivalency.*
import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.{BigPoint, RegularPolygon}
import io.github.scala_tessella.dcel.structure.{FaceId, VertexId}
import io.github.scala_tessella.ring_seq.RingSeq.reflectAt
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingEquivalencySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "TilingDCEL.deepCopy"

  it should "create a copy with same structural properties as original" in:
    val original = triangle
    val copy     = Tiling.trusted(original.deepCopy)

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

  it should "create completely independent objects" in:
    val original = square
    val copy     = Tiling.trusted(original.deepCopy)

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

  it should "preserve all cross-references correctly" in:
    val original = hexagon
    val copy     = Tiling.trusted(original.deepCopy)

    allAssert(
      {
        val assertions =
          copy.vertices.map: vertex =>
            allAssert(
              vertex.leaving shouldBe defined,
              vertex.leaving.get.origin should be theSameInstanceAs vertex
            )
        // Check vertex leaving edges
        allAssert(assertions*)
      }, {
        val assertions =
          copy.halfEdges.map: edge =>
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
        // Check half-edge relationships
        allAssert(assertions*)
      }, {
        val assertions =
          copy.faces.map: face =>
            if face.outerComponent.isDefined then
              face.outerComponent.get.incidentFace should contain(face)
            else fail()
        // Check face outer components
        allAssert(assertions*)
      }, {
        val assertions =
          copy.vertices.flatMap: vertex =>
            vertex.leaving.map: leavingEdge =>
              vertex.incidentEdgesUnsafe should contain(leavingEdge)
        // Additional check: verify that leaving edges are actually incident to their vertices
        allAssert(assertions*)
      }
    )

  it should "maintain DCEL validation after copying" in:
    val original = triangle
    val copy     = Tiling.trusted(original.deepCopy)

    // Both original and copy should validate successfully
    allAssert(
      validate(original) shouldBe Right(()),
      validate(copy) shouldBe Right(())
    )

  it should "not affect original when copy is modified" in:
    val original = square
    val copy     = Tiling.trusted(original.deepCopy)

    // Get the original boundary before modification
    val originalBoundaryBefore = original.boundaryVerticesUnsafe
    val copyBoundaryBefore     = copy.boundaryVerticesUnsafe

    allAssert(
      originalBoundaryBefore shouldEqual copyBoundaryBefore, {
        // Modify the copy by adding a polygon
        val modifiedCopy = copy.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(3))
        modifiedCopy shouldBe a[Right[?, ?]]
      }, {
        // Original should remain unchanged
        val originalBoundaryAfter = original.boundaryVerticesUnsafe
        originalBoundaryBefore shouldEqual originalBoundaryAfter
      },
      // The original structure should still be valid
      validate(original) shouldBe Right(()),
      // Original should have the same number of components as before
      original.vertices should have length 4,
      original.innerFaces should have length 1
    )

  it should "not affect copy when original is modified" in:
    val original = triangle
    val copy     = Tiling.trusted(original.deepCopy)

    // Get copy boundary before modification
    val copyBoundaryBefore = copy.boundaryVerticesUnsafe

    // Modify the original by adding a polygon
    val modifiedOriginal = original.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(4))
    allAssert(
      modifiedOriginal shouldBe a[Right[?, ?]], {
        // Copy should remain unchanged
        val copyBoundaryAfter = copy.boundaryVerticesUnsafe
        copyBoundaryBefore shouldEqual copyBoundaryAfter
      },
      // Copy structure should still be valid
      validate(copy) shouldBe Right(()),

      // Copy should have the same number of components as before
      copy.vertices should have length 3,
      copy.innerFaces should have length 1
    )

  it should "work correctly with empty tiling" in:
    val original = emptyTiling
    val copy     = Tiling.trusted(original.deepCopy)

    allAssert(
      copy.vertices shouldBe empty,
      copy.halfEdges shouldBe empty,
      copy.innerFaces shouldBe empty,
      copy.outerFace should not be theSameInstanceAs(original.outerFace),
      copy.outerFace.id shouldEqual original.outerFace.id
    )

  it should "preserve boundary traversal functionality" in:
    val original = hexagon
    val copy     = Tiling.trusted(original.deepCopy)

    // Boundary traversal should work the same way
    allAssert(
      original.boundaryVerticesUnsafe shouldEqual copy.boundaryVerticesUnsafe,
      original.boundaryVertices shouldEqual copy.boundaryVertices,

      // The actual vertex instances should be different but have the same properties
      allAssert(
        original.boundaryVerticesUnsafe.zip(copy.boundaryVerticesUnsafe).map { case (origV, copyV) =>
          allAssert(
            origV should not be theSameInstanceAs(copyV),
            origV.id shouldEqual copyV.id,
            origV.coords shouldEqual copyV.coords
          )
        }*
      )
    )

  it should "preserve angle information correctly" in:
    val original = triangle
    val copy     = Tiling.trusted(original.deepCopy)

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

  it should "maintain connectedness property" in:
    val original = square
    val copy     = Tiling.trusted(original.deepCopy)
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

  it should "work correctly for complex multi-polygon tilings" in:
    val original = triangle
      .maybeAddRegularPolygonToBoundary(V1, RegularPolygon(4)).value
      .maybeAddRegularPolygonToBoundary(V5, RegularPolygon(3)).value

    val copy = Tiling.trusted(original.deepCopy)

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

  behavior of "TilingDCEL.translatedDouble"

  it should "have the possibility to transform coordinates and vertex ids" in:
    val transformed =
      square.translatedDouble(
        _ + BigPoint(1, 0),
        vertexId => VertexId(vertexId.value + 4),
        faceId =>
          FaceId(
            faceId.value match
              case 0 => 0
              case n => n + 1
          )
      )
    allAssert(
      transformed.vertices.map(_.toString) shouldEqual
        List(
          "Vertex V5 at coords (1, 0)",
          "Vertex V6 at coords (2, 0)",
          "Vertex V7 at coords (2, 1)",
          "Vertex V8 at coords (1, 1)"
        ),
      transformed.faces.map(_.id) shouldEqual
        List(F0, F2)
    )

  /** <img src="file:../../../../../resources/shapeL.svg"/> */
  def shapeL: Tiling = square
    .maybeAddRegularPolygonToBoundary(V3, RegularPolygon(4)).value
    .maybeAddRegularPolygonToBoundary(V4, RegularPolygon(4)).value
    .maybeAddRegularPolygonToBoundary(VertexId(7), RegularPolygon(4)).value

  behavior of "TilingDCEL.reflectedCopy"

  it should "create a valid reflected copy" in:
    val reflected = shapeL.verticallyReflectedCopy
    allAssert(
      shapeL.boundaryVerticesUnsafe.map(_.id) shouldBe Vector(V1, V4, 7, 9, 10, 8, V3, V5, V6, V2),
      validate(reflected).isRight shouldBe true,
      reflected.boundaryVerticesUnsafe.map(_.id) shouldBe shapeL.boundaryVerticesUnsafe.map(_.id).reflectAt(1)
    )
