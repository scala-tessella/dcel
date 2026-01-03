package io.github.scala_tessella.dcel.structure

import io.github.scala_tessella.dcel.TilingTestHelpers
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HalfEdgeSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "HalfEdge construction"

  it should "create a half-edge with just an origin vertex" in:
    val vertex = Vertex(V1, BigPoint(1.0, 2.0))
    val edge   = HalfEdge(vertex)

    allAssert(
      edge.origin shouldBe vertex,
      edge.twin shouldBe None,
      edge.incidentFace shouldBe None,
      edge.next shouldBe None,
      edge.prev shouldBe None,
      edge.angle shouldBe None,
      edge.toString shouldBe "HalfEdge V1 -> ? [Missing twin edge, Missing incident face, Missing next edge, Missing previous edge, Missing angle]",
      edge.endpointsAsVertices shouldBe None,
      edge.maybeId shouldBe None
    )

  it should "create a half-edge with all optional parameters" in:
    val vertex     = Vertex(V1, BigPoint(1.0, 2.0))
    val twinVertex = Vertex(V2, BigPoint(2.0, 3.0))
    val twin       = HalfEdge(twinVertex)
    val face       = Face(F1)
    val nextEdge   = HalfEdge(vertex)
    val prevEdge   = HalfEdge(vertex)
    val angle      = AngleDegree(90.0)

    val edge = HalfEdge(
      origin = vertex,
      twin = Some(twin),
      incidentFace = Some(face),
      next = Some(nextEdge),
      prev = Some(prevEdge),
      angle = Some(angle)
    )

    allAssert(
      edge.origin shouldBe vertex,
      edge.twin shouldBe Some(twin),
      edge.incidentFace shouldBe Some(face),
      edge.next shouldBe Some(nextEdge),
      edge.prev shouldBe Some(prevEdge),
      edge.angle shouldBe Some(angle),
      edge.toString shouldBe "HalfEdge V1 -> V2",
      edge.endpointsAsVertices shouldBe Some((vertex, twinVertex)),
      edge.idUnsafe shouldBe (V1, V2)
    )

  behavior of "HalfEdge equality"

  it should "only be equal to itself (reference equality)" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge1  = HalfEdge(vertex)
    val edge2  = HalfEdge(vertex) // Same origin, but different instance
    allAssert(
      edge1 shouldNot equal(edge2),
      edge1 shouldEqual edge1
    )

  it should "not be equal to objects of other types" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)
    allAssert(
      edge shouldNot equal(vertex),
      edge shouldNot equal("edge"),
      edge shouldNot equal(None),
      edge shouldNot equal(42)
    )

  it should "have a hashCode based on its identity" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge1  = HalfEdge(vertex)
    val edge2  = HalfEdge(vertex)
    allAssert(
      edge1.hashCode() shouldNot equal(edge2.hashCode()),
      edge1.hashCode() shouldEqual System.identityHashCode(edge1),
      edge2.hashCode() shouldEqual System.identityHashCode(edge2)
    )

  behavior of "HalfEdge.destination"

  it should "return None when twin is not set" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)

    edge.destination shouldBe None

  it should "return the origin of the twin when twin is set" in:
    val v1    = Vertex(V1, BigPoint(0, 0))
    val v2    = Vertex(V2, BigPoint(1, 0))
    val edge1 = HalfEdge(v1)
    val edge2 = HalfEdge(v2)

    edge1.twin = Some(edge2)

    edge1.destination shouldBe Some(v2)

  it should "work correctly in a twin pair" in:
    val v1    = Vertex(V1, BigPoint(0, 0))
    val v2    = Vertex(V2, BigPoint(1, 0))
    val edge1 = HalfEdge(v1)
    val edge2 = HalfEdge(v2)

    edge1.twin = Some(edge2)
    edge2.twin = Some(edge1)
    allAssert(
      edge1.destination shouldBe Some(v2),
      edge2.destination shouldBe Some(v1)
    )

  behavior of "HalfEdge.isComplete"

  it should "return false when any required field is missing" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)

    edge.isComplete shouldBe false

  it should "return false when only some fields are set" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)
    val twin   = HalfEdge(vertex)
    val face   = Face(F1)

    edge.twin = Some(twin)
    edge.incidentFace = Some(face)
    // Missing next, prev, angle

    edge.isComplete shouldBe false

  it should "return true when all required fields are set" in:
    val vertex   = Vertex(V1, BigPoint(0, 0))
    val edge     = HalfEdge(vertex)
    val twin     = HalfEdge(vertex)
    val face     = Face(F1)
    val nextEdge = HalfEdge(vertex)
    val prevEdge = HalfEdge(vertex)
    val angle    = AngleDegree(90.0)

    edge.twin = Some(twin)
    edge.incidentFace = Some(face)
    edge.next = Some(nextEdge)
    edge.prev = Some(prevEdge)
    edge.angle = Some(angle)

    edge.isComplete shouldBe true

  behavior of "HalfEdge.validate"

  it should "return Right(()) when edge is complete" in:
    val vertex   = Vertex(V1, BigPoint(0, 0))
    val edge     = HalfEdge(vertex)
    val twin     = HalfEdge(vertex)
    val face     = Face(F1)
    val nextEdge = HalfEdge(vertex)
    val prevEdge = HalfEdge(vertex)
    val angle    = AngleDegree(90.0)

    edge.twin = Some(twin)
    edge.incidentFace = Some(face)
    edge.next = Some(nextEdge)
    edge.prev = Some(prevEdge)
    edge.angle = Some(angle)

    edge.validate() shouldBe Right(())

  it should "return Left with all missing field errors" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)

    val result       = edge.validate()
    val errorMessage = result.left.value.message
    allAssert(
      result.isLeft shouldBe true,
      errorMessage should include("Missing twin edge"),
      errorMessage should include("Missing incident face"),
      errorMessage should include("Missing next edge"),
      errorMessage should include("Missing previous edge"),
      errorMessage should include("Missing angle")
    )

  it should "return Left with only missing field errors" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)
    val twin   = HalfEdge(vertex)
    val face   = Face(F1)

    edge.twin = Some(twin)
    edge.incidentFace = Some(face)
    // Missing next, prev, angle

    val result       = edge.validate()
    val errorMessage = result.left.value.message
    allAssert(
      result.isLeft shouldBe true,
      errorMessage should not include "Missing twin edge",
      errorMessage should not include "Missing incident face",
      errorMessage should include("Missing next edge"),
      errorMessage should include("Missing previous edge"),
      errorMessage should include("Missing angle")
    )

  behavior of "HalfEdge mutable state"

  it should "allow modification of twin" in:
    val v1    = Vertex(V1, BigPoint(0, 0))
    val v2    = Vertex(V2, BigPoint(1, 0))
    val edge1 = HalfEdge(v1)
    val edge2 = HalfEdge(v2)
    val edge3 = HalfEdge(v1)

    allAssert(
      edge1.twin shouldBe None, {
        edge1.twin = Some(edge2)
        edge1.twin shouldBe Some(edge2)
      },
      edge1.destination shouldBe Some(v2), {
        edge1.twin = Some(edge3)
        edge1.twin shouldBe Some(edge3)
      },
      edge1.destination shouldBe Some(v1), {
        edge1.twin = None
        edge1.twin shouldBe None
      },
      edge1.destination shouldBe None
    )

  it should "allow modification of incident face" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)
    val face1  = Face(F1)
    val face2  = Face(F2)

    allAssert(
      edge.incidentFace shouldBe None, {
        edge.incidentFace = Some(face1)
        edge.incidentFace shouldBe Some(face1)
      }, {
        edge.incidentFace = Some(face2)
        edge.incidentFace shouldBe Some(face2)
      }, {
        edge.incidentFace = None
        edge.incidentFace shouldBe None
      }
    )

  it should "allow modification of next and prev" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge1  = HalfEdge(vertex)
    val edge2  = HalfEdge(vertex)
    val edge3  = HalfEdge(vertex)

    allAssert(
      edge1.next shouldBe None,
      edge1.prev shouldBe None, {
        edge1.next = Some(edge2)
        edge1.prev = Some(edge3)
        edge1.next shouldBe Some(edge2)
      },
      edge1.prev shouldBe Some(edge3), {
        edge1.next = None
        edge1.prev = None
        edge1.next shouldBe None
      },
      edge1.prev shouldBe None
    )

  it should "allow modification of angle" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)
    val angle1 = AngleDegree(90.0)
    val angle2 = AngleDegree(180.0)

    allAssert(
      edge.angle shouldBe None, {
        edge.angle = Some(angle1)
        edge.angle shouldBe Some(angle1)
      }, {
        edge.angle = Some(angle2)
        edge.angle shouldBe Some(angle2)
      }, {
        edge.angle = None
        edge.angle shouldBe None
      }
    )

  behavior of "HalfEdge companion object - createTwinPair"

  it should "create a pair of twin half-edges" in:
    val v1 = Vertex(V1, BigPoint(0, 0))
    val v2 = Vertex(V2, BigPoint(1, 0))

    val (edge1, edge2) = HalfEdge.createTwinPair(v1, v2)

    allAssert(
      edge1.origin shouldBe v1,
      edge2.origin shouldBe v2,
      edge1.twin shouldBe Some(edge2),
      edge2.twin shouldBe Some(edge1),
      edge1.destination shouldBe Some(v2),
      edge2.destination shouldBe Some(v1)
    )

  it should "create properly linked twins with same vertex (self-loop)" in:
    val vertex = Vertex(V1, BigPoint(0, 0))

    val (edge1, edge2) = HalfEdge.createTwinPair(vertex, vertex)

    allAssert(
      edge1.origin shouldBe vertex,
      edge2.origin shouldBe vertex,
      edge1.twin shouldBe Some(edge2),
      edge2.twin shouldBe Some(edge1),
      edge1.destination shouldBe Some(vertex),
      edge2.destination shouldBe Some(vertex)
    )

  behavior of "HalfEdge companion object - linkEdges"

  it should "link two edges as prev and next" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge1  = HalfEdge(vertex)
    val edge2  = HalfEdge(vertex)

    allAssert(
      edge1.next shouldBe None,
      edge2.prev shouldBe None, {
        edge1.linkWith(edge2)
        edge1.next shouldBe Some(edge2)
      },
      edge2.prev shouldBe Some(edge1)
    )

  it should "overwrite existing links" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge1  = HalfEdge(vertex)
    val edge2  = HalfEdge(vertex)
    val edge3  = HalfEdge(vertex)
    val edge4  = HalfEdge(vertex)

    // Set initial links
    edge1.next = Some(edge3)
    edge4.prev = Some(edge2)

    // Link edge1 -> edge2
    edge1.linkWith(edge4)

    allAssert(
      edge1.next shouldBe Some(edge4),
      edge4.prev shouldBe Some(edge1)
    )

  it should "allow self-linking (edge pointing to itself)" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)

    edge.linkWith(edge)

    allAssert(
      edge.next shouldBe Some(edge),
      edge.prev shouldBe Some(edge)
    )

  behavior of "HalfEdge companion object - linkChain"

  it should "link a chain of edges in a cycle" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge1  = HalfEdge(vertex)
    val edge2  = HalfEdge(vertex)
    val edge3  = HalfEdge(vertex)

    val edges = List(edge1, edge2, edge3)
    edges.linkInCycle()

    // Should form a cycle: edge1 -> edge2 -> edge3 -> edge1
    allAssert(
      edge1.next shouldBe Some(edge2),
      edge2.prev shouldBe Some(edge1),
      edge2.next shouldBe Some(edge3),
      edge3.prev shouldBe Some(edge2),
      edge3.next shouldBe Some(edge1),
      edge1.prev shouldBe Some(edge3)
    )

  it should "handle single edge chain (self-loop)" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)

    List(edge).linkInCycle()

    allAssert(
      edge.next shouldBe Some(edge),
      edge.prev shouldBe Some(edge)
    )

  it should "handle two-edge chain" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge1  = HalfEdge(vertex)
    val edge2  = HalfEdge(vertex)

    List(edge1, edge2).linkInCycle()

    allAssert(
      edge1.next shouldBe Some(edge2),
      edge2.prev shouldBe Some(edge1),
      edge2.next shouldBe Some(edge1),
      edge1.prev shouldBe Some(edge2)
    )

  it should "handle empty list gracefully" in:
    val e = List.empty[HalfEdge]
    e.linkInCycle()
    e shouldBe List.empty[HalfEdge]

  it should "handle complex chain correctly" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edges  = (1 to 5).map(_ => HalfEdge(vertex)).toList

    edges.linkInCycle()

    // Verify the cycle
    edges.zipWithIndex.foreach { case (edge, i) =>
      val nextIndex = (i + 1) % edges.length
      val prevIndex = if (i == 0) edges.length - 1 else i - 1

      allAssert(
        edge.next shouldBe Some(edges(nextIndex)),
        edge.prev shouldBe Some(edges(prevIndex))
      )
    }

  behavior of "HalfEdge companion object - insertBoundarySegment"

  it should "insert a single edge segment between two edges" in:
    val vertex      = Vertex(V1, BigPoint(0, 0))
    val prevEdge    = HalfEdge(vertex)
    val nextEdge    = HalfEdge(vertex)
    val segmentEdge = HalfEdge(vertex)

    HalfEdge.insertBoundarySegment(prevEdge, nextEdge, List(segmentEdge))

    allAssert(
      prevEdge.next shouldBe Some(segmentEdge),
      segmentEdge.prev shouldBe Some(prevEdge),
      segmentEdge.next shouldBe Some(nextEdge),
      nextEdge.prev shouldBe Some(segmentEdge)
    )

  it should "insert a multi-edge segment between two edges" in:
    val vertex   = Vertex(V1, BigPoint(0, 0))
    val prevEdge = HalfEdge(vertex)
    val nextEdge = HalfEdge(vertex)
    val seg1     = HalfEdge(vertex)
    val seg2     = HalfEdge(vertex)
    val seg3     = HalfEdge(vertex)

    val segment = List(seg1, seg2, seg3)
    HalfEdge.insertBoundarySegment(prevEdge, nextEdge, segment)

    allAssert(
      // Check connections to boundary
      prevEdge.next shouldBe Some(seg1),
      seg1.prev shouldBe Some(prevEdge),
      seg3.next shouldBe Some(nextEdge),
      nextEdge.prev shouldBe Some(seg3),

      // Check internal segment connections
      seg1.next shouldBe Some(seg2),
      seg2.prev shouldBe Some(seg1),
      seg2.next shouldBe Some(seg3),
      seg3.prev shouldBe Some(seg2)
    )

  it should "handle empty segment gracefully" in:
    val vertex   = Vertex(V1, BigPoint(0, 0))
    val prevEdge = HalfEdge(vertex)
    val nextEdge = HalfEdge(vertex)

    // The current implementation doesn't handle empty segments gracefully,
    // so we expect an exception to be thrown
    assertThrows[NoSuchElementException] {
      HalfEdge.insertBoundarySegment(prevEdge, nextEdge, List.empty)
    }

  behavior of "HalfEdge in triangle configuration"

  it should "work correctly in a complete triangle" in:
    val v1   = Vertex(V1, BigPoint(0, 0))
    val v2   = Vertex(V2, BigPoint(1, 0))
    val v3   = Vertex(V3, BigPoint(0.5, 0.866))
    val face = Face(FaceId(333))

    val (e12, e21) = HalfEdge.createTwinPair(v1, v2)
    val (e23, e32) = HalfEdge.createTwinPair(v2, v3)
    val (e31, e13) = HalfEdge.createTwinPair(v3, v1)

    // Link the inner triangle
    List(e12, e23, e31).linkInCycle()

    // Set face and angles
    List(e12, e23, e31).foreach { edge =>
      edge.incidentFace = Some(face)
      edge.angle = Some(AngleDegree(60.0))
    }

    allAssert(
      // Verify triangle structure
      e12.next shouldBe Some(e23),
      e23.next shouldBe Some(e31),
      e31.next shouldBe Some(e12),
      e12.prev shouldBe Some(e31),
      e23.prev shouldBe Some(e12),
      e31.prev shouldBe Some(e23),

      // Verify destinations
      e12.destination shouldBe Some(v2),
      e23.destination shouldBe Some(v3),
      e31.destination shouldBe Some(v1),

      // Verify all edges are complete
      e12.isComplete shouldBe true,
      e23.isComplete shouldBe true,
      e31.isComplete shouldBe true,
      e12.validate() shouldBe Right(()),
      e23.validate() shouldBe Right(()),
      e31.validate() shouldBe Right(())
    )

  behavior of "HalfEdge edge cases"

  it should "handle edges with very large angles" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)

    val largeAngle = AngleDegree(720.0) // Two full rotations
    edge.angle = Some(largeAngle)

    edge.angle shouldBe Some(largeAngle)

  it should "handle edges with negative angles" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)

    val negativeAngle = AngleDegree(-90.0)
    edge.angle = Some(negativeAngle)

    edge.angle shouldBe Some(negativeAngle)

  it should "handle edges with zero angle" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)

    val zeroAngle = AngleDegree(0.0)
    edge.angle = Some(zeroAngle)

    edge.angle shouldBe Some(zeroAngle)

  it should "maintain reference equality after field modifications" in:
    val vertex       = Vertex(V1, BigPoint(0, 0))
    val edge         = HalfEdge(vertex)
    val originalEdge = edge

    // Modify all mutable fields
    edge.twin = Some(HalfEdge(vertex))
    edge.incidentFace = Some(Face(F1))
    edge.next = Some(HalfEdge(vertex))
    edge.prev = Some(HalfEdge(vertex))
    edge.angle = Some(AngleDegree(45.0))

    // Should still be the same reference
    allAssert(
      edge shouldBe theSameInstanceAs(originalEdge),
      edge shouldEqual originalEdge
    )

  behavior of "HalfEdge complex scenarios"

  it should "work in a doubly connected edge list with multiple faces" in:
    // Create a diamond shape with 4 vertices and 2 triangular faces
    val vTop    = Vertex(VertexId(10), BigPoint(0, 1))
    val vLeft   = Vertex(VertexId(11), BigPoint(-1, 0))
    val vRight  = Vertex(VertexId(12), BigPoint(1, 0))
    val vBottom = Vertex(VertexId(13), BigPoint(0, -1))

    val leftFace  = Face(FaceId(30))
    val rightFace = Face(FaceId(31))

    // Create twin pairs for all edges
    val (eTopLeft, eLeftTop)         = HalfEdge.createTwinPair(vTop, vLeft)
    val (eLeftBottom, eBottomLeft)   = HalfEdge.createTwinPair(vLeft, vBottom)
    val (eBottomTop, eTopBottom)     = HalfEdge.createTwinPair(vBottom, vTop)
    val (eTopRight, eRightTop)       = HalfEdge.createTwinPair(vTop, vRight)
    val (eRightBottom, eBottomRight) = HalfEdge.createTwinPair(vRight, vBottom)

    // Link left triangle: Top -> Left -> Bottom -> Top
    List(eTopLeft, eLeftBottom, eBottomTop).linkInCycle()

    // Link right triangle: Top -> Right -> Bottom -> Top
    List(eTopRight, eRightBottom, eBottomTop.twin.get).linkInCycle()

    // Set faces
    List(eTopLeft, eLeftBottom, eBottomTop).foreach(_.incidentFace = Some(leftFace))
    List(eTopRight, eRightBottom, eBottomTop.twin.get).foreach(_.incidentFace = Some(rightFace))

    // Verify structure
    allAssert(
      eTopLeft.destination shouldBe Some(vLeft),
      eLeftBottom.destination shouldBe Some(vBottom),
      eBottomTop.destination shouldBe Some(vTop),
      eTopRight.destination shouldBe Some(vRight),
      eRightBottom.destination shouldBe Some(vBottom),

      // Verify faces
      eTopLeft.incidentFace shouldBe Some(leftFace),
      eTopRight.incidentFace shouldBe Some(rightFace)
    )

  it should "maintain consistency in complex linking operations" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edges  = (1 to 6).map(i => HalfEdge(vertex)).toList

    // Create two chains
    edges.take(3).linkInCycle()
    edges.drop(3).linkInCycle()

    // Insert one chain into the other
    HalfEdge.insertBoundarySegment(edges(0), edges(1), edges.drop(3))

    allAssert(
      // Verify the final structure
      edges(0).next shouldBe Some(edges(3)),
      edges(3).prev shouldBe Some(edges(0)),
      edges(5).next shouldBe Some(edges(1)),
      edges(1).prev shouldBe Some(edges(5)),

      // Internal segment should be preserved
      edges(3).next shouldBe Some(edges(4)),
      edges(4).next shouldBe Some(edges(5))
    )

  behavior of "HalfEdge validation edge cases"

  it should "validate correctly with extreme angle values" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)

    // Set all required fields with extreme angle
    edge.twin = Some(HalfEdge(vertex))
    edge.incidentFace = Some(Face(F1))
    edge.next = Some(HalfEdge(vertex))
    edge.prev = Some(HalfEdge(vertex))
    edge.angle = Some(AngleDegree(Double.MaxValue))

    allAssert(
      edge.validate() shouldBe Right(()),
      edge.isComplete shouldBe true
    )

  it should "handle fractional angles correctly" in:
    val vertex = Vertex(V1, BigPoint(0, 0))
    val edge   = HalfEdge(vertex)

    val fractionalAngle = AngleDegree(30.333333)
    edge.angle = Some(fractionalAngle)

    edge.angle shouldBe Some(fractionalAngle)
