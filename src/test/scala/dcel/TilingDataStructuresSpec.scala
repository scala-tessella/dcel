package io.github.scala_tessella
package dcel

import BigDecimalGeometry.BigPoint

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingDataStructuresSpec extends AnyFlatSpec with Matchers with EitherValues:

  // Mock data for testing
  val v1: Vertex = Vertex("V1", BigPoint(0, 0))
  val v2: Vertex = Vertex("V2", BigPoint(1, 0))
  val v3: Vertex = Vertex("V3", BigPoint(1, 1))

  behavior of "Vertex"

  it should "be equal to another vertex with the same ID" in {
    val v1_clone = Vertex("V1", BigPoint(9, 9)) // Different coords should not matter
    v1 shouldEqual v1_clone
  }

  it should "not be equal to another vertex with a different ID" in {
    v1 shouldNot equal(v2)
  }

  it should "have the same hashCode as another vertex with the same ID" in {
    val v1_clone = Vertex("V1", BigPoint(9, 9))
    v1.hashCode() shouldEqual v1_clone.hashCode()
  }

  behavior of "HalfEdge"

  it should "only be equal to itself (reference equality)" in {
    val he1 = HalfEdge(v1)
    val he2 = HalfEdge(v1) // Same origin, but different instance
    he1 shouldNot equal(he2)
    he1 shouldEqual he1
  }

  it should "have a hashCode based on its identity" in {
    val he1 = HalfEdge(v1)
    val he2 = HalfEdge(v1)
    he1.hashCode() shouldNot equal(he2.hashCode())
    he1.hashCode() shouldEqual System.identityHashCode(he1)
  }

  behavior of "Face"

  it should "be equal to another face with the same ID" in {
    val f1 = Face("F1")
    val f1_clone = Face("F1", outerComponent = Some(HalfEdge(v1))) // Different component
    f1 shouldEqual f1_clone
  }

  it should "not be equal to another face with a different ID" in {
    val f1 = Face("F1")
    val f2 = Face("F2")
    f1 shouldNot equal(f2)
  }

  it should "have the same hashCode as another face with the same ID" in {
    val f1 = Face("F1")
    val f1_clone = Face("F1")
    f1.hashCode() shouldEqual f1_clone.hashCode()
  }

  behavior of "Face.halfEdges"

  it should "return an empty list if the face has no outer component" in {
    val face = Face("F_empty")
    face.halfEdgesSafe shouldBe empty
  }

  it should "return all half-edges in a simple triangular face loop" in {
    // Create a simple triangular face
    val face = Face("F_tri")
    val he1 = HalfEdge(v1, incidentFace = Some(face))
    val he2 = HalfEdge(v2, incidentFace = Some(face))
    val he3 = HalfEdge(v3, incidentFace = Some(face))

    // Link them together: he1 -> he2 -> he3 -> he1
    he1.next = Some(he2)
    he2.next = Some(he3)
    he3.next = Some(he1)

    face.outerComponent = Some(he1)

    val edges = face.halfEdgesSafe
    edges should contain theSameElementsInOrderAs List(he1, he2, he3)
  }

  it should "return half-edges in order for a square face" in {
    val v4 = Vertex("V4", BigPoint(0, 1))
    val face = Face("F_square")
    val he1 = HalfEdge(v1, incidentFace = Some(face))
    val he2 = HalfEdge(v2, incidentFace = Some(face))
    val he3 = HalfEdge(v3, incidentFace = Some(face))
    val he4 = HalfEdge(v4, incidentFace = Some(face))

    he1.next = Some(he2)
    he2.next = Some(he3)
    he3.next = Some(he4)
    he4.next = Some(he1)

    face.outerComponent = Some(he1)

    face.halfEdgesSafe should contain theSameElementsInOrderAs List(he1, he2, he3, he4)
  }

  it should "handle a loop where an edge points to itself" in {
    val face = Face("F_loop")
    val he1 = HalfEdge(v1, incidentFace = Some(face))
    he1.next = Some(he1) // A loop with a single edge
    face.outerComponent = Some(he1)

    val edges = face.halfEdgesSafe
    edges should contain theSameElementsInOrderAs List(he1)
  }

  it should "not get stuck in an infinite loop on a malformed face" in {
    val face = Face("F_malformed")
    val he1 = HalfEdge(v1, incidentFace = Some(face))
    val he2 = HalfEdge(v2, incidentFace = Some(face))

    // Create a non-terminating loop: he1 -> he2 -> he2 -> ...
    he1.next = Some(he2)
    he2.next = Some(he2)

    face.outerComponent = Some(he1)

    // The implementation should detect the cycle and stop.
    face.halfEdges
    val result = face.halfEdges
    result.isLeft shouldBe true
    result.left.value should startWith("Cycle detected in face")
  }
