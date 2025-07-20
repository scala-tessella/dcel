package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, BigPoint}

object TilingExample:

  def main(args: Array[String]): Unit =
    println("This is an example object. Run tests to see the TilingBuilder in action.")
    // The test code previously here has been moved to TilingBuilderSpec.scala

  /**
   * Creates a sample TilingDCEL with two triangles sharing an edge.
   * This is a good example of manual DCEL construction.
   */
  def createTwoTrianglesTiling(): TilingDCEL =
    // 1. Define vertices
    val vA = Vertex("A", BigPoint(0.0, 0.0))
    val vB = Vertex("B", BigPoint(1.0, 0.0))
    val vC = Vertex("C", BigPoint(0.5, 0.866))
    val vD = Vertex("D", BigPoint(-0.5, 0.866))

    // 2. Define faces
    val fABC = Face("F_ABC")
    val fACD = Face("F_ACD")
    val fOuter = Face("F_Outer")

    // 3. Create half-edges
    val hAB = HalfEdge(vA); val hBA = HalfEdge(vB)
    val hBC = HalfEdge(vB); val hCB = HalfEdge(vC)
    val hCA = HalfEdge(vC); val hAC = HalfEdge(vA)
    val hAD = HalfEdge(vA); val hDA = HalfEdge(vD)
    val hDC = HalfEdge(vD); val hCD = HalfEdge(vC)

    // 4. Link everything together using Option

    // Set twins, wrapping in Some(...)
    hAB.twin = Some(hBA); hBA.twin = Some(hAB)
    hBC.twin = Some(hCB); hCB.twin = Some(hBC)
    hCA.twin = Some(hAC); hAC.twin = Some(hCA)
    hAD.twin = Some(hDA); hDA.twin = Some(hAD)
    hCD.twin = Some(hDC); hDC.twin = Some(hCD)

    // Set leaving edges for vertices
    vA.leaving = Some(hAB); vB.leaving = Some(hBC)
    vC.leaving = Some(hCA); vD.leaving = Some(hDA)

    // Set outer components for faces
    fABC.outerComponent = Some(hAB)
    fACD.outerComponent = Some(hAC)
    fOuter.outerComponent = Some(hBA)

    // --- Triangle ABC ---
    hAB.next = Some(hBC); hBC.prev = Some(hAB)
    hBC.next = Some(hCA); hCA.prev = Some(hBC)
    hCA.next = Some(hAB); hAB.prev = Some(hCA)
    hAB.incidentFace = Some(fABC); hAB.angle = AngleDegree(60)
    hBC.incidentFace = Some(fABC); hBC.angle = AngleDegree(60)
    hCA.incidentFace = Some(fABC); hCA.angle = AngleDegree(60)

    // --- Triangle ACD ---
    hAC.next = Some(hCD); hCD.prev = Some(hAC)
    hCD.next = Some(hDA); hDA.prev = Some(hCD)
    hDA.next = Some(hAC); hAC.prev = Some(hDA)
    hAC.incidentFace = Some(fACD); hAC.angle = AngleDegree(60)
    hCD.incidentFace = Some(fACD); hCD.angle = AngleDegree(60)
    hDA.incidentFace = Some(fACD); hDA.angle = AngleDegree(60)

    // --- Outer Face ---
    hBA.next = Some(hAD); hAD.prev = Some(hBA)
    hAD.next = Some(hDC); hDC.prev = Some(hAD)
    hDC.next = Some(hCB); hCB.prev = Some(hDC)
    hCB.next = Some(hBA); hBA.prev = Some(hCB)
    hBA.incidentFace = Some(fOuter); hBA.angle = AngleDegree(300)
    hCB.incidentFace = Some(fOuter); hCB.angle = AngleDegree(240)
    hDC.incidentFace = Some(fOuter); hDC.angle = AngleDegree(300)
    hAD.incidentFace = Some(fOuter); hAD.angle = AngleDegree(240)

    // 5. Populate and return the Tiling container
    TilingDCEL(
      vertices = List(vA, vB, vC, vD),
      halfEdges = List(hAB, hBA, hBC, hCB, hCA, hAC, hAD, hDA, hDC, hCD),
      innerFaces = List(fABC, fACD),
      outerFace = fOuter
    )