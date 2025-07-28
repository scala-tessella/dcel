package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, BigPoint}

object TilingExample:

  def main(args: Array[String]): Unit =
    createTwoTrianglesTiling()
    println("This is an example object. Run tests to see the TilingBuilder in action.")
    createSixTrianglesTiling()

  // The test code previously here has been moved to TilingBuilderSpec.scala

  private def linkPolygon(halfEdges: List[HalfEdge], face: Face, angleDegree: AngleDegree): Unit =
    halfEdges.linkInCycle()
    halfEdges.foreach { h =>
      h.incidentFace = Some(face)
      h.angle = Some(angleDegree)
    }

  /**
   * Creates a sample TilingDCEL with two triangles sharing an edge.
   * This is a good example of manual DCEL construction.
   * It is the smallest construction by regular polygons with a shared edge
   */
  def createTwoTrianglesTiling(): TilingDCEL =
    // 1. Define vertices
    val vA = Vertex("V1", BigPoint(0.0, 0.0))
    val vB = Vertex("V2", BigPoint(1.0, 0.0))
    val vC = Vertex("V3", BigPoint(0.5, 0.866))
    val vD = Vertex("V4", BigPoint(-0.5, 0.866))

    // 2. Define faces
    val fABC = Face("F1")
    val fACD = Face("F2")
    val fOuter = Face(Face.outerId)

    // 3. Create half-edges
    val hAB = HalfEdge(vA); val hBA = HalfEdge(vB)
    val hBC = HalfEdge(vB); val hCB = HalfEdge(vC)
    val hCA = HalfEdge(vC); val hAC = HalfEdge(vA)
    val hAD = HalfEdge(vA); val hDA = HalfEdge(vD)
    val hDC = HalfEdge(vD); val hCD = HalfEdge(vC)

    val halfEdges = List(hAB, hBA, hBC, hCB, hCA, hAC, hAD, hDA, hDC, hCD)

    // 4. Link everything together using Option

    // Set twins, wrapping in Some(...)
    halfEdges.sliding(2, 2).foreach { case h1 :: h2 :: Nil => h1.twinWith(h2) }

    // Set leaving edges for vertices
    vA.leaving = Some(hAB); vB.leaving = Some(hBC)
    vC.leaving = Some(hCA); vD.leaving = Some(hDA)

    // Set outer components for faces
    fABC.outerComponent = Some(hAB)
    fACD.outerComponent = Some(hAC)
    fOuter.outerComponent = Some(hBA)

    // --- Triangle ABC ---
    linkPolygon(List(hAB, hBC, hCA), fABC,AngleDegree(60))

    // --- Triangle ACD ---
    linkPolygon(List(hAC, hCD, hDA), fACD, AngleDegree(60))

    // --- Outer Face ---
    linkPolygon(List(hBA, hAD, hDC, hCB), fOuter, AngleDegree(300))
    hCB.angle = Some(AngleDegree(240))
    hAD.angle = Some(AngleDegree(240))

    // 5. Populate and return the Tiling container
    val result = TilingDCEL(
      vertices = List(vA, vB, vC, vD),
      halfEdges = halfEdges,
      innerFaces = List(fABC, fACD),
      outerFace = fOuter
    )

    println(TilingDCEL.validate(result))
    println(result.toSVG())
    println(TilingDCEL.spatiallyValidate(result))

    result

  /**
   * Creates a sample TilingDCEL with six triangles sharing a vertex.
   * This is a good example of manual DCEL construction.
   * It is the smallest construction by regular polygons with a shared vertex representing a full circle
   */
  def createSixTrianglesTiling(): TilingDCEL =
    // 1. Define vertices
    val vA = Vertex("V1", BigPoint(0.0, 0.0))
    val vB = Vertex("V2", BigPoint(1.0, 0.0))
    val vC = Vertex("V3", BigPoint(0.5, 0.866))
    val vD = Vertex("V4", BigPoint(-0.5, 0.866))
    val vE = Vertex("V5", BigPoint(-1.0, 0.0))
    val vF = Vertex("V6", BigPoint(-0.5, -0.866))
    val vG = Vertex("V7", BigPoint(0.5, -0.866))

    // 2. Define faces
    val fABC = Face("F1")
    val fACD = Face("F2")
    val fADE = Face("F3")
    val fAEF = Face("F4")
    val fAFG = Face("F5")
    val fAGB = Face("F6")
    val fOuter = Face(Face.outerId)

    // 3. Create half-edges
    val hAB = HalfEdge(vA); val hBA = HalfEdge(vB)
    val hBC = HalfEdge(vB); val hCB = HalfEdge(vC)
    val hCA = HalfEdge(vC); val hAC = HalfEdge(vA)
    val hAD = HalfEdge(vA); val hDA = HalfEdge(vD)
    val hDC = HalfEdge(vD); val hCD = HalfEdge(vC)
    val hAE = HalfEdge(vA); val hEA = HalfEdge(vE)
    val hED = HalfEdge(vE); val hDE = HalfEdge(vD)
    val hAF = HalfEdge(vA); val hFA = HalfEdge(vF)
    val hFE = HalfEdge(vF); val hEF = HalfEdge(vE)
    val hAG = HalfEdge(vA); val hGA = HalfEdge(vG)
    val hGF = HalfEdge(vG); val hFG = HalfEdge(vF)
    val hGB = HalfEdge(vG); val hBG = HalfEdge(vB)

    val halfEdges = List(
      hAB, hBA, hBC, hCB, hCA, hAC, hAD, hDA, hDC, hCD, hAE, hEA,
      hED, hDE, hAF, hFA, hFE, hEF, hAG, hGA, hGF, hFG, hGB, hBG
    )

    // 4. Link everything together using Option

    // Set twins, wrapping in Some(...)
    halfEdges.sliding(2, 2).foreach { case h1 :: h2 :: Nil => h1.twinWith(h2) }

    // Set leaving edges for vertices
    vA.leaving = Some(hAB); vB.leaving = Some(hBC)
    vC.leaving = Some(hCA); vD.leaving = Some(hDA)
    vE.leaving = Some(hEA); vF.leaving = Some(hFA)
    vG.leaving = Some(hGA)

    // Set outer components for faces
    fABC.outerComponent = Some(hAB)
    fACD.outerComponent = Some(hAC)
    fADE.outerComponent = Some(hAD)
    fAEF.outerComponent = Some(hAE)
    fAFG.outerComponent = Some(hAF)
    fAGB.outerComponent = Some(hAG)
    fOuter.outerComponent = Some(hCB)

    // --- Triangle ABC ---
    linkPolygon(List(hAB, hBC, hCA), fABC, AngleDegree(60))

    // --- Triangle ACD ---
    linkPolygon(List(hAC, hCD, hDA), fACD, AngleDegree(60))

    // --- Triangle ADE ---
    linkPolygon(List(hAD, hDE, hEA), fADE, AngleDegree(60))

    // --- Triangle AEF ---
    linkPolygon(List(hAE, hEF, hFA), fAEF, AngleDegree(60))

    // --- Triangle AFG ---
    linkPolygon(List(hAF, hFG, hGA), fAFG, AngleDegree(60))

    // --- Triangle AGB ---
    linkPolygon(List(hAG, hGB, hBA), fAGB, AngleDegree(60))

    // --- Outer Face ---
    linkPolygon(List(hCB, hBG, hGF, hFE, hED, hDC), fOuter, AngleDegree(240))

    // 5. Populate and return the Tiling container
    val result = TilingDCEL(
      vertices = List(vA, vB, vC, vD, vE, vF, vG),
      halfEdges = halfEdges,
      innerFaces = List(fABC, fACD, fADE, fAEF, fAFG, fAGB),
      outerFace = fOuter
    )

    println(TilingDCEL.validate(result))
//    println(result.toSVG())
    println(TilingDCEL.spatiallyValidate(result))

    result
