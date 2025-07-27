package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, BigPoint}

object TilingExample:

  def main(args: Array[String]): Unit =
    createTwoTrianglesTiling()
    println("This is an example object. Run tests to see the TilingBuilder in action.")
    createSixTrianglesTiling()

  // The test code previously here has been moved to TilingBuilderSpec.scala

  /**
   * Creates a sample TilingDCEL with two triangles sharing an edge.
   * This is a good example of manual DCEL construction.
   * It is the smallest construction by regular polygons with a shared edge
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
    val first = List(hAB, hBC, hCA)
    first.linkInCycle()
    first.foreach { h =>
      h.incidentFace = Some(fABC)
      h.angle = Some(AngleDegree(60))
    }

    // --- Triangle ACD ---
    val second = List(hAC, hCD, hDA)
    second.linkInCycle()
    second.foreach { h =>
      h.incidentFace = Some(fACD)
      h.angle = Some(AngleDegree(60))
    }

    // --- Outer Face ---
    val outer = List(hBA, hAD, hDC, hCB)
    outer.linkInCycle()
    outer.foreach { h =>
      h.incidentFace = Some(fOuter)
      h.angle = Some(AngleDegree(300))
    }
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
//    println(result.toSVG())
    println(TilingDCEL.spatiallyValidate(result))

    result

  /**
   * Creates a sample TilingDCEL with six triangles sharing a vertex.
   * This is a good example of manual DCEL construction.
   * It is the smallest construction by regular polygons with a shared vertex representing a full circle
   */
  def createSixTrianglesTiling(): TilingDCEL =
    // 1. Define vertices
    val vA = Vertex("A", BigPoint(0.0, 0.0))
    val vB = Vertex("B", BigPoint(1.0, 0.0))
    val vC = Vertex("C", BigPoint(0.5, 0.866))
    val vD = Vertex("D", BigPoint(-0.5, 0.866))
    val vE = Vertex("E", BigPoint(-1.0, 0.0))
    val vF = Vertex("F", BigPoint(-0.5, -0.866))
    val vG = Vertex("G", BigPoint(0.5, -0.866))

    // 2. Define faces
    val fABC = Face("F_ABC")
    val fACD = Face("F_ACD")
    val fADE = Face("F_ADE")
    val fAEF = Face("F_AEF")
    val fAFG = Face("F_AFG")
    val fAGB = Face("F_AGB")
    val fOuter = Face("F_Outer")

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
    val hBG = HalfEdge(vB); val hGB = HalfEdge(vG)

    val halfEdges = List(
      hAB, hBA, hBC, hCB, hCA, hAC, hAD, hDA, hDC, hCD, hAE, hEA,
      hED, hDE, hAF, hFA, hFE, hEF, hAG, hGA, hGF, hFG, hBG, hGB
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
    fOuter.outerComponent = Some(hBA)

    // --- Triangle ABC ---
    val first = List(hAB, hBC, hCA)
    first.linkInCycle()
    first.foreach { h =>
      h.incidentFace = Some(fABC)
      h.angle = Some(AngleDegree(60))
    }

    // --- Triangle ACD ---
    val second = List(hAC, hCD, hDA)
    second.linkInCycle()
    second.foreach { h =>
      h.incidentFace = Some(fACD)
      h.angle = Some(AngleDegree(60))
    }

    // --- Triangle ADE ---
    val third = List(hAD, hDE, hEA)
    third.linkInCycle()
    third.foreach { h =>
      h.incidentFace = Some(fADE)
      h.angle = Some(AngleDegree(60))
    }

    // --- Triangle AEF ---
    val fourth = List(hAE, hEF, hFA)
    fourth.linkInCycle()
    fourth.foreach { h =>
      h.incidentFace = Some(fAEF)
      h.angle = Some(AngleDegree(60))
    }

    // --- Triangle AFG ---
    val fifth = List(hAF, hFG, hGA)
    fifth.linkInCycle()
    fifth.foreach { h =>
      h.incidentFace = Some(fAFG)
      h.angle = Some(AngleDegree(60))
    }

    // --- Triangle AGB ---
    val sixth = List(hAG, hGB, hBA)
    sixth.linkInCycle()
    sixth.foreach { h =>
      h.incidentFace = Some(fAGB)
      h.angle = Some(AngleDegree(60))
    }

    // --- Outer Face ---
    val outer = List(hDC, hCB, hBG, hGF, hFE, hED)
    outer.linkInCycle()
    outer.foreach { h =>
      h.incidentFace = Some(fOuter)
      h.angle = Some(AngleDegree(300))
    }

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
