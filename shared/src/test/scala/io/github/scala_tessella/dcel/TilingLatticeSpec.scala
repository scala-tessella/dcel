package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingLattice.{
  largestContainedParallelogon,
  largestContainedParallelogonBlock,
  translationLattice
}
import io.github.scala_tessella.dcel.geometry.{BigPoint, RegularPolygon}
import org.scalatest.{Inspectors, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Largest contained parallelogon (ADR-0015): lattice detection, the maximal cell block, and the public
  * corner-vertex query with its whole-boundary fast path.
  */
class TilingLatticeSpec extends AnyFlatSpec with Matchers with OptionValues with Inspectors
    with TilingTestHelpers:

  behavior of "TilingLattice.translationLattice"

  /** Covolume (fundamental-domain area) of a detected basis. */
  private def covolume(basis: (BigPoint, BigPoint)): Double =
    basis._1.cross(basis._2).abs.toDouble

  /** Best-effort welding of up to `count` foreign `polygon`s onto the current boundary, skipping any that
    * would not fit. Mirrors the "clean tiling + a few foreign faces" motif from ADR-0015.
    */
  private def weld(base: TilingDCEL, polygon: RegularPolygon, count: Int): TilingDCEL =
    base.boundaryVerticesUnsafe.map(_.id).foldLeft((base, 0)) { case ((tiling, added), vid) =>
      if added >= count then (tiling, added)
      else
        tiling.maybeAddRegularPolygonToBoundary(vid, polygon) match
          case Right(grown) => (grown, added + 1)
          case Left(_)      => (tiling, added)
    }._1

  private val oneHexagon = 3.0 * math.sqrt(3.0) / 2.0 // primitive cell of 6.6.6 (unit edges)

  it should "recover the lattice of a 6.6.6 hexagon net" in:
    val lattice = TilingBuilder.createHexagonNet(6, 6).value.translationLattice()
    allAssert(
      lattice shouldBe defined,
      covolume(lattice.value) shouldBe oneHexagon +- 1.0e-3
    )

  it should "recover the same lattice on a larger 8x8 hexagon net" in:
    val lattice = TilingBuilder.createHexagonNet(8, 8).value.translationLattice()
    allAssert(
      lattice shouldBe defined,
      covolume(lattice.value) shouldBe oneHexagon +- 1.0e-3
    )

  it should "recover the lattice despite stray triangles welded onto the boundary" in:
    // Mirrors the motivating ADR-0015 case ("99% a clean tiling + a few foreign boundary faces"):
    // the foreign triangles add only boundary vertices, so the interior lattice must survive.
    val clean     = TilingBuilder.createHexagonNet(8, 8).value
    val perturbed =
      clean.boundaryVerticesUnsafe.map(_.id).take(12).foldLeft(clean: TilingDCEL) { (tiling, vid) =>
        tiling.maybeAddRegularPolygonToBoundary(vid, RegularPolygon(3)).getOrElse(tiling)
      }
    val lattice   = perturbed.translationLattice()
    allAssert(
      perturbed.innerFaces.size should be > clean.innerFaces.size, // sanity: we actually added triangles
      lattice shouldBe defined,
      covolume(lattice.value) shouldBe oneHexagon +- 1.0e-3
    )

  private val twoTriangles = math.sqrt(3.0) / 2.0 // primitive cell of 3.3.3.3.3.3 (unit edges)

  it should "recover the lattice of a 3.3.3.3.3.3 triangle net" in:
    val net = TilingBuilder.createTriangleNet(8, 8).value
    covolume(net.translationLattice().value) shouldBe twoTriangles +- 1.0e-3

  it should "recover the triangle-net lattice with foreign squares and hexagons welded on" in:
    val clean     = TilingBuilder.createTriangleNet(10, 10).value
    val perturbed = weld(weld(clean, RegularPolygon(4), 6), RegularPolygon(6), 4)
    allAssert(
      perturbed.innerFaces.size should be > clean.innerFaces.size,
      covolume(perturbed.translationLattice().value) shouldBe twoTriangles +- 1.0e-3
    )

  it should "recover the lattice of a 4.4.4.4 square net" in:
    val net = TilingBuilder.createRhombusNet(8, 8).value // default 90° angle ⇒ unit squares
    covolume(net.translationLattice().value) shouldBe 1.0 +- 1.0e-3

  it should "recover the square-net lattice with foreign triangles and hexagons welded on" in:
    val clean     = TilingBuilder.createRhombusNet(10, 10).value
    val perturbed = weld(weld(clean, RegularPolygon(3), 6), RegularPolygon(6), 4)
    allAssert(
      perturbed.innerFaces.size should be > clean.innerFaces.size,
      covolume(perturbed.translationLattice().value) shouldBe 1.0 +- 1.0e-3
    )

  it should "return a Lagrange-Gauss reduced basis (|v| <= |w| <= |w +- v|)" in:
    val (v, w)   = TilingBuilder.createTriangleNet(8, 8).value.translationLattice().value
    val lenV     = v.dot(v).toDouble
    val lenW     = w.dot(w).toDouble
    val lenWPlus = (w + v).dot(w + v).toDouble
    val lenWMin  = (w - v).dot(w - v).toDouble
    allAssert(
      lenV should be <= lenW + 1.0e-9,     // v is the shorter
      lenW should be <= lenWPlus + 1.0e-9, // w cannot be shortened by adding v
      lenW should be <= lenWMin + 1.0e-9   // ...or by subtracting v
    )

  it should "find no lattice in a single hexagon (no interior vertices)" in:
    TilingBuilder.createRegularPolygon(RegularPolygon(6)).translationLattice() shouldBe None

  behavior of "TilingLattice.largestContainedParallelogonBlock (package-private)"

  private def isVertexOf(p: BigPoint, tiling: TilingDCEL): Boolean =
    tiling.vertices.exists(_.coords.almostEquals(p))

  it should "cover a whole clean square net (every unit square is a complete cell)" in:
    val net   = TilingBuilder.createRhombusNet(5, 4).value // 90° ⇒ unit squares
    val block = net.largestContainedParallelogonBlock().value
    allAssert(
      // the entire net is one rectangle of complete cells
      block.area.toDouble shouldBe net.innerFaces.size.toDouble +- 1.0e-6,
      block.cellsWide * block.cellsHigh shouldBe net.innerFaces.size,
      forEvery(block.corners)(c => isVertexOf(c, net) shouldBe true)
    )

  it should "ignore foreign triangles welded onto a square net" in:
    val clean      = TilingBuilder.createRhombusNet(5, 4).value
    val cleanBlock = clean.largestContainedParallelogonBlock().value
    val perturbed  = weld(clean, RegularPolygon(3), 5)
    val grownBlock = perturbed.largestContainedParallelogonBlock().value
    allAssert(
      perturbed.innerFaces.size should be > clean.innerFaces.size,
      // the welded triangles are incomplete cells ⇒ the maximal block is unchanged
      grownBlock.area.toDouble shouldBe cleanBlock.area.toDouble +- 1.0e-6
    )

  it should "return a block of whole cells inside a hexagon net" in:
    // The block is a whole number of fundamental cells, bounded by the patch. Its ideal lattice-point corners
    // are not asserted to be tiling vertices here: for the honeycomb the block's outer corners can fall just
    // past the complete-cell region, and snapping the parallelogram to the actual whole-face boundary is the
    // job of the public corner query (run through parallelogonIndices).
    val net   = TilingBuilder.createHexagonNet(6, 6).value
    val block = net.largestContainedParallelogonBlock().value
    val total = net.innerFaces.size * oneHexagon
    allAssert(
      block.area.toDouble should be > 0.0,
      block.area.toDouble should be <= total + 1.0e-6,
      (block.area / BigDecimal(
        oneHexagon
      )).toDouble shouldBe (block.cellsWide * block.cellsHigh).toDouble +- 1.0e-3
    )

  behavior of "TilingLattice.largestContainedParallelogon (corners)"

  it should "use the whole-boundary fast path for a single square (no lattice needed)" in:
    val square  = TilingBuilder.createRegularPolygon(RegularPolygon(4))
    val corners = square.largestContainedParallelogon().value
    allAssert(
      square.translationLattice() shouldBe None, // no interior vertices ⇒ no lattice
      corners.size shouldBe 4,                   // the whole square IS a parallelogon
      corners.map(_.id).distinct.size shouldBe 4
    )

  it should "use the whole-boundary fast path for a single regular hexagon" in:
    TilingBuilder.createRegularPolygon(RegularPolygon(6)).largestContainedParallelogon().value.size shouldBe 6

  /** Shoelace area of an ordered corner polygon. */
  private def polygonArea(corners: List[BigPoint]): Double =
    val pts = corners.map(p => (p.x.toDouble, p.y.toDouble))
    val n   = pts.size
    math.abs((0 until n).map { i =>
      val (x1, y1) = pts(i)
      val (x2, y2) = pts((i + 1) % n)
      x1 * y2 - x2 * y1
    }.sum) / 2.0

  it should "give the 4 corners of a clean square net, enclosing the whole block" in:
    val net     = TilingBuilder.createRhombusNet(5, 4).value
    val corners = net.largestContainedParallelogon().value
    allAssert(
      corners.size shouldBe 4,
      corners.map(_.id).distinct.size shouldBe 4,
      polygonArea(
        corners.map(_.coords)
      ) shouldBe net.largestContainedParallelogonBlock().value.area.toDouble +- 1.0e-6
    )

  it should "give 6 corners for a hexagon net (hexagonal parallelogon)" in:
    val corners = TilingBuilder.createHexagonNet(6, 6).value.largestContainedParallelogon().value
    allAssert(
      corners.size shouldBe 6,
      corners.map(_.id).distinct.size shouldBe 6
    )

  it should "give the same corners when foreign triangles are welded onto a square net" in:
    val clean        = TilingBuilder.createRhombusNet(5, 4).value
    val cleanCorners = clean.largestContainedParallelogon().value.map(_.coords)
    val perturbed    = weld(clean, RegularPolygon(3), 5)
    val grownCorners = perturbed.largestContainedParallelogon().value.map(_.coords)
    allAssert(
      perturbed.innerFaces.size should be > clean.innerFaces.size,
      grownCorners.size shouldBe 4,
      // same four corner positions (the welded triangles are incomplete cells, excluded from the block)
      forEvery(cleanCorners)(c => grownCorners.exists(_.almostEquals(c)) shouldBe true)
    )
