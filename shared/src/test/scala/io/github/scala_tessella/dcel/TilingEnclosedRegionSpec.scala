package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint, RegularPolygon}
import io.github.scala_tessella.dcel.structure.VertexId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** ADR-0012: a merge that welds two boundaries together can enclose a region that was a face in neither
  * operand. Such a region must be materialised as a new inner face (the model has no holes — "holes are just
  * other inner polygons"), and the result must validate.
  *
  * The headline case is the reported one: a central pentagon with two pentagons on subsequent sides leaves a
  * 36° wedge open at their shared vertex; reflecting the cluster across the axis that closes it traps a
  * unit-edge 36°–144° rhombus. Before this fix the reflection returned `Left`.
  */
class TilingEnclosedRegionSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  private def pentagon: Tiling =
    TilingBuilder.createRegularPolygon(RegularPolygon(5))

  /** Central pentagon + two pentagons on subsequent sides (sharing vertex V2): 3 faces, a 36° gap at V2. */
  private def threePentagonCluster: Tiling =
    (for
      a <- pentagon.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(5))
      b <- a.maybeAddRegularPolygonToBoundary(V2, RegularPolygon(5))
    yield b).value

  /** Faces whose corners are not all 108° — i.e. not pentagons, hence the enclosed gaps. */
  private def enclosedFaces(tiling: Tiling): List[structure.Face] =
    tiling.innerFaces.filterNot: face =>
      face.anglesUnsafe.forall(_ == AngleDegree(108))

  /** A comb / "E" of unit squares: a vertical spine of `rows` squares with arms on the right at the even rows
    * (0, 2, 4, …). The odd rows (1, 3, …) are one-square-deep notches, open on the right. A square attached
    * at boundary start-vertex `(1, k)` lands at column 1, row `k - 1`, so the arm rows use start vertices
    * `(1, 1), (1, 3), …`. Built from squares only, so closing it with a translated copy two units to the
    * right caps every notch into an enclosed unit square — `(rows - 1) / 2` of them, scaling with the comb
    * height.
    */
  private def squareComb(rows: Int): Tiling =
    val spine = TilingBuilder.createRhombusNet(1, rows).value
    (0 until rows by 2).foldLeft[Either[TilingError, Tiling]](Right(spine)): (acc, row) =>
      acc.flatMap: tiling =>
        val startVertex =
          tiling.coordinates.collectFirst:
            case (id, p) if p.almostEquals(BigPoint(BigDecimal(1), BigDecimal(row + 1))) => id
        startVertex
          .toRight(ValidationError(s"no boundary vertex at (1, ${row + 1})"))
          .flatMap(tiling.maybeAddRegularPolygonToBoundary(_, RegularPolygon(4)))
    .value

  behavior of "TilingMerge enclosing a region (ADR-0012)"

  it should "materialise an enclosed rhombus when a pentagon cluster is reflected onto itself" in:
    val cluster = threePentagonCluster
    // Axis V6-V7 (two tips of the added pentagons) folds the cluster so the 36° gap closes into a rhombus.
    val result  = cluster.maybeAddMirroredCopy(cluster.coordinates(V6), cluster.coordinates(VertexId(7))).value
    val rhombi  = enclosedFaces(result)
    allAssert(
      cluster.innerFaces.size shouldBe 3,
      validate(result).isRight shouldBe true,
      result.innerFaces.size shouldBe 7, // 6 pentagons + 1 enclosed rhombus
      result.vertices.size shouldBe 18,
      result.halfEdges.size shouldBe 48,
      rhombi.size shouldBe 1,
      rhombi.head.getVerticesUnsafe.size shouldBe 4
    )

  it should "give the enclosed rhombus two 36° and two 144° corners" in:
    val result = threePentagonCluster
      .maybeAddMirroredCopy(
        threePentagonCluster.coordinates(V6),
        threePentagonCluster.coordinates(VertexId(7))
      )
      .value
    val angles = enclosedFaces(result).head.anglesUnsafe
    allAssert(
      angles.count(_ == AngleDegree(36)) shouldBe 2,
      angles.count(_ == AngleDegree(144)) shouldBe 2
    )

  it should "turn the former gap vertices into interior vertices (full-circle angle sums)" in:
    // The rhombus corners must no longer sit on the outer boundary; validateGeometrically only passes if their
    // incident angles sum to a full circle, so this is the direct evidence the region is genuinely enclosed.
    val result        = threePentagonCluster
      .maybeAddMirroredCopy(
        threePentagonCluster.coordinates(V6),
        threePentagonCluster.coordinates(VertexId(7))
      )
      .value
    val rhombusVertex = enclosedFaces(result).head.getVerticesUnsafe.map(_.id).toSet
    val boundaryIds   = result.boundaryVerticesUnsafe.map(_.id).toSet
    allAssert(
      validate(result).isRight shouldBe true,
      rhombusVertex.intersect(boundaryIds) shouldBe empty
    )

  it should "not invent a face when a copy merely doubles the tiling without enclosing" in:
    // Mirroring a single pentagon across one edge yields two edge-sharing pentagons and nothing enclosed.
    val coords = pentagon.coordinates
    val result = pentagon.maybeAddMirroredCopy(coords(V2), coords(V3)).value
    allAssert(
      validate(result).isRight shouldBe true,
      result.innerFaces.size shouldBe 2,
      enclosedFaces(result).size shouldBe 0
    )

  it should "still reject a copy that partially overlaps without enclosing cleanly" in:
    // Square reflected across a quarter-point axis: vertices land mid-edge, a partial overlap, not an
    // enclosure. The enclosure path must not turn this into a (spurious) valid tiling.
    val coords = square.coordinates
    val a      = (coords(V1) + (coords(V1) + coords(V2)) / BigDecimal(2)) / BigDecimal(2)
    val b      = a + (coords(V4) - coords(V1))
    square.maybeAddMirroredCopy(a, b).isLeft shouldBe true

  it should "keep an enclosed region intact when the holed tiling is reflected across its symmetry axis" in:
    // `seven` already contains one enclosed rhombus; reflecting a holed tiling must round-trip through the
    // merge (the prior hole's edges are inner now, so only the outer rim and any new gap are re-traced).
    val seven  = threePentagonCluster
      .maybeAddMirroredCopy(
        threePentagonCluster.coordinates(V6),
        threePentagonCluster.coordinates(VertexId(7))
      )
      .value
    val result = seven.maybeAddMirroredCopy(seven.coordinates(V6), seven.coordinates(VertexId(7))).value
    allAssert(
      validate(result).isRight shouldBe true,
      enclosedFaces(result).size should be >= 1
    )

  /** Closes the comb with a copy two units right. The copy is disjoint (it merely abuts along x = 2), so the
    * inner-face count above `2 ×` the comb's own cells is exactly how many regions the weld newly enclosed.
    */
  private def closeComb(rows: Int): (TilingDCEL, Int) =
    val comb   = squareComb(rows)
    val closed = comb.maybeAddTranslatedCopy(BigPoint.origin, BigPoint(BigDecimal(2), BigDecimal(0))).value
    (closed, closed.innerFaces.size - 2 * comb.innerFaces.size)

  it should "enclose multiple unit squares when a square E-comb is closed by a translated copy" in:
    // The reported multi-enclosure shape: a 5-row comb (spine + arms at rows 0, 2, 4) has two right-facing
    // notches. A copy translated two units right caps both into enclosed unit squares in a single merge — so
    // the boundary decomposition must split the faceless edges into THREE cycles (one outer, two holes).
    val (result, enclosed) = closeComb(5)
    allAssert(
      squareComb(5).innerFaces.size shouldBe 8, // spine of 5 + 3 arms
      validate(result).isRight shouldBe true,
      result.innerFaces.size shouldBe 18,       // 2 x 8 cells + 2 enclosed squares
      enclosed shouldBe 2,
      // every face — tiles and the two newly enclosed regions alike — is a unit square
      result.innerFaces.forall(_.getVerticesUnsafe.size == 4) shouldBe true,
      result.innerFaces.forall(_.anglesUnsafe.forall(_ == AngleDegree(90))) shouldBe true
    )

  it should "scale the number of enclosures with the comb height" in:
    // A comb of 2m+1 rows has m notches, so a single closing translate encloses m unit squares: the scheme
    // extends to any number of enclosures. Each must validate (interior vertices closing to a full circle).
    val (closed5, enclosed5) = closeComb(5)
    val (closed7, enclosed7) = closeComb(7)
    val (closed9, enclosed9) = closeComb(9)
    allAssert(
      validate(closed5).isRight shouldBe true,
      validate(closed7).isRight shouldBe true,
      validate(closed9).isRight shouldBe true,
      enclosed5 shouldBe 2,
      enclosed7 shouldBe 3,
      enclosed9 shouldBe 4
    )

  /** A T-pentomino of unit squares: a 3-tall spine with one arm left and one arm right of the top cell. The
    * base cell (bottom of the spine) is centred on (0.5, 0.5).
    */
  private def tPentomino: Tiling =
    def addSquareAt(tiling: Tiling, x: Int, y: Int): Tiling =
      tiling.coordinates
        .collectFirst:
          case (id, p) if p.almostEquals(BigPoint(BigDecimal(x), BigDecimal(y))) => id
        .flatMap: id =>
          tiling.maybeAddRegularPolygonToBoundary(id, RegularPolygon(4)).toOption
        .get
    val spine                                               = TilingBuilder.createRhombusNet(1, 3).value
    addSquareAt(addSquareAt(spine, 1, 3), 0, 2) // right arm at (1,2), left arm at (-1,2)

  it should "enclose a unit square when a T-pentomino rotates 90°/270° onto its base (a pinch vertex)" in:
    // A copy rotated 90° or 270° about the base cell traps a unit square between three tiles that meet at a
    // single **pinch** vertex (two tiles touch only at that corner). The square is filled, so its four edges
    // become internal and the outer boundary stays simple — a valid tiling. The merge must trace the rims
    // correctly at the pinch and assign a rational corner angle there; both directions pinch on opposite sides
    // (ADR-0013).
    val centre                                  = BigPoint(BigDecimal(0.5), BigDecimal(0.5)) // base cell centroid
    def closed(degrees: Int): Tiling            =
      tPentomino.maybeAddRotatedCopy(centre, AngleDegree(degrees)).value
    def enclosedSquare(tiling: Tiling): Boolean =
      tiling.innerFaces.exists: face =>
        face.getVerticesUnsafe.size == 4 &&
          face.anglesUnsafe.forall(_ == AngleDegree(90)) &&
          face.halfEdgesUnsafe.forall(e => e.twin.flatMap(_.incidentFace).exists(!_.isOuter))
    allAssert(
      validate(closed(90)).isRight shouldBe true,
      closed(90).innerFaces.size shouldBe 10, // 9 tiles (base square dedups) + 1 enclosed square
      enclosedSquare(closed(90)) shouldBe true,
      validate(closed(270)).isRight shouldBe true,
      closed(270).innerFaces.size shouldBe 10,
      enclosedSquare(closed(270)) shouldBe true
    )

  /** The set of grid cells a square tiling occupies, keyed by integer lower-left corner. */
  private def cells(tiling: Tiling): Set[(Int, Int)] =
    tiling.innerFaces.map: face =>
      val c = face.getVerticesUnsafe.map(_.coords).centroid
      (
        (c.x - BigDecimal(0.5)).setScale(0, BigDecimal.RoundingMode.HALF_UP).toInt,
        (c.y - BigDecimal(0.5)).setScale(0, BigDecimal.RoundingMode.HALF_UP).toInt
      )
    .toSet

  /** Builds a polyomino equal to `target` (which must contain cell (0,0)) by attaching unit squares one at a
    * time, trying every boundary vertex and keeping the attachment that lands a new cell inside `target`.
    */
  private def buildPolyomino(target: Set[(Int, Int)]): Tiling =
    require(target.contains((0, 0)))
    var tiling = square
    while cells(tiling) != target do
      val before = cells(tiling)
      tiling = tiling.boundaryVerticesUnsafe.iterator
        .flatMap(v => tiling.maybeAddRegularPolygonToBoundary(v.id, RegularPolygon(4)).toOption)
        .find(grown => cells(grown).subsetOf(target) && cells(grown).size == before.size + 1)
        .getOrElse(throw new IllegalStateException(s"cannot extend $before towards $target"))
    tiling

  it should "enclose two squares at a double pinch: M pentomino reflected across its base (ADR-0014)" in:
    // The reported multi-pinch case. Take a 3×3 unit-square net (vertices 1..16, left-to-right then
    // bottom-to-top) and drop vertices 1, 12, 15: the five survivors form an M/W pentomino (a staircase).
    // Its base vertices 8, 11, 14 are collinear (here, shifted down one row, on x + y = 3); reflecting across
    // that axis welds a congruent copy on and encloses TWO separate unit squares that meet at a single pinch
    // point — 12 faces in all. Each enclosed square has two pinch corners, so the single-pinch closure of
    // ADR-0013 is not enough; ADR-0014 reads the extra corner exactly off the geometry.
    val pentomino = buildPolyomino(Set((1, -1), (2, -1), (0, 0), (1, 0), (0, 1)))
    val result    = pentomino
      .maybeAddMirroredCopy(BigPoint(BigDecimal(3), BigDecimal(0)), BigPoint(BigDecimal(1), BigDecimal(2)))
      .value
    allAssert(
      pentomino.innerFaces.size shouldBe 5,
      validate(result).isRight shouldBe true,
      result.innerFaces.size shouldBe
        12, // 10 tiles (the two reflected pentominoes are disjoint) + 2 enclosed
      // The reflection welds two disjoint 5-cell pentominoes, so any faces above 2 × 5 are newly enclosed.
      result.innerFaces.size - 2 * pentomino.innerFaces.size shouldBe 2,
      result.innerFaces.forall(_.getVerticesUnsafe.size == 4) shouldBe true,
      result.innerFaces.forall(_.anglesUnsafe.forall(_ == AngleDegree(90))) shouldBe true
    )

  it should "enclose a triangle among three dodecagons (the 3.12.12 vertex)" in:
    // Two dodecagons sharing an edge, then a copy rotated 60° about the first dodecagon's centre. 60° is a
    // symmetry of the 12-gon, so the first dodecagon maps onto itself (dedup) and the second lands as a third
    // dodecagon two edges round. The 60° gap between the second and third closes into a unit equilateral
    // triangle (3 + 12 + 12 = 360 at every corner) enclosed by all three — four polygons in total.
    val twoDodecagons = dodecagon.maybeAddRegularPolygonToBoundary(V1, RegularPolygon(12)).value
    val centre        = dodecagon.coordinates.values.toList.centroid
    val result        = twoDodecagons.maybeAddRotatedCopy(centre, AngleDegree(60)).value
    val triangles     = result.innerFaces.filter(_.getVerticesUnsafe.size == 3)
    allAssert(
      validate(result).isRight shouldBe true,
      result.innerFaces.size shouldBe 4, // 3 dodecagons + 1 enclosed triangle
      triangles.size shouldBe 1,
      triangles.head.anglesUnsafe.forall(_ == AngleDegree(60)) shouldBe true
    )
