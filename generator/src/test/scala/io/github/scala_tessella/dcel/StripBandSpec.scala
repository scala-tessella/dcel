package io.github.scala_tessella.dcel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Test-first pinning of the genuine strip-stacking band generator ([[StripBand]], ADR-0037).
  *
  * A *band* is one layer of unit `{3,4,6,12}`-gons between two profiles; [[StripBand.fillAbove]] fills the
  * layer above a bottom polyline and the upper corners give the top profile. These tests pin the canonical
  * bands by their polygon multiset and their two boundary arc-degree profiles, and lock the structural
  * invariants (geometric consistency, single-layer, flip-dedup) BEFORE any stacking — so the catalogue
  * rendered for visual inspection is on verified behaviour.
  */
class StripBandSpec extends AnyFlatSpec with Matchers:

  import StripBand.{Band, fillAbove, catalogue, bandValid}

  /** Does `fillAbove(edges)` produce a band with this polygon multiset and these (sorted) bottom-arc degrees?
    */
  private def has(edges: Vector[Int], faces: List[Int], bottomArcs: List[Int]): Boolean =
    fillAbove(edges).exists(b =>
      b.faceSizes == faces.sorted && b.bottomArcs.sorted.toList == bottomArcs.sorted
    )

  behavior of "StripBand.fillAbove (the four classic canonical bands)"

  it should "build the SQUARE row: straight bottom (180°), one square per unit, straight top" in:
    val band = fillAbove(Vector(0)).find(_.faceSizes == List(4)).getOrElse(fail("no square row"))
    band.bottomArcs.sorted.toList shouldBe List(180)
    band.topArcs.sorted shouldBe List(180)
    band.faceSizes shouldBe List(4)
    bandValid(band) shouldBe true

  it should "build the TRIANGLE row: straight bottom (180°), two triangles per unit, straight top" in:
    val band = fillAbove(Vector(0)).find(_.faceSizes == List(3, 3)).getOrElse(fail("no triangle row"))
    band.bottomArcs.sorted.toList shouldBe List(180)
    band.topArcs.sorted shouldBe List(180)
    bandValid(band) shouldBe true

  it should
    "build the 3.6.3.6 hex+triangle row with a STRAIGHT boundary of alternating hexagon & triangle edges" in:
      // the user-corrected insight: a STRAIGHT profile is a polyline, not one polygon type — here the straight
      // bottom alternates a triangle edge and a hexagon edge. The OTHER boundary is NOT straight: a hexagon is
      // taller than a triangle, so the triangle apex sits in a notch ⇒ the top zig-zags. That is correct geometry.
      val band = fillAbove(Vector(0, 0)).find(_.faceSizes == List(3, 6)).getOrElse(fail("no 3.6.3.6 row"))
      band.bottomArcs.sorted.toList shouldBe List(180, 180) // straight, alternating tri-edge / hex-edge
      val tri = band.faces.find(_.size == 3).get
      val hex = band.faces.find(_.size == 6).get
      // the straight bottom carries one triangle edge and one hexagon edge (alternating), as the ADR states
      tri.corners.map(_.toBigPoint.y.toDouble).min shouldBe 0.0 +- 1e-9
      hex.corners.map(_.toBigPoint.y.toDouble).min shouldBe 0.0 +- 1e-9
      bandValid(band) shouldBe true

  it should "build the HEXAGON 120°/240° zig-zag row ([1,11]), one hexagon per period" in:
    val band = fillAbove(Vector(1, 11)).find(_.faceSizes == List(6)).getOrElse(fail("no hexagon zig-zag row"))
    band.bottomArcs.sorted.toList shouldBe
      List(120, 240) // apex 120° (1 hex corner), side 240° (2 hex corners)
    bandValid(band) shouldBe true

  behavior of "StripBand.fillAbove (the two bands the user added by visual inspection)"

  // USER BAND #1: a band of hexagons whose boundary degrees repeat 120,120,240,240.
  it should "build the 120,120,240,240 HEXAGON band ([0,2,0,10]), two hexagons per period" in:
    val band = fillAbove(Vector(0, 2, 0, 10)).find(_.faceSizes == List(6, 6))
      .getOrElse(fail("no 120,120,240,240 hexagon band"))
    band.bottomArcs.sorted.toList shouldBe List(120, 120, 240, 240)
    band.topArcs.sorted shouldBe List(120, 120, 240, 240)
    band.faceSizes shouldBe List(6, 6)
    bandValid(band) shouldBe true

  // USER BAND #2: 1 hexagon + 3 triangles (a half-hexagon trapezoid), one boundary straight, the other
  // 120,120,240,240. Reachable from the straight side ([0,0,0]) and the zig-zag side ([0,2,0,10]).
  it should "build the 1-hexagon-3-triangle band: straight boundary opposite a 120,120,240,240 boundary" in:
    val band = fillAbove(Vector(0, 0, 0)).find(_.faceSizes == List(3, 3, 3, 6))
      .getOrElse(fail("no 1-hexagon-3-triangle band"))
    band.faceSizes shouldBe List(3, 3, 3, 6)
    band.bottomArcs.sorted.toList shouldBe List(180, 180, 180) // the straight side
    band.topArcs.sorted shouldBe List(120, 120, 240, 240)      // the half-hexagon zig-zag side
    bandValid(band) shouldBe true

  it should "see the same 1-hexagon-3-triangle band from its zig-zag side ([0,2,0,10])" in:
    has(Vector(0, 2, 0, 10), faces = List(3, 3, 3, 6), bottomArcs = List(120, 120, 240, 240)) shouldBe true

  behavior of "StripBand structural invariants"

  it should "produce only geometrically-consistent single-layer bands (no overlaps, no complete vertices)" in:
    catalogue(4).foreach(b => withClue(s"invalid band ${b.label}: ")(bandValid(b) shouldBe true))

  it should "reject an overlapping fan as a band (soundness of bandValid)" in:
    import KrotenheerdtTorusMapSearch.FaceZ
    val p   = ZetaPoint.origin
    // two squares rooted at the same vertex but rotated 30° apart ⇒ their wedges collide (overlap)
    val bad = Band(
      ZetaPoint.step(0),
      Vector(0),
      Vector(List(4)),
      List(FaceZ(4, StripBand.polygon(p, 0, 4)), FaceZ(4, StripBand.polygon(p, 1, 4)))
    )
    bandValid(bad) shouldBe false

  it should "be deterministic" in:
    catalogue(4).map(_.label) shouldBe catalogue(4).map(_.label)

  it should "flip-dedup the catalogue (the two profile-views of a band collapse to one entry)" in:
    val cat = catalogue(4)
    // the 1-hex-3-triangle band has a straight side AND a 120,120,240,240 side — it must appear ONCE
    val hht = cat.filter(b =>
      b.faceSizes == List(3, 3, 3, 6) &&
        b.typeKey._2 == Set(List(180, 180, 180), List(120, 120, 240, 240))
    )
    hht.size shouldBe 1
    // and every type key is unique (dedup is by typeKey)
    cat.map(_.typeKey).distinct.size shouldBe cat.size

  it should "contain all six named canonical bands (by polygon multiset + a named boundary profile)" in:
    val cat      = catalogue(4)
    // each canonical band, identified by its polygon multiset and ONE boundary profile the user/ADR named
    val expected = List(
      (List(4), List(180)),                        // square row — straight
      (List(3, 3), List(180)),                     // triangle row — straight
      (List(3, 6), List(180, 180)),                // 3.6.3.6 — straight alternating tri-edge / hex-edge
      (List(6), List(120, 240)),                   // hexagon 120/240 zig-zag
      (List(6, 6), List(120, 120, 240, 240)),      // hexagon 120,120,240,240
      (List(3, 3, 3, 6), List(120, 120, 240, 240)) // 1 hexagon + 3 triangles (the half-hexagon zig-zag side)
    )
    expected.foreach: (faces, boundary) =>
      withClue(s"missing canonical band faces=$faces boundary=$boundary: ")(
        cat.exists(b => b.faceSizes == faces && b.typeKey._2.contains(boundary)) shouldBe true
      )
