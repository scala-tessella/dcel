package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.DelaneySymbols.DSymbol
import io.github.scala_tessella.dcel.SymbolRenderer.*
import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The barycentric developer, validated ground-up: exact chamber geometry and reflection on hand-computed
  * values, then development of KNOWN tilings from their oracle minimal symbols — lattice coordinates for
  * 4⁴/3⁶, the octagon case 4.8² (which the ℤ[ζ₁₂] machinery cannot draw), and every Archimedean tiling
  * producing only its own polygon sizes with correctly-sized complete faces.
  */
class SymbolRendererSpec extends AnyFlatSpec with Matchers:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  private lazy val oracle: Map[VertexSignature, DSymbol] =
    DelaneySymbols.enumerateSymbols(maxN = 1, maxSize = 12).map((_, sigs, ds) => sigs.head -> ds).toMap

  behavior of "the geometry leaves (hand-computed)"

  it should "compute apothem and circumradius of the square" in:
    apothem(4) shouldBe 0.5 +- 1e-12
    circumradius(4) shouldBe math.sqrt(2) / 2 +- 1e-12

  it should "reflect points across a line" in:
    val r = reflect((1.0, 1.0), (0.0, 0.0), (2.0, 0.0)) // across the x-axis
    r._1 shouldBe 1.0 +- 1e-12
    r._2 shouldBe -1.0 +- 1e-12
    val s = reflect((0.0, 1.0), (0.0, 0.0), (1.0, 1.0)) // across the diagonal
    s._1 shouldBe 1.0 +- 1e-12
    s._2 shouldBe 0.0 +- 1e-12

  behavior of "development of known tilings (from their oracle minimal symbols)"

  it should "develop 4⁴ into unit squares on the integer lattice" in:
    val faces = develop(oracle(sig("4.4.4.4")), radius = 3.5)
    faces.size should be >= 25
    all(faces.map(_._1)) shouldBe 4
    all(faces.map(_._2.size)) shouldBe 4
    // every corner sits on the integer lattice
    for (_, pts) <- faces; (x, y) <- pts do
      math.abs(x - math.round(x)) should be < 1e-6
      math.abs(y - math.round(y)) should be < 1e-6

  it should "develop 3⁶ into unit triangles on the triangular lattice" in:
    val faces = develop(oracle(sig("3.3.3.3.3.3")), radius = 3.0)
    faces.size should be >= 30
    all(faces.map(_._1)) shouldBe 3
    all(faces.map(_._2.size)) shouldBe 3
    val h     = math.sqrt(3) / 2
    for (_, pts) <- faces; (x, y) <- pts do
      math.abs(x * 2 - math.round(x * 2)) should be < 1e-6
      math.abs(y / h - math.round(y / h)) should be < 1e-6

  it should "develop 4.8² — the octagon tiling no ζ₁₂ engine can draw" in:
    val faces = develop(oracle(sig("4.8.8")), radius = 5.0)
    faces.map(_._1).toSet shouldBe Set(4, 8)
    for (p, pts) <- faces do pts.size shouldBe p

  it should "develop every Archimedean tiling with exactly its own polygon sizes, faces complete" in:
    for (t, ds) <- oracle do
      withClue(s"${t.mkString(".")}: "):
        val faces = develop(ds, radius = 4.5)
        faces.nonEmpty shouldBe true
        faces.map(_._1).toSet shouldBe t.toSet
        for (p, pts) <- faces do pts.size shouldBe p

  behavior of "the SVG output"

  it should "emit one polygon per face plus the banner" in:
    val faces = develop(oracle(sig("3.6.3.6")), radius = 3.5)
    val svg   = toSvg(faces, "test banner")
    svg should include("<svg")
    svg should include("test banner")
    svg.linesIterator.count(_.startsWith("<polygon")) shouldBe faces.size
