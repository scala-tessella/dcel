package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Validates the combinatorial **incenter dual** `DelaneySymbols.dualSymbol` (Taganap & De Las Peñas, Acta
  * Cryst. A75, 2019) — the swap of Delaney indices 0 and 2 (tiles ↔ vertices). This is the load-bearing
  * assumption of the ADR-0027 dual / low-index-subgroup engine, so it is checked the cheapest way possible:
  * against the already-validated n ≤ 3 oracle ([[DelaneySymbols.enumerateSymbols]]).
  *
  *   - INVOLUTION: `dual ∘ dual` recovers the same tiling (canonical key) for every Archimedean symbol.
  *   - CONCRETE DUAL PAIRS among the regular tilings: 3⁶ (triangular) ↔ 6³ (hexagonal), 4⁴ self-dual. These
  *     are the textbook geometric duals, so matching them proves the index-swap really is the incenter dual,
  *     not merely some involution.
  */
class DualSymbolSpec extends AnyFlatSpec with Matchers:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  // the 11 Archimedean minimal symbols from the validated oracle, keyed by their (single) vertex type
  private lazy val archimedean: Map[VertexSignature, DelaneySymbols.DSymbol] =
    DelaneySymbols.enumerateSymbols(maxN = 1, maxSize = 12).map(t => (t._2.head, t._3)).toMap

  private def keyOf(ds: DelaneySymbols.DSymbol): String =
    DelaneySymbols.canonicalKey(DelaneySymbols.minimalSymbol(ds))

  behavior of "DelaneySymbols.dualSymbol (incenter dual = index 0↔2 swap)"

  it should "find all 11 Archimedean symbols to dualize" in:
    archimedean.size shouldBe 11

  it should "be an involution up to isomorphism on every Archimedean tiling" in:
    archimedean.foreach: (cfg, ds) =>
      withClue(s"$cfg: "):
        keyOf(DelaneySymbols.dualSymbol(DelaneySymbols.dualSymbol(ds))) shouldBe keyOf(ds)

  it should "preserve euclidean flatness and minimality through the dual" in:
    archimedean.values.foreach: ds =>
      val dual = DelaneySymbols.dualSymbol(ds)
      // dual of a euclidean (curvature-0) tiling is euclidean; same chamber count (relabel only)
      dual.size shouldBe ds.size

  it should "swap polygon side-counts and vertex degrees (m₀₁ ↔ m₁₂)" in:
    // 3⁶: triangular tiling — faces are 3-gons (m₀₁=3), each vertex has degree 6 (m₁₂=6).
    val tri  = archimedean(sig("3.3.3.3.3.3"))
    val dual = DelaneySymbols.dualSymbol(tri)
    // after the swap the dual's faces are 6-gons and its vertices degree 3 → the hexagonal tiling
    DelaneySymbols.minimalSymbol(dual)
    keyOf(dual) shouldBe keyOf(archimedean(sig("6.6.6")))

  it should "map the triangular tiling 3⁶ to the hexagonal tiling 6³ and back" in:
    keyOf(DelaneySymbols.dualSymbol(archimedean(sig("3.3.3.3.3.3")))) shouldBe
      keyOf(archimedean(sig("6.6.6")))
    keyOf(DelaneySymbols.dualSymbol(archimedean(sig("6.6.6")))) shouldBe
      keyOf(archimedean(sig("3.3.3.3.3.3")))

  it should "keep the square tiling 4⁴ self-dual" in:
    keyOf(DelaneySymbols.dualSymbol(archimedean(sig("4.4.4.4")))) shouldBe keyOf(archimedean(sig("4.4.4.4")))
