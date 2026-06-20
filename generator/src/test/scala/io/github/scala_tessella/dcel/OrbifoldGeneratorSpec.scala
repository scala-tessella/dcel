package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize, validSignatures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Validation of the ADR-0023 Stage-1 ORIENTED-slice generator ([[DelaneySymbols.orientedRegularSymbols]]) —
  * the engine that first crosses n = 3 by generating only the rotation-orbifold (oriented closed) D-sets and
  * recovering mirror tilings as their oriented double covers. These facts were previously checked only by
  * probes; this spec locks them into CI.
  */
class OrbifoldGeneratorSpec extends AnyFlatSpec with Matchers:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  // oracle (generate-all) keys, complete for n = 1 at this budget
  private lazy val oracleKeysN1: Set[String] =
    DelaneySymbols.keyedTilings(1, 12).filter(_._1 == 1).map(_._3).toSet

  behavior of "orientedRegularSymbols — n = 1 exact, keyed identically to the generate-all oracle"

  private lazy val oriented1 = DelaneySymbols.orientedRegularSymbols(maxN = 1, maxSize = 20)

  it should "reproduce exactly the 11 Archimedean tilings (A068600(1))" in:
    oriented1.size shouldBe 11
    oriented1.map(_._1).toSet shouldBe Set(1)

  it should "produce minimal-symbol canonical keys identical to the oracle (sound AND complete at n = 1)" in:
    oriented1.map(_._3).toSet shouldBe oracleKeysN1

  it should "include 4.8.8 (octagon) and never the non-tilings 3.3.6.6 / 3.4.4.6" in:
    val types = oriented1.flatMap(_._2).toSet
    types should contain(sig("4.8.8"))
    types should not contain sig("3.3.6.6")
    types should not contain sig("3.4.4.6")

  behavior of "orientedRegularSymbols — soundness (every result is a genuine Krötenheerdt tiling)"

  private lazy val oriented2 = DelaneySymbols.orientedRegularSymbols(maxN = 2, maxSize = 20)

  it should "satisfy the A068600 condition (n vertex orbits = n distinct types) with valid 360° types" in:
    oriented2.foreach: (n, vertices, _) =>
      vertices.size shouldBe n
      vertices.toSet.size shouldBe n
      vertices.foreach(v => withClue(s"$v: ")(validSignatures.contains(v) shouldBe true))

  it should "deduplicate by minimal canonical key (no repeated keys)" in:
    val keys = oriented2.map(_._3)
    keys.distinct.size shouldBe keys.size

  it should "reach a known 2-uniform tiling (not vacuous)" in:
    oriented2.filter(_._1 == 2).map(_._2.toSet).toSet should contain(
      Set(sig("4.4.4.4"), sig("3.3.3.4.4"))
    )

  behavior of "orientedRegularSymbols — the structural win (oriented slice ≪ generate-all tree)"

  it should "walk a far smaller, far more efficient D-set tree than generate-all" in:
    val (allTotal, _, allReg) = DelaneySymbols.generationStats(maxN = 3, maxSize = 14)
    val (oriTotal, _, oriReg) = DelaneySymbols.orientedGenerationStats(maxN = 3, maxSize = 14)
    allReg should be > 0L
    oriReg should be > 0L
    // the oriented tree is dramatically smaller (measured ~104× at size 14: 103 vs 10716)
    (oriTotal * 20) should be < allTotal
    // and far more efficient per tiling found (dsets per tiling), cross-multiplied to avoid floats
    (oriTotal * allReg) should be < (allTotal * oriReg)

  behavior of "corona-first generation — the euclidean prune cannot cut the PARTIAL tree (negative result)"

  // coronaStats walks DISTINCT partial oriented D-sets passing the early 360° angle prune. A partial corona is
  // under-angle until it closes, so the euclidean condition has no early-firing power: the partial tree is far
  // LARGER than the set of complete D-sets — confirming no generation order (corona-first included) breaks the
  // D-symbol generation wall. The search is still CORRECT (same regular tilings as the oriented generator).
  it should "find the same tilings but visit far MORE partial nodes than complete D-sets" in:
    val (oriTotal, _, oriReg) = DelaneySymbols.orientedGenerationStats(3, 16)
    val (nodes, reg)          = DelaneySymbols.coronaStats(3, 16)
    reg shouldBe oriReg // correct: same regular tilings recovered
    nodes should
      be > (oriTotal * 5) // the partial tree is many× larger than the complete count (measured ~21×)
