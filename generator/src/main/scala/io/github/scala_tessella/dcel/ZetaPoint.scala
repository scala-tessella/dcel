package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.BigPoint

/** Exact integer coordinates in the cyclotomic module ℤ[ζ₁₂], ζ = e^{iπ/6} (a 30° rotation).
  *
  * Every vertex of an edge-to-edge tiling by unit regular polygons from `{3,4,6,12}` lies in this rank-4
  * module (edge directions are multiples of 30°, i.e. powers of ζ; the octagon's `4.8.8` needs the finer
  * ℤ[ζ₂₄] and is handled separately). Representing a vertex as the integer 4-tuple `a₀ + a₁ζ + a₂ζ² + a₃ζ³` —
  * with the minimal-polynomial relation `ζ⁴ = ζ² − 1` keeping the degree ≤ 3 — makes the fixed-Λ flood-fill
  * **exact and copy-free**: a unit edge step is integer multiplication by a power of ζ, two paths to the same
  * vertex land on the *same* 4-tuple (identity is integer equality, not a congruence key), and "same torus
  * vertex" / "legal placement" reduce to integer linear algebra rather than floating DCEL geometry. This is
  * the representation that removes both the deep-copy and the congruence-key costs profiled in ADR-0019.
  */
final case class ZetaPoint(a0: Long, a1: Long, a2: Long, a3: Long):

  def +(that: ZetaPoint): ZetaPoint =
    ZetaPoint(a0 + that.a0, a1 + that.a1, a2 + that.a2, a3 + that.a3)

  def -(that: ZetaPoint): ZetaPoint =
    ZetaPoint(a0 - that.a0, a1 - that.a1, a2 - that.a2, a3 - that.a3)

  def unary_- : ZetaPoint =
    ZetaPoint(-a0, -a1, -a2, -a3)

  /** Multiply by ζ (rotate 30° CCW). Derived from `ζ·(a₀+a₁ζ+a₂ζ²+a₃ζ³) = a₀ζ+a₁ζ²+a₂ζ³+a₃(ζ²−1)`. */
  def timesZeta: ZetaPoint =
    ZetaPoint(-a3, a0, a1 + a3, a2)

  /** Complex conjugation `ζ → ζ⁻¹` — reflection across the x-axis. Using `ζ⁹=(0,0,0,−1)`, `ζ¹⁰=(1,0,−1,0)`,
    * `ζ¹¹=(0,1,0,−1)` to re-express `conj(ζ^k)=ζ^{12−k}` in the degree-≤3 basis: it negates the embedded y.
    */
  def conjugate: ZetaPoint =
    ZetaPoint(a0 + a2, a1, -a2, -a1 - a3)

  def isOrigin: Boolean =
    a0 == 0 && a1 == 0 && a2 == 0 && a3 == 0

object ZetaPoint:

  val origin: ZetaPoint = ZetaPoint(0, 0, 0, 0)

  /** Lexicographic order on the integer coordinates — a canonical, trig-free tie-break for canonical keys. */
  given Ordering[ZetaPoint] =
    Ordering.by(z => (z.a0, z.a1, z.a2, z.a3))

  /** √3 to ample precision for the exact-as-needed embedding into the plane (only used to hand coordinates to
    * the BigDecimal verifier; the search itself never leaves the integers).
    */
  private val sqrt3: BigDecimal =
    BigDecimal("1.7320508075688772935274463415058723669428052538103806280558069794")

  private val half: BigDecimal = BigDecimal("0.5")

  /** The 12 unit vectors `ζ^s`, `s` in `0..11` — the legal unit-edge directions in 30° steps. */
  val unit: IndexedSeq[ZetaPoint] =
    val buf = Array.ofDim[ZetaPoint](12)
    buf(0) = ZetaPoint(1, 0, 0, 0)
    for s <- 1 until 12 do buf(s) = buf(s - 1).timesZeta
    buf.toIndexedSeq

  /** The unit edge in 30°-slot `s` (`s` taken mod 12). */
  def step(s: Int): ZetaPoint =
    unit(((s % 12) + 12) % 12)

  extension (p: ZetaPoint)

    /** Embed into the plane. `2x = (2a₀+a₂) + a₁√3`, `2y = (2a₃+a₁) + a₂√3`. */
    def toBigPoint: BigPoint =
      val x = (BigDecimal(2 * p.a0 + p.a2) + BigDecimal(p.a1) * sqrt3) * half
      val y = (BigDecimal(2 * p.a3 + p.a1) + BigDecimal(p.a2) * sqrt3) * half
      BigPoint(x, y)

    /** True iff `p ≡ q (mod Λ)` for Λ = (v, w): there exist integers m, n with `m·v + n·w = p − q`. Pure
      * integer linear algebra over the rank-4 module — solve a non-degenerate 2×2 component minor by Cramer's
      * rule, require an integer solution, and verify it against all four components.
      */
    def congruentMod(q: ZetaPoint, v: ZetaPoint, w: ZetaPoint): Boolean =
      val d  = p - q
      val vs = Array(v.a0, v.a1, v.a2, v.a3)
      val ws = Array(w.a0, w.a1, w.a2, w.a3)
      val ds = Array(d.a0, d.a1, d.a2, d.a3)
      var i  = 0
      while i < 4 do
        var j = i + 1
        while j < 4 do
          val det = vs(i) * ws(j) - vs(j) * ws(i)
          if det != 0 then
            val mNum = ds(i) * ws(j) - ds(j) * ws(i)
            val nNum = vs(i) * ds(j) - vs(j) * ds(i)
            if mNum % det == 0 && nNum % det == 0 then
              val m  = mNum / det
              val n  = nNum / det
              var k  = 0
              var ok = true
              while k < 4 && ok do
                if m * vs(k) + n * ws(k) != ds(k) then ok = false
                k += 1
              return ok
            else return false
          j += 1
        i += 1
      // All minors zero ⇒ v and w are parallel (degenerate Λ); congruent only if d itself is the zero vector.
      d.isOrigin
