# ADR-0020: Exact-coordinate torus engine (the ADR-0019 "different search")

- **Status:** Accepted (prototype, validated) — a third enumeration engine, faster than and key-equivalent to
  the DCEL fixed-Λ engine on every tested case; kept alongside it as the DCEL engine remains the reference.
- **Date:** 2026-06-18

> Implemented in `generator/.../ZetaPoint.scala` and `KrotenheerdtTorusSearch.scala`, validated by
> `ZetaPointSpec`, `KrotenheerdtTorusSearchSpec`, and the head-to-head `TorusTimingProbe`. Reuses the DCEL
> engine's verification tail, extracted as `KrotenheerdtLatticeSearch.verifyContent`.

## Context

ADR-0019 profiled the DCEL fixed-Λ engine ([[KrotenheerdtLatticeSearch]]) as **~45 % canonical-congruence
key + ~37 % immutable-DCEL deep-copy** per state, and concluded both are irreducible *within that model*: a
real speedup needs **a different search — one that generates each patch once (no dedup key) or enumerates the
torus-cell contents directly** — not a per-state tweak. This ADR is that different search (Option B), built as
a prototype to decide empirically whether the idea pays off.

## Decision — represent the patch in exact integer ℤ[ζ₁₂] coordinates, grow on the torus

Every vertex of a `{3,4,6,12}` tiling lies in the cyclotomic module ℤ[ζ₁₂], ζ = e^{iπ/6} (edge directions are
multiples of 30°). Represent each vertex as an exact integer 4-tuple `a₀ + a₁ζ + a₂ζ² + a₃ζ³` (relation
`ζ⁴ = ζ² − 1` keeps degree ≤ 3; [[ZetaPoint]]). A unit edge is integer multiplication by a power of ζ. This
single change removes **both** ADR-0019 costs:

- **No deep-copy.** A patch is a cheap immutable `List[FaceZ]` of unit polygons; growth prepends one polygon
  built by integer ζ-steps. (The DCEL's overlap / coincident-vertex / enclosed-region bookkeeping is recovered
  for free: coincident vertices are *equal residues*, and "overlap" is *a 30°-slot already owned* — i.e. the
  Λ-consistency oracle, now on exact integers.)
- **No trig congruence key.** Λ is fixed and oriented, so two growth orders that reach the same patch reach the
  *same integer point set*. The dedup key is the face-set canonicalised over **Λ's point group + translation**,
  computed in small integers (`canonicalKey`): for each lattice automorphism `g` (a module isometry —
  rotation `ζ^k` and reflection `conjugate` — that maps Λ to itself, tested by exact integer congruence),
  transform, translation-anchor on the lex-min corner, and take the min serialization. This is the trig-free
  replacement for the DCEL `congruenceKey` (ADR-0019: 45 %, fragile, un-substitutable in Double/exact-trig
  variants). Canonicalising over the **Λ-automorphism subgroup only** (not the full module group) is what keeps
  it sound — it never merges patches with different Λ-continuations.

Candidate lattices are enumerated as exact integer pairs over the L1 step-budget `Σ|aᵢ| ≤ k` (the exact
analogue of the DCEL `candidateBases`), sign-canonicalised and deduped. Orientation is swept by the candidate
set (which holds each lattice's rotated copies), with a single slot-0 seed per polygon — the same convention as
the DCEL engine, so the completeness profile in `k` is identical. Per-Λ growth is gated by the exact
Λ-consistency slot check; a completed cell (distinct faces tile one covolume) is handed to the shared
`verifyContent`.

## Verification is shared, so soundness is inherited

The completed-cell gate is the DCEL engine's proven tail, extracted verbatim as `verifyContent`: primitive-
lattice re-keying, the EXACT vertex-orbit count (not 1-WL — ADR-0019's central soundness fix), and the
up-to-all-isometries canonical key. The torus engine reconstructs each torus vertex's fan by unioning its
planar instances' corners keyed by angle (as `verifyTorus` does), then calls `verifyContent`. So a tiling the
torus engine reports is sound by exactly the ADR-0019 argument; only the *generation* of candidate cells
changed.

One bug surfaced and was fixed during validation, improving **both** engines: `primitiveBasis` derived its
sublattice periods only from same-size face-centroid differences, so a **single-face sublattice cell** (a
doubled `4⁴` — one square but two vertices) had no pair to reduce by and was reported as a distinct tiling.
The fix derives periods from same-type **vertex** positions as well (and requires a period to preserve the
vertex content), which is strictly more correct.

## Results (validated, `TorusTimingProbe`, single 8-core host)

Key sets are **identical** to the DCEL engine (octagon-free) on every tested case — n=1 k=3, n=1 k=4, n=2 k=4.

| case | torus ∥4 | DCEL ∥4 | speedup | torus bases / states | DCEL bases / states |
|------|---------:|--------:|--------:|----------------------|---------------------|
| n=1 k=3 | 1.15 s | 1.31 s | 1.14× | 197 / 3 475 | 281 / 5 346 |
| n=1 k=4 | 40.0 s | 49.8 s | 1.25× | 2 363 / 419 230 | 3 558 / 263 044 |
| n=2 k=4 | **11.7 s** | **21.7 s** | **1.85×** | 1 925 / 191 009 | 1 947 / 122 282 |

The micro-thesis holds: the exact engine is **~3× cheaper per state** (no key, no copy) and uses a **smaller
candidate set**. It explores **~1.6× more states** than the DCEL engine — the canonical key over Λ's
automorphism group merges less aggressively than the DCEL full-congruence key — so the net is a solid **1.1–1.9×**
(largest on the heaviest case), not an order of magnitude. The octagon's `4.8.8` (45°, ℤ[ζ₂₄]) is out of scope
for this engine, exactly as it is past the DCEL engine's current `k`.

## Consequences

- **Positive.** A faster, fully **exact-integer** engine (no Double/√3 in the search; the rounding fragility
  ADR-0019 warned about is gone), parallel like the DCEL engine, key-equivalent on every tested case, with a
  smaller candidate set. The sublattice fix hardened the shared verification for both engines.
- **The order-of-magnitude is still ahead.** The remaining lever is the **state count**, not per-state cost:
  this engine still grows one polygon at a time (like the DCEL engine), so it inherits a similar state count.
  The Galebach-scale win is the deeper **vertex-completion constraint propagation** — commit a whole vertex
  type at the most-constrained torus vertex and propagate forced neighbours, backtracking early — built on
  this exact-coordinate foundation. That is the recommended next step for n = 4–7.
- **Scope unchanged from ADR-0019.** Completeness still rests on `(k, maxCovolume)`; the practical wall for high
  n is still the *number* of candidate lattices. The library is untouched; `KrotenheerdtSearch` (ADR-0018)
  remains the rigorous n ≤ 2 reference and `KrotenheerdtLatticeSearch` (ADR-0019) the validated fixed-Λ
  reference for cross-checking.

## Pitfalls hit and fixed (do not repeat)

- Enumerating candidate module points over the integer **box** `|aᵢ| ≤ k` instead of the L1 **budget**
  `Σ|aᵢ| ≤ k` blows the candidate count up by ~k⁴ extra points and hangs the BigDecimal pairing. Cache each
  point's `BigPoint` embedding.
- Seeding all 12 edge orientations (instead of slot-0 only) ×12s the state count: with the absolute key it
  spawns congruent copies that never merge (the ADR-0019 "absolute key inflates states" effect). Seed slot-0
  and let the candidate set sweep orientation.
- The dedup key must canonicalise over Λ's **automorphism subgroup**, not the full 24-element module group —
  the latter would merge patches with different Λ-continuations (unsound).
