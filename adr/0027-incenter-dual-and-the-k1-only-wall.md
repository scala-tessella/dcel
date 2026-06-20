# ADR-0027: The incenter dual works — but the Taganap method is k = 1-only (the k-isocoronal ≠ k-uniform wall)

- **Status:** Measured. Supersedes the misnamed ADR-0026 (whose "DOCUMENTED ALGORITHM" section over-read
  Taganap & De Las Peñas, *Acta Cryst.* A75 (2019) 94–106 as a complete n ≤ 7 generator).
- **Date:** 2026-06-20

## What was de-risked (per the ADR-0026 plan: "k = 1 first")

The plan was: implement the incenter dual + low-index-subgroup construction, reproduce the 11 Archimedean
(k = 1) against the `DelaneySymbols` oracle, then scale to n ≤ 7 by enumerating finite-index subgroups of the
~11–20 tile-transitive seeds.

### POSITIVE — the incenter dual is a 5-line Delaney-symbol primitive (`DelaneySymbols.dualSymbol`)

The paper's incenter dual `T → T*` (Theorem 3.1/3.2: tiles ↔ vertices, edges fixed) is, on a 2D
Delaney–Dress symbol, **exactly the swap of indices 0 and 2** — `σ₀ ↔ σ₂` and `m₀₁ ↔ m₁₂`. No geometry, no
coordinates. Implemented as `dualSymbol` and validated the cheapest possible way against the n ≤ 3 oracle
(`DualSymbolSpec`, 8 tests, <1 s):

- involution up to isomorphism (`key ∘ dual ∘ dual = key`) on all 11 Archimedean symbols;
- the textbook geometric pairs: triangular 3⁶ ↔ hexagonal 6³, square 4⁴ self-dual;
- the **full k = 1 pipeline**: the 11 tile-transitive Laves seeds (= duals of the 11 Archimedean, Theorem
  4.2) dualize back to **exactly** the 11 Archimedean canonical keys.

So the dual + canonical-keying machinery is correct and end-to-end. This is a genuine keeper primitive.

### NEGATIVE (decisive) — the method does NOT generate k ≥ 2 uniform tilings

Reading the paper closely (it is `k`-**isocoronal**, orbits of *coronae*) exposes the gap the ADR-0026 memory
flagged but under-weighted ("k-isocoronal ≠ k-uniform"):

- A tile-transitive **monohedral** seed (each of the 11 Laves is one tile shape, one orbit under its group
  `G`) under a finite-index subgroup `H ≤ G` gets its single tile-orbit **relabelled** into `k` orbits of
  **congruent** tiles. The dual is then the *same* Archimedean tiling carrying `k` vertex-orbits **all of one
  vertex type** — a k-iso**gonal** (k-isocoronal) tiling, NOT k-**uniform**. A068600 counts *distinct* vertex
  types, so these do not count.
- A genuine n ≥ 2 Krötenheerdt tiling has **distinct** vertex types, so its dual is a **multi-prototile**
  tile-k-transitive Laves tiling (e.g. {3⁶; 3⁴.6}: vertex degrees 6 and 5 ⟹ the dual has a hexagon *and* a
  pentagon tile). A subgroup of a *fixed monohedral seed* can never introduce a second tile shape.
- The paper's k = 1 result (Theorem 4.2: exactly 11 Laves ⟺ 11 Archimedean) is **complete with proof**
  *because* the 11 monohedral Laves are the complete tile-transitive-by-tangential-regular-vertex list. For
  k ≥ 2 the paper only gives the **framework** (Theorem 3.1) + worked examples (the 3-isocoronal of Fig. 2,
  the 8-uniform of Fig. 6) whose seeds are **already** multi-prototile tile-k-transitive tilings — i.e. the
  very objects we are trying to enumerate. It supplies no bounded way to generate those seeds.
- The "[G : H] ≈ k, so index ≲ 12" bound is on **refining a given seed**, not on generating seeds. The
  multi-prototile seed carries the translation cell, and **cell covolume is not bounded by k** (ADR-0020).
  So the seed-generation step re-enters the **covolume wall**; the dual does not escape it.

`DualSymbolSpec`'s last test encodes this conclusion: the 11 duals already span 3-, 4-, 5-, 6-gon Laves
tiles, none of which a single monohedral seed can mix.

## The map of walls (updated)

| family | route | wall |
|--------|-------|------|
| D-symbol generation | generate-all, oriented-slice (best: n=4 5/33, n=5 1/15), corona-first, orbit-count | partial-D-set tree ~4×/+2; no prune cuts it (ADR-0022/0023) |
| cell assembly | fixed-Λ, bounded-V dart | size = covolume, exponential; reaches n ≤ 2 only (ADR-0020/0025) |
| **incenter dual / subgroup** (this ADR) | Taganap A75 (2019) | **complete for k = 1 only**; k ≥ 2 needs multi-prototile seeds whose generation re-enters the covolume wall |

## Decision

`dualSymbol` is kept as a validated primitive (classification / cross-check / dedup, and the
tile-side ↔ vertex-side bridge). The Taganap subgroup construction is **not** a viable n ≤ 7 *generator* and
is closed out as a generator, for the measured reason above. The strategic question — how to attack k ≥ 2
generation given every documented route now has a measured wall — is deferred to the user (see session
notes); the leading candidates are (a) optimise + parallelise the surviving oriented-slice generator with
the dual as a fast classifier, (b) reconstruct Galebach's undocumented growth method, (c) renegotiate the
n ≤ 7 scope.

## Keepers

- `DelaneySymbols.dualSymbol` + `DualSymbolSpec` — the validated incenter dual.
- `DelaneySymbols` oracle (n ≤ 3) + `minimalSymbol`/`canonicalKey` — verify/dedup tail.
- Oriented-slice generator (ADR-0023) — the D-symbol ceiling, the best surviving generator.
- `BucketAssembly` — validated low-V torus engine, cross-check.
