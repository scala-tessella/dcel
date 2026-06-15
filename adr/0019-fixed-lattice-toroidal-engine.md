# ADR-0019: Fixed-Λ toroidal enumeration engine (scaling A068600 to n ≥ 3)

- **Status:** Accepted — the engine is **sound** (validated, no false positives); completeness is bounded by
  the empirical `(k, maxCovolume)` and full published counts are pending a larger step bound / candidate
  reduction
- **Date:** 2026-06-15

> Implemented (WIP) in `generator/.../KrotenheerdtLatticeSearch.scala` and
> `LatticeConsistency.scala`, with Phase-0 tests in `LatticeConsistencySpec.scala`.
> Builds on the n ≤ 2 replication of ADR-0018.

## Context and problem statement

ADR-0018's patch-growth engine (`KrotenheerdtSearch`) rigorously replicated A068600 for n = 1 (11)
and n = 2 (20), and demonstrated n = 3 (18/39 certified before a multi-day run). It does not scale.
The bottleneck is **scatter**: free growth builds enormous numbers of aperiodic "decorated field"
patches (a triangle/square field decorated at arbitrary spacing) that share a Krotenheerdt composition
but have no 2D period, and are only rejected at the certification horizon. n = 3 is a multi-day run;
n = 4–7 (33/15/10/7) are hopeless this way.

A scalable engine must **stop building aperiodic patches**. The decision (after a design review and
three implementation attempts) is a **fixed-lattice toroidal** approach: enumerate candidate
translation lattices Λ, and for each fixed Λ grow only Λ-consistent patches, verifying each completed
fundamental cell directly on the torus. The per-Λ search is tiny and scatter-free, so cost scales.

## Decision — the architecture

Three parts (all in the JVM-only `generator/` subproject; the library is untouched):

### 1. Candidate-Λ enumerator (`candidateBases`)
The vertices of any regular-polygon tiling lie in the module ℤ[ζ₁₂] (edge directions are multiples of
30°; the octagon's `4.8.8` adds ℤ[ζ₈], 45°), and **every translation period is itself a module
vector** — so a step-bounded enumeration of the module contains every true lattice. Module points are
generated within an integer edge-step bound `k`, paired, Lagrange–Gauss reduced, sign-canonicalised,
deduped by reduced basis, and **filtered by covolume**: a cell holds whole polygons, so its covolume
must be a non-negative integer combination of unit-polygon areas (`achievableCovolumes`), bounded
`[minCovolume, maxCovolume]`. Ordered by covolume (smallest cell first).

### 2. Λ-consistency oracle (`LatticeConsistency`, Phase 0 — done and tested)
Given Λ = (v, w), a planar patch is **Λ-consistent** iff every *torus vertex* (planar vertices
identified mod Λ) carries conflict-free angular-slot coverage — no 15°-slot (24 per circle; triangle
4, square 6, octagon 9, hexagon 8, dodecagon 10) claimed by two different incident polygons. A genuine
Λ-periodic patch always passes; an off-lattice placement conflicts at the slot it lands on. Exact
(directions are exact multiples of 15°); independent of the CCW/CW sector convention. Used as the
growth gate so off-lattice scatter is pruned at the branch point.

### 3. Torus-native verification (`verifyTorus`) — the performance breakthrough
The earlier plan was "grow a Λ-consistent window, hand it to `certify`". That is impractical (see
Consequences): `certify` needs a ~5×5-cell patch, too expensive to grow planarly. Instead, grow only
until the patch's *distinct torus faces tile one covolume* (so the patch stays ~1 cell), then verify
**on the torus**. The verifier has four checks, each of which needed a non-obvious idea to get right:

- **Complete fans, from one cell.** Every torus vertex must have a complete 360° fan. Requiring a single
  *interior* planar instance would need a full corona (≈7 hexagons for `6.6.6`), defeating the "~1 cell"
  goal. Instead each torus vertex's fan is **reconstructed by unioning the partial coronas of all its
  planar instances, keyed by the corner angle** (translation-invariant). A single cell suffices — this is
  what makes multi-vertex cells (`6.6.6`, the snubs) findable cheaply. Faces are deduped by *angle*, not
  identity: in a one-face cell the three faces around a vertex are translates of the same face.
- **Types = n.** The set of fan signatures has exactly n elements.
- **Orbits = n, counted EXACTLY.** Two cell vertices are in the same orbit iff the whole cell content
  rooted at one maps onto the content rooted at the other under a lattice **isometry** (a point-group
  frame — basis pairs with the same Gram matrix as `(v, w)`); the orbit count is the number of distinct
  rooted canonical fingerprints. This **replaces 1-WL colour refinement**, which only *lower-bounds* the
  orbit count and silently merged distinct orbits (the `3.3.3.3.6 / 3.6.3.6` cell has four `3.3.3.3.6`
  vertices in two orbits — 1-WL saw one, passing a 3-uniform tiling as 2-uniform). This was the engine's
  single biggest soundness hole and is now closed.
- **Canonical key.** The one-cell content (faces + typed vertices in lattice coordinates) lex-min over
  equivalent bases (including reflected, det < 0, frames) and origins, with **primitive-basis reduction**
  so a sublattice cell collapses to its primitive key and a chiral tiling and its mirror collapse to one
  key (the up-to-all-isometries convention). Tilings are deduped globally by this key.

## Status (what is done / what remains)

**Done and validated — the engine is SOUND.** Every tiling it reports is a genuine Krotenheerdt n-uniform
tiling with the correct multiplicity, confirmed with **no false positives**:
- Phase 0 oracle (4/4 tests); candidate enumerator (correct, fast).
- **Multi-vertex cells** (`6.6.6`, the snub `3.3.3.3.6`, `3.6.3.6`, …) — solved by the union fan
  reconstruction and by keying the torus-vertex grouping with a *wrapping* integer snap (folding the
  ≈0 / ≈1 boundary coordinates that previously split translate-equivalent vertices).
- **Sublattice deduplication** (a finer period is no longer than the *longer* basis vector — a `min`
  bound missed the `2×1`-of-`4⁴` case) and **chirality** (the up-to-all-isometries convention: reflected
  frames in the key; and a latent `Map.map` collapse that dropped same-type vertices, double-counting
  mirror images).
- **Orbit-count soundness** — 1-WL colour refinement replaced by the exact rooted-fingerprint count over
  point-group frames (see the verification section). This closed the central risk: the engine no longer
  accepts a >n-orbit tiling as n-uniform. Regression-tested on the `3.3.3.3.6 / 3.6.3.6` case.
- **Validation:** n = 1 → 10 of the 11 Archimedean tilings; n = 2 → 16 of the 20 — every result correct,
  none spurious, multiplicities matching the published table (the chiral `3.12.12 / 3.4.3.12` once; the
  genuine same-composition pairs kept apart). `KrotenheerdtLatticeSearchSpec` locks the fast cases.
- One technical fix worth recording: `primitiveBasis` must Gauss-reduce in **Double**, not the shared
  exact-`BigDecimal` `gaussReduced`, which can *fail to terminate* on messy centroid-difference vectors.

**Not done — completeness, which is bounded by `(k, maxCovolume)`:**
- The few missing tilings are the **largest dodecagon cells** (`4.6.12` at n = 1; `4.6.12`, `3.3.4.12` and
  the larger pairs at n = 2). Their translation lattices have long vectors that a step bound of `k = 5`
  does not reach; `k = 6` does but the candidate set (~k⁴) makes it a ~hour run. Reaching the full
  published counts (n = 1 = 11, n = 2 = 20, n = 3 = 39) needs either a larger `k` or the candidate-set
  reduction below.
- **Candidate-set reduction** is the lever for n ≥ 3 and especially n = 4–7: per-Λ cost is small and flat
  across n, so the wall is purely the *number* of candidate lattices. Deduplicating them by reduced shape
  (Gram matrix) — most are the same lattice at a different orientation and find nothing — is the planned
  next optimisation (with care for the polygon-orientation subtlety it introduces).

## Consequences

- **Positive:** the scatter wall is gone — per-Λ work is bounded by one cell, scatter-free, and verified
  in milliseconds on the torus rather than by replicating a large planar patch. The architecture is the
  one that can reach n = 4–7. The library is untouched; the original `KrotenheerdtSearch` remains the
  reference that rigorously did n ≤ 2.
- **Negative / open:** completeness rests on the empirical bounds `k` / `maxCovolume` (must reproduce the
  published count per n, exactly as ADR-0018's radius gate is validated). The candidate set is still
  large (the dense rank-4 module), though most spurious lattices die in the growth quickly; further
  reduction (shape/orientation dedup) may be needed at higher n. The orbit-count soundness caveat above.

## Alternatives considered (and why they failed) — lessons for the next attempt

- **Orbit-gate retrofit** (promote the witnessed-corona aperiodicity check to a growth gate on the
  existing engine). Empirically *worse*: it relabels scatter without a combinatorial reduction (the
  inconsistency is only witnessable after the decorations have already branched) and reorders the search
  so genuine finds are delayed. Confirmed on a live n = 3 run.
- **Hybrid planar fixed-Λ (grow window → `certify`).** Impractical: `certify` needs the witness cell's
  corona deep-interior (~5×5 cells — a 19-cell hexagon field still rejected `TooShallow`); growing that
  planarly (deep-copy per step) × the candidate set is far too slow; and the Λ-oracle only prunes once a
  patch wraps a cell, so there is free scatter within the first cell.
- **Replicate-then-certify** (grow ~1 cell, replicate by lattice translation to ~5×5, then `certify`).
  Correct (found 3⁶, 4⁴) but the per-lattice replicate + `certify` is tens of ms, so even n = 1 small
  cells took minutes. Superseded by torus-native verification.

### Technical pitfalls already hit and fixed (do not repeat)
- Exact `BigDecimal` arithmetic on arbitrary module points makes Gauss reduction explode in precision
  and hang — enumerate candidates in `Double` (the oracle/verify recompute precisely at SCALE 9).
- Float Gauss reduction oscillates on equal-length 60° pairs (`round(0.5)` over-reduces) — stop at
  `2|a·b| ≤ |a|²`, with an iteration cap.
- The dense rank-4 module needs **both** a covolume floor (else infinitely many tiny spurious lattices)
  **and** the achievable-covolume filter.
- Sublattice (doubled) cells give a different key than the primitive cell — primitive-basis reduction in
  the key is required.
