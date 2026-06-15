# ADR-0019: Fixed-Λ toroidal enumeration engine (scaling A068600 to n ≥ 3)

- **Status:** Proposed — work in progress (the performance approach is settled; full validation is not done)
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
**on the torus**: every torus vertex has a complete 360° valid fan; the vertex types are exactly n;
the vertex **orbits** (color refinement of the torus vertex graph, labelled by incident face sizes)
number exactly n; and the canonical key is the one-cell content (faces + typed vertices in lattice
coordinates) lex-min over equivalent bases and origins, with **primitive-basis reduction** so a
sublattice (doubled) cell collapses to its primitive key. Tilings are deduped globally by this key.

## Status (what is done / what remains)

**Done and validated:** Phase 0 oracle (4/4 tests). Candidate enumerator (correct, fast). Torus-native
verification correct for **single-vertex-cell tilings** — n = 1 small cells (3⁶, 4⁴, 3³.4²) enumerate
in ~2 s, where the planar-replicate approach timed out. The achievable-covolume filter and primitive
reduction (no duplicate keys) work.

**Not done — the next session's work:**
- **Multi-vertex-cell tilings** (6.6.6 has 2 vertices/cell; the snubs, 3.6.3.6, …) are not yet found.
  `verifyTorus` requires *every* torus vertex to have a completed (interior) planar instance, which
  needs more growth than the current `covol * 9` cap provides for cells with several vertices; and the
  torus-vertex grouping (`tkey`, frac-rounded mod-Λ coordinate) needs hardening against rounding that
  splits translate-equivalent vertices (seen: a hexagon field grouped into 7 classes instead of 2).
- **Validation** against the published counts (n = 1 = 11, n = 2 = 20, n = 3 = 39), tuning `k` and
  `maxCovolume` per n, and a regression test.
- **Soundness of the orbit count.** Color refinement is an orbit *lower bound*; for these highly
  symmetric tilings it is expected exact, but it can in principle under-count (merge two orbits) and so
  wrongly accept a >n-orbit tiling. This must be cross-checked against the published counts before the
  engine is trusted — the single most important correctness risk, and the reason the WIP was paused
  rather than rushed.

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
