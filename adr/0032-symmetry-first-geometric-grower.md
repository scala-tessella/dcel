# ADR-0032: Symmetry-first geometric grower — the scatter wall falls (Phase 2 spike GO)

- **Status:** Accepted (spike GO + build-out: n=1 complete in-scope, n=2 = 15/20; parallel; tested)
- **Date:** 2026-06-21 (build-out 2026-06-22)

## Context — reopening the ~30/33 ceiling

ADRs 0029–0031 concluded that no engine the project had built could *complete* n = 4: bounded-V dart
assembly (ADR-0025/0030) walls on large torus cells (V ≥ 19 OOMs), the oriented-slice generator (ADR-0023)
walls on large minimal symbols (low symmetry), and their union tops out at ~30/33 because the `4.6.12`-mixed
tilings are walled on *both* axes. The user reopened this: Galebach exhaustively did n ≤ 6 in ~1 month of 2002
compute, so n ≤ 7 is feasible on modern hardware, and the project's "walls" are more likely *implementation
artifacts* than the problem itself — hard evidence being the 2026-06-21 chirality bug that had masqueraded as
a covolume wall (ADR-0031). The mandate: (1) a correctness audit + benchmark *before* new work, then (2) a
genuinely new engine — a **symmetry-first geometric grower** reconstructing Galebach's method.

## Phase 1 — correctness audit (no new wall-dissolving bug)

`CrossEngineSpec` (committed `367096a`): the three surviving sound engines — the generate-all D-symbol oracle
(`keyedTilings`), the oriented-slice generator (`orientedRegularSymbols`), and the bounded-V dart assembler
(`BucketAssembly.enumerateBucket`) — all dedup in the **same canonical-key space** (`DelaneySymbols.
minimalSymbol`/`canonicalKey`), so any two that reach a tiling must produce *identical* keys. Measured
(`CrossEngineMeasure`) and asserted: they agree key-for-key on n = 1 (all 11, incl. the chiral `4.6.12`) and
on the four chiral n = 2 type-sets (`3.4.4.6`/`3.3.4.12`/`3.4.3.12` carriers; multiplicities 1,2,1,1), with
bounded-V matching on the two small-cell ones. The 3-min n=2-exact/n=3-no-spurious oracle check was re-run on
demand and still holds. `EngineBenchmark` gives a reproducible per-engine cost baseline. **Verdict: the
chirality bug was the only wall-that-was-a-bug; the ~30/33 ceiling for the *existing* engines is real, so the
way past it is a genuinely different engine.**

## Decision — commit to a rotation symmetry and grow only the fundamental sector

Build the grower ADR-0023 called "architecture B" but never built. Every periodic regular tiling has a
rotation centre of order m ∈ {2,3,4,6} (**crystallographic restriction** — order 5 and ≥ 7 are impossible; a
dodecagon's C₁₂ is broken to at most C₆ in any periodic tiling). Commit to such a centre at the origin, then
**at every growth step place the full C_m orbit of a single new regular polygon**, so the patch is
*periodic-by-construction*. This escapes both walls at once:

- **euclidean by construction** — it places only `{3,4,6,12}` polygons, so it never walks the oriented-slice's
  98 %-non-euclidean D-set tree (generation was 97 % of that engine's cost);
- **scatter-free** — the symmetry commitment kills the free grower's 1D-aperiodic stackings (ADR-0028, "the
  covolume wall's twin"); the patch grows as a compact symmetric disk whose translation lattice is discovered
  fast.

Cost is bounded by the **fundamental domain** (1/m of the cell, ≤ 1/12 with a reflection), not by the
covolume nor the D-set tree.

### Exact rotation about *any* centre (the key enabler)

A tiling-preserving rotation is an exact **integer affine map** `r(p) = ζ^(12/m)·p + t`, where
`t = (image corner) − ζ^(12/m)·(corner)` is read off **one** corner→image pair. Both terms are integral
`ZetaPoint`s, so `t` is integral — *even though the geometric centre is irrational* (a dodecagon centre
involves √2, outside ℤ[ζ₁₂]). So the centre's coordinates are never needed, and rotation about a polygon
centre (incl. dodecagon), a vertex, or an edge midpoint is all equally exact. This dissolves the "a dodecagon
can't be centred at the origin in ℤ[ζ₁₂]" blocker.

### Seed taxonomy (the central configurations)

| seed kind | m it supports | reaches |
|---|---|---|
| **polygon centre** (p ∈ {3,4,6,12}, m ∣ sym, m ≤ 6) | hex/dodec → 6; square/dodec → 4; tri/hex/dodec → 3 | 6³, 3.6.3.6, 3.4.6.4, **4.6.12**, 3.12.12 |
| **vertex** (corona's cyclic order = m) | 3⁶ → 6/3/2, 4⁴ → 4/2, 6³ → 3, … | triangular, square, snubs |
| **edge midpoint** (two congruent p-gons) | 2 | the low-symmetry p2/pgg ones |

## The spike (m = 6, central hexagon) — GO

`KrotenheerdtTorusMapSearch.enumerateBySymmetry(m, maxN, maxFaces)` reuses the existing geometry
(`ZetaPoint`, `FaceZ`, `polygon`, `boundaryGlueBases`, `verifyCell`); the only new core is single-tile
symmetric growth (`growBySymmetry`: place one polygon at the MRV vertex's open arc + its C_m orbit, dedup by
`(size, corner-set)`, keep iff planar-consistent and sound). Result on the central hexagon:

```
states = 28          6.6.6 closes @ 7 faces
                     3.6.3.6 @ 19,  3.4.6.4 @ 37
sound, deterministic, reproduces all three hexagon-centred m=6 Archimedean
```

**28 search states.** The scatter the free grower drowned in (hundreds–thousands of aperiodic states) is
gone; the state count tracks domain size. `SymmetryGrowerSpec` (6 green) locks in soundness (only real
Archimedean, no `3.3.6.6`/`3.4.4.6` leakage), reproduction by type-set, determinism, bounded cost, and a
tested `completeVertexTypes` diagnostic lens.

Two findings from the spike:

1. **Closure must be gated on the seed polygon's corona being committed.** A lone hexagon glues into the
   `6.6.6` cell at face-count 1 and the branch stops — but the central polygon is the order-m centre of *many*
   tilings, so closure is deferred until its corona (the neighbour choice) is committed, forcing the first
   ring to branch (hexagons → 6³, triangles → 3.6.3.6, squares → 3.4.6.4, …).
2. **Per-state cost (~1.7 s) is `verifyCell`'s BigDecimal overlap on 40+-face patches** — the optimisation
   target (run it only at genuine closure / move to the intrinsic form). State count, the thing that was the
   wall, is already tiny.

## Speculative hypothesis — *why there is no published Galebach algorithm*

A k-uniform tiling generally has **several inequivalent rotation centres** (e.g. a p6m tiling has 6-fold,
3-fold and 2-fold centres). A seed-driven grower therefore produces the **same tiling from multiple seeds**.
The *generation* step is clean and mechanical (grow from each seed to a determining radius); the hard part is
**reconciling those duplicates into one canonical list**. The hypothesis: Galebach's "algorithm" was never
published as a clean algorithm because the dedup/reconcile step was effectively **manual** — done by eye over
his rendered catalogue (which is exactly why his output survives only as an *image* catalogue,
`probabilitysports.com/tilings.html`, and why it took ~a month). If so, the generation was algorithmic but the
identification was human.

**Consequence for us (this is the actionable part).** Our advantage over Galebach is precisely an *algorithmic*
canonical invariant: the **D-symbol `canonicalKey`** (minimal Delaney–Dress symbol), which collapses the same
tiling grown from different seeds/centres/orientations to one key with no human in the loop. So:

- The grower's per-seed *completeness* is not what has to be perfect; **seed coverage** is — every target
  tiling must be reachable from *at least one* enumerated seed, and the canonical key handles the rest.
- The grower currently keys via `verifyCell`'s geometric content key (same space as `KrotenheerdtTorusSearch`,
  cross-validated). **Unifying its output into the D-symbol key space** (`op` → `classifyClosedMap`) is
  therefore not a nicety but the load-bearing step that makes the cross-seed, cross-engine dedup automatic —
  the thing the hypothesis says Galebach lacked.

This reframes the completeness argument the whole project rests on: *sound generation + algorithmic canonical
dedup + count = A068600(n)* certifies the exact set, **regardless of how many seeds redundantly produce each
tiling**.

## Build-out results (2026-06-22)

The full seed catalogue and a parallel driver were built on the spike; the grower now covers the rotational
majority of n ≤ 2, the per-state cost is measured, and the complementary-coverage thesis is demonstrated (not
just hypothesised). Commits `721c592`→`17df4f5`; `SymmetryGrowerSpec` 14 green.

**Coverage (`enumerateAllSeedsParallel`, maxFaces 36):**

| n | reached | notes |
|---|---------|-------|
| 1 | **10 / 10 in-scope** | only the octagon `4.8.8` out (needs ℤ[ζ₂₄]); incl. `4.6.12` / `3.12.12` from the dodecagon-centre seed |
| 2 | **15 / 20** | the rotational majority — incl. every dodecagon/`4.6.12` tiling the old engines walled on |

The 5 missing n = 2 split exactly as predicted: **1 budget gap** (`{3⁶; 3.3.4.12}`, dodecagon — `poly12` hit
`maxFaces=36`) + **~4 low-symmetry siblings** (the multiplicity-2 reference sets whose **p1/pg partner has no
rotation centre**, so no rotation seed can reach it — the bounded-V job). So:

```
symmetry-growth  =  rotational majority (cheap, incl. dodecagons)
bounded-V        =  no-rotation-centre residual
```

**The seed catalogue + exact-affine rotation.** `allSeeds` enumerates polygon-centre (incl. dodecagon),
vertex-centre (gated by an angular/slot `isCoronaSymmetric` check), and edge-midpoint (m=2) seeds over
m ∈ {2,3,4,6}, each carrying its exact integer rotation `r(p)=ζ^(12/m)·p+t`. A regression asserts **every seed
is C_m-invariant under its own rotation** (catches an `t` error for the dodecagon/vertex/edge cases before any
grow), that the dodecagon centre is order 6 (not 12), and that the C₁ vertex `3.4.6.4` yields no vertex seed.

**A closure-timing fix.** Closure is gated on the seed polygon's corona being committed — else a lone hexagon
glues into `6.6.6` at face-count 1 and the branch stops; deferring forces the first ring (the neighbour choice)
to branch, so `6³`/`3.6.3.6`/`3.4.6.4`/… all appear.

**Per-state cost — measured, and the lever found.** `profileSeed`/`profileClose`: `tryClose` is 99.8 % of
wall-clock, all in `verifyCell` (`primitiveBasis`, BigDecimal), ~20 candidate bases/state. A SOUND cheap gate
(`tryCloseFast`: skip a candidate when `distinctArea < covolume`) bought only ~4 % — most candidates are small
cells the patch overfills (gate passes) and are rejected deeper. So the per-state cost is **not** cheaply
reducible; it is **embarrassingly parallel**.

**Parallelism + key unification (the two follow-ups, both landed).**
- **#1 D-symbol key unification.** `closeCell` keeps `verifyCell` as the SOUNDNESS gate (its
  `tilesWithoutOverlap` rejects false-period non-tilings like `3.3.6.6` that the purely-combinatorial
  classifier would accept) and then keys the confirmed cell via `torusMapClassify` — which builds the closed
  cell's barycentric `op` (dart = `(vertexResidue mod Λ, outSlot)`; `α` = reverse half-edge, `σ` = rotation
  CCW; same `op` as `BucketAssembly`) and calls `classifyClosedMap`. So the grower now dedups in the **shared
  D-symbol key space** = the oracle's = the bounded-V assembler's. Validated key-for-key on 5 configs
  (hand-built `4.4.4.4` + `3⁶`; grower `6.6.6`/`3.6.3.6`/`3.4.6.4`). This is the keystone for the Phase-3 union.
- **#2 work-stealing.** `enumerateAllSeedsParallel` is a `ForkJoinPool` with **one task per patch** (children
  submitted as tasks), stealing across seeds AND within the heavy seed's subtree — no single-seed tail. n=2
  wall-clock: **266 s** (work-stealing) vs 458 s (seed-per-task) vs 1180 s (sequential) ≈ **4.4×**; rate holds
  ~10–13/s through the middle (vs the seed-silo collapsing to 3/s), dipping to ~5/s only at the final
  single-deep-branch tail.

**Certified counts need a no-budget run (the operational rule).** Under `budgetHit`, a *budget-boundary*
tiling — one whose closing patch is ≈ `maxFaces` — closes-before-the-cut only in some exploration orders, so
parallel and sequential (and run-to-run) legitimately differ; both are sound LOWER BOUNDS. Concretely the
`4.6.12` cell from the dodecagon seed needs `maxFaces ≥ ~44`: at `maxFaces=36` it is a boundary tiling, present
in the seed-silo order but cut in the work-stealing order (n=1 = 9 vs 10) — **verified not a regression**: the
dodecagon seed in isolation at `maxFaces=44` yields `{3.12.12, 4.6.12}` with `budgetHit=false`, both keyed to
the oracle. So a count is only *certified* when the run reports `budgetHit=false` (or `maxFaces` is high enough
that every reached cell closes with headroom); the equivalence test asserts only the order-INDEPENDENT
invariants (both runs sound + find the budget-stable cheap core).

**Live instrumentation.** A daemon heartbeat (elapsed / seeds-done / states + rate / faces / tilings, every
10 s) so long runs report progress instead of waiting blind.

**Next (build-out → completion):** (1) **D-symbol key unification** — key the grower via `classifyClosedMap`
(not the geometric content key) so it and bounded-V dedup in ONE space (ADR-0032's load-bearing step);
(2) **within-seed work-stealing** for n ≥ 3 wall-clock; (3) the **Phase-3 union** with bounded-V (closes the
no-rotation-centre residual) + a no-budget run to certify counts against `TilingReference`.

## Consequences

- **Positive:** first project engine that is geometric *and* scatter-free; bounded by fundamental-domain size;
  exact (ℤ[ζ₁₂]); reproduces the m=6 Archimedean in 28 states. Validated, tested, committed (`721c592`).
- **Negative / open:** per-state cost is high (BigDecimal `verifyCell`); only the central-hexagon seed is
  wired so far; the geometric→D-symbol key unification is not yet done (needed for cross-seed/cross-engine
  dedup, per the hypothesis); n ≥ 2 not yet validated.

## Next (build-out)

1. **Breadth first** — wire all seed kinds (polygon-centre incl. dodecagon, vertex-centre, edge-midpoint) over
   m ∈ {2,3,4,6}; show full n ≤ 3 coverage; test each seed kind before probing.
2. **Key unification** — `op` → `classifyClosedMap` so the grower dedups in the D-symbol space (the
   hypothesis's load-bearing step) — cross-seed and cross-engine (Phase 3 union with bounded-V).
3. **Optimise per-state cost**, then push n = 2 → 4 and validate exact vs the oracle, then `TilingReference`.

## Alternatives considered

- **Keep optimising the existing engines** (oriented-slice parallelism, disk-backed bounded-V) — ADR-0031
  showed their union cannot pass ~30/33; rejected as the path to *complete* n = 4.
- **Construct the both-walled tilings from Galebach's PNG data** — completes the count but uses the known
  answer; kept only as a last-resort cross-check, not the engine.

## Update (2026-06-22): rotation-symmetry REFERENCE — n=2 = 20/20, every tiling is rotational

To test the load-bearing hypothesis of this ADR — *that the Krötenheerdt tilings can be reached purely by
rotation seeds* — we built a D-symbol→geometry realizer and measured the rotation symmetry of **every** n=2
tiling directly, as a visually-checkable artifact.

**Method (all tested before any probe, `SymmetryGrowerSpec`):**
- `realizeCell(op)` — BFS-develops a closed torus `op` to exact ℤ[ζ₁₂] faces + lattice Λ (the inverse of
  `cellToOp`; round-trips on 4⁴/3⁶, and faithfully realizes bounded-V ops).
- `rotationCenters(faces, pvB, pwB)` — geometric: tests every face-centre / vertex / edge-midpoint for exact
  C_m invariance mod Λ, returns `{(centre-kind, max order)}`. Validated on known orbifolds: unit-square 4⁴ →
  442 (face 4, vertex 4, edge 2); two-triangle 3⁶ → 632 (vertex 6, face 3, edge 2).
- `symmetryRotationReferenceParallel` — the work-stealing twin of `enumerateAllSeedsParallel`
  (`closeCellWithCentres` = `closeCell` + `rotationCenters` on the chosen minimal-covolume basis), so the
  large rotational cells (dodecagons) are reached in minutes, keyed in the shared D-symbol space.
- `UnionRotationTableProbe` — union of bounded-V (small cells) ∪ parallel grower (large rotational cells);
  both phases instrumented. Run: `generator/Test/runMain …UnionRotationTableProbe 2 20 52` (~34 min).

**Result — the complete n=2 rotation-symmetry table (centre-kind : angle, where order m ↦ 360/m°):**

| vertex-type set | rotation centres | by |
|---|---|---|
| 3.12.12; 3.4.3.12 | edge 180°, face 90° | bounded-V |
| 3⁶; 3.3.3.3.6 | edge 180°, face 60°, vertex 120° | grower |
| 3⁶; 3.3.3.3.6 | edge 180°, face 60°, face 120° | grower |
| 3⁶; 3.3.3.4.4 | edge 180°, face 180° | bounded-V |
| 3⁶; 3.3.3.4.4 | edge 180°, face 180°, vertex 180° | bounded-V |
| 3⁶; 3.3.4.12 | face 60°, face 180°, vertex 120° | grower |
| 3⁶; 3.3.4.3.4 | face 120°, face 180°, vertex 60° | bounded-V |
| 3⁶; 3.3.6.6 | edge 180°, face 120°, vertex 60° | bounded-V |
| 3.3.3.3.6; 3.3.6.6 | edge 180°, face 180° | bounded-V |
| 3.3.3.4.4; 3.3.4.3.4 | edge 180° | bounded-V |
| 3.3.3.4.4; 3.3.4.3.4 | edge 180°, face 90° | grower |
| 3.3.3.4.4; 3.4.6.4 | edge 180°, face 60°, face 120° | bounded-V |
| 3.3.3.4.4; 4.4.4.4 | edge 180°, face 180° | bounded-V |
| 3.3.3.4.4; 4.4.4.4 | edge 180°, vertex 180° | bounded-V |
| 3.3.4.3.4; 3.4.6.4 | edge 180°, face 60°, face 120° | bounded-V |
| 3.3.6.6; 3.6.3.6 | edge 180°, face 120°, vertex 120°, vertex 180° | bounded-V |
| 3.4.4.6; 3.4.6.4 | face 60°, face 120°, face 180° | grower |
| 3.4.4.6; 3.6.3.6 | edge 180°, face 180°, vertex 180° | bounded-V |
| 3.4.4.6; 3.6.3.6 | face 180°, vertex 180° | bounded-V |
| 3.4.6.4; 4.6.12 | face 60°, face 120°, face 180° | bounded-V |

15 distinct type-sets, multiplicities summing to **20 = A068600(2)** ✓ (5 mult-2 sets appear as 2 siblings
with *different* centre signatures — distinct D-symbol keys; e.g. the {3³.4²;3².4.3.4} pair differs by a
square-centre C₄ axis, the {4⁴;3³.4²} pair by edge-vs-vertex C₂).

**VERDICT: every one of the 20 n=2 tilings has ≥ 1 rotation centre — ZERO are rotation-free.** The minimum is
a C₂ (`edge 180°`) on a shared edge-midpoint (the "mid-edge of two squares / two triangles" axis). This
*confirms the hypothesis for n=2*: a rotation-seeded grower can in principle reach all of them. It also
**corrects** the earlier inference (ADR-0030/0031 era) that 2 of the n=2 tilings were rotation-free — that was
read off the grower's *failure to reach* the {4⁴;3³.4²}/{3⁶;3³.4²} second siblings, but the failure was a
seed/closure gap (the m=2 edge-mid axis), not absence of symmetry: bounded-V realizes those siblings and they
*do* have C₂ centres. The grower supplied exactly the 4 cells bounded-V could not afford within budget (the
dodecagon {3⁶;3.3.4.12}, the hexagon {3.4.4.6;3.4.6.4}, and the {3⁶;3⁴.6} / {3³.4²;3².4.3.4} siblings).

## Update (2026-06-22): n=3 rotation table + the rotation-only strategic verdict

Ran the same union rotation table at n=3 (`UnionRotationTableProbe 3 12 58 40`, 40-min grower cap):
**28/39 reached** (a LOWER BOUND — the 11 unreached are budget/time-capped LARGE cells, not rotation-free).
Point-group distribution over the 28: order-6 = 8, order-4 = 1, order-3 = 1, **C₂-only = 18, rotation-free = 0**.

Two findings settle the "is rotation enough, or do we need another isometry" question:

1. **ZERO rotation-free at n=3 too** (as at n=2). So a rotation-seeded grower is **complete** — it can reach
   every Krötenheerdt tiling. **Translations are not a growth lever**: the translation lattice *is* the cell we
   are trying not to brute-force (the covolume wall), so "use translation" = the baseline we already beat.
2. **~64% (18/28) are C₂-only** — for the majority, rotation buys only a 2× domain cut, so cell size dominates.
   The only *additional* isometry lever is the **mirror** (a further 2×), and it is **situational, not general**:
   many C₂-only tilings are CHIRAL (they carry the snub motif 3.3.3.3.6, which has no mirror by construction —
   e.g. Galebach n=3 t=3 = {6.6.6; 3.3.3.3.6; 3.3.6.6}, a single edge-midpoint C₂ axis, zero reflection). A
   mirror can only ever help the *achiral-with-mirror* subset.

**Strategic decision: rotation-first is the engine (complete + optimal for the chiral/high-order majority);
reflections are a later, situational add-on for the achiral-mirror subset; the general lever for the C₂-only
majority is per-state cost.** Commit the grower to each tiling's *maximal* point group — chiral ⇒ Cₘ only
(reflection seeds never fire, no waste), achiral-with-mirror ⇒ + reflection (the bonus 2×).

## Update (2026-06-22): per-state cost — ~35-50× faster grower (the C₂-majority lever)

Profiling (`SymmetryProfileProbe` + a temporary in-`verifyCell` breakdown) pinned the per-state hotspot
exactly: `tryClose` = 99.8% of grower time; within it `verifyCell` (~15-21 calls/state, ~50 ms each) split as
**`tilesWithoutOverlap` 80-87%** (an O((9F)²) BigDecimal cross-product overlap test), `primitiveBasis` 11-19%,
everything else < 2%. Three correctness-preserving changes (each measured, all 31 `SymmetryGrowerSpec` +
`UnionSpec` tests green — soundness gate, reproduction, centres, union all intact):

1. **Exact-integer orientation** (`ZetaPoint.crossSign`): the cross product of two ℤ[ζ₁₂] points is `(A+B√3)/4`
   with integer `A, B`, so its *sign* is a pure `Long`/`BigInt` computation — the exact predicate the BigDecimal
   `> 1e-9` test was approximating (no epsilon). Tested vs BigDecimal on 5000 random triples + explicit cases.
2. **Spatial pruning** of the overlap test: unit polygons overlap only within centroid distance R₁+R₂ ≤ 3.87
   (dodecagon circumradius), so bucket faces on a size-4 grid and test only 3×3 neighbour buckets — the O(F²)
   far pairs (the majority) are skipped; every truly-overlapping pair shares a neighbour bucket.
3. **`verifyCell` overlap-first reorder + `closeCell` short-circuit**: run the now-cheap (~0.3 ms)
   `tilesWithoutOverlap` FIRST so a non-period is rejected before the BigDecimal O(F²) `primitiveBasis` (which
   is then paid only on candidates that genuinely tile ≈ closing states, not every patch's ~20 candidates); and
   stop `closeCell`/`closeCellWithCentres`/`tryClose` at the first verifying candidate (covolume-ascending ⇒
   first verify = primitive = the unique answer). This cuts `primitiveBasis`'s call-count by *avoidance* — no
   rewrite of it needed.

**Measured (identical states + tilings throughout):** `tilesWithoutOverlap` 76 350 → 618 ms (123×) / 51 011 →
310 ms (164×); `verifyCell` 5-7×; **end-to-end grower `profileSeed`: poly6/m6 59.1 s → 1.7 s (35×, 0.8 → 26.7
states/s), edge4 86.8 s → 1.7 s (51×)**. This directly accelerates the C₂-only majority (the cost-bound zone),
and makes pushing the grower to n = 3/4 counts far more tractable.

### Validation: the speedup re-run of the n=3 table

Re-running the n=3 union table after the per-state win (`UnionRotationTableProbe 3 12 58 40`) confirms the
end-to-end effect: **total wall-clock 46 min → 6 min 17 s (~7×)**, and — decisively — the grower phase now
**quiesces in 374 s** where before it was *capped* at the 40-min wall-clock limit and still running. So the
result is the grower's true reach at `maxFaces = 58`, not a timeout: **33/39 reached** (up from 28/39), grower
throughput ~4 → ~380 states/s (~95× on the parallel driver). The distribution is unchanged in character —
**zero rotation-free, 18/33 C₂-only** — corroborating the rotation-first verdict with more data. The 6 still
short are the largest cells (need `maxFaces > 58`) plus any low-symmetry small residual for bounded-V. With the
grower this fast, pushing to the n = 4 table and to certified counts (the union sized against
`TilingReference`) becomes tractable.

## Update (2026-06-22): n=4 table — the grower runs to completion at the WALLED level

n=4 is where the older engines stalled: ADR-0031 records their union unable to pass ~30/33, and crucially none
ran to *completion* at n=4 in reasonable time. With the per-state win the symmetry grower now does:
`UnionRotationTableProbe 4 12 64 60` **QUIESCED at 2039 s (34 min total, NOT the 60-min cap)** — i.e. it
exhausted `maxFaces = 64` and reported its true reach, **26/33**, at ~470 states/s. bounded-V contributed only
9 (n=4 cells are mostly too large for it); the grower carried the level (71 of 75 n ≤ 4 keys).

Point-group distribution over the 26: order-6 = 8, order-4 = 2, order-3 = 1, **C₂-only = 15, rotation-free = 0**.
**Zero rotation-free at n=4 too** — now consistent across n = 2/3/4 (0/20, 0/33, 0/26), so rotation-first stays
complete at the walled level. C₂-only ≈ 58%, the same character as n = 3.

The 26 is a quiesced **lower bound**: because the run reached quiescence (not the time cap), the 7 short are
the *largest* cells, which need patches deeper than `maxFaces = 64` — not a timeout. Since the grower
terminates rather than running away, raising `maxFaces` is the direct lever to push 26 → 33. Next: re-run at
`maxFaces ≈ 80`.

## Update (2026-06-22): deeper n=4 — the memory wall and its fix (grower can now run for hours)

Pushing n=4 to the full 33 means deeper patches (the 7 missing cells exceed `maxFaces = 64`). A first attempt
at `maxFaces = 80` **OOM'd the 16g heap into 100% swap** (~60 min, never quiesced, and — telling — it reached
only 74 n ≤ 4 keys, *fewer* than maxFaces = 64's quiesced 75, because the bigger frontier hadn't even re-found
the small cells yet). So brute-raising `maxFaces` was the wrong first move, and it exposed a real scaling wall.

**Root cause — the `visited` set, not the live patches.** `canonicalKey` is a `Vector[Long]` of length
≈ faces × corners × 4 (≈ 30 KB for an 80-face patch), and `visited` retains ONE per state and never shrinks.
At > 1M states that is tens of GB. The live frontier (~30-100 patches) is negligible by comparison.

**Fix — hash the visited key.** Split `canonicalKey` into `canonicalKeyVec` (the full canonical vector,
unchanged) plus a new `canonicalKey` that returns its **128-bit hash** as a 2-element `Vector[Long]` (two
independent 64-bit rolling hashes, polynomial + FNV-1a). Same value *type*, so every `visited` declaration and
`.add(canonicalKey(..))` call site is untouched; the retained entry shrinks ~1000× (16 bytes), so `visited`
stays flat (tens of MB) into the tens of millions of states. Birthday collision probability at ~10⁷ states is
~10⁻²⁵ — far below any other failure mode, so dedup stays exact in practice. All 31 `SymmetryGrowerSpec` +
`UnionSpec` tests green (reproduction, soundness, parallel equivalence, centres) ⇒ the hash does not change
which tilings are found. (Committed `afe5112`.)

With `visited` flat, the grower's footprint is bounded (visited hashes + a small live frontier), so it can run
for hours within a modest heap — and the box's free RAM, not the search, sets the heap. The deep n = 4 run is
now `maxFaces = 80` to quiescence under a multi-hour cap (the probe still reports a sound lower bound if the
cap is hit). A later throughput refinement: fold the 128-bit hash directly into `canonicalKeyVec`'s
construction so the big vector is never even materialised per state.
