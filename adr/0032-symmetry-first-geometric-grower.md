# ADR-0032: Symmetry-first geometric grower — the scatter wall falls (Phase 2 spike GO)

- **Status:** Proposed (de-risk spike GO; build-out in progress)
- **Date:** 2026-06-21

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
