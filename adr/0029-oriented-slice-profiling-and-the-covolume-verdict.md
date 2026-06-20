# ADR-0029: Oriented-slice generator profiled at n=4 — the covolume wall is cross-family and fundamental

- **Status:** Measured (2026-06-21). Profiles the ADR-0023 oriented-slice generator — the best surviving
  engine, chosen after the growth reconstruction was closed out (ADR-0028) — and records the cross-family
  verdict on n ≤ 7.

## The profile (where the wall-clock goes)

`OrbifoldProfileProbe` / `OrbifoldPushProbe`, maxN = 4:

| oriSize | total | dsets | euclidean-feasible | generation | euclGate+DSymGen+minimal+key |
|--------:|------:|------:|-------------------:|-----------:|-----------------------------:|
| 28 | 5.9 s  | 52 835  | 2252 (4.3%) | 5.3 s (90%) | <0.6 s |
| 30 | 20.3 s | 137 971 | 3783 (2.7%) | 19.4 s (96%) | <1.0 s |
| 32 | 75.6 s | 409 661 | 6877 (1.7%) | 73.5 s (97%) | <2.0 s |

- **Generation is 90→97 %** and rising — the `OrientedDSetGenerator` tree, dominated by `checkCanonicity`
  (O(size²)/node, the orderly-generation canonicity test). The tree grows **2.6→3.8×/+2 oriSize and
  accelerates**; only ~1.7 % of generated D-sets are even euclidean-feasible.
- Everything downstream (euclidean gate, `DSymGenerator`, `minimalSymbol`, `canonicalKey`, and so the
  `dualSymbol` classifier) is **<3 %** — irrelevant to the bottleneck.

## The trajectory (the decisive finding)

`OrbifoldPushProbe` per-n recovery vs A068600:

| oriSize | time | n=2 | n=3 | n=4 | n=5 |
|--------:|-----:|----:|----:|----:|----:|
| 30 | 20 s  | 19/20 | 22/39 | 3/33 | 1/15 |
| 32 | 75 s  | 19/20 | 28/39 | 3/33 | 1/15 |
| 34 | 286 s | 19/20 | 29/39 | 5/33 | 1/15 |

**Even n = 2 is not complete (19/20) at oriSize 34.** The missing tilings are the **low-symmetry** ones
(p1/p2/pg/…): with little or no point group, the Euclidean orbifold is essentially the **torus**, so the
minimal-symbol chamber count ≈ **covolume**, and the oriented *double* doubles it. So **the covolume wall
reappears inside the orbifold size**, precisely on the tilings the point-group reduction cannot help.

## Optimisation ceiling (honest estimate)

Incremental `checkCanonicity` (O(size²)→O(size)/O(1), ~5–20×) + parallel subtree generation (~16×) ≈ **+5–8
oriSize** → ~oriSize 40–42. By the trajectory that likely **completes n = 3** (which the generate-all oracle
already does) and reaches **n = 4 ≈ 10–18/33 — partial, not complete**. **n = 4 complete and n ≥ 5 are out of
reach** for the oriented-slice route. So optimising it yields no *complete new count*, which is the goal.

## The cross-family verdict

The covolume wall is now measured across **all five** families that were tried:

| family | route | wall |
|--------|-------|------|
| D-symbol generation | generate-all, oriented-slice, corona-first, orbit-count | partial-D-set tree ~3×/+2; low-symmetry symbol ≈ covolume |
| cell assembly | fixed-Λ, bounded-V dart | size = covolume, exponential |
| incenter dual / subgroup | Taganap A75 (2019) | k = 1 only; k ≥ 2 seeds re-enter covolume |
| free growth | KrotenheerdtSearch + corona-stabilization | 1D-stacking scatter ≡ covolume (ADR-0028) |
| oriented-slice / orbifold | ADR-0023 (this ADR) | low-symmetry minimal symbol ≈ covolume |

**Common cause:** a *low-symmetry* k-uniform tiling's combinatorial complexity (cell, minimal symbol, patch)
**is its covolume**, and there is **no symmetry to reduce it**. Covolume grows with k. This is almost
certainly why Galebach's exhaustive k ≤ 6 took ~1 month of 2002 compute and was never given an elegant
description — it is a **brute-force-the-covolume** result, not a clever-reduction one.

## Decision

An *elegant* original algorithm reaching n ≤ 7 in ≤ 1 week does **not** appear to exist — the covolume is
irreducible for the low-symmetry tilings. The only route that can produce a **complete** new count (n = 4) is
to **accept the covolume cost and brute-force it with parallelism** inside the week budget. The sound, exact
**bounded-V dart assembler** (ADR-0025) is built for exactly this. Next step (this session): a **feasibility
measurement** — push it on representative n = 4 buckets to get the *V-needed* and *cost-per-V* curve, and
extrapolate whether complete n = 4 fits a week at full parallelism. That single measurement decides whether
n = 4 is reachable at all; results recorded in a follow-up.

## Keepers

- ADR-0023 oriented-slice generator — best partial engine (validated; n=4 5/33, n=5 1/15).
- `DelaneySymbols` oracle (n ≤ 3 exact) + `minimalSymbol`/`canonicalKey`/`dualSymbol` — verify/key/dedup tail.
- `BucketAssembly` — sound, exact bounded-V engine; the brute-force candidate now under measurement.
