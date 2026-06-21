# ADR-0030: Bounded-V at n=4 is FEASIBLE — the covolume "wall" is port-constraint-dependent, not uniform

- **Status:** Measured (2026-06-21). The feasibility measurement called for in ADR-0029. **Overturns the prior
  pessimism** ([[project_fixed_lattice_engine]] / ADR-0025: "n=4–7 NOT reachable by bounded-V").
- **Date:** 2026-06-21

## What was measured

The sound, exact bounded-V dart assembler (`BucketAssembly`, ADR-0025) pushed on three representative n=4
type-sets spanning the symmetry range (`BucketN4Probe`), escalating the torus-cell vertex count V and recording
the per-V state cost (single-thread):

| bucket (n=4 type-set) | regime | cell V-needed | tree growth | observed cost |
|---|---|---|---|---|
| `{3⁶;3⁴.6;3².6²;6³}` | high-symmetry | **7** (2nd tiling at 8) | ~5×/+V (slowing) | found at V=7: 0.56M states / 13 s; V=8: 3.0M / 73 s |
| `{3⁶;3³.4²;3².4.3.4;4⁴}` | square-rich | **≥11** (V=10 exhausted empty) | ~4×/+V | V=10 full: 15.1M / 6 min; V=11 ≈ 75M (not completed) |
| `{3².4.12;3.4.3.12;3.4.6.4;4.6.12}` | dodecagon | **>24** | **~1.4×/+V** | whole V=4→24 sweep: 0.33M states / 69 s |

## The key finding — cost tracks PORT-CONSTRAINT TIGHTNESS, not covolume

The bounded-V cost is **not** uniformly `c^covolume`. With the three ADR-0025 prunes (ordered antiparallel
port, partial-map canonical dedup, fail-fast face closure) it splits into regimes by how tightly the vertex
types constrain the dart-port matching:

- **Dodecagons over-constrain** → **large** cells (V > 24) but **tiny, slow-growing** trees (~1.4×/+V): a
  V=24 sweep is 0.33M states / 69 s. Large cell, **cheap**.
- **Squares under-constrain** (flexible 90° ports) → **moderate** cells (V ≈ 11–12) but **fast** trees
  (~4×/+V): the **expensive** regime, ~75M states at V=11 (~30 min single-thread).
- **High-symmetry** (triangle/hex) → **small** cells (V = 7–8), found in seconds.

Crucially, **no n=4 bucket combines large-V with a fast tree** — the two cost factors are anti-correlated
(tight ports ⇒ big cell but small tree; loose ports ⇒ small cell but big tree). So the worst case is bounded:
the square-rich V≈11–12 regime at ~10⁸ states, **not** the unbounded `c^covolume` the earlier analysis feared.
The large dodecagon cells — the things the covolume wall pointed at — are the **cheapest**.

## Verdict — complete n=4 is reachable

- Worst observed per-bucket cost ≈ **75–375M states** (square-rich V=11–12) ≈ 30 min – 2.5 h single-thread;
  most buckets far cheaper (high-symmetry seconds, dodecagon minutes).
- ~33 tilings across the distinct n=4 type-sets ⇒ total single-thread is hours-to-a-day worst case, and the
  dart-matching DFS is **embarrassingly parallel** (the same work-stealing structure `KrotenheerdtSearch`
  already uses), plus the ~40k states/s rate is dominated by canonical partial-key **string hashing**
  (a ~10× per-state optimisation target). With both, complete n=4 is **well inside the 1-week budget**.
- **Completeness is certifiable** by the project's validation principle: run every n=4 type-set bucket,
  escalating V, until the sound + deduped total equals **A068600(4) = 33** ⇒ the exact set. Cross-check
  configs against the oriented-slice partials and the ANTWERP library.

## Risks / open questions

- **Worst-case V-needed not pinned**: the square-rich V=11 run was stopped before completing; the cell is
  V≥11, possibly 12–13. At ~4×/+V, V=13 ≈ 1.6B states ≈ 11 h single-thread/bucket — still within a week with
  parallelism + the keying optimisation, but tighter. Pin it early.
- **Stopping rule**: per-bucket "escalate V until found" needs an upper V bound; the count-match to 33 is the
  global oracle, but a shortfall can't distinguish "need higher V" from a bug. Mitigate by cross-checking the
  per-bucket finds against oriented-slice / ANTWERP.
- **n=5–7 not yet assessed**: cells grow but counts shrink (15,10,7). The dodecagon-cheap finding is
  encouraging for large cells; the danger is any square-rich V≥14 cell. Assess after n=4 lands.

## Decision

Build the **n=4 completion driver** on `BucketAssembly`: enumerate the distinct n=4 type-sets, run each with
escalating V (parallel across buckets and within), dedup by canonical key, and stop when the sound total = 33.
Optimise the per-state keying first (the measured bottleneck). This is the concrete path to the **first new
exact count beyond the n≤3 oracle**.

## Build status (2026-06-21) — driver built; first n=4 pass corrects the cost estimate

Built `BucketAssembly.enumerateBuckets` (parallel multi-bucket fan-out + global canonical-key dedup,
`DriverResult`) + `N4DriverProbe` (Wikipedia compact-notation parser → 21 distinct n=4 type-sets) +
`BucketDriverSpec` (parser + fan-out/dedup guards). Two optimisations to the validated `Assembler`, both
behaviour-preserving (all 15 BucketAssembly/Driver tests green):
1. **Keying — 2.1×.** Reused scratch buffers + int-array labelling stamp eliminate the per-state HashMap /
   StringBuilder / Stack / Array allocation (was GC-bound). tri/hex V=8 identical 2.96M states, 73 s → 35 s.
2. **Memory — `LongFpSet`.** The per-bucket `seen` dedup stored full canonical strings and OOM'd at scale.
   Replaced with an open-addressing 128-bit fingerprint set (~24 B/entry, no boxing, no retained strings;
   collision ~10⁻²³).

**First n=4 pass (24 GB heap, parallel=3, 90M budget) corrects the §verdict's optimism:** the cost-driver
buckets are worse than V≈11. The hexagon/square-mix sets containing `{3.3.6.6, 3.4.4.6}` (e.g.
`{3³.4²;3².6²;3.4².6;4.6.12}`) budget-hit at 90M with **found=0** — their cells are **V≥12 (~375M states,
~9 GB fingerprint memory each)**, the flexible-port (fast-tree) regime at a larger cell than tri/sq. At a
memory-bound parallel=3 these dominate wall-clock (~18 min each at 90M), so a flat parallel-across-buckets
pass churns ~2 h and still leaves that tail short.

**Path to complete n=4 — WITHIN-bucket parallelism (BUILT 2026-06-21).** Parallelised inside a bucket across
its independent oriented **assignments** (a V-layer has hundreds), sharing only a thread-safe striped
fingerprint set (`LongFpSet`) and a shared `Budget` (atomic spent + volatile hit, batched per 1024 states);
buckets now run **sequentially** so one fingerprint set is live ⇒ bounded memory (a 500M-state bucket ≈ 12 GB
fits the 24 GB heap). Result-invariant (tests: `LongFpSet` dedup/resize/8-thread-concurrent, `Budget` latch,
parallelism=1-vs-8 same keys). **Speedup measured: only ~2.4× — load-imbalance-limited** (one assignment
dominates a layer; finer within-assignment DFS splitting would scale better but is a larger change). Still
enough for a few-hour full n=4 run.

Remaining: escalate the V≥12 (3.3.6.6/3.4.4.6-rich) buckets to confirm found-vs-truly-empty (some Wikipedia
rows may be artifacts), and run all `C(15,4)` subsets (not just the 21 Wikipedia sets) for
reference-independent completeness.

## FINAL n=4 VERDICT (2026-06-21, after the chirality fix + cell-V pin)

Two corrections landed:
1. **Chirality BUG (fixed).** `orientedAssignments` used the bracelet (reflection-folding) normal form to
   decide both-orientation placement, so oriented-chiral types (`4.6.12`, `3.4.4.6`, `3.3.4.12`) were placed
   forward-only and assembled **0 tori**. Fixed (`isRotation`, rotation-only); all 11 Archimedean now
   reproduce key-for-key + a multi-type chiral bucket (`{3.4².6;3.6.3.6}`) gives its multiplicity 2. **24
   tests.** This invalidated the earlier "closed=0 ⇒ V>20 wall" reading.
2. **Cell-V PIN (the real wall).** With the fix, the heavy `{3³.4²;3².6²;3.4².6;4.6.12}` bucket (Galebach n=4
   #10) was searched exhaustively through **V=18 (568M states, closed=0)** ⇒ its cell is **V≥19**. And V=19 is
   the memory ceiling: the `seen` dedup set ≈ 1.3B entries ≈ 34 GB > 31 GB RAM. So the cell is genuinely large
   (sparse dodecagons) AND beyond exhaustive bounded-V reach.

**⇒ bounded-V reaches n=4 only PARTIALLY** (~25–28/33, the cells ≤ V16); the `4.6.12`/dodecagon-mixed buckets
(cells V≥19) are the real covolume wall (ADR-0029), now correctly attributed.

**Complementary-engines hypothesis (the promising lever).** bounded-V walls on LARGE CELLS; the oriented-slice
generator (ADR-0023) walls on LOW SYMMETRY (large minimal symbol). These are *different* axes — a
large-cell-but-HIGH-symmetry tiling (4.6.12-mixed is typically p6m) has a SMALL minimal symbol, so it should be
CHEAP for oriented-slice. So the two engines may have **complementary coverage that together spans n=4**. Next:
test whether the oriented-slice generator reaches exactly the large-cell buckets bounded-V misses.

## Keepers

- `BucketAssembly` (ADR-0025) — sound, exact, and now the **leading** engine for n=4. `BucketN4Probe` — the
  feasibility/measurement harness.
- `DelaneySymbols` oracle + `classifyClosedMap` + `canonicalKey`/`dualSymbol` — verify/key/dedup tail.
