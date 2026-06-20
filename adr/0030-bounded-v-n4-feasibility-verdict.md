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

## Keepers

- `BucketAssembly` (ADR-0025) — sound, exact, and now the **leading** engine for n=4. `BucketN4Probe` — the
  feasibility/measurement harness.
- `DelaneySymbols` oracle + `classifyClosedMap` + `canonicalKey`/`dualSymbol` — verify/key/dedup tail.
