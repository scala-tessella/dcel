# ADR-0031: The complementary-engines union, and the both-walled n=4 tilings (~30/33 ceiling)

- **Status:** Measured (2026-06-21). Capstone of the n=4 investigation: combines the two surviving engines
  (bounded-V dart assembly, ADR-0025/0030; oriented-slice generator, ADR-0023) and locates precisely why a
  pure from-scratch enumeration cannot *complete* n=4.
- **Date:** 2026-06-21

## The two engines wall on DIFFERENT axes

| engine | reaches cheaply | walls on |
|--------|-----------------|----------|
| **bounded-V dart assembly** | small/moderate **torus cells** (V ≤ ~16) | **large cells**: cost ≈ `c^V` and the `seen` dedup set OOMs (V=19 ≈ 1.3 B entries ≈ 34 GB > 31 GB RAM) |
| **oriented-slice generator** | **small minimal symbols** (high symmetry) | **large minimal symbols** (low symmetry / big oriented double): tree ≈ 4×/+2, so oriSize 44 ≈ 10 h, 46 ≈ days |

These are *independent* axes. A tiling can be small-cell-but-low-symmetry (bounded-V yes, oriented-slice no),
large-cell-but-high-symmetry (oriented-slice yes, bounded-V no), or — fatally — **large-cell AND low-symmetry**
(both no).

## A correctness bug found en route (and the lesson)

While pinning the heavy n=4 cells, `BucketAssembly` was found to assemble **0 tori** for the oriented-chiral
vertex types `4.6.12` / `3.4.4.6` / `3.3.4.12`: `orientedAssignments` decided both-orientation placement with
the **bracelet** normal form (reflection-folding), wrongly calling them achiral, so a forward-only placement
had no antiparallel dart partner. Fixed with a **rotation-only** `isRotation`. It hid because the spec covered
only 6 of the 11 Archimedean — all achiral. Now all 11 reproduce key-for-key + a multi-type chiral regression
(`{3.4².6;3.6.3.6}`, multiplicity 2). **Lesson (recorded as feedback): test the complete space, not a
convenient subset; a fast deterministic test beats hours of probe runs.** This bug had invalidated an earlier
"closed=0 ⇒ covolume wall" reading — the heavy buckets *could not close* due to the bug, not a wall.

## Engineering: parallel, work-stealing, instrumented oriented generator

To push the oriented-slice deep, `BackTracker.parallelForeach` was made **work-stealing** (ForkJoin: each
node forks its child subtrees, idle workers steal the giant subtree's branches) — fixing a fixed-frontier
version that left one giant subtree grinding single-threaded in the tail. `orientedRegularSymbolsParallel`
adds a 15-s progress logger (dsets + rate + reg) so long runs report progress and ETAs self-calibrate.
Validated **result-identical** to the sequential generator (`DelaneySymbolsSpec`). oriSize 34: 283 s → 42 s
(6.7×); oriSize 42 ran 2.7 h at ~13 cores with no single-core tail.

## The coverage measurement (the decisive data)

Oriented-slice n=4 recovery vs A068600(4) = 33:

| oriSize | time | n=4 reached |
|--------:|-----:|------------:|
| 34 | 42 s (parallel) | 5/33 |
| 40 | 48 min | 12/33 |
| 42 | 2.7 h | **12/33 (plateau)** |

The 12 include **3 dodecagon (3.12.12-based) large-cell tilings** bounded-V walls on — confirming
complementary coverage — but the other 9 overlap bounded-V's reachable set, and **no `4.6.12`-based tiling
appears even at oriSize 42** (`4.6.12`'s oriented symbol is > 42 chambers).

## Verdict — the union does NOT complete n=4

```
union ≈ bounded-V (~25–28 small/moderate cells) + oriented-slice's 3 unique dodecagon large-cell tilings
      ≈ ~28–31 / 33
```

The **gap** is the handful of **`4.6.12`-mixed n=4 tilings** (Galebach #10 = `{3³.4²;3².6²;3.4².6;4.6.12}` and
siblings). These are **both-walled**: cell **V ≥ 19** (bounded-V OOMs) **and** oriented symbol **> 42**
(oriented-slice can't reach). So **~30/33 is the practical ceiling**; a *pure* from-scratch enumeration cannot
close them. This is the covolume/complexity wall (ADR-0029) in its hardest form — a tiling whose complexity is
irreducible on *both* the cell and the symmetry axis. (Galebach's per-tiling catalogue confirms these are real
tilings, not artifacts: probabilitysports.com/tilings.html?u=1&n=4&t=T.)

## Open options (deferred — user deciding)

1. **Accept ~30/33** — ship the union as a rigorous validated lower bound + documented wall.
2. **Hybrid for the hard core** — construct the ~2–5 both-walled tilings directly from Galebach's per-tiling
   seed data, build the torus map, verify + key via `classifyClosedMap`, and add to the union (completes n=4
   to 33, but uses the known answer for those few — not pure independent enumeration).
3. **Re-scope** the goal: a from-scratch algorithm to complete n ≤ 7 is not reachable for the hardest
   tilings; define the deliverable around what *is* (n ≤ 3 exact via the `DelaneySymbols` oracle, plus the
   validated multi-engine partial coverage for n ≥ 4).

## Keepers (all validated, parallel where it matters, tested)

- `BucketAssembly` — bounded-V, now chiral-correct + within-bucket parallel + bounded-memory; small/moderate
  cells. (ADR-0025/0030)
- `DelaneySymbols` oriented-slice generator — work-stealing parallel + instrumented; high-symmetry tilings.
- `DelaneySymbols` oracle (n ≤ 3 exact) + `minimalSymbol`/`canonicalKey`/`dualSymbol` — the shared verify/key
  /dedup tail; both engines dedup in the same key space, so any future union is clean.
