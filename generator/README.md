# `generator` — Krotenheerdt tiling enumeration (OEIS A068600)

A research-grade subproject that independently confirms and replicates
[OEIS A068600](https://oeis.org/A068600): the number of *n*-uniform edge-to-edge
regular-polygon tilings with exactly *n* distinct vertex types
(11, 20, 39, 33, 15, 10, 7 for *n* = 1…7, then 0).

It enumerates tilings from scratch over the polygon alphabet `{3, 4, 6, 8, 12}`,
**certifies** each as a genuine infinite periodic *n*-uniform tiling, and counts
them up to isometry. The design, certification chain, and assumptions are in
[ADR-0018](../adr/0018-krotenheerdt-enumeration.md).

## Pieces

| File | Role |
|------|------|
| `VertexTypes` | the 15 valid vertex types over `{3,4,6,8,12}` and the partial-fan pruning table |
| `PatchCanonical` | reflection-invariant congruence key for sound search-state dedup |
| `KrotenheerdtSearch` | canonical-growth DFS with a work-stealing parallel scheduler |
| `TilingCertifier` | patch → certified infinite tiling (lattice → parallelogon block → witness cell → orbit count), plus the canonical `torusKey` |
| `KrotenheerdtApp` | `runMain` driver: runs a search and persists each tiling as SVG + metadata + an index line |
| `LatticeConsistency` | Λ-consistency oracle for the second (fixed-Λ) engine — see below |
| `KrotenheerdtLatticeSearch` | fixed-Λ toroidal engine (WIP, ADR-0019) — the scalable replacement for `KrotenheerdtSearch` |

The certifier builds on the library's lattice/parallelogon machinery
(`TilingLattice`, ADR-0015) and uses `TilingLattice.validatedPeriods`.

### Two engines

`KrotenheerdtSearch` (the **reference** engine, ADR-0018) grows patches freely and certifies horizon
survivors. It rigorously enumerated **n ≤ 2** but does not scale (aperiodic "scatter" dominates at n ≥ 3).

`KrotenheerdtLatticeSearch` (ADR-0019) is the scalable replacement: it enumerates candidate translation
lattices Λ and, for each fixed Λ, grows only Λ-consistent patches, verifying each one-cell patch directly
on the torus (`verifyTorus`) — no scatter is ever built. It is **sound**: every tiling it reports is a
genuine Krotenheerdt tiling with the correct multiplicity (multi-vertex cells, sublattice/chirality
deduplication, and an **exact vertex-orbit count** that replaced the unsound 1-WL colour refinement).
Validated with no false positives against n=1 (10 of 11) and n=2 (16 of 20); the few missing are the
largest dodecagon cells, reached only at a larger step bound `k`. **Completeness is bounded by the
empirical `(k, maxCovolume)`, so the full published counts below still come from `KrotenheerdtSearch`.**
See ADR-0019 for the design, the verification, and the lessons from the approaches that failed.

## Running

```
sbt "generator/runMain io.github.scala_tessella.dcel.KrotenheerdtApp <n> <maxVertices> \
     [outDir] [parallelism] [hardCapFactor] [earlyTypeGate] [typeBallRadius]"
```

For example, the n = 2 replication:

```
sbt "generator/runMain io.github.scala_tessella.dcel.KrotenheerdtApp 2 80 generator/results/n2 12"
```

Artifacts land in `outDir`: `tiling-<key>.svg` (render), `tiling-<key>.xml`
(round-trippable metadata), and `index.tsv` (composition, torus key, basis,
certifying patch size) — written incrementally, so an interrupted run still
leaves usable output.

## Results

| n | A068600 | Replicated | Runtime |
|---|---------|------------|---------|
| 1 | 11 | **11** — the Archimedean tilings, incl. `4.8.8`, `6.6.6`, `3.12.12`, `4.6.12` | ~45 s |
| 2 | 20 | **20** — composition multiset matches the published table | multi-hour (12-way) |
| 3 | 39 | not yet run | — |

The n = 2 result reproduces the published 2-uniform table exactly: five vertex-type
compositions occurring twice and ten once. The chiral `3.12.12 + 3.4.3.12` is counted
once (mirror images identified, per the up-to-all-isometries convention); the five
doubled compositions are pairs of genuinely distinct tilings, kept apart by their
torus keys.

## Tests

`KrotenheerdtSpec` runs in CI:

- the 15 vertex types and partial-fan pruning;
- congruence-key invariance under translation/rotation/reflection;
- **n = 1 → exactly the 11 Archimedean tilings**;
- the torus-key identity on the discriminating n = 2 compositions (a genuine
  same-composition pair → two keys; the chiral tiling → one key).

The full n = 2 enumeration is a multi-hour run via `KrotenheerdtApp`, not a unit test.
