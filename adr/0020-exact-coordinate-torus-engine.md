# ADR-0020: Exact-coordinate torus engine (the ADR-0019 "different search")

- **Status:** Accepted — a third enumeration engine, **key-equivalent to the DCEL fixed-Λ engine on every
  tested case and ~3–4× faster** in its default **vertex-completion constraint-propagation** mode; kept
  alongside the DCEL engine, which remains the cross-check reference. **Characterized limit (see below): cost is
  exponential in covolume, so n = 4–7 and the dodecagon cells are out of reach by this approach — superseded for
  those by the direct combinatorial enumeration of ADR-0021.**
- **Date:** 2026-06-18 (propagation mode same day; scaling work, combined pass, and the covolume-wall
  characterization 2026-06-19)

> Implemented in `generator/.../ZetaPoint.scala` and `KrotenheerdtTorusSearch.scala`, validated by
> `ZetaPointSpec`, `KrotenheerdtTorusSearchSpec`, and the head-to-head `TorusTimingProbe`. Reuses the DCEL
> engine's verification tail, extracted as `KrotenheerdtLatticeSearch.verifyContent`.

## Context

ADR-0019 profiled the DCEL fixed-Λ engine ([[KrotenheerdtLatticeSearch]]) as **~45 % canonical-congruence
key + ~37 % immutable-DCEL deep-copy** per state, and concluded both are irreducible *within that model*: a
real speedup needs **a different search — one that generates each patch once (no dedup key) or enumerates the
torus-cell contents directly** — not a per-state tweak. This ADR is that different search (Option B), built as
a prototype to decide empirically whether the idea pays off.

## Decision — represent the patch in exact integer ℤ[ζ₁₂] coordinates, grow on the torus

Every vertex of a `{3,4,6,12}` tiling lies in the cyclotomic module ℤ[ζ₁₂], ζ = e^{iπ/6} (edge directions are
multiples of 30°). Represent each vertex as an exact integer 4-tuple `a₀ + a₁ζ + a₂ζ² + a₃ζ³` (relation
`ζ⁴ = ζ² − 1` keeps degree ≤ 3; [[ZetaPoint]]). A unit edge is integer multiplication by a power of ζ. This
single change removes **both** ADR-0019 costs:

- **No deep-copy.** A patch is a cheap immutable `List[FaceZ]` of unit polygons; growth prepends one polygon
  built by integer ζ-steps. (The DCEL's overlap / coincident-vertex / enclosed-region bookkeeping is recovered
  for free: coincident vertices are *equal residues*, and "overlap" is *a 30°-slot already owned* — i.e. the
  Λ-consistency oracle, now on exact integers.)
- **No trig congruence key.** Λ is fixed and oriented, so two growth orders that reach the same patch reach the
  *same integer point set*. The dedup key is the face-set canonicalised over **Λ's point group + translation**,
  computed in small integers (`canonicalKey`): for each lattice automorphism `g` (a module isometry —
  rotation `ζ^k` and reflection `conjugate` — that maps Λ to itself, tested by exact integer congruence),
  transform, translation-anchor on the lex-min corner, and take the min serialization. This is the trig-free
  replacement for the DCEL `congruenceKey` (ADR-0019: 45 %, fragile, un-substitutable in Double/exact-trig
  variants). Canonicalising over the **Λ-automorphism subgroup only** (not the full module group) is what keeps
  it sound — it never merges patches with different Λ-continuations.

Candidate lattices are enumerated as exact integer pairs over the L1 step-budget `Σ|aᵢ| ≤ k` (the exact
analogue of the DCEL `candidateBases`), sign-canonicalised and deduped. Orientation is swept by the candidate
set (which holds each lattice's rotated copies), with a single slot-0 seed per polygon — the same convention as
the DCEL engine, so the completeness profile in `k` is identical. Per-Λ growth is gated by the exact
Λ-consistency slot check; a completed cell (distinct faces tile one covolume) is handed to the shared
`verifyContent`.

## Verification is shared, so soundness is inherited

The completed-cell gate is the DCEL engine's proven tail, extracted verbatim as `verifyContent`: primitive-
lattice re-keying, the EXACT vertex-orbit count (not 1-WL — ADR-0019's central soundness fix), and the
up-to-all-isometries canonical key. The torus engine reconstructs each torus vertex's fan by unioning its
planar instances' corners keyed by angle (as `verifyTorus` does), then calls `verifyContent`. So a tiling the
torus engine reports is sound by exactly the ADR-0019 argument; only the *generation* of candidate cells
changed.

One bug surfaced and was fixed during validation, improving **both** engines: `primitiveBasis` derived its
sublattice periods only from same-size face-centroid differences, so a **single-face sublattice cell** (a
doubled `4⁴` — one square but two vertices) had no pair to reduce by and was reported as a distinct tiling.
The fix derives periods from same-type **vertex** positions as well (and requires a period to preserve the
vertex content), which is strictly more correct.

## Two growth strategies on the exact foundation

The engine has two interchangeable growth strategies (the `completion` flag), both sound and complete, both
key-equivalent to the DCEL engine; they differ only in how candidate cells are generated:

- **One-polygon growth** (`completion = false`) — adds a single polygon to the centroid-nearest boundary gap,
  the direct port of the DCEL engine's growth onto exact coordinates. Removes the deep-copy and trig key, but
  inherits a DCEL-like *state count*.
- **Vertex-completion constraint propagation** (`completion = true`, the default) — the Galebach-scale search.
  Seeds a **whole vertex** (a full corona of a valid vertex type, both chiralities — `seedTypes` /
  `coronaFaces`), then repeatedly commits the **most-constrained** incomplete vertex (MRV: fewest valid
  `completions` of its forced partial fan) all at once, branching only over those completions. A vertex with a
  single completion is forced (no branch); one with none prunes the branch. Most of a cell is *forced*, not
  *branched*, so the state count drops below even the DCEL engine's. Corona seeding is what makes it pay: it
  constrains the corona's outer vertices immediately (and a 1-uniform cell often verifies at the seed itself),
  removing the up-front branching explosion a single-polygon seed leaves on the first, unconstrained vertex.

## Results (validated, `TorusTimingProbe`, single 8-core host, parallelism 4)

Key sets are **identical** across all three engines (octagon-free) on every tested case — n=1 k=3, n=1 k=4,
n=2 k=4.

| case | **PROP** ∥4 | 1-poly ∥4 | DCEL ∥4 | PROP vs DCEL | PROP / 1-poly / DCEL states |
|------|------------:|----------:|--------:|-------------:|-----------------------------|
| n=1 k=3 | 0.9 s | 0.3 s | 1.2 s | 1.3× | 2 590 / 3 475 / 5 346 |
| n=1 k=4 | **17.6 s** | 34 s | 48.8 s | **2.8×** | 184 841 / 421 365 / 263 044 |
| n=2 k=4 | **6.8 s** | 18 s | 28.3 s | **4.2×** | 87 257 / 192 480 / 122 282 |

The propagation engine explores **fewer states than even the DCEL engine** (n=2 k=4: 87 k vs 122 k — and 2.2×
fewer than one-polygon growth), on top of the exact engine's **~3× cheaper per state** (no key, no copy) and
**smaller candidate set** (n=2 k=4: 1 925 vs 1 947 bases). Net: **~3–4× faster than the DCEL engine** at the
heavier cases — for scale, ADR-0019 clocked n=2 k=4 at ~290 s single-thread; this is 6.8 s. The octagon's
`4.8.8` (45°, ℤ[ζ₂₄]) is out of scope for this engine, exactly as it is past the DCEL engine's current `k`.

## Scaling toward the published counts (n ≥ 2)

Reaching the full A068600 counts needs `(k, maxCovolume)` large enough for the largest (dodecagon) cells, and
that exposes a different cost than the per-state one above: the *number* of candidate lattices (~k⁴) and the
per-Λ work at high covolume. Four changes make the production runs tractable (`KrotenheerdtTorusApp` is the
runner; `krot.growcells` / `krot.facecap` tune the growth bound):

- **Candidate enumeration in Double.** The O(points²) covolume filter and Gauss reduction run in `Double` with
  an O(1) achievable-covolume lookup, so enumerating ~37 k bases at k=6 takes ~0.2 s (was minutes in
  BigDecimal). Exact integer ℤ[ζ₁₂] bases are still what the search consumes.
- **Early type-count prune.** `isSound` drops a branch the moment its *completed* vertices show more than `n`
  distinct types — an n-uniform tiling can never contain more, so near-miss growth dies long before the verify
  horizon.
- **Verify at the patch's primitive period, not at Λ** (`classify`). The dominant high-covolume cost is
  *sublattice fields*: a tiling periodic with a small cell is also periodic with any coarser candidate Λ, so the
  engine would re-grow the full big Λ-cell (e.g. a covol-28 candidate = a 64-triangle 3⁶ field) only for
  `primitiveBasis` to collapse it at verify. Instead every grown patch is classified against its *own* primitive
  period (read from the face content via the shared `primitiveBasis` with empty verts): a finished sub-tiling is
  emitted (n-uniform — deduped against its primitive-lattice candidate) or pruned (wrong count), and only a
  genuinely half-built cell — recognised by having an *incomplete boundary fan* under that period — keeps
  growing. Sound, and it cuts the n=1 k=4 state count 2.6× (185 k → 72 k) with identical keys.
- **Tighter, n-scaled growth bound.** A branch grows only to `~(n+2)` cells of area (an n-uniform cell verifies
  once its ~n orbits each have a reconstructable fan), not the earlier ×6.
- **Crash-safety: per-lattice state cap + halved default parallelism.** A pathological near-miss lattice can
  explore *millions* of states, each adding a key to its `visited` set; an uncapped full-core run searched 16
  such lattices at once and **exhausted the host's memory (OOM crash)**. `krot.percap` (default 100 000) aborts
  any lattice that exceeds it — a real cell resolves in far fewer states, so this bounds memory and time per
  lattice while only ever dropping pathological (spurious) lattices. Capped lattices are **counted and warned**
  ("completeness caveat"), like the `(k, maxCovolume)` bound. The runner also defaults to **half the cores**
  (override explicitly), so a run leaves the machine usable.

**Calibration result (n = 2, ∥16).** The DCEL fixed-Λ engine (ADR-0019) reached **16 of 20**. The torus engine
at `k=6, maxCovolume=28` reaches **18 of 20** — the two it adds are genuine covol≈27–28 cells (the 16→18 jump
is in the top covolume band). The final two (the largest `3.12.12` / `4.6.12` pairs) have basis vectors past the
`k=6` L1 budget and need `k=7`. So the completeness law is `(k, maxCovolume)` exactly as ADR-0019 posited, and
the engine extends it further than the DCEL engine did, at the same soundness bar (every tiling cross-checks to
a genuine Krotenheerdt tiling; the n=1/n=2-small key sets match the DCEL engine).

## Consequences

- **Positive.** A fully **exact-integer** engine (no Double/√3 in the search; the rounding fragility ADR-0019
  warned about is gone), parallel, key-equivalent on every tested case, with a smaller candidate set, and —
  in propagation mode — **~3–4× faster than the DCEL engine with fewer states than it**. The sublattice fix
  hardened the shared verification for both engines. This is the realised "different search" ADR-0019 asked
  for, and the right engine to push toward the higher-n counts.
- **Remaining levers for n = 4–7.** Per-Λ cost is now small; the wall is again the **number of candidate
  lattices** (and the `k` needed to reach the largest dodecagon cells). The next gains are candidate-set
  reduction (sound this time, since orientation is no longer seed-bound the same way) and the standard
  constraint-search refinements (forward-checking / a visited set tuned to the propagation order — much of
  the residual state count is still re-derived partial cells).
- **Scope unchanged from ADR-0019.** Completeness still rests on `(k, maxCovolume)`, verified per n by
  reproducing the published count. The library is untouched; `KrotenheerdtSearch` (ADR-0018) remains the
  rigorous n ≤ 2 reference and `KrotenheerdtLatticeSearch` (ADR-0019) the validated fixed-Λ cross-check.

## The characterized limit: cost is exponential in covolume (the wall for n ≥ 4)

After the optimisation work below (Double candidates, type prune, primitive-period `classify`, compact key,
crash-safety cap, the combined all-n pass), the engine is sound, crash-safe, ~2× faster, and independently
reproduces the published counts **up to a covolume ceiling** — but it cannot reach the full table. Per-checkpoint
timing of a combined sweep (candidate lattices are covolume-sorted) makes the wall explicit:

| covolume | time / 200 lattices | states / lattice |
|---------:|--------------------:|-----------------:|
| 4.0 | 0.3 s | ~49 |
| 6.5 | 3.3 s | ~140 |
| 7.6 | 8.8 s | ~260 |
| 8.0 | 11.6 s | ~376 |

Per-lattice cost grows **~2.5× per +1 covolume** — *exponential in covolume*, the compound of two effects:
**states/lattice ~1.7×/covol** (a near-miss / spurious lattice grows a search tree that is exponential in patch
size, and the cell — hence patch — grows with covolume) **× per-state cost ~1.5×/covol** (bigger patches → bigger
`primitiveBasis` / key work). The type prune and `classify` cut the *constant*, not the exponential.

**Consequence:** extrapolating, around **covol ≈ 17** lattices begin hitting the `krot.percap` cap (losing
completeness there) and each costs minutes. The full A068600 table needs exactly the high-covolume cells past
this wall — the dodecagon cells (`3.12.12`, `4.6.12`, covol ~12–24) and **all of n = 4–7**, whose cells are
larger still. So **n = 4–7 (and the dodecagon parts of n = 2, 3) are not reachable in days by this fixed-Λ
approach.** What *is* feasible is the lower-covolume cells of each n — most of n ≤ 3.

The root cause is structural: the engine grows **planar patches under candidate lattices**, and the vast majority
of candidates are spurious. Reaching n = 4–7 needs a method that enumerates the finite **torus quotient directly**
— a combinatorial (Delaney–Dress-style) enumeration with no covolume-exponential and no spurious-lattice growth.
That is the subject of **ADR-0021**.

## Pitfalls hit and fixed (do not repeat)

- Enumerating candidate module points over the integer **box** `|aᵢ| ≤ k` instead of the L1 **budget**
  `Σ|aᵢ| ≤ k` blows the candidate count up by ~k⁴ extra points and hangs the BigDecimal pairing. Cache each
  point's `BigPoint` embedding.
- Seeding all 12 edge orientations (instead of slot-0 only) ×12s the state count: with the absolute key it
  spawns congruent copies that never merge (the ADR-0019 "absolute key inflates states" effect). Seed slot-0
  and let the candidate set sweep orientation.
- The dedup key must canonicalise over Λ's **automorphism subgroup**, not the full 24-element module group —
  the latter would merge patches with different Λ-continuations (unsound).
