# ADR-0025: Bucketed assembly — enumerate by vertex-type-set × bounded fundamental domain

- **Status:** Proposed — the new plan after re-examining ADRs 0019–0024. Replaces "grow a patch and hope it
  closes" with a BOUNDED, finite assembly per bucket.
- **Date:** 2026-06-20

## Re-examination — the one root cause behind every dead end

| approach | wall | mechanism |
|----------|------|-----------|
| fixed-Λ sweep (0019/0020) | covolume-exponential | ~37k candidate lattices, mostly spurious; each spurious one grows a near-miss patch exponential in covolume |
| DelaneySymbols generate-all (0022) | explodes past ~16 chambers | enumerates EVERY 2-manifold D-set (the hyperbolic universe), filters euclidean+regular afterwards |
| geometric early-gluing (0023) | pre-Λ growth scatter | grows the planar patch UNBOUNDED before the deck lattice is known; aperiodic-trending branches dominate |
| isocoronal prune (0024) | no-op | the "≤ n distinct coronas" invariant only exists on COMPLETED rings — it fires after the scatter, not before |

The common failure is **an unbounded or super-set search**. The fixed-Λ engine is the telling exception: with Λ
FIXED, growth is bounded to one cell and there is NO scatter — its only fault was trying too many lattices,
each in isolation, blind to the others. So the cure is: **make the search bounded and finite per unit of work,
and make the units few.**

## Key structural facts established this session

- The objects are TINY: fundamental cells are 3–8 faces; minimal D-symbols are ≤ ~24 chambers at n = 3,
  ≈ 45–60 at n = 7. The whole A068600 set (n = 1–7) is 135 tilings.
- There are only **15 valid vertex types** over `{3,4,6,8,12}`. An n-uniform tiling uses exactly `n` of them.
- The minimal D-symbol is a complete, canonical, coordinate-free invariant (sound; gives exact dedup and the
  A068600 condition `k orbits = k types`). `DelaneySymbols` already computes it and is the validated n ≤ 3
  oracle.

## Decision — bucketed assembly

Stop growing patches. Instead enumerate by **buckets** and, inside each, solve a BOUNDED, finite assembly:

```
for k in 1..7:
  for T in feasible k-subsets of the 15 vertex types:        # the OUTER loop — small and prunable
    for V in k .. Vmax(k):                                   # fundamental-domain vertex count, bounded
      enumerate torus maps with exactly V vertices, each typed from T, all edges port-matched
      keep those whose minimal D-symbol has exactly k vertex orbits and k distinct types
```

Why this escapes the wall:
- **Bounded, not grown.** Each inner problem is "assemble V typed vertices into a torus", a FINITE CSP — never
  the unbounded "grow until it maybe closes". Closure (the torus) is a *constraint of the assembly*, not a
  hoped-for outcome of growth. This is the fixed-Λ engine's scatter-freedom without fixing Λ.
- **The units are few.** `Σ_k C(15,k)` ≈ 18k buckets, and most die immediately to local port-consistency
  (every port of every type in `T` must be matchable within `T`). The survivors are tiny.
- **Native constraints.** Only the `k` types of `T` are placed, every edge is port-matched, every face is a
  regular `{3,4,6,8,12}`-gon by construction — the regular-polygon + valid-vertex constraints are intrinsic,
  not filters. No hyperbolic universe, no spurious lattices, no aperiodic scatter to prune.

## Enablers / components

1. **Port table (precomputed, small).** Each of the 15 vertex types is a cyclic polygon sequence; each of its
   edges is a port labelled by the ordered pair of polygons it separates. Type `t1` may be adjacent to `t2`
   across an edge iff a port of `t1` matches the reverse port of `t2`. A finite relation, computed once.
2. **Feasible-bucket prune.** Drop `T` unless every port of every type in `T` is matched by some type in `T`
   (necessary for a tiling using only `T`). Cheap, kills most buckets.
3. **Inner assembly = build the combinatorial map (darts / half-edges).** Place typed vertices, match ports
   to form edges, let faces fall out of the dart cycles, identify the boundary into a torus. Intrinsic
   (no coordinates ⇒ sound by construction, no overlap test). `Vmax(k)` bounds the domain (a completeness
   caveat, like the chamber/covolume caps, but the bound is tiny).
4. **Verify / key / dedup = reuse `DelaneySymbols`.** Minimal-image D-symbol = canonical key (dedup across
   buckets and assembly orders) + the `k orbits = k types` test. The geometric `verifyCell` stays available as
   an independent cross-check at small n.

## De-risking spike — first pass (restricted-growth proxy) DONE: NEGATIVE but decisive

A cheap proxy was built first: `enumerateBucket` = the early-gluing grower restricted to a single type-set
`T`. Result on three known 2-uniform buckets: state counts **851 / 405 / 854** — i.e. NO reduction vs the
unrestricted n=2 search (~856), and one bucket even leaked a `{3⁶,3³.4²}` tiling into `{3³.4²,4⁴}` (the 3⁶
vertices appear only at torus-closure, not during planar growth).

**Lesson (decisive):** the scatter is INHERENT to growing partial patches — it is *independent of the type
constraint*. No restriction on a *growth* search reduces it. So the bucketed plan only works with the **actual
bounded-V assembly** (enumerate ONLY complete `V`-vertex tori, never partial patches) — which the proxy did
not build. The restricted-growth realization is rejected; the bounded-V assembly is unbuilt and is the real
test.

## The real spike: bounded-V dart assembly — BUILT (2026-06-20). Verdict: SOUND & CORRECT, but not yet small.

Built as `BucketAssembly` (+ a `DelaneySymbols` bridge: `closedMapSymbol` / `minimalSymbol` / `canonicalKey` /
`classifyClosedMap` / `keyedTilings`, validated by `BucketAssemblySpec`). The assembler lays out `V` typed
vertices as darts, enumerates the **port-matched perfect matchings** of those darts (edge involution `α`; faces
fall out as the cycles of `φ = σ∘α`), and hands each CLOSED connected genus-1 map to the oracle: bridged to its
`v = 1` Delaney symbol, reduced to its minimal symbol, keyed canonically. Only complete-torus space is searched
— no partial patches.

**Two of the three risks are decisively retired:**
- **Soundness (PASS).** The angle-valid non-tilings `3.3.6.6` and `3.4.4.6` assemble into ZERO tori — the
  ADR-0022 win, now combinatorial, no overlap test. (`3.4.4.6` is killed at the port stage: 0 states.)
- **Assembly correctness / identity (PASS).** Square, triangular, hexagonal, the octagon **`4.8.8`** (which the
  ζ engines cannot represent), `3.6.3.6` and `3.4.6.4` all reproduce with minimal-symbol canonical keys that
  match the `DelaneySymbols` oracle **key-for-key**; the 2-uniform `{4⁴; 3³.4²}` bucket yields its correct
  **multiplicity of 2** distinct tilings. The dart bookkeeping, torus closure, and orbit counting are right.

**The size risk (#1) MATERIALIZED — the raw enumeration is not small.** The decisive prune is the *ordered
antiparallel* port match (`after(g)==before(h) && before(g)==after(h)`, validated correct by key-equality); it
cut `3.6.3.6` 44× (4.9M → 111k states). But ports do nothing for single-port, triangle-rich vertices (every
`3⁶` / `3.6.3.6` dart has the same port), so the perfect-matching enumeration still scatters:

| bucket | minimal cell | states (maxV) | distinct tilings |
|--------|--------------|---------------|------------------|
| `4.4.4.4` | tiny | 258 (V≤2) | 1 |
| `6.6.6` | tiny | 35 (V≤3) | 1 |
| `4.8.8` | small | 270 (V≤4) | 1 |
| `3.6.3.6` | medium | 1.1e5 (V≤4) | 1 |
| `3.4.6.4` | large | 2.0e6 (V≤6) | 1 |
| `{4⁴; 3³.4²}` | small | 2.2e4 (V≤4) | 2 |
| `{3⁶; 3⁴.6}` | — | >5e6, **over budget** | (0 found) |

So vs the patch-growth ~850 for a 2-uniform bucket, the bounded-V assembler is **worse** (2e4–5e6): it trades
growth-scatter for **matching-scatter**. Note `mapsClosed` ≫ distinct tilings (e.g. `3.6.3.6`: 34 502 closed
maps → 1 tiling), so the waste is **isomorphic partial matchings**, not genuine candidates.

**Consequence — the ADR's "few assemblies per V" needs more than ports.** Bounding `V` removes the
aperiodic/covolume scatter as promised, but raw port-matched perfect-matching introduces its own combinatorial
blow-up, dominated by **isomorphic partial matchings**.

## Partial-map canonical dedup — BUILT (2026-06-20). The bucketed plan is ALIVE for fitting cells.

Added to `BucketAssembly`: at each search node, prune the partial matching unless its orientation-preserving
canonical form (a sorted multiset of per-connected-component BFS-min strings, reflection excluded so chiral
enantiomorphs are not merged) is new in a GLOBAL `seen` set. Sound for completeness — an isomorphism maps
unmatched darts to unmatched darts, so every completion is reached via the first-seen representative; verified
by the oracle key-equality tests still passing. Effect (correctness unchanged — keys still match the oracle,
`{4⁴;3³.4²}` still multiplicity 2, soundness still holds):

| bucket | states before | states after | factor |
|--------|---------------|--------------|--------|
| `3⁶` (V≤2) | 2.5e4 | 1.9e3 | 13× |
| `3.6.3.6` (V≤4) | 1.1e5 | 2.0e3 | 57× |
| `3.4.6.4` (V≤6) | 2.0e6 | 1.6e4 | 120× |
| `{4⁴; 3³.4²}` (V≤4) | 2.2e4 | **1.0e3** | 22× |

The decisive number: a real 2-uniform bucket now assembles in **~1 000 states — on par with patch-growth's
~850, but SOUND and exactly identified** (the two distinct tilings, by canonical key). So bounded-V dart
assembly + partial-map dedup is a *viable* engine for buckets whose minimal cell fits the `V` window — it does
what growth could not (soundness + exact identity) at comparable cost.

**Still open — the large-cell wall.** `{3⁶; 3⁴.6}` burns ≈ 2.3e6 states at `V ≤ 4` and finds nothing (its
minimal cell needs `V ≥ 5`); triangle-rich, high-degree vertices give 20–24 darts whose partial-map
iso-classes are themselves numerous. So the open risk is no longer "isomorphic redundancy" but the **raw count
of non-isomorphic partial assemblies for big triangle-heavy cells**. Next levers, in order: (1) MRV /
closure-directed dart ordering (match the most-constrained dart, fail fast on dead branches); (2) skip
redundant higher-`V` layers (a tiling found at `V` need not be re-sought at multiples of its cell); (3) the
outer-loop feasible-bucket prune (most `k`-type subsets die to port-consistency). The gate before n = 4–7:
does a known 3-uniform bucket assemble in a tractable budget once its `Vmax` is reached.

## Validation ladder

1. Spike bucket reproduces its tilings exactly (vs `DelaneySymbols`).
2. Full outer loop reproduces `DelaneySymbols` EXACTLY at n ≤ 3 (counts + vertex-type sets — the oracle).
3. Then `TilingReference` counts for n = 4, 5, 6, 7 (33, 15, 10, 7).

## Risks / unknowns

- **Inner assembly size.** The claim is "few port-matched torus assemblies of `V` typed vertices". Plausible
  for small `V`, but UNPROVEN — the spike settles it cheaply before any scale-up.
- **`Vmax(k)` bound.** A k-uniform tiling's p1 cell has `V ≥ k` vertices (more when the tiling has symmetry
  beyond translation). `Vmax` must cover the largest p1 cell at each k; too small ⇒ misses tilings, too large
  ⇒ slow. Tune empirically against the n ≤ 3 oracle.
- **Assembly correctness** (dart bookkeeping, torus closure, orbit counting) is intricate — but it is the
  ONLY new code; everything downstream (key, dedup, A068600 test, validation) is reused and proven.

## Provenance

This is the *documented* lineage (unlike Galebach's unpublished method): Krötenheerdt (1969, 2-uniform) and
Chavey (1989, 3-uniform) classified by vertex-type combination and adjacency — exactly the bucket × port-match
structure here — and the Delaney–Dress machinery supplies the canonical key they lacked. We reinvent that
classification constructively, bounded per bucket, with the D-symbol as the soundness/dedup spine.

## Alternatives considered (and why not)

- **Optimise grow-cover / early-gluing.** Both are unbounded-growth searches; the scatter is intrinsic, and
  grow-cover is even incomplete at n = 2. No amount of per-state speedup changes the asymptotics.
- **Per-orbifold D-symbol generation (0023 B).** Still "generate structures, filter" — superset problem.
- **Reuse Tegula/Gavrog.** Excluded by the goal (must be our own algorithm).
- **Ship n ≤ 3 only.** The fallback if the spike shows the inner assembly is not small.
