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

## The real spike (still to build): bounded-V dart assembly

Implement the port table + a **fixed-`V` combinatorial-map assembler** (darts/half-edges): take `V` typed
vertices, enumerate the port-matched perfect matchings of their darts that close into a torus with all faces
regular, for the one bucket `T`. Confirm it produces exactly that bucket's tilings with a state count that
tracks `V` (not the patch-growth ~850). Only the *complete-torus* space is searched — there are no partial
patches to scatter over. This is the genuine test of the ADR, and the genuine new code.

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
