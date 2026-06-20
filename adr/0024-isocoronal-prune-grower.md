# ADR-0024: Isocoronal-prune vertex-star grower (the n = 4–7 anti-scatter lever)

- **Status:** Rejected (measured 2026-06-20) — the r = 2 prune is a NO-OP for the grow-cover scatter (see
  "Measurement result"). The structural idea is sound; this *implementation* of it does not fire early enough.
- **Date:** 2026-06-20

## Context

ADR-0022 (`DelaneySymbols`) is validated through **n ≤ 3** but cannot reach n = 4–7 (generate-all-D-sets
wall). ADR-0023's geometric early-gluing grower (`enumerateByGluing`) is sound but scatters: EXTEND before the
deck lattice is discovered grows the planar patch freely, branching over many locally-valid patches. Galebach
reached n ≤ 6 in 2002, but **his method is undocumented** — there is no algorithm to port, only the result. So
this is a *reinvention*, grounded in the one structural lever the literature makes clear.

## The lever — bound by DISTINCT CORONAS, not distinct TYPES

The grow/glue engines prune with "≤ n distinct vertex **types**" (`isSound`). That is far too weak: there are
many locally-valid ≤ n-type planar patches (the scatter). The defining property of an **n-uniform** tiling is
**n vertex ORBITS** — and two vertices share an orbit only if their entire neighbourhoods coincide. So:

> Prune any patch with more than `n` distinct **r-ring coronas** (the cyclic arrangement of polygons within
> graph-distance r of a completed vertex). For r = 1 this is just the vertex type (today's weak prune); for
> r ≥ 2 it is strictly stronger and, as r grows, converges to the orbit count. A genuine n-uniform tiling has
> ≤ n distinct r-ring coronas for every r, so the prune is SOUND; an aperiodic / wrong patch exceeds n
> distinct coronas quickly and is cut early. This is the "isocoronal" structure that forces the patch to
> repeat — the anti-scatter mechanism the type-prune lacks.

## Decision (proposed)

Keep the existing geometric grower (`KrotenheerdtTorusMapSearch`: corona seeds, MRV vertex completion,
boundary gluing, `verifyCell` closure with the overlap soundness check) and ADD the **isocoronal prune**:
after each completion, compute every completed vertex's r-ring corona signature and reject the patch if the
distinct count exceeds `n`. Start at r = 2; raise r only if a higher n still scatters. This is a small,
well-scoped change to a working engine, not a new architecture.

- **Corona signature (r-ring):** BFS from a completed vertex over completed vertices to depth r; record the
  multiset/cyclic structure of incident polygon sizes, canonicalised (bracelet) — a hashable key. Reuse
  `planarFan` / `coveredSlots` for the per-vertex part.
- **Prune:** `distinctCoronas(faces, r).size <= n` as an additional `isSound` conjunct in
  `growByCompletionPlanar` and in the gluing extend.
- **Closure / key / soundness:** unchanged — `verifyCell` (overlap-checked) + `torusContentKey`, cross-checked
  against `DelaneySymbols` and `TilingReference`.

## Measurement result (2026-06-20) — NO-OP, rejected

Implemented `isocoronalOK` (BFS r-ring corona signature over completed vertices) and gated growth children
with it. States explored (overlap check off, to isolate state count):

| run | r = 1 (type-only) | r = 2 (iso prune) |
|-----|-------------------|-------------------|
| n = 1 covol≤6 | 99 states / 6 cells | **99** / 6 |
| n = 2 covol≤4 | 856 states / 7 cells | **876** / 7 |

No reduction (n = 2 even rose slightly from the extra check). **Root cause:** the r-ring signature requires a
vertex's ENTIRE r-ring to be completed, but during growth almost every vertex sits on or near the incomplete
boundary, so there are essentially no interior vertices to judge — the prune has no data and **fires only
after the patch is already large, i.e. after the scatter has happened.** The "≤ n distinct coronas" property
is a true orbit invariant (the idea is sound), but checking it on *completed* rings is fundamentally too late
for a boundary-growth search. A version that could fire early would need to bound *partial* coronas, which is
not an orbit invariant (would risk dropping valid tilings). Rejected.

## Validation ladder (not reached — gate failed at step 1)

1. **Re-measure states/cell** with the r = 2 prune at n = 2/3; it must drop sharply vs the ~131 grow-cover
   baseline and stay flat across n. THIS is the go/no-go. → **FAILED (no-op).**
2. **Oracle:** reproduce `DelaneySymbols` exactly at n ≤ 3 (counts + vertex-type sets).
3. **Reference:** then `TilingReference` counts for n = 4, 5, 6, 7 (33, 15, 10, 7).

## Risks / unknowns

- **Does the r-ring prune actually collapse the scatter?** Plausible (it forces repetition) but unproven; the
  r = 2 measurement in step 1 settles it cheaply before any big build.
- **Choice of r:** too small → residual scatter; too large → it only fires after the patch is already big.
  Likely r = 2–3; tune empirically.
- **Closure still needs the deck lattice** (the gluing from ADR-0023) — the prune reduces scatter but does not
  replace gluing; the two compose.
- This remains research-grade. If the r = 2 measurement does not show a sharp drop, the honest outcome is to
  **ship `DelaneySymbols` (n ≤ 3)** and leave n = 4–7 open.

## Alternatives considered

- **Intrinsic D-symbol grower (ADR-0023 A, pure form):** sound by construction but a larger rewrite; the
  isocoronal prune is orthogonal and could be added there too.
- **Per-orbifold generation (ADR-0023 B):** 17 cases, heavier; deferred.
- **Ship n ≤ 3 only:** the safe default if the prune does not pay off.
