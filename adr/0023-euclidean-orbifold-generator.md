# ADR-0023: Euclidean wallpaper-orbifold generator (reaching n = 4–7)

- **Status:** Proposed — scope/design only. Extends ADR-0022's `DelaneySymbols` engine, which is correct and
  complete through n = 3 but cannot reach n = 4–7 by generate-all-then-filter.
- **Date:** 2026-06-20

## Context — where ADR-0022 lands and why it stops

`DelaneySymbols` (ADR-0022) enumerates the Krotenheerdt tilings as minimal euclidean Delaney–Dress symbols,
filtered to regular `{3,4,6,8,12}`-gons with valid 360° vertices. It is **validated element-for-element**:
n = 1 = 11 and n = 2 = 20 exactly (vertex-type sets match the reference, zero spurious / missing); n = 3 = 38/39
with all 26 distinct type-sets present and none spurious (the one gap is a geometric duplicate sharing a
type-set). It also independently cross-checks the reference (engine and Wikipedia agree on the 26 n = 3
type-sets — and on **26**, not tessella3's untrusted 25).

It stops at the **D-set generation wall**. Measured facts that frame this ADR:

- **Minimal-symbol size grows ~linearly with n** (chambers): n = 1 → 1–10, n = 2 → 5–~22, n = 3 → 6–~24. So
  n = 7 is ≈ 45–60 chambers.
- **"Dress complexity" = chamber count.** The prebuilt genDSyms / Tegula databases (all euclidean ≤ complexity
  24) therefore cover **only n ≤ 3** — they cannot supply n = 4–7.
- ADR-0022's `DSetGenerator` enumerates ALL 2-manifold D-sets up to `maxSize`, then filters euclidean +
  regular-polygon. The sound prunes (r-values, max-curvature gate) reach n ≤ 3 in minutes, but cost grows ~5×
  per +2 chambers; ~50 chambers is hopeless. The general Huson/Delgado-Friedrichs euclidean enumerator
  (Tegula/Gavrog) is efficient for ALL euclidean tilings, but those are still an astronomical superset of our
  135 regular-polygon ones — the regular-polygon constraint must be woven INTO generation, not applied after.

## Decision (proposed) — generate by symmetry type, regular-polygon-native

Enumerate the tilings by building, for each **euclidean orbifold** (the 17 wallpaper symmetry types), the
**fundamental-domain map** out of regular polygons directly — never the hyperbolic universe, never the
non-regular euclidean tilings. The minimal Delaney–Dress symbol of each result is the canonical key
(deduplication) and the soundness certificate (intrinsic ⇒ the 3.3.6.6-style false-period overlaps cannot
arise — ADR-0022). `k`-uniform = `k` vertex orbits in the minimal symbol.

The 17 euclidean orbifolds (Conway): `o ×× ** *× 2222 22× 22* *2222 2*22 442 *442 4*2 333 *333 3*3 632 *632`.
Each fixes a bounded cone/mirror structure with cone orders ∈ {2,3,4,6} (crystallographic restriction).

### Two architectures (recommend A; B is the fallback if A's bucketing proves wrong)

**A. Torus map ("o") + minimization (recommended).** Every periodic tiling is, under its translation subgroup,
a **flat torus** tiling ("o" orbifold) — a finite combinatorial map of regular polygons with every vertex a
valid 360° type. Enumerate THOSE (the ADR-0021 torus-map idea, but built **intrinsically as a combinatorial
map / D-symbol**, so it is sound by construction — no coordinates, no guessed Λ, no overlap test). Then take
each map's **minimal** D-symbol (full symmetry, via the proven `isMinimal`/minimal-image) and bucket by its
vertex-orbit count `k`. This needs only ONE generator (the torus map), and minimization recovers the true `k`
(e.g. the honeycomb's 2 translation-orbits collapse to the 1 orbit of 6.6.6). Bounded by torus-cell size
(translation-orbit count ≈ k … ~2–3k), i.e. ~10–20 vertices for n = 7 — not ~50 chambers.

**B. Per-orbifold (fallback).** A direct generator per orbifold: place regular tiles in the orbifold's
fundamental domain consistent with its cones/mirrors. Avoids minimization, but is 17 cases with intricate
cone/mirror bookkeeping.

### Reuse vs new

- **Reuse from `DelaneySymbols`:** the D-symbol structure, orbit/curvature machinery, `isMinimal` /
  minimal-image, the regular-polygon + valid-vertex filter, vertex-config reconstruction, and the `Frac`
  curvature algebra — all become the **verify / canonical-key tail**.
- **New core:** the regular-polygon **fundamental-domain map grower** — vertex-completion growth that places
  only `{3,4,6,8,12}` tiles, completes every vertex to a valid 360° type, and closes the map (edge-pairings).
  For architecture A this is the torus-map grower; the genuine new work is the gluing/closure search.

## The crux risk (state it plainly)

The hard part is **tractable fundamental-domain map enumeration** — the "which boundary edge does a new edge
glue to" branching (ADR-0021's documented crux). The D-symbol framing removes the *soundness* and
*duplication* problems but NOT the branching. Controls (from ADR-0021, now on the intrinsic map): MRV
vertex-completion, antiparallel-only gluings, canonical-map dedup of partial states, and a measurement gate
(states-per-cell must track cell SIZE, not blow up) before any high-n run. This is the genuine research
unknown; everything else is engineering.

## Validation ladder

1. **Oracle cross-check:** the existing `DelaneySymbols` engine is correct through n = 3 — the new generator
   must reproduce it EXACTLY (counts and vertex-type sets) for n ≤ 3, by symmetry type where possible.
2. **Reference:** then `TilingReference` counts for n = 4, 5, 6, 7 (33, 15, 10, 7) and, where the
   appendix has them, the n = 4/5 vertex-type sets.
3. **Measurement gate:** instrument states-per-cell at n = 2/3; proceed to n = 4–7 only if it tracks cell size.

## Consequences

- The only route to n = 4–7 (≈ 45–60-chamber symbols) within the project: the cost tracks fundamental-domain
  size, not the hyperbolic D-set universe.
- Larger and higher-risk than ADR-0022 (a new search, with the ADR-0021 branching risk re-incurred — but now
  sound and canonical via D-symbols).
- `DelaneySymbols` stays as the validated oracle for n ≤ 3 and the verify/key tail.

## Alternatives considered

- **Prebuilt genDSyms/Tegula database (≤ complexity 24).** Covers only n ≤ 3 (chamber-count wall). Rejected for
  the goal; already subsumed by ADR-0022.
- **Reuse Tegula/Gavrog's euclidean enumerator.** JVM and efficient, but enumerates ALL euclidean tilings —
  filtering our regular-polygon needle from that haystack at ~50 chambers is itself intractable, plus
  integration risk. Rejected as the core; possibly useful as an independent oracle for small n.
- **Galebach's vertex-star gluing (2002).** The proven method for exactly this problem (reached n ≤ 6) and
  regular-polygon-native — but it is NOT D-symbol-based, so it forgoes the canonical-key / soundness reuse and
  would re-introduce a bespoke dedup/soundness layer. A strong alternative if architecture A's torus-map
  branching proves intractable; worth a literature pass before committing.
- **Stop at n ≤ 3.** `DelaneySymbols` already gives validated n ≤ 3; ship that and defer n = 4–7. The honest
  default if the branching risk is judged too high for the payoff.
