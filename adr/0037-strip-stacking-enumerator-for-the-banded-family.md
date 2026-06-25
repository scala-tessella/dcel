# ADR-0037: Strip-stacking enumerator for the banded family

- **Status:** Proposed (build in progress). 2026-06-25.
- **Follows:** [[0036-non-monotonic-difficulty-and-top-down-type-set-derivation]] (constraint-first; derive
  type-sets). Prompted by the reference audit + grower re-measure ([[0035…]] retraction) and a user
  observation that the grower-missed n=3 tilings are all **banded** (single-direction strips ⇒ C₂-max).

## Context — the characterization data

The reference audit corrected `TilingReference` (2 compensating n=3 transcription errors) and the grower's true
n=3 reach is **32/39** (not 36), missing 7. `CharacterizeBandedProbe` classified every n=3 oracle tiling by
max rotation order (D-symbol) and a geometric **fault-line** test (≥4 collinear edges spanning the patch = a
band boundary), realized via bounded-V, crossed with grower hits/misses:

- n=3: 39 cells — **20 C₂-max**, 19 higher (C₃=1, C₄=2, C₆=16). Realized: **banded=17, non-banded=5**, 17
  unrealized (mostly the C₆ *isotropic* cells bounded-V couldn't reach at maxV=12; the grower reaches 16 of
  those 17).
- **Of the 7 grower MISSES: 6 are banded (C₂), 0 are non-banded, 1 is the C₆ D=11 outlier (unrealized).**
- **The grower misses ZERO non-banded cells.**
- Banded family (realized) = 17; grower **misses 6 (35%), reaches 11 (65%)**.

**Reading (careful):** *banded is NECESSARY for a grower miss* (every realized miss is banded; no non-banded
cell is ever missed) — the user's instinct holds at the family level. But *banded is NOT sufficient* (the
grower reaches 11/17 banded cells), so it identifies the right neighbourhood, not the exact failing cell. The
single non-banded outlier among the misses (C₆ D=11) is a separate high-symmetry case.

Why banded breaks the grower: a banded tiling is **anisotropic** (short period along the band, long across the
stack); the grower grows a *compact disk* from a rotation centre and reads the period off the *shortest*
boundary gluings — both biased against anisotropic cells.

## Decision — a dedicated strip-stacking enumerator (the cylinder model)

Enumerate the banded family directly, answer-blind (ADR-0034 §2): a banded tiling is a **periodic vertical
stack of horizontal strips** sharing one in-band translation period. Build by the **cylinder model**:

1. Fix a horizontal in-band period `h` (try lengths P over the {3,4,6,12}-compatible directions; WLOG
   horizontal by rotation). Identify `x ≡ x + h` ⇒ a **cylinder** — this BUILDS IN horizontal periodicity and
   kills the 1-D horizontal scatter that sank the free planar grower (ADR-0028).
2. **Grow upward** on the cylinder (scanline order: fill the lowest incomplete vertex), placing unit regular
   polygons, under the same `isPlanarConsistent` + valid-vertex + ≤n-distinct-type prune as the grower.
3. **Close** when a vertical translation period appears (the configuration repeats at `y` and `y+Δ`) ⇒ a torus
   cell `h × Δ`; gate with `verifyCell` (soundness) and key via `classifyClosedMap` (shared D-symbol key, so
   results dedup with the oracle / grower / bounded-V). Bounded by a max vertical height (the analogue of
   `maxFaces`); aperiodic stacks never close and are discarded.

This reuses the exact ℤ[ζ₁₂] primitives (`polygon`, `FaceZ`, `isPlanarConsistent`, `verifyCell`,
`classifyClosedMap`) so it is sound by construction and consistent with the other engines.

## Plan (the user's order: build → test thoroughly → visualize → validate n=3)

1. Build the cylinder strip-stack search.
2. **Test thoroughly:** soundness (every output `verifyCell`-valid, no spurious); reproduces known banded
   tilings (3³.4² elongated-triangular; the {3⁶;3³.4²;4⁴} stackings); deterministic.
3. **Visual outputs:** SVG of the enumerated banded tilings (reuse the `realizeCell`/FaceZ → SVG path).
4. **Validate at n=3:** reproduce the banded family (cross-check key-for-key against the oracle's banded cells
   — should recover all 17, including the 6 the grower misses).
5. Then extend to n≥4 to size + capture the banded class there.

## Consequences

- **Positive:** a dedicated engine for exactly the class that breaks the grower; fair/answer-blind; sound +
  shared-key (composes into the union); targets the anisotropic structure 2-D growth can't.
- **Negative / open:** the within-band distinguisher (why the grower reaches 11/17) stays unexplained — but it
  becomes *moot* if the strip enumerator covers the whole banded family. The single C₆ non-banded miss is out
  of scope (handled elsewhere). The band-direction/period search adds a small constant (a few P × directions).
