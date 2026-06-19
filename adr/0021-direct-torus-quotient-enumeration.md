# ADR-0021: Direct combinatorial torus-quotient enumeration (reaching n = 4–7)

- **Status:** Accepted (goal + analysis) — its *realization* is superseded by **ADR-0022**. The
  "discovered-Λ propagation" prototype here exposed a soundness defect (false-period overlaps, e.g. 3.3.6.6 /
  3.4.4.6) and completeness gaps; ADR-0022 keeps this ADR's goal (reach n = 4–7, no covolume wall) but builds
  the quotient as an *intrinsic combinatorial map* (Delaney–Dress) so soundness and canonical dedup hold by
  construction. Still supersedes the fixed-Λ engines (ADR-0019, ADR-0020) for the cells they cannot reach.
- **Date:** 2026-06-19 (realization superseded by ADR-0022, 2026-06-20)

## Context and problem statement

Both fixed-Λ engines — the DCEL one (ADR-0019) and the exact-coordinate torus one (ADR-0020) — enumerate
**candidate translation lattices Λ** and, for each, grow **planar patches**. ADR-0020 characterized the
resulting cost as **exponential in covolume** (~2.5× per +1 covolume): a near-miss/spurious lattice grows a
search tree exponential in patch size, and the cell — hence patch — grows with covolume. The full A068600
table (11, 20, 39, 33, 15, 10, 7) needs exactly the **high-covolume cells**: the dodecagon types (`3.12.12`,
`4.6.12`, …) and **all of n = 4–7**. Those sit past the wall, so the fixed-Λ approach **cannot reach them in
days**. ADR-0019's earlier patch-growth engine (ADR-0018) drowns in aperiodic "scatter" there instead.

Both failure modes — covolume-exponential and scatter — come from the same root: **the engines build the
infinite tiling (a planar patch), gated by a lattice that is mostly spurious.** The fix is to enumerate the
**finite quotient** — one fundamental cell as a combinatorial object — directly, never trying a lattice and
never growing the infinite cover.

## Key fact that makes a direct enumeration sound

A **combinatorial torus map** by unit regular polygons — a finite set of `{3,4,6,12}` faces with every edge
shared by exactly two faces, connected, of Euler characteristic `V − E + F = 0` (genus 1) — in which **every
vertex is a valid 360° vertex type** develops to a **flat, periodic planar tiling, automatically**:

- All-360° vertices ⇒ the holonomy around every small loop is a pure translation (zero rotation) ⇒ the map is
  *flat* ⇒ it develops consistently on its universal cover, the plane.
- Genus 1 ⇒ the deck group of that cover is a rank-2 **translation lattice** Λ — *derived from the map*, not
  guessed. The map *is* one fundamental cell of the resulting periodic tiling.

So: **enumerate combinatorial torus maps with all-valid-vertices, and every one is a genuine periodic
regular-polygon tiling.** There is no lattice to sweep (Λ falls out of the map) and no scatter (a finite map is
periodic by construction; aperiodic tilings would need infinitely many faces per cell and never appear).

## Decision — the architecture

Three parts, in the JVM-only `generator/` subproject; the library and both fixed-Λ engines are untouched.

### 1. Map enumeration by boundary gluing (the new core)
Grow a **partial map with a boundary** (a planar patch that may also *close back on itself*), face by face,
exactly as the propagation engine grows — pick the most-constrained boundary vertex and fill its gap — **but
each new polygon edge may either open a new boundary edge OR glue to an existing boundary edge** (the torus
identification). Enumerate both. When the boundary is empty and every vertex is complete, the map is a closed
torus cell → a candidate tiling. Bound by **face count** (cells are small), not covolume. This reuses the
vertex-type tables (`VertexTypes`), the angular-slot bookkeeping, and the MRV completion logic of ADR-0020;
the *new* element is the edge-gluing branch.

### 2. Exact development + consistency (reuse ZetaPoint)
Develop the map in exact integer ℤ[ζ₁₂] coordinates ([[ZetaPoint]]): place a seed face, and each glued edge
imposes an exact coordinate identification. A gluing is **geometrically consistent** iff the two identified
vertices coincide mod the emergent lattice (exact integer test, as `ZetaPoint.congruentMod` already does).
Inconsistent gluings are pruned at the branch point — the analogue of the Λ-consistency oracle, but the
lattice is *derived*, so there are no spurious lattices to pay for.

### 3. Verification + identity (reuse verbatim)
A closed, consistent cell is handed to the **existing** `KrotenheerdtLatticeSearch.verifyContentAnyN`
(orbits = types = n) and `torusContentKey` for the canonical up-to-all-isometries key — so soundness,
chirality, sublattice reduction, and the n-bucketing are inherited unchanged, and every result cross-checks
against the fixed-Λ engines on the cells both can reach.

## Why this avoids the wall

- **No candidate lattices.** The ~37k (k=6) / ~100k (k=7) lattices — almost all spurious — are gone; Λ is read
  off each map. The covolume-exponential, which was *per-lattice × number-of-lattices*, has no analogue.
- **No scatter.** Aperiodic patches never arise; only finite cells are built, bounded by face count.
- **Each cell once.** A tiling is enumerated as its (canonical) cell, not re-discovered under every sublattice.

The remaining cost is the **map-gluing search** itself, bounded by face count with the valid-vertex prune doing
the heavy lifting — polynomial-ish in cell size rather than exponential in covolume. This is the standard route
(Delaney–Dress symbols formalize the same quotient; Gavrog/3dt enumerates periodic tilings this way); this ADR
takes the concrete, coordinate-backed form that reuses the ADR-0020 infrastructure.

## Status / plan

- **Prototype (starting):** `KrotenheerdtTorusMapSearch` — boundary-gluing enumeration with exact-coordinate
  consistency, reusing `ZetaPoint`, `VertexTypes`, and `verifyContentAnyN`. Validate by **reproducing the
  fixed-Λ engines exactly** on the cells they reach (n = 1 small cells, n = 2), then push to the dodecagon
  cells and n = 3–7 that the fixed-Λ engines cannot.
- **Risks / open questions** (to be resolved in the prototype): the gluing branch factor (does the valid-vertex
  prune keep it small enough at high n?); the exact closure/consistency test for non-convex or multiply-glued
  boundaries; canonical dedup of maps before verification to avoid enumerating each cell in many gluing orders
  (the analogue of the ADR-0020 canonical key, on the abstract map).

## Consequences

- A genuinely different enumeration that, if it works, reaches **n = 4–7** and the dodecagon cells the fixed-Λ
  engines cannot — the only honest route to the full A068600 table after the ADR-0020 wall.
- Larger, higher-risk than the ADR-0020 optimisations (it is a new search, not a tweak), so it lands behind a
  validation bar: reproduce the fixed-Λ counts exactly before trusting it past them.
- The fixed-Λ engines stay as the validated reference for the low-covolume cells and the cross-check.

## Design refinements (from the planning phase — these sharpen the architecture above)

**Reframing — "discovered-Λ propagation".** This is NOT a new kind of search: it is the ADR-0020
vertex-completion propagation engine with **Λ promoted from a swept parameter to a discovered search
variable**. The engine develops in ONE plane frame (the universal cover) in exact `ZetaPoint` and never
physically wraps; a "gluing" is the assertion *"the face across this boundary edge is an already-placed face
translated by a deck vector t"*, which adds `t` to a partial lattice of rank 0/1/2. Growth, slot bookkeeping,
and the valid-vertex prune are the fixed-Λ engine's; the only new degree of freedom is the **extend-vs-glue
branch** + incremental rank-≤2 accumulation.

**Soundness — orientability is the one condition beyond all-360°.** All-360° vertices ⇒ the surface is a flat
*manifold*, but that admits a flat **Klein bottle** (whose deck group has a glide reflection — not a translation
lattice). The torus needs **orientability**; then the free-action / Bieberbach argument forces the deck group to
be pure translations (a cone angle of exactly 2π ⇒ the cover acts freely ⇒ an orientation-preserving
fixed-point-free isometry is a translation). "No net rotation around the generators" is then a *theorem*, not a
side condition. Enforce orientability constructively, for free: develop with **direct isometries only**
(`+`/`timesZeta`, never `conjugate`) and glue a boundary half-edge at slot `b` only to an existing open
boundary half-edge at slot **`b+6`** (antiparallel). Assert each loop's rotational part `k ≡ 0 (mod 12)` as a
bug detector.

**Exact gluing/consistency (integers only).** A gluing yields a translation `t = q' − p` (`ZetaPoint`
subtraction); it is edge-consistent iff the *other* identified endpoints also differ by exactly `t`. Accumulate
`gens`: `Rank0+t→Rank1(t)`; `Rank1(g1)+t→Rank2(g1,t)` if `cross(g1,t)≠0` else require `t` a multiple of `g1`;
`Rank2(g1,g2)+t→` accept iff `t.congruentMod(origin,g1,g2)` **else PRUNE** (a third independent generator ⇒
non-discrete periods ⇒ not a torus). Re-validate standing vertex identifications after every generator change.
Reject any `Rank2` whose covolume `< distinctArea(faces)` (over-gluing / overlap). Only `congruentMod` + an
integer cross-product — no float in the lattice algebra.

**Closure = verifiability (reuse `cellData`).** Drive closure off the existing `cellData(faces, g1.toBigPoint,
g2.toBigPoint, origin)`: `Left(Grow)` ⇔ some torus fan is still incomplete ⇒ keep developing (place the thin
corona of cheap Λ-translate faces until fans are physically complete — this is the honest answer to the corona
question); `Right(...)` ⇒ hand to `verifyContentAnyN`. "Closed" and "verifiable" become the same predicate.

**The crux risk — the gluing branch factor.** A′'s cost lives here: at a boundary vertex, *any* compatible
existing boundary edge is a candidate identification (the localized analogue of "which lattice"). Controls:
antiparallel-only candidates, glue only at the MRV vertex, dedup candidate `t`s congruent mod current Λ,
best-first toward closure, the rank/covolume integer prunes, and a flag/dart canonical visited-set (geometry-free
canonical labeling of the partial map). **Measurement gate before any high-n run:** instrument states-per-cell
on n=2/n=3 and confirm it tracks cell *size*, not covolume. If it tracks covolume, tighten controls before
attempting n=4–7.

**Reuse — staged duplicate-then-extract.** Do NOT edit `KrotenheerdtTorusSearch` until the new engine reproduces
its counts. Call `verifyContentAnyN`/`primitiveBasis` directly (`private[dcel]`). Duplicate the small pure
helpers (`polygon`, `FaceZ`, slot tables, the `cellData`→`reconstructFans`→`verifyContentAnyN` "verify-a-
developed-cell" pipeline) and reuse `completions`/seed tables; re-implement **rank-aware** `isConsistent`/
boundary (the originals hard-assume a fixed rank-2 Λ). After validation, extract a `private[dcel] object
TorusCellGeometry` shared by both engines.

**Validation ladder (each milestone gates the next):** (1) single-face cells — square `polygon(origin,0,4)`,
glues `t1=(1,0,0,0)`,`t2=(0,0,0,1)`, Λ covol 1, one vertex `4.4.4.4`; hexagon `polygon(origin,0,6)`,
opposite-edge glues, Λ = triangular covol 3√3/2, **two** vertices `6.6.6` (the third glue is the redundant
`congruentMod`-true path) — assert `verifyContentAnyN → Some((1, key))` with `key` == the fixed-Λ key. (2)
multi-face small cells (`3⁶` = 2-triangle rhombus, `3.4.6.4`, `3.3.3.4.4`, `3.6.3.6`, the chiral snub
`3.3.3.3.6`). (3) key-for-key cross-check vs `KrotenheerdtTorusSearch.enumerate` for n=1 small + n=2 (the trust
gate). (4) the measurement gate, then n=3–7 + the dodecagon cells.

> Full implementation plan with the worked coordinates and the algorithm step-by-step lives in the session plan
> `.claude/plans/purring-herding-gosling.md`; the scaffold is `KrotenheerdtTorusMapSearch.scala`.
