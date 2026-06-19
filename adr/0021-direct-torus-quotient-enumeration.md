# ADR-0021: Direct combinatorial torus-quotient enumeration (reaching n = 4–7)

- **Status:** Proposed — design accepted, prototype starting. Supersedes the fixed-Λ engines (ADR-0019,
  ADR-0020) **for the high-covolume / high-n cells they cannot reach**; those engines remain the validated
  cross-check for the cells they *can* reach.
- **Date:** 2026-06-19

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
