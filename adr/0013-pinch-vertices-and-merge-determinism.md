# ADR-0013: Pinch vertices and merge determinism

- **Status:** Proposed
- **Date:** 2026-05-31

## Context and problem statement

[ADR-0012](0012-enclosed-region-faces-on-merge.md) makes `mergeTilings`
materialise regions that a weld encloses. It works for **fully-surrounded**
enclosures (every boundary vertex has exactly one faceless edge leaving it), but
two related problems surface at a **pinch vertex** — a vertex where two boundary
rims meet, so several faceless edges leave it.

### The motivating case

A T-pentomino of unit squares, with a copy **rotated 90° (or 270°) about its
base cell**, traps a unit square. Three tiles of the union meet that square's
corner at a single vertex; the fourth quadrant is the outer face. Once the
square is filled (its four edges become internal), the outer boundary is a
**simple polygon and the tiling is valid** — exactly what `maybeAddSimplePolygon`
produces for the same shape. So the operation *should* return `Right` (10 faces:
9 tiles after the base square dedups, plus the enclosed square).

It does — but only **intermittently**. Three distinct causes, peeled back one at
a time:

1. **Non-determinism (the blocker).** `HalfEdge` and `Face` hash by
   `System.identityHashCode`, and `mergeTilings` rebuilt connectivity through
   `HashMap`/`HashSet` keyed by those objects — twin-wiring (`step 2.e`, pairing
   edges by buffer index) and seam-collapse (`step 3`, picking `.head`), and the
   merged vertex/edge lists came from `HashMap.values`. Iteration order varied
   per JVM run, so at a pinch combined with the base-square self-overlap the
   rebuilt DCEL was sometimes correct and sometimes corrupt. Over five fresh
   JVMs the identical 90° rotation returned a valid `Right` three times and a
   `Left` (self-intersecting, or "boundary does not close, 0.4142") twice.

2. **Wrong-rim rotation.** ADR-0012's tracer advanced along a rim with
   `prev.twin` (rotate through the inner fan). At a pinch the region's own wedge
   is *empty* — no inner edges to rotate across — so `prev.twin` crosses from the
   outer rim onto the hole rim, producing a self-intersecting figure-eight. The
   next-rim edge must be chosen **geometrically**: the first faceless edge
   *clockwise* from `edge.twin`.

3. **Inexact pinch angle.** The conjugate-of-surrounding-tiles angle formula
   over-counts at a pinch (the corner is shared with the outer face), so the
   angle there must come from the geometry. But an `atan2`-derived angle is
   *irrational* (`89.9999…°` instead of `90°`), and the boundary's **angle-based**
   simplicity check (`SimplePolygon.fromUntrusted` reconstructs the polygon from
   its angles via `unitPath`) is sensitive to that error when the silhouette has
   collinear `180°` runs — it reports a *false* self-intersection. The pinch
   angle must stay **rational**.

The non-determinism was the deepest: a **pre-existing fragility of `mergeTilings`**
that fully-surrounded enclosures never exercised. Fixing tracing or angles
without it would still flake.

## Decision

**Fix all three, in `mergeTilings`.** Validation stays the arbiter of validity:
the merge builds a faithful DCEL *deterministically*, and
`TilingValidation.validate` decides whether the result is a legal tiling.
Genuinely degenerate pinches (real overlaps, crossing edges) still return `Left`.

1. **Order-independence.** Drive the connectivity rebuild from `allOldEdges`
   (base edges before copy edges) instead of `HashMap.values`: twin-wiring
   buckets, the merged-edge list, and (sorted by id) the merged-vertex list are
   now pure functions of the geometric input. Coincident edges from the same
   operand pair with each other; seam-collapse then merges across operands. This
   benefits *every* copy operation, not just pinches.
2. **Geometric rim tracing.** `nextBoundary` picks the next faceless edge by
   geometry at a pinch — the first one clockwise from `edge.twin`. Ordinary
   vertices have a single candidate and are unaffected.
3. **Rational pinch angle.** A simple polygon's interior angles sum to
   `(n − 2)·180`, so a *single* pinch corner's angle is exactly that total minus
   the other (exact, conjugate) corners — no `atan2`. The boundary stays rational
   and the angle-based simplicity check passes. (A rare multi-pinch hole falls
   back to reading each pinch angle off the geometry.)

## Consequences

### Positive

- Pinch enclosures are valid and **deterministic**: the T-pentomino rotated
  90°/270° onto its base cell returns `Right` (10 faces, one enclosed square) on
  every run, and a fan that leaves triangular gaps (`fanHoles`) now fills them.
- Order-independence removes a latent source of flakiness from **all** copy
  operations, not only pinches — the merge is now a pure function of its inputs.
- No change to the public API or to validation; genuinely degenerate pinches
  still fail.

### Negative / risks

- The connectivity rebuild (twin-wiring, seam-collapse) is the oldest, most
  delicate part of the merge; the change is guarded by running the full suite
  repeatedly across JVMs, not once.
- Multi-pinch holes still use an inexact `atan2` angle and could, in principle,
  trip the angle-based boundary check on a silhouette with collinear runs; no
  such case is reachable from the current copy operations, and it is noted in the
  code for if one ever is.

## Testing strategy

- **Determinism:** the pinch construction is run across five fresh JVMs (stable
  `Right` every time), and the full suite three times (stable 738 green).
- **T-pentomino test (un-pended):** 90° and 270° → `Right`, 10 faces, exactly one
  enclosed unit square, `validate` green.
- **`fanHoles`:** now fans successfully, filling six unit-triangle gaps.
- **Negatives kept:** real overlaps / crossings still `Left`.

## Alternatives considered

- **Treat every pinch as invalid (always `Left`).** Rejected: pinch results are
  *sometimes* valid tilings (the T-pentomino is), so a blanket rejection would
  refuse legitimate tilings.
- **Snap the `atan2` pinch angle to the nearest nice rational.** Rejected: the
  angle-sum closure gives the exact value directly for a single pinch, with no
  tolerance to tune and no risk of snapping a genuinely-unusual angle wrong.
- **Loosen the boundary simplicity check to tolerate angle error.** Rejected:
  weakening a validation rule to paper over an inexact input is the wrong layer;
  keep the input exact instead.
