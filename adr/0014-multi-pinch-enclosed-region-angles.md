# ADR-0014: Exact corner angles for multi-pinch enclosed regions

- **Status:** Accepted
- **Date:** 2026-06-02

## Context and problem statement

[ADR-0012](0012-enclosed-region-faces-on-merge.md) materialises a region the
weld encloses as a new inner face; [ADR-0013](0013-pinch-vertices-and-merge-determinism.md)
extended that to a **single pinch** — a vertex where one enclosed region touches
the outer face — by tracing the rims geometrically and giving the pinch corner a
*rational* interior angle (so the angle-based simplicity check stays exact). It
left one case open, explicitly, as a residual risk:

> "Multi-pinch holes still use an inexact `atan2` angle and could, in principle,
> trip the angle-based boundary check on a silhouette with collinear runs."

That case is now reachable and reported.

### The motivating case

Build a 3×3 net of unit squares (vertices `1..16`, left-to-right then
bottom-to-top) and drop vertices `1`, `12`, `15`. The five surviving cells form
an **M / W pentomino** (a staircase). Its three base vertices `8`, `11`, `14`
are collinear; reflecting the pentomino across that axis welds a congruent copy
on and **encloses two separate unit squares** — twelve faces in all
(5 + 5 tiles + 2 enclosed).

Today the merge returns `Left`: *"boundary does not close, final edge 2.1248"*
and *"angles around interior vertex do not sum to a full circle: …359.9999…"*.

### Why it fails

Instrumenting `mergeTilings` shows the topology is already right — the geometric
tracer decomposes the faceless edges into exactly three cycles (two `+2`
unit-square holes, one `−24` outer silhouette), correctly separated even through
the point where both holes and two tiles meet. **The failure is purely
angular.** Each hole has **two** pinch corners, not one:

- the **shared** corner, where the two holes and two tiles meet at a single
  point (no outer face there); and
- an **outer** corner, where that hole touches the outer silhouette
  (vertex `8` for one hole, `14` for the other).

ADR-0013's exact rule only fires for `pinchEdges == single :: Nil`. With two
pinch corners per hole the code falls into the `case _ => geometricInteriorAngle`
fallback — an `atan2`-derived, *irrational* angle (`89.9999…°`). That single
inexact value then fails the interior-vertex 360° check and, propagated through
the outer face's `conjugate`-of-incident-angles, the angle-based outer-boundary
reconstruction (`SimplePolygon.fromUntrusted`) — hence both error messages.

### Why one inexact corner is enough to break it, and why algebra alone can't fix it

The angle-based checks are *exact*: a sum of `89.9999…°` and rationals is not
`360°`, and a polygon reconstructed from such angles does not close. So **every**
corner angle must be an exact `Rational`.

But pure algebra cannot supply them. A hole with `k` pinch corners has `k`
unknown angles, and its polygon angle-sum `(n − 2)·180°` is a **single**
equation. For `k = 1` (ADR-0013) that one equation closes the one unknown
exactly. For `k = 2` it leaves **one degree of freedom**: the split of the
shared vertex's empty angle between the two holes. Geometry fixes that split
(here 90°/90°, because the holes are unit squares), but the algebra is blind to
it. Adding the per-vertex 360° constraints and the second hole's closure does
**not** remove the freedom — the system stays underdetermined by exactly that
symmetric split. So **at least one geometric reading per hole is unavoidable**;
the only question is whether it is *exact*.

The decisive observation: **one exact reading per hole is also sufficient.** Read
`k − 1` corners exactly and the polygon angle-sum closes the last; then every
downstream sum — the interior-vertex 360° checks and the outer face's
`conjugate` angles — closes exactly by construction, because each is a sum or
difference of exact rationals.

## Decision

**Generalise ADR-0013's exact pinch rule from one corner to `k`.** In
`mergeTilings` step 5, for an enclosed cycle with pinch corners:

1. **Read `k − 1` pinch corners exactly from the geometry.** The interior wedge
   of the hole at a pinch corner is the angle between its two incident unit
   edges. Compute it from the **exact** dot product `cos = (a − v)·(b − v)` (and
   the cross-product sign, combined with the hole's known CCW orientation, to
   resolve a reflex wedge), then map it to the canonical tiling angle — the
   admissible wedge cosines (`0, ±½, ±√2⁄2, ±√3⁄2, ±1`, …) are well separated,
   so a tolerance snap (the same Double-geometry tolerance philosophy as
   [ADR-0010](0010-validation-geometry-double.md) and ADR-0013's tracer) returns
   an **exact** `AngleDegree`.
2. **Close the last pinch corner** with the polygon angle-sum, exactly as
   ADR-0013 does for the single-pinch case: `interiorSum − (all other corners)`.

`k = 1` reduces to ADR-0013 unchanged (zero geometric reads, one closure);
`k = 0` is the fully-surrounded ADR-0012 path. Validation stays the arbiter:
the merge builds the faithful DCEL and `TilingValidation.validate` still decides
legality, so degenerate pinches (real overlaps, crossings) still return `Left`.

## Consequences

### Positive

- The reported double-pinch (M-pentomino reflected across its base) returns
  `Right` with twelve faces and two enclosed unit squares, deterministically.
- The fix is a **strict generalisation**: single-pinch and fully-surrounded
  enclosures take exactly the same code path and exact values as before, so the
  whole ADR-0012/0013 test corpus is unaffected.
- No change to the public API, to validation, or to the merge's topology
  (cycle decomposition is already correct); the change is confined to per-corner
  angle assignment in step 5.
- It closes the one residual risk ADR-0013 documented rather than leaving it
  latent.

### Negative / risks

- **A canonical-angle recognition step enters the merge.** Mapping an exact
  cosine to an exact `AngleDegree` needs a small table of admissible wedge
  cosines and a tolerance. This is the same trade-off ADR-0010 already accepted
  for validation geometry, but it is new to the *constructor*, and the table
  must cover every wedge the supported tilings can produce (squares, triangles,
  hexagons → `{0, ±½, ±1}`; octagons/dodecagons add `±√2⁄2`, `±√3⁄2`).
- **An unrecognised wedge degrades, it does not crash.** If a future tiling
  produces a wedge cosine outside the table, the corner falls back to the
  inexact `atan2` value (today's behaviour) and that one merge may fail
  validation. The fallback is logged-in-code, not silent, and is strictly no
  worse than the status quo.
- **The exact reading depends on coordinate accuracy.** Reflection/rotation
  carry ~1e-15 float error (ADR-0010), so the dot product is `~1e-16`, not a
  clean `0`; the tolerance snap absorbs that. A near-degenerate (near-zero-area)
  sliver would be ambiguous — but such a sliver is not a valid unit tile and is
  rejected by validation anyway.
- Marginally more work per enclosed corner (one dot/cross product and a table
  lookup); negligible against validation.

## Testing strategy

- **Headline (the reported case):** the M/W pentomino reflected across its
  `8–11–14` base axis → `Right`; twelve faces; exactly two enclosed unit squares
  (4-gons, all 90°); the shared and outer pinch vertices close to a full circle.
- **Determinism:** run the construction across several fresh JVMs (stable
  `Right`) and the full suite repeatedly, as ADR-0013 did.
- **No regression:** the single-pinch (T-pentomino), the rhombus, the square
  E-comb, the 3.12.12 triangle, and `fanHoles` all stay green and unchanged —
  they exercise `k ≤ 1` and must take the identical path.
- **Cross-platform:** JVM and Scala.js parity, since the dot/cross product and
  tolerance snap are pure `BigDecimal`/`Double` arithmetic.

## Alternatives considered

- **Snap the `atan2` angle to the nearest nice rational (degrees, not cosine).**
  Rejected: the admissible *angle* set is denser and less uniform than the
  admissible *cosine* set (108°/36° from pentagons, 45°/135° from octagons, …),
  with no single base to snap to; snapping the cosine keys off a sparse,
  well-separated set and is robust.
- **Distribute the polygon-sum residual evenly across the `k` pinch corners.**
  Rejected: it makes the *hole's* angle-sum exact but leaves each individual
  corner inexact, so the outer face's `conjugate` angle at the shared/outer
  vertices stays irrational and the outer boundary still fails to close. Only
  per-corner exactness fixes it.
- **Solve the coupled linear system (all holes + all shared vertices) and read
  the remaining DOF geometrically.** Rejected as over-engineered: the system is
  always underdetermined by exactly the per-hole geometric split, so it reduces
  to "read `k − 1` corners geometrically" anyway — with far more bookkeeping.
- **Loosen the angle-based boundary/vertex checks to tolerate the error.**
  Rejected for the same reason as ADR-0013: weakening a validation rule to paper
  over an inexact input is the wrong layer. Keep the input exact.
- **Reject multi-pinch as invalid.** Rejected: the result *is* a valid tiling
  (twelve unit squares), exactly as `maybeAddSimplePolygon` would produce; a
  blanket refusal would reject legitimate tilings, contradicting the
  ADR-0011/0012 contract.
