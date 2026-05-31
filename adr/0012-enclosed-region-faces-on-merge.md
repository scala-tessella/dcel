# ADR-0012: Materialise enclosed regions as faces during merge

- **Status:** Proposed
- **Date:** 2026-05-31

## Context and problem statement

The isometric-copy operations of [ADR-0011](0011-isometric-copy-operations.md)
grow a tiling by welding a transformed copy onto it through
`TilingMerge.mergeTilings` (`TilingMerge.scala:18`). When the two boundaries
meet, they can **enclose a region that was a face in neither operand** — a gap
that the union turns into a bounded polygon.

Concrete case (the reported one): a central pentagon with two further pentagons
on subsequent sides leaves a 36° wedge open at their shared vertex. Reflecting
that 3-pentagon cluster across the right axis closes the wedge from the other
side, enclosing a unit-edge **36°–144° rhombus** (the gap shape of the Dürer
pentagonal construction). That rhombus is a legitimate new tile, so the
operation should return `Right`. **Today it returns `Left`.**

### Why it fails today

`mergeTilings` rebuilds the boundary in step 4 (`TilingMerge.scala:204-227`)
by collecting *every* half-edge with `incidentFace.isEmpty` into a single bag
and ordering it with `orderBoundary` (`HalfEdge.scala:402`):

- After a merge that encloses a region, the faceless half-edges form **two
  disjoint cycles**: the true outer silhouette, and the inward-facing twins
  around the enclosed gap.
- `orderBoundary` greedily chains from `halfEdges.head` and `return`s the
  moment the chain cannot continue — it produces **one** cycle and silently
  **drops the rest**. The enclosed rhombus is never turned into a face; its
  edges are left faceless with stale `next`/`prev`, and validation rejects the
  result (incomplete / topology, plus the rhombus's now-interior vertices fail
  the "angles sum to a full circle" check at `TilingValidation.scala:211-221`).

### Why this is a gap, not a new feature

The model already *intends* enclosed regions to be faces. `validateTopologically`
forbids faces with holes precisely because (`TilingValidation.scala:141-145`):

> "This is specific to the tessellation we want, without holes, because holes
> are just other inner polygons in this model."

So the invariant is already "no holes — an enclosed region is an inner
polygon." `mergeTilings` simply fails to honour it when the region is born
*from the merge itself* rather than inherited from an operand.

And the enclosed polygon is already a **legal tile**: faces are validated only
by `SimplePolygon.fromUntrusted` (closure + simplicity) and a unit-edge check
(`TilingValidation.scala:163-172, 238-247`) — there is **no regularity
constraint**. A unit-edge 36°–144° rhombus passes. Nothing in validation needs
to change to *accept* the region; the merge only needs to *create* it.

## Decision

**In `mergeTilings`, replace "order all faceless edges into one outer cycle"
with "decompose the faceless edges into all their cycles, then classify each by
orientation."**

1. **Partition** the faceless half-edges into closed cycles.
2. **Classify by signed area / winding:**
   - the unique **clockwise** (negative signed-area) cycle is the true outer
     boundary → the **outer face**, exactly as today;
   - every **counterclockwise** (positive-area) cycle bounds an enclosed empty
     region → materialise a **new inner `Face`**.
3. **Materialise each enclosed face:** link the cycle (`linkInCycle`), set
   `incidentFace` and `outerComponent`, register it in `newInnerFaces`, and set
   each corner's interior angle to `(sum of the already-known incident tile
   angles at that vertex).conjugate` — the **same per-vertex formula** the outer
   boundary already uses at `TilingMerge.scala:221-227`, since the surrounding
   tiles' angles are known by that point.
4. **Validate as usual.** A genuinely-bad enclosed region (non-simple,
   non-unit, self-touching) is still rejected downstream — the
   merge-then-validate contract of ADR-0011 is unchanged.

The discriminator is **orientation**, not size: in a connected silhouette there
is exactly one clockwise cycle (the outer face, whose boundary keeps the
unbounded region on its left) and every other faceless cycle is a hole to fill.

### Cycle tracing must be rotational, not origin-matched

`orderBoundary`'s "find any remaining edge whose origin matches the current
destination" is ambiguous — and wrong — at a **pinch vertex**, where the outer
boundary and an enclosed region (or two enclosed regions) touch at a single
shared vertex. At such a vertex several faceless half-edges share an origin, and
naive matching can jump between cycles, producing a self-crossing pseudo-cycle.

The decomposition will instead follow the **rotational next-boundary-edge
rule**: from a faceless half-edge, the next edge of the *same* cycle is found by
rotating around the destination vertex to the adjacent faceless half-edge
(the standard DCEL `next` along a face boundary). This keeps each cycle on a
single region for every **fully-surrounded** enclosure (one faceless edge per
boundary vertex).

> **Note (added after implementation):** this `prev.twin` rule is correct only at
> **fully-surrounded** vertices. At a genuine **pinch** vertex — several faceless
> edges leaving one vertex — it can cross onto the wrong rim, and it depends on a
> deterministic merge. Both were handled in a follow-up:
> [ADR-0013](0013-pinch-vertices-and-merge-determinism.md) makes the merge
> order-independent and switches `nextBoundary` to a geometric clockwise rule, so
> pinch enclosures (a T-pentomino rotated onto itself) are now valid too.

## Consequences

### Positive

- The reported reflection case (and any merge that encloses a gap) now returns
  `Right` with the gap as a proper tile — the behaviour the ADR-0011 contract
  always implied ("exactly follows its composition").
- The model's existing "no holes — holes are inner polygons" invariant is now
  actually upheld by the constructor that can create holes.
- No change to validation, to the `Isometry` API, or to any operation's
  signature: this is purely a correctness fix inside the shared merge core, so
  it benefits translate / rotate / mirror / glide / `fanAround` / `repeat*`
  uniformly.
- Enclosed non-regular tiles (rhombi, etc.) are supported with no special case —
  they were always valid to the polygon checker; they were just never built.

### Negative / risks

- **Boundary tracing is the sharp edge.** Moving from origin-matching to
  rotational tracing touches the most delicate part of the merge. Pinch
  vertices, and a cycle that is simultaneously a hole of one region and the
  outer silhouette elsewhere, must be covered by tests, not assumed.
- **Orientation classification depends on robust signed area.** Coordinates
  carry the usual rotation/oblique-reflection float error (~1e-15, ADR-0010);
  signed-area sign is safe far from that, but a near-degenerate (near-zero-area)
  enclosed sliver would be ambiguous. Such a sliver is not a valid unit-edge
  tile and should fail validation anyway; the tracing must not crash on it.
- **Disconnected results.** If a merge leaves two separate islands, the outer
  face legitimately has two clockwise cycles. This ADR targets the connected
  case; multi-component silhouettes remain governed by whatever connectivity
  rule the operations already enforce, and must not regress.
- Slightly more work per merge (cycle decomposition + per-face angle fill), but
  bounded by the number of boundary edges — negligible against validation.

## Validity contract (unchanged)

Every operation still returns `Either[TilingError, TilingDCEL]` and runs the
full `TilingValidation.validate` after the merge. The only change is that a
result which *previously* failed because an enclosed region was left unfaced now
**succeeds** when that region is a valid tile, and still **fails** when it is
not.

## Testing strategy

- **Headline (the reported case):** central pentagon + two pentagons on
  subsequent sides, reflected across the closing axis → `Right`; exactly one new
  inner face; that face is a 4-gon with two 36° and two 144° corners; the former
  gap vertices are now interior with 360° angle sums.
- **Multi-enclosure (square "E"-comb):** a spine of `rows` unit squares with
  arms on the even rows, closed by a copy translated two units right, caps every
  notch into an enclosed unit square in a *single merge* — so the decomposition
  splits the faceless edges into three cycles (one outer, two holes) for a 5-row
  comb, and it scales: 5/7/9 rows → 2/3/4 enclosures, each validating. This is
  the multi-cycle coverage.
- **Non-square tile (3.12.12):** two dodecagons sharing an edge, plus a copy
  rotated 60° about one centre, encloses a unit equilateral triangle among three
  dodecagons (3 + 12 + 12 = 360) — four polygons in all. Confirms enclosed tiles
  need not be squares or rhombi.
- **Pinch (handled in [ADR-0013](0013-pinch-vertices-and-merge-determinism.md)).**
  All the enclosures above have every vertex fully surrounded. A true **pinch**
  (two rims meeting at a single vertex, e.g. a T-pentomino rotated 90°/270° onto
  its base cell) needed three further fixes — an order-independent merge, a
  geometric clockwise rim trace, and a rational pinch corner angle — delivered in
  ADR-0013. With those, the T-pentomino case returns `Right` (10 faces, one
  enclosed square) deterministically.
- **Negative (still rejects):** a merge that leaves a *partial* (open) gap, a
  proper edge crossing, or a non-unit sliver → `Left` with the relevant
  `TilingError`.
- **No-regression:** every existing merge that encloses nothing stays valid and
  equivalent (the clockwise-cycle path subsumes the old behaviour; the traced
  outer cycle may differ in starting edge, but the tiling is equivalent). The
  full JVM suite (736 green) and the cross-built copy specs on JS stay green.
- **Symmetry oracle reuse:** the ADR-0011 idempotence tests (mirror/rotate a
  symmetric tiling onto itself) already exercise the outer-cycle path and must
  stay green.

## Build order

As implemented, in `mergeTilings` step 4:

1. Replace `orderBoundary`'s single-cycle bag with a **rotational cycle
   decomposition**: `nextBoundary` rotates around a faceless edge's destination
   via `prev.twin` through the inner fan until the next faceless edge, tracing
   every rim into a separate cycle.
2. **Orientation classification** by signed shoelace area: CW (negative) is the
   outer face, CCW (positive) is an enclosed region.
3. Materialise each enclosed cycle as a new inner face with per-corner angles
   (`(surrounding tile angles at the vertex).conjugate`, the rule the outer
   boundary already uses); keep the CW cycle(s) as the outer face.
4. Land the headline + multi-enclosure + no-spurious + negative tests; confirm
   no regression on JVM and JS.

## Alternatives considered

- **Detect-and-reject (keep failing, but with a clear error).** Rejected: the
  enclosed tile is *valid* and useful (the Dürer construction depends on it);
  refusing it would permanently block the editor's pentaflake-style workflows.
- **Represent the gap as a hole of the outer face (`innerComponents`).**
  Rejected: it contradicts the model's stated invariant
  (`TilingValidation.scala:141-145`) that there are no holes — holes are inner
  polygons. Filling the region as a face keeps a single, uniform representation.
- **Keep `orderBoundary`, just loop it to extract multiple cycles.** Rejected:
  origin-matching is wrong at pinch vertices regardless of how many times it is
  run; the rotational rule is required for correctness, not convenience.
- **Fill enclosed regions in a post-merge pass over the whole tiling.**
  Rejected: the information needed (which edges are faceless, the surrounding
  tile angles) is already in hand inside `mergeTilings` step 4; a separate pass
  would re-derive it and risk drifting from the merge's own bookkeeping.
