# ADR-0015: Largest parallelogon contained in a tiling

- **Status:** Accepted
- **Date:** 2026-06-06

> Implemented in `TilingLattice.scala` / `TilingLatticeSpec.scala`. Where the
> build refined the plan below, see **Implementation notes (as built)**.

## Context and problem statement

We want a public query on a tiling:

> Given a finite patch, return the **largest parallelogon it contains**, as the
> ordered corner vertices (4 or 6) where the parallelogon's sides meet.

The motivation is *semantic*: a parallelogon — in this codebase's sense
(`SimplePolygon.parallelogonIndices`, see "definition note" below) — tiles the
plane by translation, carrying its internal faces. So the largest parallelogon
contained in a patch is **the infinite tessellation the patch tends to**. If a
patch is 99% a `4.8.8` arrangement plus a few stray triangles welded at the
boundary, its largest contained parallelogon is a big `4.8.8` super-cell — the
triangles are excluded because including them breaks the parallelogon
condition — and the answer correctly reads "this is `4.8.8`".

The trivial case is already solved. When the patch's *own* boundary is a
parallelogon, `boundarySimplePolygon.parallelogonIndices`
(`TilingDCEL.scala:300`) returns its corner indices directly. This ADR is about
the general case, where the outer boundary is **not** a parallelogon but a
sub-region is.

### Definition note (why this isn't the textbook parallelogon)

`parallelogonIndices` does **not** implement the strict convex definition
(parallelogram or centrally-symmetric hexagon). It implements the general
**translational-prototile** condition: the boundary splits into three pairs of
opposite arcs that fit antiparallel, where each arc (a "side") may be a
*polyline* and the shape may be *non-convex*. The `4.8.8` fundamental domain —
one octagon sharing an edge with one square, the non-convex decagon
`SimplePolygon(90,90,225,135,135,135,135,135,135,225)` (the "bulb" test in
`SimplePolygonSpec`) — is therefore a valid 6-corner parallelogon:
`parallelogonIndices` returns `List(0,1,3,5,6,8)`. The "comma", "devil",
"carved", and joined-hexagon tests confirm the breadth of non-convex shapes that
qualify. Any reasoning about this feature must be checked against those tests,
not against the convex definition.

## Decision

**Define the answer as the maximum-area, whole-face, simply-connected
sub-region whose boundary is a parallelogon, and return its ordered corner
vertices.** "Largest" is the correctness criterion, not a preference (see
Alternatives). Scope of this ADR: **periodic patches**, **whole-face
(combinatorial) containment**, **both 4- and 6-corner** parallelogons.

```
// extensions on TilingDCEL (in object TilingLattice, like TilingSymmetry / TilingUniformity)
def largestContainedParallelogon: Option[List[Vertex]]        // headline: ordered 4/6 corner vertices
def largestContainedParallelogonBlock: Option[ParallelogonBlock] // the same block: cell dimensions + area
def translationLattice: Option[(BigPoint, BigPoint)]          // the reduced primitive basis
```

`Option`, not `Either[TilingError, _]` (cf. ADR-0004): "no parallelogon found"
is a legitimate empty result for a query, not a failure — consistent with
`parallelogonIndices` returning `Nil`. The returned list is the ordered corner
`Vertex`es (4 or 6); the caller reads side decomposition, if needed, the same
way `parallelogonIndexClasses` already exposes it.

### Algorithm

1. **Whole-boundary fast path.** If `boundarySimplePolygon.parallelogonIndices`
   is non-empty, map those boundary indices to vertices and return — the patch
   *is* its own largest contained parallelogon.

2. **Detect the translation lattice `{v, w}`** *(new primitive — see Risks).*
   A finite patch has no true translational self-symmetry (a shift pushes the
   boundary off the patch), so detect the *underlying* lattice from interior
   structure (see Implementation notes for what was actually built):
   - Type each interior vertex by an orientation-aware **signature** — the sorted
     directions to its neighbours — and keep the single most populous class (one
     translation orbit), whose pairwise differences are all genuine lattice
     vectors.
   - Validate each candidate `t` over the whole interior: every vertex it lands
     on must coincide (within `ACCURACY = 1e-10`) with a same-signature vertex, a
     few welded-face defects tolerated.
   - Lagrange–Gauss reduce the two shortest independent periods to the primitive
     basis.
   - No valid basis ⇒ not periodic ⇒ return the fast-path result or `None`.

3. **Reduce to a maximal lattice-cell block.** In lattice coordinates `{v, w}`,
   the patch's faces partition into fundamental-domain cells; stray boundary
   triangles fall outside any complete cell. The largest contained parallelogon
   is the largest lattice-aligned block of fully-occupied cells — the classic
   **maximal rectangle in a binary grid** (histogram method, near-linear).

4. **Extract corners.** Build the geometric boundary of the winning block (a
   polyline along tiling edges, with the block interior angle at each boundary
   vertex = the sum of incident block-face corner angles), run it through
   `parallelogonIndices` to obtain the 4/6 corner indices, map those to the
   actual boundary `Vertex`es, and rotate to start at the lowest corner (by
   coordinates) for determinism. These corners are genuine tiling vertices.

Crucially, step 4 does **not** pre-decide 4 vs 6 corners: the block boundary is
whatever it is, and `parallelogonIndices` classifies it (the `4.8.8` doubled
bulb stays a 6-corner parallelogon).

## Consequences

### Positive

- Reuses the proven detection primitive (`parallelogonIndices`) and
  `boundarySimplePolygon` rather than introducing parallel geometry; the
  orientation-aware vertex signature is local and self-contained.
- The new `translationLattice` detector is independently useful: ADR-0011's
  "translate a periodic tiling by a lattice vector" extension currently relies
  on a hand-picked vector; a detector supplies it.
- Well-defined semantics: the result is the limit tessellation, and boundary
  defects are excluded by construction, not by ad-hoc cleanup.

### Negative / risks

- **Lattice detection is the linchpin and is new.** Periodicity of a *finite*
  patch is necessarily heuristic (no exact translational symmetry exists on a
  finite set); correctness rests on the overlap-match validation. This is the
  step to de-risk first (see Build order).
- **Non-periodic patches.** When no lattice is found the method falls back to
  the whole-boundary result or `None`. A general O(V³) max-area search over
  arbitrary corner/direction triples (the aperiodic case) is deliberately
  **out of scope here** and left as a later extension.
- **"Largest" is generally non-unique** (translated copies of equal area) —
  resolved by the `VertexId` tie-break, documented as arbitrary-but-stable.
- **Holes/defects.** A candidate region with an interior defect is not
  simply-connected and is disqualified; the cell-block search must require full
  occupancy (no interior gaps), not merely a parallelogon outline.
- **Float error** enters only the coincidence test in lattice validation, at the
  established `1e-10` threshold (ADR-0010 conventions); cell membership is
  otherwise combinatorial.

## Testing strategy

- **Clean `4.8.8` patch** (built by merging copies of the bulb) → largest
  parallelogon is the whole patch; corners match
  `boundarySimplePolygon.parallelogonIndices` mapped to vertices.
- **`4.8.8` + a few boundary triangles** → result is the interior `4.8.8` block,
  triangles excluded; assert the corner count and that the block's side-vectors
  are lattice periods (it reproduces `4.8.8`, not `4.4.4.4`).
- **Single fundamental domain** (one bulb) → returns its 6 corners, matching the
  existing `bulb.parallelogonIndices == List(0,1,3,5,6,8)`.
- **Square grid** → returns the whole grid (a 4-corner parallelogram).
- **Lattice detector units** — recovers the known basis on clean periodic
  patches; returns `None` on a deliberately non-periodic patch.
- **Property** (`PropertyBasedDCELSpec` style) — any `Some(corners)` result has a
  boundary that passes `parallelogonIndices` and bounds a whole-face,
  simply-connected region.
- **Visual regression** — lock results with `.svg` resources, as the parallelogon
  suite already does (`toParallelogonTiling`).

## Build order

Smallest-risk-first:

1. **`translationLattice` detector + tests** — the one genuine unknown; prove it
   recovers the square lattice on a clean `4.8.8` patch and the
   triangle-perturbed variant before building anything on top.
2. **Lattice-coordinate cell map + maximal-block search** (maximal rectangle in a
   grid).
3. **Boundary extraction → `parallelogonIndices` validation → corner→`Vertex`
   mapping**, with the tie-break.
4. **Public `largestContainedParallelogon`** on `TilingDCEL`, with the
   whole-boundary fast path wired in first.
5. *(Later, separate ADR)* aperiodic general search; richer return exposing the
   side decomposition for the editor.

## Implementation notes (as built)

Where the build refined the plan:

- **Vertex typing is an orientation-aware edge-direction signature, not
  `regularPolygonsUnsafeFrom`.** Polygon-multiset typing cannot separate the two
  `[6,6,6]` orientation classes of `6.6.6` (the two honeycomb sublattices); the
  sorted set of neighbour directions can. Detection then reads the lattice off
  the single most populous signature class — one translation orbit — so every
  candidate difference is already a genuine lattice vector.
- **Point-in-polygon rejection (a listed risk) turned out moot.** Because
  candidates are within-orbit differences, they are genuine periods and never
  drop a vertex into a face interior. The remaining soundness check is structural
  (signature-preserving on the overlap, defect-tolerant) over the whole interior,
  which also rejects coincidences that break non-dominant vertex types.
- **A "common-signature ≥ k" defect filter was tried and rejected.** Welded
  foreign faces create *repeated* junction vertices that reached the threshold
  and re-vetoed the primitive period (2×/4× lattices). Restricting to the single
  dominant orbit fixed it.
- **Tie-break is by corner coordinates, not `VertexId`** (the opaque id has no
  exposed ordering): rotate the cycle to start at the lowest `(x, y)` corner.
- **Validation coverage.** The lattice + block + corner pipeline is validated on
  `3⁶`, `4⁴`, and `6⁶` nets — each clean and with foreign squares / triangles /
  hexagons welded on — plus the whole-boundary fast path on single units (a lone
  square, a regular hexagon). A genuine `4.8.8` patch was **not** built (no cheap
  constructor in `shared`); `4.8.8` is covered indirectly by the existing
  `SimplePolygon` "bulb" tests for `parallelogonIndices`.
- **Gotcha:** `.view.mapValues(...).toMap` is a lazy `MapView` that re-evaluated
  nondeterministically here; cell occupancy uses a strict `map` instead.

The deferred items (aperiodic O(V³) search; a second-ring signature for the
unlikely two-orbits-share-a-first-ring-signature case) remain future work.

## Alternatives considered

- **Return the smallest / fundamental parallelogon instead.** Rejected: the
  *smallest* contained parallelogon is a lone square face, whose translational
  tiling is `4.4.4.4` — a sub-motif that misrepresents a `4.8.8` patch. Smallest
  does not capture the limit tessellation; largest does. This is the core reason
  the feature is framed around "largest".
- **Detect the exact fundamental domain and report it.** Rejected: maximising
  area over *all* contained parallelogons is more robust and excludes boundary
  defects automatically, without a separate fundamental-domain identification
  step (and the fundamental domain is recovered anyway as the unit the maximal
  block is built from).
- **Geometric (face-cutting) containment.** Rejected (consistent with the DCEL
  model): corners must be tiling vertices and sides must run along tiling edges.
- **`Either[TilingError, _]` return.** Rejected for this query: absence of a
  parallelogon is not an error. ADR-0004's discipline applies to fallible
  operations, not to lookups that can legitimately be empty.
- **General O(V³) search as the v1 algorithm.** Deferred: the periodic case via
  lattice detection is tractable now and covers the motivating use; the general
  search is a later, separable addition.
