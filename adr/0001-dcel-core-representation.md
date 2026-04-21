# ADR-0001: DCEL as the core representation

- **Status:** Accepted
- **Date:** 2026-04-21

## Context

The library models edge-to-edge tessellations of unit-side polygons and
supports all of the following as first-class operations:

- Face insertion and deletion on a growing tiling
  (`TilingAddition.mergeTilings`, `TilingDeletion.deleteFace`).
- Boundary walks (`TilingDCEL.boundaryVertices`, `boundaryEdges`).
- Uniformity/gonality analysis, which repeatedly takes a local patch around
  each vertex (`TilingUniformity.uniformityTree`, `scanUniformityTree`).
- Round-trip export to SVG/DOT and reconstruction from SVG metadata.

All of these need constant-time navigation between neighbouring vertices,
edges, and faces. They also need a clear, single representation of the
"outside" of the tiling (the outer face).

## Decision

Represent every tiling as a **Doubly Connected Edge List (DCEL)**: each
undirected edge is split into two oppositely oriented half-edges; each
half-edge carries pointers to its origin vertex, its incident face, its
twin, its successor (`next`), and its predecessor (`prev`). A single
`outerFace` represents the unbounded region; inner faces are held in a
separate list.

Concretely:

- `structure.HalfEdge` holds the five pointers plus an interior angle.
- `structure.Vertex` holds a `leaving` half-edge.
- `structure.Face` holds an `outerComponent` half-edge (plus optional
  `innerComponents` for holes — unused in practice, see ADR-0002).
- `TilingDCEL` is the case-class wrapper over the four lists.

## Consequences

**Positive**

- O(1) traversal of the five neighbour relations on any half-edge.
- Face iteration reduces to walking `next` pointers until the start edge
  reappears — the same primitive powers boundary walks, angle sums, and
  area calculation (`Face.halfEdges`, `faceTraversal`).
- The outer face is an explicit, named entity (`FaceId.outerId`), which
  makes boundary checks (`TilingDCEL.isBoundaryEdge`) trivial.
- DCEL is the textbook representation for planar subdivisions (de Berg,
  *Computational Geometry*); newcomers with a CG background recognise it.

**Negative / tradeoffs**

- Memory: each undirected edge is stored as two half-edges, roughly
  doubling edge-level allocation vs. an undirected-edge representation.
- Bookkeeping: inserting/removing a single polygon touches `next`, `prev`,
  `twin`, `incidentFace`, and `leaving` on multiple entities; mistakes are
  hard to diagnose. See ADR-0002 for how mutation is contained, and
  `TilingValidation` for the invariants that back the algorithms.
- The invariants are not enforced by the types — they are maintained by
  the algorithms and checked by `TilingValidation.validate`.

## Alternatives considered

- **Winged-edge.** Similar expressive power but the API is harder to
  explain (one edge struct, four pointers per orientation) and less
  natural for orientable planar tilings where we already pay for a
  normalised direction.
- **Quad-edge.** Strictly more general (supports non-orientable surfaces
  and the planar dual for free) but a heavier data model than we need —
  every operation carries `rot`/`flip` indirections.
- **Face–edge list / simplex arrays.** Cheap to store but O(n) to answer
  questions like "which faces share this vertex?" — the exact queries the
  uniformity analysis makes in a hot loop.
- **Persistent combinatorial maps.** Appealing in principle, but we
  couldn't find a Scala implementation with the right performance profile;
  the benefits would largely be shadowed by the deep-copy-on-mutation
  contract in ADR-0002.

## References

- M. de Berg et al., *Computational Geometry: Algorithms and Applications*,
  §2 (planar subdivisions).
