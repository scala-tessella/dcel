# ADR-0002: Mutation via deep-copy on the public boundary

- **Status:** Accepted
- **Date:** 2026-04-21

## Context

A DCEL update (adding a polygon to the boundary, deleting a face, fanning
out a vertex) typically touches dozens of pointers spread across `Vertex`,
`HalfEdge`, and `Face` instances:

- `HalfEdge.{twin, incidentFace, next, prev, angle}` — five mutable
  references per edge (`structure/HalfEdge.scala:26`).
- `Vertex.leaving` — one mutable reference per vertex
  (`structure/Vertex.scala:18`).
- `Face.{outerComponent, innerComponents}` — two mutable references per
  face (`structure/Face.scala:17`).

All of these fields are declared as `var` with `private[dcel]` visibility.
Rebuilding the whole graph in a persistent, allocation-heavy style on every
single polygon insertion would make both `TilingAddition.mergeTilings` and
the search in `TilingGenerator.findTilings` unattractively slow.

But exposing that mutation to callers would make composition dangerous: one
leaked reference, one stale pointer, and the tiling is corrupted for every
other holder of the same `TilingDCEL`.

## Decision

**Treat `TilingDCEL` as a value type on the public boundary, and mutate
privately inside it.**

- `TilingDCEL` is a `final case class` (structural equality, copy-friendly).
- Every public mutating operation is named `maybe<Verb>` and begins with
  `this.deepCopy`, then applies the internal mutating primitive to the
  copy, and returns `Either[TilingError, TilingDCEL]`:

  ```scala
  def maybeAddRegularPolygonToBoundary(
      onEdgeStartingWith: VertexId,
      polygon: RegularPolygon
  ): Either[TilingError, TilingDCEL] =
    this.deepCopy.addRegularPolygonToBoundary(onEdgeStartingWith, polygon)
  ```

  (`TilingDCEL.scala:277`). The same shape is used by
  `maybeAddSimplePolygon*`, `maybeDeleteVertex`, `maybeDeleteEdge`,
  `maybeDeleteFace`, `doubleArea`, and `fanAt`.
- The `private[dcel]` addition/deletion primitives in `TilingAddition` and
  `TilingDeletion` assume exclusive ownership of their input and mutate
  freely.

This means: from the outside, every edit is pure (a new `TilingDCEL`
comes back or an `Either.Left`), and the input is never observed to
change. From the inside, we still write straight-line pointer code.

## Consequences

**Positive**

- Callers never see mutation; they can freely share `TilingDCEL` values
  across threads for *read* operations.
- Writing algorithms inside `TilingAddition`/`TilingDeletion` stays
  ergonomic — no zipper, no persistent-map plumbing.
- Equality and `copy` behave sensibly thanks to the case-class wrapper.

**Negative / tradeoffs**

- Deep-copy cost scales with tiling size, not with the number of entities
  actually touched by the edit. For `TilingGenerator.findTilings`, which
  expands thousands of tilings per run, this is a measurable cost.
  (Current trade is acceptable per the benchmark matrix; see ADR-0008.)
- Invariants live in the *algorithms*, not the *types*: the validity of a
  `TilingDCEL` ultimately depends on the discipline of code under
  `dcel.*`. `TilingValidation.validate` is the backstop.
- Any new public mutation must follow the same `deepCopy` + mutate +
  `Either` pattern. Missing a `deepCopy` would silently break purity of
  the input.

## Alternatives considered

- **Fully persistent representation** (e.g. `Map[VertexId, Vertex]` +
  `Map[HalfEdgeId, HalfEdge]`). Cleanest in principle, but updates
  propagate through all three maps on every edit; the cross-references
  force full traversals to re-stitch. The allocation profile is worse
  than deep-copy for realistic tilings, and the code becomes harder to
  read.
- **Mutable API, no copy.** Rejected: callers can't reason locally about
  whether a helper function corrupted their state.
- **Single global "builder" vs. immutable "snapshot".** Would split the
  API into two types (builder + tiling). The case for one type with a
  copy-on-write public boundary is that downstream code almost always
  wants the tiling value (for SVG export, uniformity trees, etc.), not
  the builder.

## How to apply

- Every new public operation that modifies structure should be named
  `maybe<Verb>`, return `Either[TilingError, TilingDCEL]`, and start with
  `this.deepCopy.<primitive>(…)`.
- New `private[dcel]` primitives may mutate in place — document their
  preconditions and the fact that they consume the receiver.
- If you find yourself wanting to return a modified `TilingDCEL` from a
  public method *without* `deepCopy`, don't — either route it through a
  `maybe` counterpart or make the method `private[dcel]`.

## Related

- ADR-0001 (DCEL representation — explains why the invariants are cross-cutting).
- ADR-0003 (safe vs. `Unsafe` pairs — explains how hot-path code opts out of
  the safe defaults).
