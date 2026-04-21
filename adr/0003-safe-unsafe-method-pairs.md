# ADR-0003: Paired safe and `Unsafe` methods

- **Status:** Accepted
- **Date:** 2026-04-21

## Context

Many DCEL queries depend on invariants that are either:

1. **Not yet established** — e.g. during construction in `TilingBuilder`,
   half-edges are wired up step by step, so `twin`, `next`, `prev`, and
   `angle` are temporarily `None`.
2. **Already established** by a caller that just ran `validate` or
   constructed the tiling through the private API.

A query like "give me all half-edges around this face" is naturally
`Either[TopologyError, List[HalfEdge]]` in case (1) and a plain
`List[HalfEdge]` in case (2). Hot-path code (notably
`TilingUniformity.uniformityTreeUncompressed` and
`TilingGenerator.expand`) calls this kind of query per vertex per step,
and every `Either` allocation costs.

## Decision

**For queries that can fail only if the DCEL is malformed, provide two
methods: a safe public one and an `Unsafe` private counterpart.**

Naming convention:

- Safe form: `def foo(...): Either[TilingError, A]` — visible in the
  public API, no preconditions beyond what the types already guarantee.
- Unsafe form: `private[dcel] def fooUnsafe(...): A` — assumes the
  invariants are satisfied and either returns directly or throws an
  exception (`NoSuchElementException`, `IllegalStateException`) if they
  aren't.

Representative pairs:

| Safe                                    | `Unsafe` counterpart                              | Location                  |
|-----------------------------------------|---------------------------------------------------|---------------------------|
| `HalfEdge.destination: Option[Vertex]`  | `HalfEdge.destinationUnsafe: Vertex`              | `structure/HalfEdge.scala` |
| `Vertex.incidentEdges`                  | `Vertex.incidentEdgesUnsafe`                      | `structure/Vertex.scala`   |
| `Face.halfEdges`                        | `Face.halfEdgesUnsafe`                            | `structure/Face.scala`     |
| `Face.angles`                           | `Face.anglesUnsafe`                               | `structure/Face.scala`     |
| `TilingDCEL.findVertex`                 | `TilingDCEL.findVertexUnsafe`                     | `TilingDCEL.scala`         |
| `TilingDCEL.getInnerAnglesAtVertex`     | `TilingDCEL.getInnerAnglesAtVertexUnsafe`         | `TilingDCEL.scala`         |

The `Unsafe` variant is almost always `private[dcel]`. When a safe
counterpart doesn't exist (e.g. `boundaryVertices` assumes a well-formed
outer face), the safe version exists as `<name>Safer` and is also
`private[dcel]`, scoped to validation and testing (see
`TilingDCEL.boundaryVerticesSafer` / `boundaryEdgesSafer`).

## Consequences

**Positive**

- Public callers get a safe, compositional API by default. They never see
  `throw`.
- Hot paths can drop into the `Unsafe` lane without allocating `Either`
  wrappers or `Option` maps — the uniformity benchmark depends on this.
- The pair surfaces the distinction — reading a method name and seeing
  `Unsafe` tells the reader: "this assumes invariants; blast radius is
  internal".

**Negative / tradeoffs**

- API surface roughly doubles for entities with many queries
  (≈163 `Unsafe` occurrences across 15 files today). The two versions can
  drift if only one is updated.
- The types don't enforce the precondition. Misusing an `Unsafe` variant
  from a code path that hasn't validated the tiling is a bug that only
  shows up as a runtime exception.
- Reviewers have to grade each new query: does it need a safe/`Unsafe`
  pair, or is a single form enough?

## How to apply

- Add a safe method first. Only add an `Unsafe` counterpart when you have
  a hot path that measurably needs it (profiling or benchmark evidence).
- Keep the `Unsafe` method `private[dcel]` unless you have a concrete
  reason to expose it.
- Document the preconditions inline — what state the receiver must be in.
  "Assumes the DCEL has passed `TilingValidation.validate`" is a common
  and sufficient precondition.
- If you find an `Unsafe` method that no `dcel.*` code uses any more,
  delete it. The asymmetry is only worth carrying while the perf gain is
  real.

## Alternatives considered

- **Always-safe API.** The straightforward choice. Rejected because
  uniformity/gonality walks allocate millions of `Either`/`Option`
  instances per run in realistic benchmarks, and the JIT can't always
  erase them.
- **Always-unsafe API (exceptions only).** Rejected: public callers
  shouldn't have to `try/catch` to perform routine queries, and the
  sealed error taxonomy (ADR-0004) would lose its purpose.
- **A typed "validated tiling" wrapper.** Attractive — a distinct type
  for DCELs that have passed `validate`, carrying the `Unsafe` methods
  only in that scope. Deferred: it's a large refactor and the current
  convention has been sufficient so far.

## Related

- ADR-0002 (deep-copy on mutation — explains why invariants are trusted
  inside private primitives).
- ADR-0004 (Either-based error ADT — what the safe variants return).
