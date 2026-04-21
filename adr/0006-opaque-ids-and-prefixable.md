# ADR-0006: Opaque `VertexId` / `FaceId` with `Prefixable`

- **Status:** Accepted
- **Date:** 2026-04-21

## Context

A `TilingDCEL` indexes its entities by integer ids. Many operations
accept an id and look up the matching entity (`findVertex`, `findFace`,
`maybeDeleteVertex`, `maybeDeleteEdge(start: VertexId, end: VertexId)`).
There is genuine risk in swapping the wrong kind of id at a call site,
and raw `Int` gives no help:

```scala
// Raw Int, everything compiles, runtime error only if lookup fails
def maybeDeleteEdge(start: Int, end: Int): Either[TilingError, TilingDCEL]

deleteSomething(faceId, vertexId) // oops, arguments swapped
```

We also need compact, deterministic string forms for ids in two places:

- Log / error messages (`NotFoundError`).
- SVG metadata used by `TilingSVG.toMetadata` and `fromMetadata`; the
  metadata strings are part of the round-trip contract, so parsing them
  back must not allocate noisily in hot paths.

## Decision

**Use Scala 3 opaque types for ids, with a small `Prefixable` trait for
string conversion.**

```scala
opaque type VertexId = Int
opaque type FaceId   = Int
type HalfEdgeId      = (VertexId, VertexId)
```

(`structure/VertexId.scala`, `structure/FaceId.scala`)

Each id companion extends `Prefixable`, which supplies a short prefix
(`"V"`, `"F"`) and three helpers:

- `prefixedString(i: Int): String` — produces `"V42"` / `"F7"`.
- `fromStringTrusted(s: String): Int` — fast-path parse, assumes the
  input is well-formed.
- `fromStringUntrusted(s: String): Either[ValidationError, Int]` —
  hand-rolled parse that deliberately avoids wrapping in `Try`:
  allocating a `Try` per id during bulk metadata deserialisation was
  measurable, so the method uses `try/catch` around
  `Integer.parseInt` instead.

The types carry an `Ordering` and an extension-based `.value` accessor,
so they compose into `Map`/`Set` with no boxing, and callers get
`VertexId(1) < VertexId(2)` ergonomically.

## Consequences

**Positive**

- Swapping a `VertexId` for a `FaceId` at a call site fails to compile.
- Zero runtime overhead: opaque types erase to `Int` at the JVM level.
- Serialised id strings (`"V1"`, `"F3"`) are self-describing — reading
  an SVG metadata dump tells you which kind of id you're looking at.
- `Prefixable`'s hot-path parse skips the `Try` allocation; empirically
  this matters during `TilingSVG.fromMetadata` where thousands of ids
  are parsed.

**Negative / tradeoffs**

- Opaque types are invisible to Java callers. If we ever ship a Java
  API, ids will need to be re-exposed as wrapper classes.
- An `Ordering` typeclass instance exists per id type; reordering or
  adding new id types means adding the boilerplate each time.
- The `fromStringUntrusted` fast path hand-parses the numeric suffix. If
  the id scheme ever grows beyond `prefix + positive-int`, the parser
  needs updating. The method comment documents the perf rationale.

## How to apply

- New id-like fields get a new opaque type whose companion extends
  `Prefixable` with a unique, uppercase, single-letter (or short)
  prefix. Check `structure/VertexId.scala` as the template.
- Don't leak the backing `Int`: use the `value` extension only when
  crossing a boundary that genuinely needs the underlying int (e.g.
  producing an SVG `id` attribute that also carries other numeric
  suffixes).
- Don't mix id types in a tuple/map key without making the mix explicit;
  `type HalfEdgeId = (VertexId, VertexId)` is fine because both sides
  are the same kind.

## Alternatives considered

- **Raw `Int`.** Zero overhead but zero safety; current-state risk
  described above.
- **`final case class VertexId(value: Int)`.** Type-safe but allocates a
  wrapper per id. Scala 3 opaque types dominate this trade.
- **Value classes (`extends AnyVal`).** Scala 3 opaque types are the
  preferred successor; no need to special-case.
- **Named tuples for composite ids.** Works but has weaker ergonomics
  around pattern matching today; revisit when the Scala 3 named-tuple
  story is more mature.

## Related

- ADR-0004 (`NotFoundError(entity: String, id: String)` consumes
  `toPrefixedString`; keeping that format stable is a cross-cutting
  concern).
