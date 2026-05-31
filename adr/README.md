# Architecture Decision Records

This directory captures the non-obvious architectural decisions behind this
codebase. Each ADR explains *why* a particular choice was made, its tradeoffs,
and what would have to change for it to be revisited.

## Format

Short [MADR](https://adr.github.io/madr/)-style markdown, one file per
decision. Each file has:

- **Status** — `Proposed`, `Accepted`, `Deprecated`, or `Superseded by ADR-NNNN`.
- **Date** — ISO-8601.
- **Context and problem statement** — what forced the decision.
- **Decision** — the one-liner.
- **Consequences** — positive and negative, as concretely as possible.
- **Alternatives considered** — what was rejected and why.

## Index

| #    | Title                                                              | Status                 |
|------|--------------------------------------------------------------------|------------------------|
| 0001 | [DCEL as the core representation](0001-dcel-core-representation.md)                          | Accepted               |
| 0002 | [Mutation via deep-copy on the public boundary](0002-deep-copy-on-mutation.md)               | Accepted               |
| 0003 | [Paired safe and `Unsafe` methods](0003-safe-unsafe-method-pairs.md)                         | Accepted               |
| 0004 | [`Either[TilingError, A]` with a sealed error ADT](0004-either-based-error-handling.md)      | Accepted               |
| 0005 | [Exact arithmetic via `BigDecimal` and Spire `Rational`](0005-exact-arithmetic.md)           | Superseded by ADR-0010 |
| 0006 | [Opaque `VertexId` / `FaceId` with `Prefixable`](0006-opaque-ids-and-prefixable.md)          | Accepted               |
| 0007 | [Cross-platform: JVM + Scala.js shipped, Native blocked](0007-cross-platform-targets.md)     | Accepted               |
| 0008 | [JMH benchmarks in an opt-in subproject](0008-jmh-benchmarks-subproject.md)                  | Accepted               |
| 0009 | [Validation geometry — precision vs. performance](0009-validation-geometry-precision-performance.md) | Superseded by ADR-0010 |
| 0010 | [Validation geometry on `java.lang.Math` + `Double`](0010-validation-geometry-double.md)     | Accepted               |
| 0011 | [Isometric copy operations (mirror / translate / rotate / glide reflect)](0011-isometric-copy-operations.md) | Accepted               |
| 0012 | [Materialise enclosed regions as faces during merge](0012-enclosed-region-faces-on-merge.md) | Proposed               |
| 0013 | [Pinch vertices and merge determinism](0013-pinch-vertices-and-merge-determinism.md) | Proposed               |

## When to add an ADR

Open an ADR when you're about to make a decision that future contributors
cannot re-derive from reading the code:

- A choice between comparable technical options (library, representation, protocol).
- A discipline the code depends on but doesn't enforce (naming conventions,
  mutation contracts, validation boundaries).
- A constraint imposed from outside (dependency that blocks a target, upstream
  breaking change, performance envelope).

Don't open an ADR to document what `git blame` already shows: a bug fix, a
rename, a one-line refactor.

## Lifecycle

- New ADRs start as `Proposed`. Flip to `Accepted` once the decision is in main.
- A decision that gets replaced stays in the repo as `Superseded by ADR-NNNN`
  — don't delete it. The trail is the point.
- If you change your mind about a detail, add a new ADR that references the
  older one. ADRs are append-only.
