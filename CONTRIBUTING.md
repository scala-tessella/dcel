# Contributing to dcel

Thanks for the interest. This document covers the conventions a PR needs to
follow to be merged; the **why** behind each is recorded in the relevant ADR
under [`adr/`](adr/).

## Build, test, format

```bash
# compile + test on both platforms
sbt dcelJVM/test dcelJS/test

# format + scalafix + tests (the convenient alias)
sbt qa

# Scaladoc — CI runs this; it catches malformed @link references
sbt dcelJVM/doc dcelJS/doc

# Build the docs site locally (output: target/site/, includes Scaladoc)
sbt makeSite

# experimental n-uniform generator (not published; opt-in)
sbt generator/test

# JMH benchmarks (not published; opt-in)
sbt "benchmarks/Jmh/run -prof gc -- UniformityBenchmark"
```

CI (`.github/workflows/ci.yml`) runs the test matrix plus
`scalafmtCheckAll`, `scalafixAll --check`, `dcelJVM/doc`, `dcelJS/doc`, and
the generator test on every push and PR. Releases publish to Maven Central
*and* the docs site to GitHub Pages on push of a `v*` git tag, via
`sbt-ci-release` (`.github/workflows/release.yml`) and `sbt makeSite`
(`.github/workflows/site.yml`) respectively. See
[`adr/0007-cross-platform-targets.md`](adr/0007-cross-platform-targets.md)
for the current platform matrix.

## Code conventions

These four conventions are non-negotiable. Each has an ADR recording the
reasoning, the alternatives considered, and the trigger that would prompt a
revisit.

### 1. Public mutating APIs operate on a deep copy

Every method that *changes* a `TilingDCEL` — `maybeAddRegularPolygonToBoundary`,
`maybeDeleteFace`, `fanAt`, `doubleArea`, … — must deep-copy the input,
mutate the copy, and return a fresh value. The original is never modified.
This makes `TilingDCEL` behave as a value type from the caller's point of
view despite using mutable internals for performance.
See [`adr/0002-deep-copy-on-mutation.md`](adr/0002-deep-copy-on-mutation.md).

### 2. Fallible APIs return `Either[TilingError, A]`

Never throw from a public method that can fail on legitimate input. Wrap the
failure in one of the existing [`TilingError`](shared/src/main/scala/io/github/scala_tessella/dcel/TilingError.scala)
categories (`ValidationError`, `IncompleteError`, `TopologyError`,
`GeometryError`, `SpatialError`, `NotFoundError`). Pick the category that
matches the invariant being broken; if multiple errors need to be combined
into one, use `TilingError.combineErrors`.
See [`adr/0004-either-based-error-handling.md`](adr/0004-either-based-error-handling.md).

### 3. `*Unsafe` methods skip defensive checks and may throw

When the same operation has a safe `Either`-returning version and a fast
version meant for already-validated inputs, name the latter with the
`Unsafe` suffix (e.g. `boundaryVertices` vs `boundaryVerticesUnsafe`,
`getAnglesAtVertex` vs `getAnglesAtVertexUnsafe`). The `Unsafe` version
should be `private[dcel]` whenever possible; only expose it publicly when
a hot path outside the library would otherwise re-pay the validation cost.
See [`adr/0003-safe-unsafe-method-pairs.md`](adr/0003-safe-unsafe-method-pairs.md).

### 4. IDs are opaque types with a string prefix

`VertexId`, `FaceId`, and `HalfEdgeId` are opaque (`opaque type … = Int` or
pair thereof). Their string form goes through the `Prefixable` mixin
(`V1`, `F7`, …). When introducing a new entity-id type, follow the same
pattern.
See [`adr/0006-opaque-ids-and-prefixable.md`](adr/0006-opaque-ids-and-prefixable.md).

## Cross-platform discipline

The library cross-publishes for JVM and Scala.js from a single source tree
under `shared/`. Any new dependency must cross-publish for both targets
(use `%%%`, not `%%`). If a dep is JVM-only and you really need it, justify
the platform drop in a new ADR rather than silently breaking JS support —
the downstream `tessella editor` depends on JS support.

Scala Native is scaffolded but intentionally not wired into the build; the
blocker is Spire's Native availability. See
[`adr/0007-cross-platform-targets.md`](adr/0007-cross-platform-targets.md)
for the unblock criteria.

## ADR practice

`adr/` records architectural decisions in numbered Markdown files with a
consistent shape: Status, Date, Context, Decision, Consequences, Alternatives
considered, Related.

Open a new ADR when you are about to:

- introduce, remove, or change a cross-platform target;
- change the error categorisation, the safe/unsafe pairing rule, or the
  deep-copy contract;
- change how exact arithmetic is done in validation
  (the latest decision is [`adr/0010`](adr/0010-validation-geometry-double.md));
- introduce a new subproject (`benchmarks/`, `generator/`, …) or change
  the publishing setup.

Smaller changes — bug fixes, doc work, internal refactors, dep bumps — do
not need an ADR. When in doubt, look at the existing ADRs: that's the bar.

## Non-published subprojects

Two sbt subprojects live in the repo but are not part of the published
`dcel_3` artifact (both set `publish / skip := true`):

- **`benchmarks/`** — JMH micro-benchmarks
  ([`adr/0008`](adr/0008-jmh-benchmarks-subproject.md)).
- **`generator/`** — experimental n-uniform / n-archimedean tiling
  enumerator. Research-grade, kept out of the artifact until the API and
  search heuristics stabilise. CI still runs its tests so regressions are
  caught.

When working in either subproject, the `Unsafe` / `Either` / deep-copy
conventions above still apply — they are properties of the `dcel` core,
and these subprojects depend on it.

## Submitting a pull request

1. Branch from `main`.
2. Make sure `sbt qa` passes locally (covers format + lint + test).
3. If the change is more than a small bugfix or doc tweak, add a short note
   to the **Unreleased** section of [`CHANGELOG.md`](CHANGELOG.md) under
   the appropriate Keep-a-Changelog heading
   (Added / Changed / Fixed / Performance / Infrastructure).
4. Open the PR against `main`. CI will run the full matrix.

If you're unsure whether something is the right approach, open the PR
anyway — early feedback beats late rework.
