# Changelog

All notable changes to this project are documented here.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

First public release. The library has been developed under `0.1.0-SNAPSHOT`
since 2025-07; tagging will publish the current API surface to Maven Central.

### Added

- **Core DCEL representation.** `TilingDCEL` (final case class) modelling
  edge-to-edge tessellations of unit-side polygons as a Doubly Connected Edge
  List. Half-edges, vertices and faces are first-class. IDs are opaque
  (`VertexId`, `FaceId`) and self-format via the `Prefixable` trait
  (`v1`, `f7`, …).
- **Constructors.** `TilingDCEL.createRegularPolygon`,
  `TilingDCEL.createSimplePolygon`; `TilingBuilder.createTriangleNet`,
  `createRhombusNet`, `createHexagonNet`, `createRing`,
  `createHoledTriangleNet`.
- **Growth and removal.** Validation-aware `maybeAddRegularPolygon*`,
  `maybeAddSimplePolygon*`, `maybeDeleteVertex`, `maybeDeleteEdge`,
  `maybeDeleteFace`, plus `fanAt` and `doubleArea`. Every mutating public API
  returns `Either[TilingError, TilingDCEL]` and works on a deep copy
  (the input value is never modified).
- **Analysis.** Boundary and inner-vertex queries, vertex angles,
  `uniformityTree`, `scanUniformityTree`, `gonalityTrees`,
  boundary-equivalence grouping.
- **Validation.** `TilingValidation.validate` plus the narrower
  `validateTopologically`, `validateGeometrically`, `validateSpatially`,
  `validateCompletely`. Errors are surfaced as a `TilingError` ADT
  (`ValidationError`, `IncompleteError`, `TopologyError`, `GeometryError`,
  `SpatialError`, `NotFoundError`).
- **Export.** SVG with optional uniformity colouring, arrows and labels;
  DOT graphs; round-trip serialisation via `toMetadata` /
  `TilingSVG.fromMetadata` embedded inside the SVG.
- **Cross-build.** Published for JVM and Scala.js (CommonJS on Node.js).
  Scala Native is scaffolded but not wired in — see
  [ADR-0007](adr/0007-cross-platform-targets.md) for the blocker.

### Performance

- Validation hot path migrated from Spire `BigDecimal` trigonometry to
  `java.lang.Math.{cos,sin,sqrt,atan2}` on `Double` with epsilon comparison
  — see [ADR-0010](adr/0010-validation-geometry-double.md). Measured ~6.18×
  speedup on the largest acceptance template, zero disagreements across
  15 348 cross-check cases.
- Boundary-canonicalisation uses ring-seq's `bracelet` (Booth's O(n)
  algorithm) instead of the previous O(n²) `rotationsAndReflections.min`
  pattern.

### Infrastructure

- Dual-licensed under Apache-2.0 OR MIT.
- Publishing via `sbt-ci-release` to the Sonatype Central Portal; releases
  are triggered by pushing a `v*` git tag.
- GitHub Actions CI for JVM + Scala.js test matrices and scalafmt /
  scalafix checks.
- Architecture Decision Records (ADRs) under `adr/` document core design
  choices: DCEL representation, deep-copy semantics, safe/unsafe naming,
  `Either`-based errors, exact arithmetic, opaque IDs, cross-platform
  targets, benchmarks, geometry-double validation.
- JMH benchmarks (`benchmarks/`) and an experimental n-uniform tiling
  generator (`generator/`) live in the repo as opt-in, non-published
  subprojects.
