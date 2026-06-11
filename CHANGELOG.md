# Changelog

All notable changes to this project are documented here.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed (breaking — next release starts the 0.2.x line)

- **`Tiling`: the certified, validated tiling type.** New opaque subtype of
  `TilingDCEL` proving its value passed full validation — the compiler-checked
  form of the safe/`Unsafe` convention. Public constructors and `fromMetadata`
  now return `Tiling` (source-compatible narrowing); the seventeen mutating
  operations (`maybeAdd*`, `maybeDelete*`, `doubleArea`, `fanAt`, `fanAround`,
  `repeatAlong`, `repeatGrid`) moved from `TilingDCEL` to `Tiling` with
  unchanged names and parameters, returning `Either[TilingError, Tiling]`.
  Editor migration: hold `Tiling` as state, start from `Tiling.empty`, load
  with `fromMetadata`, certify foreign DCELs with `Tiling.from`. Code breaks
  only where a mutated value was explicitly annotated `: TilingDCEL` — change
  the annotation to `Tiling`. Every query keeps working on both types. See
  [ADR-0017](adr/0017-validated-tiling.md).
- **Hidden-throw queries scoped to `Tiling`.** `gonalityTrees` and
  `boundarySimplePolygon` are now total queries on `Tiling` (no longer on the
  raw type); `gonalityTreesUnsafe` is renamed `gonalityTreesWithPolygons`.
  `getPathUnsafe`, `maybePathUnsafe` and `adjacencyMapUnsafe` are no longer
  public.

- **SVG export split by concern.** The `SimplePolygon` rendering extensions
  (`toScalableVectorG`, `toParallelogonTiling`) moved from `TilingSVG` to the
  new `SimplePolygonSVG`, and `toUniformityAnimation` moved to the new
  `SvgAnimation` — update imports accordingly. `TilingSVG` keeps the static
  tiling renderer, `SvgOptions` and the metadata round-trip.
- **`setOuterEdgeAngles` removed from the public surface.** It was a
  package-internal structural mutator that leaked onto `TilingBuilder`; it now
  lives package-private alongside the other `List[HalfEdge]` wiring helpers.

### Changed

- **Hardened growth, uniformity and traversal internals.** Broken-topology
  states in the growth pipeline now surface as `TopologyError` instead of
  throwing (shared-edge traversal, boundary-intersection reporting); the
  uniformity analysis builds its local DCELs in stable id order rather than
  hash iteration order (extending the determinism discipline of
  [ADR-0013](adr/0013-pinch-vertices-and-merge-determinism.md)); accidental
  quadratics removed from boundary-path collection and intersection
  decoding. No public-API change.

### Infrastructure

- **Construction benchmark.** New JMH suite timing the net builders
  end-to-end — the polygon-by-polygon growth pipeline, previously the one
  hot path no benchmark measured — with a committed 30-iteration JSON
  baseline for before/after comparisons (see `benchmarks/RUNBOOK.md`).
- **ADR-0016.** Scala Native remains blocked: Spire publishes no Native 0.5
  artifact while the rest of the dependency set is Native 0.5-only. Decision:
  keep waiting (no demand), with the Spire-replacement contingency documented.
  See [ADR-0016](adr/0016-native-wait-for-spire.md).

## [0.1.4] — 2026-06-11

### Added

- **Largest contained parallelogon.** New query
  `largestContainedParallelogon` (extension on `TilingDCEL`, in
  `TilingLattice`) returning the ordered corner vertices (4 or 6) of the
  largest parallelogon contained in a finite patch — the translational unit
  of the infinite tessellation the patch tends to. Stray boundary faces that
  break the parallelogon condition are excluded by construction; when the
  patch's own boundary is already a parallelogon, its corners are returned
  directly. Returns `None` when the patch neither is nor contains one. See
  [ADR-0015](adr/0015-largest-contained-parallelogon.md).
- **Translation-lattice detection.** `translationLattice` returns the
  primitive translation basis `(v, w)` of a periodic patch
  (Lagrange–Gauss reduced, sign-canonicalised), detected from interior
  structure via orientation-aware vertex signatures and robust to a few
  welded foreign faces. Independently useful as a periodicity test and as
  the lattice-vector supplier for the translated-copy operation. See
  [ADR-0015](adr/0015-largest-contained-parallelogon.md).

## [0.1.3] — 2026-06-02

### Fixed

- **Multi-pinch enclosures.** A merge that encloses two regions meeting at a
  shared pinch vertex — e.g. an M/W pentomino reflected across its base, which
  traps two unit squares that touch at one point — now returns a valid tiling
  instead of `Left`. Each enclosed region's pinch corners get exact, rational
  interior angles (read from the geometry and snapped to the canonical tiling
  cosines), so the angle-based validation closes. Generalises the single-pinch
  handling of 0.1.2; no public-API change. See
  [ADR-0014](adr/0014-multi-pinch-enclosed-region-angles.md).

## [0.1.2] — 2026-05-31

### Added

- **Enclosed regions become faces.** When welding a copy onto a tiling encloses
  a region that was a face in neither operand — e.g. reflecting a three-pentagon
  cluster traps a 36°–144° rhombus — that region is now materialised as a new
  inner face (the model has no holes — "holes are just inner polygons") and the
  result validates, where it previously failed. Benefits every copy operation.
  See [ADR-0012](adr/0012-enclosed-region-faces-on-merge.md).
- **Pinch vertices.** Enclosures that touch the rest of the tiling at a single
  vertex — e.g. a T-pentomino rotated 90°/270° onto its base cell, trapping a
  unit square — are traced and materialised correctly, with an exact rational
  corner angle at the pinch. See
  [ADR-0013](adr/0013-pinch-vertices-and-merge-determinism.md).

### Changed

- **`mergeTilings` is now deterministic.** The connectivity rebuild is driven
  from the geometric input rather than `HashMap` iteration order, so the merged
  tiling is a pure function of its operands — removing a latent source of
  per-run flakiness from *all* copy operations, not only pinches. See
  [ADR-0013](adr/0013-pinch-vertices-and-merge-determinism.md).

## [0.1.1] — 2026-05-31

### Added

- **Isometric copy operations.** Grow a tiling by welding a transformed copy of
  itself onto it, validated in full: `maybeAddTranslatedCopy`,
  `maybeAddRotatedCopy`, `maybeAddMirroredCopy`, `maybeAddGlideReflectedCopy`,
  and the underlying `maybeAddCopy(Isometry)`. The copy is accepted only when it
  lands outside the existing tiling or exactly follows its composition
  (coincident vertices unified, shared edges collapsed, overlapping faces
  deduplicated). See
  [ADR-0011](adr/0011-isometric-copy-operations.md).

## [0.1.0] — 2026-05-17

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
