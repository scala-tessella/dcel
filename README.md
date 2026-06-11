# dcel

[![Maven Central](https://img.shields.io/maven-central/v/io.github.scala-tessella/dcel_3.svg?label=Maven%20Central)](https://central.sonatype.com/artifact/io.github.scala-tessella/dcel)
[![Scala 3](https://img.shields.io/badge/scala-3.8.4-red.svg)](https://scala-lang.org)
[![Scala.js](https://img.shields.io/badge/scala.js-1.21.0-blue.svg)](https://www.scala-js.org)
[![CI](https://github.com/scala-tessella/dcel/actions/workflows/ci.yml/badge.svg)](https://github.com/scala-tessella/dcel/actions/workflows/ci.yml)
[![Scaladoc](https://img.shields.io/badge/docs-scaladoc-blue.svg)](https://scala-tessella.github.io/dcel/)
[![License](https://img.shields.io/badge/license-Apache--2.0%20OR%20MIT-green.svg)](#license)

**Docs:** <https://scala-tessella.github.io/dcel/>

DCEL utilities for representing, building, editing and analysing **edge-to-edge
tessellations of unit-side polygons** in Scala 3.

Tessellations are modelled as a **Doubly Connected Edge List (DCEL)**: a
classical planar-subdivision representation in which every edge is split into
two oppositely oriented *half-edges*, and each half-edge knows its origin
vertex, its incident face, its twin, its predecessor, and its successor. On top
of this base representation, the library provides builders, validation,
topology/geometry operations, symmetry/uniformity/lattice analysis, and
import/export.

The primary public type is **`Tiling`** — a tessellation *certified* to satisfy
the full DCEL invariants. Constructors return it, all mutating operations live
on it, and holding one is a compile-time proof of validity (see
[ADR-0017](adr/0017-validated-tiling.md)). The raw `TilingDCEL` structure stays
available for uncertified data and read-only queries.

> **Status:** early development (pre-1.0). The API may still change between
> minor versions; 0.2.0 introduced breaking changes over 0.1.x (see the
> [changelog](CHANGELOG.md)).

## Tessella editor

The Tessella web and desktop editor, see https://tessell.art/editor (open here the [web editor](https://editor.tessell.art)), is built on the **dcel** library.

## Setup

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "io.github.scala-tessella" %% "dcel" % "0.2.0"
// Use %%% instead of %% for Scala.js
```

Published for **Scala 3** on the **JVM** and **Scala.js**. Scala Native is
scaffolded but blocked: Spire publishes no artifact for the Scala Native 0.5
ecosystem (see [ADR-0016](adr/0016-native-wait-for-spire.md)).

## Features

- **A certified tiling type.** `Tiling` proves its value passed full
  validation: get one from any constructor, from `Tiling.empty` (the blank
  canvas), or by certifying a raw `TilingDCEL` with `Tiling.from`. Every
  mutating operation takes and returns `Tiling`, so validity is preserved by
  the type system rather than by convention.
- **Construct** tilings from regular or simple unit-side polygons
  (`TilingDCEL.createRegularPolygon`, `TilingDCEL.createSimplePolygon`).
- **Build lattices** quickly: triangle, rhombus/square, and hexagon nets, rings,
  and holed triangle nets (`TilingBuilder.createTriangleNet`,
  `createRhombusNet`, `createHexagonNet`, `createRing`, `createHoledTriangleNet`).
- **Grow and shrink** tessellations with validation-aware APIs:
  `maybeAddRegularPolygon*`, `maybeAddSimplePolygon*`, `maybeDeleteVertex`,
  `maybeDeleteEdge`, `maybeDeleteFace`, `fanAt`, `doubleArea`.
- **Multiply by isometries**: weld a transformed copy of the whole tiling onto
  itself — `maybeAddTranslatedCopy`, `maybeAddRotatedCopy`,
  `maybeAddMirroredCopy`, `maybeAddGlideReflectedCopy` (or `maybeAddCopy` with
  an `Isometry`) — plus full rings and strips: `fanAround`, `repeatAlong`,
  `repeatGrid`.
- **Detect periodicity**: `translationLattice` recovers the primitive
  translation basis of a periodic patch, and `largestContainedParallelogon`
  returns the corner vertices of the largest translational unit the patch
  contains — the infinite tessellation it tends to.
- **Analyse** topology, symmetry and uniformity: boundary, inner vertices,
  vertex angles, `uniformityTree`, `scanUniformityTree`, `gonalityTrees`,
  boundary-equivalence grouping, rotational/reflectional symmetry.
- **Validate** completeness, topology, geometry, and spatial consistency in
  one call (`TilingValidation.validate`). Failures are surfaced as a sealed
  `TilingError` ADT (`IncompleteError`, `TopologyError`, `GeometryError`,
  `SpatialError`, `ValidationError`, `NotFoundError`).
- **Export** to SVG (with optional uniformity colouring, arrows, labels) and
  DOT, animated uniformity refinement (`SvgAnimation`), single-polygon and
  parallelogon-tiling renders (`SimplePolygonSVG`), plus round-trip
  serialisation through SVG metadata (`toMetadata` / `TilingSVG.fromMetadata`).

## Tech Stack

| Item                                | Version / Status                              |
|-------------------------------------|-----------------------------------------------|
| Scala                               | 3.8.4                                         |
| Published artifact                  | `"io.github.scala-tessella" %% "dcel" % "0.2.0"` (Maven Central) |
| SBT                                 | 1.12.11                                       |
| Platforms                           | JVM, Scala.js (CommonJS on Node.js)           |
| Scala Native                        | scaffolded; blocked — no Spire artifact for Native 0.5 ([ADR-0016](adr/0016-native-wait-for-spire.md)) |
| Core deps                           | [ring-seq](https://github.com/scala-tessella/ring-seq) `0.8.0`, [iron](https://github.com/Iltotore/iron) `3.3.1`, [spire](https://typelevel.org/spire) `0.18.0` |
| Test deps                           | ScalaTest `3.2.20`, ScalaCheck `1.19.0`        |

## Project Layout

```
dcel/
├── build.sbt                  # Cross-project (JVM + JS) build and compiler flags
├── project/plugins.sbt        # sbt-scalajs, sbt-scalafmt, sbt-scalafix, sbt-ci-release, …
├── adr/                       # Architecture Decision Records (design rationale)
├── shared/src/main/scala/io/github/scala_tessella/dcel/
│   ├── Tiling.scala             # The certified tiling type: trust boundary + all mutators
│   ├── TilingDCEL.scala         # Raw DCEL case class; read-only queries
│   ├── TilingBuilder.scala      # Polygon/net/ring constructors (validation facade)
│   ├── TilingNetBuilder.scala   # Lattice-construction plumbing (package-private)
│   ├── TilingAddition.scala     # Polygon-by-polygon growth pipeline
│   ├── TilingGrowthWiring.scala # Low-level DCEL surgery behind growth (package-private)
│   ├── TilingDeletion.scala     # Removal primitives
│   ├── TilingMerge.scala        # Tiling union: coincidence matching, enclosed regions
│   ├── TilingMultiplication.scala # Copy-based growth: isometric copies, fans, repeats
│   ├── Isometry.scala           # Translation / Rotation / Reflection / GlideReflection
│   ├── TilingValidation.scala   # Completeness, topology, geometry, spatial checks
│   ├── TilingEquivalency.scala  # Deep copy, boundary signature & equivalence grouping
│   ├── TilingUniformity.scala   # Uniformity/gonality trees
│   ├── TilingSymmetry.scala     # Rotational/reflectional symmetry queries
│   ├── TilingLattice.scala      # Translation-lattice detection, largest parallelogon
│   ├── TilingError.scala        # Sealed error ADT + helpers
│   ├── Tree.scala               # Algebraic Leaf/Branch tree used for uniformity
│   ├── geometry/                # BigDecimal-based primitives (see below)
│   ├── structure/               # DCEL entities: Vertex, HalfEdge, Face, IDs, Prefixable
│   └── conversion/              # SVG/DOT exporters, animation, SVG-metadata round-trip
├── shared/src/test/scala/…      # Unit + ScalaCheck property tests
├── shared/src/test/resources/   # SVG fixtures used by tests
├── benchmarks/                  # JMH suites + RUNBOOK.md (opt-in, unpublished)
├── generator/                   # Experimental n-uniform tiling search (opt-in, unpublished)
└── README.md
```

### Non-published subprojects

Two opt-in sbt subprojects live in this repo but are not part of the published
`dcel_3` artifact:

- **`benchmarks/`** — JMH micro-benchmarks for the construction, validation,
  uniformity, and copy-operation hot paths, with committed JSON baselines for
  before/after comparison. See `benchmarks/RUNBOOK.md`; run with
  `sbt 'benchmarks/Jmh/run …'`.
- **`generator/`** — research-grade enumerator of n-uniform n-archimedean
  tilings (`TilingGenerator.findTilings`, `expandRotationally`,
  `expandRotationallyMore`). API and search heuristics are still in flux;
  several exploratory tests are disabled. Run with `sbt generator/test`.
  When the surface stabilises, this may be promoted to its own published
  artifact.

### Packages

| Package                                              | Role |
|------------------------------------------------------|------|
| `io.github.scala_tessella.dcel`                      | Core: `Tiling` (certified type + mutators), `TilingDCEL` (raw structure + queries), builders, validation, analysis, errors |
| `io.github.scala_tessella.dcel.geometry`             | Deterministic `BigDecimal`/Spire primitives: `BigPoint`, `BigLineSegment`, `BigBox`, `BigRadian`, `AngleDegree`, `RegularPolygon`, `SimplePolygon`, `Orientation`, `SpatialGrid`, `IntersectionDetection` |
| `io.github.scala_tessella.dcel.structure`            | `Vertex`, `HalfEdge`, `Face` + opaque `VertexId`/`FaceId`, `Prefixable` |
| `io.github.scala_tessella.dcel.conversion`           | `TilingSVG` (with `SvgOptions`), `SimplePolygonSVG`, `SvgAnimation`, `TilingDOT`, SVG-metadata round-trip |

Only the root package depends on the other three; `conversion` depends on
everything, `structure` depends on `geometry`, `geometry` has no internal deps.

## Building from Source

### Prerequisites

- JDK 17 or later (CI runs on Temurin 17).
- sbt (`1.12.11` used by this project; launcher will pick up the version).
- Node.js on `PATH` for the Scala.js test runner.

### Build and Test

```bash
# compile all modules
sbt compile

# run all tests
sbt test

# run target-specific tests
sbt dcelJVM/test
sbt dcelJS/test

# format + scalafix + tests (alias defined in build.sbt)
sbt qa
```

## Usage

### 1. Create and grow a tiling

```scala
import io.github.scala_tessella.dcel.{Tiling, TilingDCEL}
import io.github.scala_tessella.dcel.geometry.RegularPolygon
import io.github.scala_tessella.dcel.structure.VertexId

val base: Tiling = TilingDCEL.createRegularPolygon(RegularPolygon(6))

val grownEither = // Either[TilingError, Tiling]
  base.maybeAddRegularPolygonToBoundary(
    onEdgeStartingWith = VertexId(1),
    polygon            = RegularPolygon(3)
  )

val svgEither = grownEither.map(_.toSVG(scale = 80.0))
```

Every mutating operation lives on the certified `Tiling` type, returns an
`Either[TilingError, Tiling]`, and operates on an internal deep copy — the
input value is never modified. Start an editor session from `Tiling.empty`,
or certify a hand-assembled `TilingDCEL` with `Tiling.from` (which runs the
full validation).

### 2. Build a net with `TilingBuilder`

```scala
import io.github.scala_tessella.dcel.TilingBuilder

val rhombusNetEither   = TilingBuilder.createRhombusNet(width = 3, height = 3)
val holedTriangleEither =
  TilingBuilder.createHoledTriangleNet(10, 10): (x, y) =>
    x > 2 && x < 7 && y > 2 && y < 7
```

### 3. Multiply by isometries

```scala
// Mirror a pentagon across one of its edges, then fan the cluster:
// the six-pentagon Dürer cluster in two calls.
val pentagon = TilingDCEL.createRegularPolygon(RegularPolygon(5))
val coords   = pentagon.coordinates

val duerer =
  for
    two <- pentagon.maybeAddMirroredCopy(coords(VertexId(2)), coords(VertexId(3)))
    all <- two.fanAround(pentagon.coordinates.values.toList.centroid, 5)
  yield all
```

### 4. Detect periodicity

```scala
import io.github.scala_tessella.dcel.TilingLattice.{largestContainedParallelogon, translationLattice}

val netEither = TilingBuilder.createHexagonNet(6, 6)
val basis     = netEither.map(_.translationLattice())             // primitive lattice basis
val corners   = netEither.map(_.largestContainedParallelogon())   // ordered corner vertices
```

### 5. Analyse uniformity

The uniformity operations live as extension methods in `TilingUniformity`:

```scala
import io.github.scala_tessella.dcel.TilingUniformity.*

val tree         = base.uniformityTree
val uncompressed = base.uniformityTreeUncompressed()
val scan         = base.scanUniformityTree
```

### 6. Export topology / graphics

```scala
import io.github.scala_tessella.dcel.conversion.TilingSVG.SvgOptions

val dot: String = base.toDOT
val svg: String = base.toSVG(SvgOptions(scale = 80.0, showUniformity = true))
```

### 7. Serialise and reconstruct metadata

```scala
import io.github.scala_tessella.dcel.conversion.TilingSVG.{fromMetadata, toMetadata}

val metadata      = base.toMetadata               // <tessella:tessella-dcel> XML element
val reconstructed = fromMetadata(metadata)        // Either[TilingError, Tiling]
```

## API Notes

- Public fallible APIs generally return `Either[TilingError, A]`.
- `TilingError` is a sealed trait with `ValidationError`, `IncompleteError`,
  `TopologyError`, `GeometryError`, `SpatialError`, and `NotFoundError`
  alternatives, plus a `TilingError.combineErrors` helper.
- Validity lives in the type system: a `Tiling` is proof its value passed
  `TilingValidation.validate`, so queries that depend on well-formedness
  (e.g. `boundarySimplePolygon`, `gonalityTrees`) are total on `Tiling` and
  simply absent from the raw `TilingDCEL`. No public `Unsafe` methods remain —
  the safe/`Unsafe` pairing of ADR-0003 is now an internal convention only.
- IDs are opaque types (`VertexId`, `FaceId`) and format themselves through the
  `Prefixable` trait (e.g. `v1`, `f7`).
- Design rationale is recorded as Architecture Decision Records under
  [`adr/`](adr/README.md).

## Development

- **Formatting:** `scalafmt` via SBT plugin; `scalafmtOnCompile` is enabled.
- **Lint / refactoring:** `scalafix` with SemanticDB.
- **Compiler hygiene:** `-deprecation -feature -unchecked -Wvalue-discard
  -Wnonunit-statement -Wunused:imports`, elevated to `-Werror` in `Compile`
  only; tests are compiled with warnings, not fatal.
- **Cross-building:** JVM and Scala.js share all sources via `crossType(Full)`.
  A Scala Native project is scaffolded in `native/` and will be enabled as soon
  as Spire ships an artifact for the Scala Native 0.5 ecosystem
  ([ADR-0016](adr/0016-native-wait-for-spire.md)).

### Tests

Pure unit tests plus ScalaCheck property-based tests (see
`PropertyBasedDCELSpec.scala`), including the ADR-0017 certification property
(every tiling reachable through the public API must re-certify via
`Tiling.from`) and a merge-engine property over random isometric-copy
sequences. SVG fixtures under `shared/src/test/resources/` are used as
reference snapshots for the builders and the SVG exporter.

## License

Dual-licensed under either of

- Apache License, Version 2.0 ([`LICENSE-APACHE`](LICENSE-APACHE) or
  <https://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([`LICENSE-MIT`](LICENSE-MIT) or
  <https://opensource.org/licenses/MIT>)

at your option.
