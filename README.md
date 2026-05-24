# dcel

[![Maven Central](https://img.shields.io/maven-central/v/io.github.scala-tessella/dcel_3.svg?label=Maven%20Central)](https://central.sonatype.com/artifact/io.github.scala-tessella/dcel)
[![Scala 3](https://img.shields.io/badge/scala-3.8.3-red.svg)](https://scala-lang.org)
[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-1.16.0.svg)](https://www.scala-js.org)
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
topology/geometry operations, symmetry and uniformity analysis, and import/export.

> **Status:** early development (pre-1.0). The API may still change between
> minor versions.

## Tessella editor

The Tessella web and desktop editor, see https://tessell.art/editor (open here the [web editor](https://editor.tessell.art)), is built on the **dcel** library.

## Setup

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "io.github.scala-tessella" %% "dcel" % "0.1.0"
// Use %%% instead of %% for Scala.js
```

Published for **Scala 3** on the **JVM** and **Scala.js**. Scala Native is
scaffolded but blocked by Spire's Native support (see Tech Stack below).

## Features

- **Construct** tilings from regular or simple unit-side polygons
  (`TilingDCEL.createRegularPolygon`, `TilingDCEL.createSimplePolygon`).
- **Build lattices** quickly: triangle, rhombus/square, and hexagon nets, rings,
  and holed triangle nets (`TilingBuilder.createTriangleNet`,
  `createRhombusNet`, `createHexagonNet`, `createRing`, `createHoledTriangleNet`).
- **Grow and shrink** tessellations with validation-aware APIs:
  `maybeAddRegularPolygon*`, `maybeAddSimplePolygon*`, `maybeDeleteVertex`,
  `maybeDeleteEdge`, `maybeDeleteFace`, `fanAt`, `doubleArea`.
- **Analyse** topology, symmetry and uniformity: boundary, inner vertices,
  vertex angles, `uniformityTree`, `scanUniformityTree`, `gonalityTrees`,
  boundary-equivalence grouping.
- **Validate** completeness, topology, geometry, and spatial consistency in
  one call (`TilingValidation.validate`). Failures are surfaced as a sealed
  `TilingError` ADT (`IncompleteError`, `TopologyError`, `GeometryError`,
  `SpatialError`, `ValidationError`, `NotFoundError`).
- **Export** to SVG (with optional uniformity colouring, arrows, labels) and
  DOT, plus round-trip serialisation through SVG metadata
  (`toMetadata` / `TilingSVG.fromMetadata`).

## Tech Stack

| Item                                | Version / Status                              |
|-------------------------------------|-----------------------------------------------|
| Scala                               | 3.8.3                                         |
| Published artifact                  | `"io.github.scala-tessella" %% "dcel" % "0.1.0"` (Maven Central) |
| SBT                                 | 1.12.9                                        |
| Platforms                           | JVM, Scala.js (CommonJS on Node.js)           |
| Scala Native                        | scaffolded; blocked by Spire Native support   |
| Core deps                           | [ring-seq](https://github.com/scala-tessella/ring-seq) `0.8.0`, [iron](https://github.com/Iltotore/iron) `3.2.3`, [spire](https://typelevel.org/spire) `0.18.0` |
| Test deps                           | ScalaTest `3.2.19`, ScalaCheck `1.18.1`        |

## Project Layout

```
dcel/
├── build.sbt                  # Cross-project (JVM + JS) build and compiler flags
├── project/plugins.sbt        # sbt-scalajs, sbt-scalafmt, sbt-scalafix, sbt-ide-settings
├── shared/src/main/scala/io/github/scala_tessella/dcel/
│   ├── TilingDCEL.scala         # Main case class; container for vertices/edges/faces
│   ├── TilingBuilder.scala      # Nets, rings, simple & regular polygon constructors
│   ├── TilingAddition.scala     # In-place growth primitives (used by deep-copied tilings)
│   ├── TilingDeletion.scala     # In-place removal primitives
│   ├── TilingValidation.scala   # Completeness, topology, geometry, spatial checks
│   ├── TilingEquivalency.scala  # Boundary signature & equivalence grouping
│   ├── TilingUniformity.scala   # Uniformity/gonality trees
│   ├── TilingSymmetry.scala     # BoundaryLocation, BoundaryVertex, BoundaryEdge
│   ├── TilingError.scala        # Sealed error ADT + helpers
│   ├── Tree.scala               # Algebraic Leaf/Branch tree used for uniformity
│   ├── Utils.scala              # Small shared helpers (traverse, sequence, associate)
│   ├── geometry/                # BigDecimal-based primitives (see below)
│   ├── structure/               # DCEL entities: Vertex, HalfEdge, Face, IDs, Prefixable
│   └── conversion/              # SVG and DOT exporters + SVG-metadata reader
├── shared/src/test/scala/…      # Unit + ScalaCheck property tests
├── shared/src/test/resources/   # SVG fixtures used by tests
├── benchmarks/src/main/scala/…/benchmark/UniformityBenchmark.scala  # JMH (opt-in, unpublished)
├── generator/src/main/scala/…/TilingGenerator.scala  # Experimental n-uniform/n-archimedean search (opt-in, unpublished)
└── README.md
```

### Non-published subprojects

Two opt-in sbt subprojects live in this repo but are not part of the published
`dcel_3` artifact:

- **`benchmarks/`** — JMH micro-benchmarks for the validation/uniformity hot
  paths. Run with `sbt 'benchmarks/Jmh/run …'`.
- **`generator/`** — research-grade enumerator of n-uniform n-archimedean
  tilings (`TilingGenerator.findTilings`, `expandRotationally`,
  `expandRotationallyMore`). API and search heuristics are still in flux;
  several exploratory tests are disabled. Run with `sbt generator/test`.
  When the surface stabilises, this may be promoted to its own published
  artifact.

### Packages

| Package                                              | Role |
|------------------------------------------------------|------|
| `io.github.scala_tessella.dcel`                      | Core: `TilingDCEL`, builders, operations, validation, errors |
| `io.github.scala_tessella.dcel.geometry`             | Deterministic `BigDecimal`/Spire primitives: `BigPoint`, `BigLineSegment`, `BigBox`, `BigRadian`, `AngleDegree`, `RegularPolygon`, `SimplePolygon`, `Orientation`, `SpatialGrid`, `IntersectionDetection` |
| `io.github.scala_tessella.dcel.structure`            | `Vertex`, `HalfEdge`, `Face` + opaque `VertexId`/`FaceId`, `Prefixable` |
| `io.github.scala_tessella.dcel.conversion`           | `TilingSVG` (with `SvgOptions`), `TilingDOT`, SVG-metadata round-trip |

Only the root package depends on the other three; `conversion` depends on
everything, `structure` depends on `geometry`, `geometry` has no internal deps.

## Building from Source

### Prerequisites

- JDK 17 or later (CI runs on Temurin 17).
- sbt (`1.12.9` used by this project; launcher will pick up the version).
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
import io.github.scala_tessella.dcel.TilingDCEL
import io.github.scala_tessella.dcel.geometry.RegularPolygon
import io.github.scala_tessella.dcel.structure.VertexId

val base = TilingDCEL.createRegularPolygon(RegularPolygon(6))

val grownEither =
  base.maybeAddRegularPolygonToBoundary(
    onEdgeStartingWith = VertexId(1),
    polygon            = RegularPolygon(3)
  )

val svgEither = grownEither.map(_.toSVG(scale = 80.0))
```

Every mutating public API returns an `Either[TilingError, TilingDCEL]` and
operates on an internal deep copy, so the input value is never modified.

### 2. Build a net with `TilingBuilder`

```scala
import io.github.scala_tessella.dcel.TilingBuilder

val rhombusNetEither   = TilingBuilder.createRhombusNet(width = 3, height = 3)
val holedTriangleEither =
  TilingBuilder.createHoledTriangleNet(10, 10): (x, y) =>
    x > 2 && x < 7 && y > 2 && y < 7
```

### 3. Analyse uniformity

The uniformity operations live as extension methods in `TilingUniformity`:

```scala
import io.github.scala_tessella.dcel.TilingUniformity.*

val tree         = base.uniformityTree
val uncompressed = base.uniformityTreeUncompressed()
val scan         = base.scanUniformityTree
```

### 4. Export topology / graphics

```scala
import io.github.scala_tessella.dcel.conversion.TilingSVG.SvgOptions

val dot: String = base.toDOT
val svg: String = base.toSVG(SvgOptions(scale = 80.0, showUniformity = true))
```

### 5. Serialise and reconstruct metadata

```scala
import io.github.scala_tessella.dcel.conversion.TilingSVG.fromMetadata

val svgWithMeta  = base.toSVG(SvgOptions(scale = 80.0))
val reconstructed = fromMetadata(svgWithMeta)  // Either[TilingError, TilingDCEL]
```

## API Notes

- Public fallible APIs generally return `Either[TilingError, A]`.
- `TilingError` is a sealed trait with `ValidationError`, `IncompleteError`,
  `TopologyError`, `GeometryError`, `SpatialError`, and `NotFoundError`
  alternatives, plus a `TilingError.combineErrors` helper.
- Methods ending in `Unsafe` skip defensive checks for performance on
  already-validated inputs; they are meant for internal or hot paths and can
  throw. Prefer the non-`Unsafe` counterparts from client code.
- IDs are opaque types (`VertexId`, `FaceId`) and format themselves through the
  `Prefixable` trait (e.g. `v1`, `f7`).

## Development

- **Formatting:** `scalafmt` via SBT plugin; `scalafmtOnCompile` is enabled.
- **Lint / refactoring:** `scalafix` with SemanticDB.
- **Compiler hygiene:** `-deprecation -feature -unchecked -Wvalue-discard
  -Wnonunit-statement -Wunused:imports`, elevated to `-Werror` in `Compile`
  only; tests are compiled with warnings, not fatal.
- **Cross-building:** JVM and Scala.js share all sources via `crossType(Full)`.
  A Scala Native project is scaffolded in `native/` and will be enabled as soon
  as Spire ships a compatible Native build.

### Tests

Pure unit tests plus ScalaCheck property-based tests (see
`PropertyBasedDCELSpec.scala`). SVG fixtures under `shared/src/test/resources/`
are used as reference snapshots for the builders and the SVG exporter.

## License

Dual-licensed under either of

- Apache License, Version 2.0 ([`LICENSE-APACHE`](LICENSE-APACHE) or
  <https://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([`LICENSE-MIT`](LICENSE-MIT) or
  <https://opensource.org/licenses/MIT>)

at your option.
