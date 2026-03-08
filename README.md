# dcel

DCEL utilities for representing and editing edge-to-edge tessellations of unit-side polygons in Scala 3.

This project models tessellations with a **Doubly Connected Edge List (DCEL)** and provides builders, validation, topology/geometry operations, and export utilities.

## Scope

This README documents the core project **excluding** the `torus` package:

- `shared/src/main/scala/io/github/scala_tessella/dcel/torus`
- `shared/src/test/scala/io/github/scala_tessella/dcel/torus`

## Features

- Build tilings from regular and simple polygons.
- Build lattice-style tilings (triangle, rhombus/square, hexagon nets, rings).
- Add/delete polygons, edges, vertices, and faces with validation-aware APIs.
- Compute boundary, local neighborhoods, uniformity trees, and symmetry-related structures.
- Validate completeness, topology, geometry, and spatial consistency.
- Export to SVG and DOT.
- Serialize/deserialize full DCEL metadata.

## Tech Stack

- Scala `3.8.2`
- SBT `1.11.7`
- Scala.js cross-project support (tests use Node.js for JS target)

## Project Layout

- `build.sbt`: cross-project build (JVM + JS), compiler/lint settings.
- `shared/src/main/scala/io/github/scala_tessella/dcel`: core DCEL logic.
- `shared/src/main/scala/io/github/scala_tessella/dcel/geometry`: geometric primitives and polygon math.
- `shared/src/main/scala/io/github/scala_tessella/dcel/structure`: DCEL entities (`Vertex`, `HalfEdge`, `Face`, IDs).
- `shared/src/main/scala/io/github/scala_tessella/dcel/conversion`: SVG/DOT conversion and metadata I/O.
- `shared/src/test/scala/io/github/scala_tessella/dcel`: unit and property tests.

## Getting Started

### Prerequisites

- A recent JDK compatible with SBT (`JDK 17+` recommended)
- `sbt` installed (`1.11.7` used by this project)
- `node` available in `PATH` for Scala.js tests

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
    polygon = RegularPolygon(3)
  )

val svgEither = grownEither.map(_.toSVG(scale = 80.0))
```

### 2. Build a net with `TilingBuilder`

```scala
import io.github.scala_tessella.dcel.TilingBuilder

val rhombusNetEither = TilingBuilder.createRhombusNet(width = 3, height = 3)
```

### 3. Export topology/graphics

```scala
val dot: String = base.toDOT
val svg: String = base.toSVG()
```

### 4. Serialize and reconstruct metadata

```scala
import io.github.scala_tessella.dcel.conversion.TilingSVG.*

val metadata: String = base.toMetadata
val reconstructed = fromMetadata(metadata)
```

## API Notes

- Public fallible APIs generally return `Either[TilingError, A]`.
- Error categories include validation, topology, geometry, spatial, and not-found cases.
- Opaque IDs (`VertexId`, `FaceId`) are used across public APIs.

## Development

- Formatting: `scalafmt` via SBT plugin.
- Lint/refactoring: `scalafix` with SemanticDB enabled.
- Compiler strictness: warnings are elevated in `Compile` scope (`-Werror`), relaxed in tests.

### Uniformity benchmark

Run the JVM benchmark runner for uniformity-related operations:

```bash
# quick mode (default: warmup=3, runs=8)
sbt "dcelJVM/Test/runMain io.github.scala_tessella.dcel.benchmark.UniformityBenchmark"

# quick mode with custom sample counts
sbt "dcelJVM/Test/runMain io.github.scala_tessella.dcel.benchmark.UniformityBenchmark --warmup=5 --runs=15"

# stable mode (larger sample counts for commit-to-commit comparison)
sbt "dcelJVM/Test/runMain io.github.scala_tessella.dcel.benchmark.UniformityBenchmark --mode=stable"
```

For reproducible cross-commit comparisons, run stable mode with fixed JVM flags:

```bash
# convenience wrapper
./scripts/run-uniformity-benchmark-stable.sh

# equivalent explicit command
sbt -J-Xms2g -J-Xmx2g -J-XX:+AlwaysPreTouch -J-XX:+UseParallelGC \
  "dcelJVM/Test/runMain io.github.scala_tessella.dcel.benchmark.UniformityBenchmark --mode=stable"
```

The output is CSV-style rows with per-case timing stats (`min`, `median`, `p95`, `mean` in milliseconds).

See [ContributingGuidelines.md](./ContributingGuidelines.md) for project-specific conventions.

## License

No license file is currently present in this repository.
