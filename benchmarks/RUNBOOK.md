## JMH benchmarks

Performances are benchmarked with
[sbt-jmh](https://github.com/sbt/sbt-jmh) in a dedicated opt-in subproject
(`benchmarks/`), not aggregated into `root` so regular `sbt test` / `sbt qa`
runs stay fast.

Stable JVM flags (`-Xms2g -Xmx2g -XX:+AlwaysPreTouch -XX:+UseParallelGC`) are
baked into the benchmark class via `@Fork(jvmArgsAppend = ...)`, so every fork
already starts with the same heap + GC configuration — no wrapper script
needed. Override them with `-jvmArgs` on the JMH CLI if you want to experiment.

### Uniformity

Five cases (`triangle-net-8x8`, `rhombus-net-8x8`, `hexagon-net-5x5`,
`holed-triangle-10x10`, `ring-12`) are cross-multiplied with three operations
(`uniformityTreeUncompressed`, `uniformityTree`, `scanUniformityTree`) via a
JMH `@Param`-parameterised `@State` class — 15 benchmark rows per run.

```bash
# run the full matrix (5 cases × 3 operations) with default JMH settings
sbt "benchmarks/Jmh/run -i 10 -wi 5 -f 1 -t 1 .*UniformityBenchmark.*"

# machine-readable output for cross-commit comparison
sbt "benchmarks/Jmh/run -i 30 -wi 10 -f 1 -t 1 -rf json -rff bench-results.json .*UniformityBenchmark.*"

# restrict to a single case or operation
sbt "benchmarks/Jmh/run -p caseName=ring-12 .*UniformityBenchmark.uniformityTree"
```

### Validation (ADR-0009 baseline)

Five SVG template fixtures (`aperiodic/domino`, `regular/regular_6-6-6`,
`semiregular/semiregular_3-6-3-6`, `semiregular/semiregular_3-3-4-3-4`,
`semiregular/semiregular_4-6-12`) are cross-multiplied with two operations
(`fromMetadataImport`, `validateOnly`) — 10 benchmark rows per run. Fixtures
are read from `shared/src/test/resources/templates/` at trial setup; the
benchmark walks up from the fork's working directory to locate them, so it
works regardless of how sbt launches the fork.

`fromMetadataImport` mirrors the editor's `SvgImporter.parseTiling` hot path
(parse + validate). `validateOnly` isolates the geometry/spatial checks on a
pre-parsed tiling — this is the figure the ADR-0009 acceptance criterion
("JVM JMH for `validate` on the domino fixture improves by at least 5×")
compares against.

```bash
# full matrix (5 fixtures × 2 operations) with default JMH settings
sbt "benchmarks/Jmh/run -i 10 -wi 5 -f 1 -t 1 .*ValidationBenchmark.*"

# machine-readable output for before/after comparison across branches
sbt "benchmarks/Jmh/run -i 30 -wi 10 -f 1 -t 1 -rf json -rff validation-baseline.json .*ValidationBenchmark.*"

# single fixture, single operation (e.g. validate-only on domino)
sbt "benchmarks/Jmh/run -p fixture=aperiodic/domino .*ValidationBenchmark.validateOnly"
```

### Construction

Three net builders (`triangleNet`, `rhombusNet`, `hexagonNet`) timed
end-to-end over `size` ∈ {4, 8, 12, 16} — 12 benchmark rows per run. Unlike
the other suites, the fixture build *is* the measured operation: this is the
polygon-by-polygon growth pipeline (`TilingAddition` + per-step validation)
the editor exercises on every edit. Watch the growth curve over `size` —
each addition walks the existing boundary, so a super-linear regression in a
single growth step shows up as a worse-than-expected curve.

```bash
# full matrix (3 builders × 4 sizes) with default JMH settings
sbt "benchmarks/Jmh/run -i 10 -wi 5 -f 1 -t 1 .*ConstructionBenchmark.*"

# machine-readable output for before/after comparison across branches
sbt "benchmarks/Jmh/run -i 30 -wi 10 -f 1 -t 1 -rf json -rff construction-baseline.json .*ConstructionBenchmark.*"

# quick scaling check on the largest sizes only
sbt "benchmarks/Jmh/run -p size=8,16 .*ConstructionBenchmark.*"
```
