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
