# ADR-0008: JMH benchmarks in an opt-in subproject

- **Status:** Accepted
- **Date:** 2026-04-21

## Context

Exact arithmetic (ADR-0005) makes performance regression a real risk:
`TilingUniformity` walks every inner vertex and invokes
`BigDecimal`/`Rational` operations per step, so a seemingly innocent
refactor can double a workload's runtime without any test failing.

The previous benchmark setup:

- Lived in `jvm/src/test/scala/…/benchmark/UniformityBenchmark.scala` as
  a hand-rolled `object` with a `main` method, using `System.nanoTime`
  and a `@volatile sink` as a poor man's blackhole.
- Was invoked as `sbt "dcelJVM/Test/runMain …"` with a paired
  `scripts/run-uniformity-benchmark-stable.sh` that set JVM flags
  externally.
- Coupled the benchmark code into the `Test` classpath, which meant it
  was compiled (and potentially affected) every time `sbt test` ran.

This worked but had several problems: no proper fork isolation, no
warmup semantics the JIT understands, no machine-readable output,
`-Werror` in the test scope occasionally tripped on benchmark code,
and the wrapper shell script added one more place to keep JVM flags
in sync.

## Decision

**Move benchmarks to [sbt-jmh](https://github.com/sbt/sbt-jmh) in a
dedicated top-level `benchmarks/` subproject that is not aggregated by
the root project.**

- `project/plugins.sbt` adds `pl.project13.scala % sbt-jmh % 0.4.7`.
- `build.sbt` defines `lazy val benchmarks` with `enablePlugins(JmhPlugin)`
  and `dependsOn(dcelJVM)`. It sets `publish / skip := true`,
  `Jmh / bspEnabled := false`, and keeps its own permissive
  `scalacOptions` (no `-Werror`; JMH-generated code is machine-written).
- `benchmarks/src/main/scala/…/benchmark/UniformityBenchmark.scala`
  defines a single `@State(Scope.Benchmark)` class with a `@Param`
  over the five case names and three `@Benchmark` methods. Stable
  JVM flags are baked into the class via
  `@Fork(jvmArgsAppend = Array("-Xms2g", "-Xmx2g", "-XX:+AlwaysPreTouch", "-XX:+UseParallelGC"))`.
- Invocation: `sbt "benchmarks/Jmh/run -i 10 -wi 5 -f 1 -t 1 .*UniformityBenchmark.*"`.
- Operational details (run recipes, JSON output for cross-commit
  comparison, filtering by `caseName`) live in
  `benchmarks/RUNBOOK.md`.

## Consequences

**Positive**

- Proper JMH semantics: warmup vs. measurement iterations, fork
  isolation, `Blackhole.consume` for return values, deterministic
  param expansion.
- Machine-readable output via `-rf json -rff bench-results.json`.
- The benchmark module is *opt-in*: `sbt test` / `sbt qa` never touch
  it. CI can add a targeted smoke run separately without affecting the
  main build pipeline.
- No shell wrapper to maintain; JVM flags live with the benchmark code,
  not alongside it.

**Negative / tradeoffs**

- One more sbt plugin in the build.
- Benchmarks are a separate compile target; refactors in the main
  library that rename public APIs will not be caught by benchmark
  compilation unless the benchmark subproject is explicitly built
  (`sbt benchmarks/compile`). Mitigation: CI should compile the
  benchmark module on every PR even if it doesn't run it.
- JMH-generated code is Java; the `.jmh/generated` directory pollutes
  `target/` with sources the IDE may spuriously index.
- Adding a new benchmark operation means adding a `@Benchmark` method;
  adding a new case means editing the `@Param` string list and
  extending the `buildCase` match. Straightforward, but a change in
  two places.

## How to apply

- New benchmark methods belong in `UniformityBenchmark.scala` or in a
  new `*Benchmark.scala` next to it, following the same annotation
  pattern.
- Prefer parameterising over input shape via `@Param` on a small enum
  or String whitelist over creating a new `@State` class per fixture
  — it keeps the registration matrix visible.
- When you add a new benchmark case, update `benchmarks/RUNBOOK.md` to
  describe what the case exercises and why.
- Keep the JVM flags in `@Fork(jvmArgsAppend = …)` in lockstep with
  whatever configuration you expect to run in CI; if you find
  yourself wanting to override at the command line, that's a sign the
  default should change.

## Alternatives considered

- **Keep the hand-rolled runner.** Rejected: warmup/fork isolation
  and JSON output are both worth more than the cost of the plugin,
  and the shell wrapper was accumulating drift.
- **[ScalaMeter](https://scalameter.github.io/).** Less actively
  maintained; the tooling around JMH (visualisers, JSON diff tools,
  `jmh-visualizer`) is considerably richer.
- **JMH in the JVM `Test` scope of the main project.** Cleaner than
  the previous state but still couples benchmark bytecode to the test
  classpath and would re-impose `-Werror` concerns. Keeping it as a
  separate subproject isolates the concerns cleanly.
- **Aggregate `benchmarks` into `root`.** Catches compile errors
  earlier but forces `sbt compile` / `sbt test` to pay the JMH
  bytecode-generation cost. Rejected in favour of explicit
  `sbt benchmarks/Jmh/compile` plus a CI job.

## Related

- ADR-0005 (exact arithmetic — the reason benchmarks are worth running
  regularly).
- ADR-0007 (cross-platform — benchmarks are JVM-only; JS/Native
  benchmarks can be added separately when needed).
