# ADR-0007: Cross-platform: JVM + Scala.js shipped, Native blocked

- **Status:** Accepted
- **Date:** 2026-04-21

## Context

The library's output (SVG tilings, DOT graphs, uniformity analysis) is
useful in three deployment contexts:

1. **Browser / Node.** Interactive visualisations and, most importantly,
   the **tessellation editor** that is the first real downstream
   application of this library. The editor runs entirely in the browser
   and needs the full DCEL API — construction, growth, deletion, SVG
   export — available as Scala.js. This is the load-bearing target:
   breaking JS support breaks the flagship user-facing product.
2. **JVM.** Backend tooling, data pipelines, the benchmark harness
   (ADR-0008). Easier to instrument and profile, so a lot of the
   library's correctness and performance work is validated here first.
3. **Native.** Command-line tools, embedded use, lightweight distribution
   without a JVM. Nice-to-have, not blocking anyone today.

Supporting (1) and (2) is non-negotiable; (3) is attractive but not yet
unblocked.

## Decision

**Cross-build for JVM and Scala.js from a single source tree; keep a
Scala Native scaffold in place but don't wire it into the build until
the dependency story allows it.**

- `dcel` is a `crossProject(JVMPlatform, JSPlatform)` with
  `crossType(CrossType.Full)`. All production code lives in
  `shared/src/main/scala`; neither `jvm/src/main` nor `js/src/main` has
  platform-specific source.
- Scala.js settings: CommonJS module kind (Node.js-friendly),
  `scalaJSUseMainModuleInitializer := false` (library, not app),
  `NodeJSEnv` for tests.
- A `native/` directory is present in the repo layout and the
  `NativePlatform` is commented out in `build.sbt` with a note that
  Spire is the blocker. The corresponding `sbt-scala-native` plugin is
  also commented out in `project/plugins.sbt`.

## Consequences

**Positive**

- One source tree, one set of tests, two published artifacts.
- Test matrix catches JVM/JS divergence early. Deterministic arithmetic
  (ADR-0005) means the tests can assert identical output across platforms.
- JS consumers get the full API without any bridging layer.
- The Native scaffold is ready to enable in a single commit once the
  dependency unblocks.

**Negative / tradeoffs**

- Every dependency must cross-publish: see `build.sbt` where each
  library uses `%%%` rather than `%%`. A new dep that only ships for
  the JVM forces a choice (drop JS support or drop the dep).
- No `java.*` APIs that Scala.js doesn't implement. This has
  occasionally constrained implementation choices (e.g. regex parsing
  in `TilingSVG` uses `scala.util.matching.Regex` rather than
  JVM-specific variants).
- Scala.js runtime is significantly slower than the JVM for
  `BigDecimal` operations (ADR-0005); JS-side performance-sensitive
  code paths should stay shallow.
- Scala Native is a deliberate compromise: the platform is scaffolded
  and advertised, but not actually compiled in CI. Users expecting
  Native support will bounce off the current state. The README and the
  build must be honest about this.

## Unblock criteria for Native

1. Spire (`org.typelevel %%% spire % 0.18.0`) publishes a Scala Native
   artifact compatible with Scala 3.
2. Ring-seq and Iron, the other two cross-published dependencies, also
   have Native artifacts at the version we use (they currently do — the
   missing link is Spire).
3. `sbt-scala-native-crossproject` and `sbt-scala-native` plugins
   matching our Scala.js generation get added to
   `project/plugins.sbt`.

When (1) lands, the change is mechanical:

- Uncomment the Scala Native plugin lines in `project/plugins.sbt`.
- Uncomment `NativePlatform` and `.nativeSettings(...)` in `build.sbt`.
- Add `dcelNative = dcel.native` and include it in the root aggregate.
- Add a `native/src/test/scala/…/TilingNativeSpec.scala` smoke test (or
  rely on the shared tests running under Native).
- Delete this section of the ADR and bump the status to "revisited".

## Alternatives considered

- **JVM-only.** Rejected: the tessellation editor downstream depends on
  this library running as Scala.js in the browser, so dropping JS would
  break the flagship consumer.
- **JS-only.** Rejected — the benchmark harness needs the JVM for any
  kind of stable measurement, and JVM tooling is where correctness
  regressions are easiest to diagnose.
- **Publish JVM today, add JS later.** No concrete reason to delay; the
  cost of `%%%` is minimal when set up from the start, and the editor
  would have been stuck waiting.
- **Ship JVM + JS now, drop Native scaffolding entirely.** Leaves us
  re-bootstrapping the Native build from scratch when Spire unblocks.
  Keeping the scaffold (and this ADR) costs very little and documents
  intent.

## How to apply

- Keep platform-specific code out of `shared/` unless there's no
  alternative. If a case arises, prefer creating a platform-specific
  file under `jvm/src/main/scala` / `js/src/main/scala` with a clear
  shared signature, instead of using `if (Platform.isJS) …`.
- When adding a dependency, check cross-publishing first. If it doesn't
  cross-publish, justify the platform drop in a follow-up ADR rather
  than silently adding it.
- Run `sbt dcelJVM/test dcelJS/test` (or the `qa` alias) before
  merging anything that touches the `shared/` tree.

## Related

- ADR-0005 (exact arithmetic — Spire is the reason Native is blocked).
- ADR-0008 (JMH benchmarks — JVM-only; does not affect this decision
  since the library itself still cross-builds).
