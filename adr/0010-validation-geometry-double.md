# ADR-0010: Validation geometry on `java.lang.Math` + `Double`

- **Status:** Accepted
- **Date:** 2026-04-23

## Context

ADR-0009 framed the precision-vs-performance investigation and set the
acceptance criteria. Its methodology ran:

1. A JMH baseline on `master` (`benchmarks/validation-baseline-master.json`,
   5 fixtures × 2 operations).
2. A geometry-layer closure test at `N ∈ {3, 10, 46, 92, 100, 200, 500}`
   in `BigLineSegmentSpec`.
3. Two candidate branches carried in parallel:
   - `perf/validation-double-local` (B) — private `DoubleGeometry` fast
     path inside `SimplePolygon.fromUntrusted`.
   - `perf/geometry-double` (A+C+G) — library-wide `java.lang.Math.*` on
     `Double` for trig primitives, `BigRadian` collapsed onto `Double`,
     and `BigPoint.isSimplePolygon` delegated to the
     `IntersectionDetection.hasSelfIntersection` sweep-line already used
     by `validateSpatially`.
4. A cross-check against the pre-change Spire-`BigDecimal` pipeline on
   15 348 generated cases (`ValidationLibraryCrossCheckSpec`,
   `SpireBigDecimalReference`).
5. Full-suite and Scala.js test runs on both branches.

Both branches cleared every acceptance criterion. This ADR records the
decision between them.

## Decision

**Merge `perf/geometry-double` (A+C+G).** Across the library, the
Spire-`BigDecimal` trig pipeline is replaced by `java.lang.Math.*` on
`Double`, `BigRadian` becomes an `opaque type BigRadian = Double`, and
the hand-rolled O(n²) `BigPoint.isSimplePolygon` pair loop is replaced
by the existing sweep-line primitive. `BigPoint` / `BigLineSegment` /
`BigBox` continue to expose `BigDecimal` coordinate types unchanged —
accumulation, orientation tests, and the `ACCURACY`-based comparisons
all remain on `BigDecimal`. Only the trig primitives and the simplicity
check move off `BigDecimal`.

Candidate B was ~1.8× faster on domino (11.4× vs 6.18×) but both clear
the ≥ 5× acceptance bar with room; both are well under the editor's
1 s user-visible target. The decision is made on architectural grounds,
not raw perf:

- Finding 1 from ADR-0009 (exactness is lost at `AngleDegree.toBigRadian`)
  applies library-wide, not just to validation. B's narrow scope left
  the `BigDecimal` trig ceiling in place for `TilingBuilder`,
  `TilingAddition`, and `TilingSVG`. A+C+G eliminates it.
- B introduced a parallel `DoubleGeometry` subsystem that future perf
  work would have to keep in sync with the `BigPoint` / `BigLineSegment`
  path. A+C+G has one primitive.
- A+C+G supersedes ADR-0005 cleanly. B would have left ADR-0005 partly
  intact and partly not, which is harder to document correctly over time.

### Measured outcomes (JMH, 20 iter × 5 warmup × 1 fork, ms/op)

Machine: same as baseline (Linux 6.8, JDK 21.0.10, `-Xms2g -Xmx2g
-XX:+AlwaysPreTouch -XX:+UseParallelGC`).

| Fixture | master `validateOnly` | A+C+G `validateOnly` | Speedup |
|---|---|---|---|
| `aperiodic/domino` (1023 V) | 609.9 ± 6.1 | 98.7 ± 0.5 | **6.18×** |
| `regular/regular_6-6-6` (54 V) | 87.9 ± 0.3 | 4.6 ± 0.0 | 19.0× |
| `semiregular/3-6-3-6` | 110.5 ± 0.6 | 7.2 ± 0.0 | 15.4× |
| `semiregular/3-3-4-3-4` | 46.5 ± 0.3 | 4.6 ± 0.0 | 10.0× |
| `semiregular/4-6-12` | 139.0 ± 1.2 | 6.7 ± 0.0 | 20.9× |

JSON: `benchmarks/validation-perf-geometry-double-sweepline.json`.

Sweep-line substitution alone (G) saved ~26 ms on domino
(A+C without G = 124.7 ms, A+C+G = 98.7 ms) — exactly the 21 % slice
ADR-0009 finding 3 predicted.

### Acceptance criteria — all met

- **Perf (JVM):** domino `validateOnly` 6.18× vs. the 5× bar; every
  other fixture clears by wider margin.
- **Numerical contract:** centagon ring (9 800 vertices) and the
  `BigLineSegmentSpec` closure test at `N ∈ {3..500}` pass at the
  current `ACCURACY = 1.0e-10`.
- **Property tests:** `ValidationLibraryCrossCheckSpec` runs 15 348
  cross-check cases (10 000 random + 118 regular N-gons + 5 230
  harvested from 400 random-growth tilings) against the preserved
  Spire-`BigDecimal` reference — zero disagreements.
- **JVM↔JS parity:** see the downgrade below.

## Consequences

**Positive**

- **~10× to ~20× faster on JVM** across realistic fixtures for both
  `TilingSVG.fromMetadata` and isolated `TilingValidation.validate`.
  The downstream editor's 6.5 s domino import collapses to well under
  1 s, re-enabling the F-Droid / Android packaging path that prompted
  the investigation.
- **Spire `BigDecimal` trig is removed from the library.** Not just the
  validation path: `TilingBuilder` (hex grid construction),
  `TilingAddition` (polygon rotation), `TilingSVG` (label placement) all
  share the same `Math.*` primitives now. Future perf work does not have
  to reason about "which implementation is in play".
- **Architectural clarity.** `BigRadian` is a `Double`-backed opaque
  type; there is no longer a reason to pay the `BigDecimal` allocation
  cost for angular values that will only feed trig.
- **Closure test and cross-check spec land on `master` regardless of
  candidate,** so they guard future regressions even though the winning
  branch is A+C+G.

**Negative / tradeoffs**

- **JVM↔JS bitwise-parity guarantee is downgraded** from the one ADR-0005
  recorded ("JVM and Scala.js produce bitwise-identical results") to
  **"agree to within `ACCURACY = 1.0e-10`"**. IEEE-754 arithmetic is
  deterministic per-operation, but `Math.{cos,sin,atan2,sqrt}` library
  implementations are not guaranteed identical across runtimes, and
  Scala.js uses the host JavaScript engine's `Math`. Empirical check:
  the full suite (including `ValidationLibraryCrossCheckSpec`) passes
  on both JVM and JS. The downgrade is deliberate, documented here, and
  covered by the cross-check spec rather than asserted by construction.
- **`BigDecimal` trig precision is gone** from the library. The 3-ULP
  accumulation bound on pure-`Double` paths at `N=9 800` works out to
  `9 800 × 2.2e-16 ≈ 2.2e-12`, two orders of magnitude inside the
  `ACCURACY` threshold — confirmed by the centagon-ring test passing.
  Future work that genuinely needs 34-digit trig would have to inline
  Spire in the callsite.
- **One regression surface to maintain:** the cross-check spec compares
  against the *preserved* Spire-`BigDecimal` reference. If Spire itself
  updates and drifts (improbable on a deprecated 0.18 line), the
  reference could silently change. Acceptable.

## Alternatives considered

- **Candidate B alone** (`perf/validation-double-local`). 11.4× on
  domino, narrower blast radius, preserves the library-wide
  `BigDecimal` trig. Rejected on architectural grounds (see Decision);
  kept alive as a viable fallback if a future finding forces the
  narrow-scope interpretation back into contention.
- **Candidate A+C without G.** Missed the 5× domino bar at 4.89×
  because the O(n²) `BigDecimal` pair-loop simplicity check remained.
  Rejected; G closes the gap.
- **Candidates D (memoise `toBigRadian`), E (fused rotation
  composition), F (dedupe boundary work), H (trusted-metadata
  fast-path)** — all listed in ADR-0009. None were needed to meet the
  acceptance criteria once A+C+G was in place. They remain available
  if a future workload pushes past the current bar.
- **Declare 6.5 s domino import acceptable.** Rejected in ADR-0009 on
  the grounds that the downstream Android/F-Droid packaging depends on
  mobile-CPU reasonability. The decision still stands.

## Revisit if

- A downstream consumer materially changes the workload (tilings with
  ≥ 10 000 vertices become routine, `validate` dominates again). The
  next candidate on deck is G's cousin — sweep-line simplicity could
  be pushed lower, into `isSimplePolygon`'s cellSize heuristic.
- Scala.js / Node drifts from JVM on `Math.*` in a way the cross-check
  spec catches. Response: either change the spec to report the actual
  disagreement, or bound the drift in `ACCURACY`.
- Spire publishes a Scala Native artifact (would unblock ADR-0007 for
  reasons outside this ADR's scope).

## Related

- ADR-0005 (exact arithmetic) — superseded by this decision. Finding 1
  of ADR-0009 showed its trig-path exactness premise was illusory;
  coordinate-space `BigDecimal` remains in force.
- ADR-0007 (cross-platform targets) — the bitwise-parity guarantee it
  recorded is explicitly downgraded here.
- ADR-0009 (investigation) — superseded by this decision.
- ADR-0008 (JMH benchmarks) — the infrastructure this ADR's numbers
  were measured on.
- `dcel-validation-perf-investigation.md` — the upstream investigation
  note that seeded ADR-0009.
