# ADR-0009: Validation geometry — precision vs. performance

- **Status:** Superseded by ADR-0010
- **Date:** 2026-04-23

## Context

A downstream consumer (the Tessella editor, Scala.js target — see the
memory pinning JS as first-class) reported multi-second import times on
the largest SVG templates it ships. Investigation
(`dcel-validation-perf-investigation.md`) profiled the hot path and
identified that `TilingSVG.fromMetadata` is dominated by
`TilingValidation.validateGeometrically`, which is in turn dominated by
`BigPoint.fromPolar` — specifically by Spire's
`BigDecimalIsTrig.{cos,sin}`, which evaluates a `spire.math.Real`
Cauchy sequence per call and runs 100–1000× slower than
`java.lang.Math.{cos,sin}` on `Double`.

Measured on Firefox, production build, `domino.svg`
(1023 V / 3004 HE / 481 F): **~6.5 s** per import, linearly scaling
as `T(n) ≈ 370 ms + 6 ms/vertex`. The regular
`6.6.6` template (54 V) takes **~0.7 s**. The downstream Android/F-Droid
packaging work in that project's ADR-005 depends on this cost being
reasonable on mobile CPUs.

This is the exact scenario ADR-0005 flags as a
"Revisit if" trigger: *"a profile shows `BigDecimal` coordinates are
the bottleneck in a hot path that could accept a local `Double`
implementation with a re-validation step."* That ADR explicitly
warns — in its "Alternatives considered" section — that
`spire.math.Real` is a known worst case for values involving π;
`BigDecimalIsTrig` is layered on exactly that machinery.

A library-side source audit surfaced four observations that sharpen the
picture beyond the profile:

1. **The exactness the `BigDecimal` trig pipeline is paying for is
   already lost upstream.** `AngleDegree.toBigRadian` does
   `BigRadian(BigDecimal(spire.math.pi) * (d / 180).toDouble)` — the
   `.toDouble` caps the input to ~1 ULP of `Double` before any trig
   runs. `spire.math.cos(BigDecimal)` then faithfully propagates a
   `Double`-precision input at ~1000× the cost. Every caller that
   eventually goes through `toBigRadian` (all validation callers do)
   is paying for exactness it does not actually hold.
2. **`SimplePolygon.fromUntrusted` is invoked three times per tiling
   boundary.** `TilingValidation.validateGeometrically` calls it for
   the interior and exterior boundary views, and
   `validateSpatially` calls it again via
   `tiling.boundarySimplePolygon.toAngles`. Each invocation
   re-runs `BigLineSegment.unitPath`, reconstructing `(n-2)`
   `BigPoint.fromPolar` calls.
3. **`BigPoint.isSimplePolygon` is O(n²) and runs on
   `BigDecimal`.** Each pair invokes four `BigPoint.orientation`
   calls with `BigDecimal` multiplies and subtractions. The cost
   scales with boundary length, not face count — so the 21 % slice
   on `domino` is almost entirely the boundary polygon, not the
   inner quads. A spatial-grid variant already exists in
   `BigDecimalGeometry.IntersectionDetection.hasSelfIntersection`
   (used by `validateSpatially`) and could subsume the hand-rolled
   pair loop.
4. **Other `BigDecimal` Spire-math hotspots sit below the current
   threshold.** `BigLineSegment.length` uses `spire.math.sqrt` on
   `BigDecimal`; `horizontalAngle` uses `spire.math.atan2` on
   `BigDecimal`. These appear as "<1 % each" today only because
   `fromPolar` dominates so heavily; they become the new ceiling
   once `fromPolar` drops.

### The numerical floor

`TilingBuilderSpec` contains a centagon-ring test — `createRing` of
100 regular 100-gons, producing 9 800 vertices — that must validate
at `ACCURACY = 1.0e-10`. The test's `@note` records that tightening
to `1e-12` fails from 46 sides onwards and `1e-11` fails from 92
sides onwards. That floor reflects the **Double-precision input to
the current trig pipeline** (see finding 1 above), not any
`BigDecimal`-specific capability. Any replacement must pass this
test at the current `ACCURACY`. Worst-case linear ULP accumulation
for a pure-`Double` path at N=9 800 is
`9 800 × 2.2e-16 ≈ 2.2e-12`, two orders of magnitude inside
tolerance — but the margin is not overwhelming, and numerical
behaviour must be demonstrated, not assumed.

### Why this needs an ADR, not a patch

The obvious fix is "swap Spire `BigDecimal` trig for `Math` on
`Double` on the validation path". But:

- It cuts across ADR-0005, which is Accepted and load-bearing for
  other invariants (vertex-coincidence tests,
  `TilingGenerator.findTilings` determinism,
  JVM↔JS bitwise equality).
- The right scope is not obvious. A validation-only change keeps the
  exactness story intact elsewhere but creates two parallel code
  paths. A library-wide change is cleaner but larger and has more
  surface area for numerical regressions.
- The right *primitive* is not obvious either. Memoising by angle,
  fusing rotation composition, and spatial-grid intersection are
  all partial solutions with different shapes of win.
- The precision question — will the centagon ring and the
  `PropertyBasedDCELSpec` generators still close cleanly — is
  empirical, not deducible from the source.

The decision therefore needs benchmarking across real fixtures and
numerical cross-checking, not a single judgment call.

## Decision

**Investigate and benchmark a set of candidate paths in separate
branches, then land whichever one meets the acceptance criteria below.
Do not merge a speculative fix.**

The options below are to be explored; they are not yet ranked or
committed to.

### Candidate paths

- **A. Library-wide `Double` for trig primitives.** Replace
  `spire.math.{cos,sin,atan2,sqrt}(BigDecimal)` with
  `java.lang.Math.*` on `Double` in `geometry/` (and the 3 call sites
  in `TilingBuilder`, the 2 in `TilingAddition`, the 1 in
  `TilingSVG.scala:350`). Justified by finding 1: exactness is
  already lost at `toBigRadian`. Smallest conceptual surface; also
  the most aggressive reinterpretation of ADR-0005.
- **B. Validation-only fast path.** Keep `BigPoint.fromPolar` as it
  is; introduce `BigPoint.fromPolarD` (or a parallel `unitPathD`
  helper) on `Double` and route `SimplePolygon.fromUntrusted` /
  `validateGeometrically` to it. Narrowest scope; preserves
  everything else. Two code paths to maintain.
- **C. `BigRadian` collapses to `Double`.** Make
  `opaque type BigRadian = Double` (or keep the type but have all
  constructors and extensions operate on `Double`). Removes the
  per-angle `BigDecimal` allocation and the conversion hop.
  Mechanical refactor; preserves signatures because of opacity.
  Pairs naturally with A.
- **D. Memoise `AngleDegree.toBigRadian` (or `toDouble` under C).**
  A typical tiling uses 2–8 distinct interior angles. Cache per
  distinct value per validation. Small absolute win once (A) or (B)
  drops cos/sin cost, but cheap to add.
- **E. Fused rotation composition in `unitPath`.** Keep heading as
  `(cos_h, sin_h)` and advance per step via the rotation addition
  formulas with a precomputed `(cos_turn, sin_turn)` per distinct
  turn angle (typically 2–8 per tiling). Replaces the per-vertex
  trig call with 4 muls + 2 adds. Folds (D) in naturally. Likely
  *better* precision than the current sequential-heading approach
  for large N, because no growing-heading value ever flows into
  `cos`/`sin`.
- **F. Dedupe redundant boundary work.** Cache
  `tiling.boundarySimplePolygon` once and reuse it between
  `validateGeometrically` (interior + exterior views) and
  `validateSpatially`. Orthogonal to A–E; collapses 3× `unitPath`
  calls to 1×.
- **G. Replace `BigPoint.isSimplePolygon`'s pair loop with the
  existing `IntersectionDetection.hasSelfIntersection` sweep-line
  path.** Already used in `validateSpatially`; reusing it yields a
  single primitive instead of two. Addresses the 21 % slice on
  `domino`.
- **H. `SimplePolygon.fromUntrusted` split into angle-only and
  angle+coordinates forms.** `SvgMetadata.fromMetadata` has the
  stored coordinates — the reconstruction via `unitPath` could be
  skipped entirely if the caller hands in the actual coords and the
  validator compares against the unit-path prediction at epsilon
  (or omits the reconstruction on the `fromTrustedMetadata`
  contract from the perf report's §4). Bigger change, touches
  caller contracts.

### Methodology

1. Land a JMH benchmark covering `TilingSVG.fromMetadata` on
   representative fixtures (`domino.svg`, `regular_6-6-6.svg`,
   and 2–3 semiregular templates) in the existing
   `benchmarks/` subproject (ADR-0008) first, on `master`. This
   captures the baseline before any branch-work. The editor's
   `before.json` / `after.json` in `benchmarks/` appear to be the
   JS-side numbers from the investigation; a JVM JMH baseline is
   additive.
2. Extend `BigPointSpec` or `BigLineSegmentSpec` with an explicit
   unit-path closure test at `N ∈ {3, 10, 46, 92, 100, 200, 500}`
   asserting closure error ≤ `1e-10`. This pins the numerical
   contract at the geometry layer so the centagon integration test
   is not the only backstop.
3. Explore candidates in separate branches:
   `perf/validation-double-local` (B), `perf/geometry-double` (A+C),
   `perf/rotation-composition` (E, probably on top of A or B),
   `perf/boundary-dedup` (F), `perf/sweepline-simplicity` (G),
   `perf/metadata-fastpath` (H). Each branch runs the JMH baseline
   and the full test suite; D is absorbed into whichever trig-cost
   branch wins.
4. Cross-verify numerically: run the property-based generators
   (`PropertyBasedDCELSpec`) *also* against the Spire-`BigDecimal`
   reference implementation and assert Right/Left agreement. This
   is cheap to wire up (two implementations, same fixtures) and
   catches silent precision regressions that unit tests miss.
5. Decide on merge based on the acceptance criteria below. Record
   the chosen path in a follow-up ADR
   (`0010-validation-geometry-double.md` or similar) that
   references this one.

### Acceptance criteria

A branch is mergeable iff it satisfies **all** of:

- **Perf** — Editor median `domino.svg` import < 1 000 ms on the
  same Firefox/localhost preview setup; `regular_6-6-6.svg` <
  100 ms. JVM JMH for `validate` on the domino fixture improves by
  at least 5×.
- **Numerical contract** — `TilingBuilderSpec` centagon ring test
  passes at the current `ACCURACY = 1.0e-10`. The new geometry-layer
  closure test (step 2 above) passes at all listed N.
- **Property tests** — `PropertyBasedDCELSpec` cross-check (step 4
  above) agrees with the Spire-`BigDecimal` reference on ≥ 10 000
  generated tilings.
- **JVM↔JS parity** — ADR-0005's bitwise-identity guarantee is either
  preserved, or explicitly downgraded to "agrees to within
  `ACCURACY`" and the downgrade is called out in the follow-up ADR.
  (This is the most likely point where ADR-0005's assumptions are
  revised.)

A branch that meets perf but misses the numerical floor is
**rejected**, not merged with tolerance tuning. The `1e-10`
tolerance is load-bearing for vertex-coincidence checks; loosening
it is a separate decision with its own ADR.

## Consequences

**Positive**

- Decisions about `BigDecimal` vs. `Double` are made against
  real numbers from representative fixtures, not against the
  architectural principle in ADR-0005 in the abstract.
- Finding 1 (exactness is illusory on this path) gets documented in
  the audit trail whether or not the follow-up ADR flips ADR-0005's
  stance. Future contributors encountering Spire `BigDecimal` trig
  elsewhere will have this analysis to refer back to.
- The extra JMH coverage on `validate` / `fromMetadata` and the
  explicit closure-error test at the geometry layer are net-positive
  regardless of which candidate path wins. They land early and stay.

**Negative / tradeoffs**

- Investigation time. Several candidate branches to carry in
  parallel, each needing benchmark runs and property-test sweeps.
  The risk of doing nothing is a user-visible 6.5 s pause on import;
  the risk of rushing is a silent precision regression that
  `PropertyBasedDCELSpec` doesn't currently catch, hence the
  cross-check step.
- Possible revision of ADR-0005. If path A or C wins, ADR-0005's
  "Decision" line needs to be superseded or qualified. The repo
  convention (see `adr/README.md`) is to leave the old ADR in place
  marked `Superseded by ADR-NNNN` — that will be the outcome if a
  library-wide `Double` path lands.
- Scala.js bitwise parity. ADR-0005 listed bitwise JS/JVM identity
  as a positive; `Math.{cos,sin,atan2,sqrt}` on `Double` is not
  guaranteed bitwise-identical across runtimes (IEEE-754 is
  deterministic per-operation but library implementations vary).
  Acceptance criteria treat this as an explicit flip to consider,
  not a silent regression.

## Alternatives considered

- **Ship a patch without an ADR.** Rejected: this is the exact
  shape of decision ADR-0005 flagged as a future revisit, and the
  scope is not small enough for a `git blame`-readable change.
- **Declare the editor's 6.5 s import acceptable and move on.**
  Rejected: the downstream F-Droid/Android packaging explicitly
  depends on mobile-CPU reasonability, and the profile identifies a
  clear algorithmic-level over-spend, not a physical-work floor.
- **`fromTrustedMetadata(meta, sha256)` as the primary fix.** The
  perf report's §4 considered this and downgraded it to "tactical,
  not strategic" on the grounds that fixing the primitive obviates
  the need. Kept as path H for completeness in case the primitive
  fix doesn't land cleanly.
- **Memoise at the Spire layer (cache `cos`/`sin` on `BigDecimal`
  keys).** The perf report's §2 fallback. Modest win for regular
  tilings (few distinct angles), small for aperiodic. Captured as
  path D; only attractive as a layer on top of A or B.

## Revisit if

- Once a branch meets all acceptance criteria: flip this ADR to
  `Superseded by ADR-NNNN` where NNNN records the landed design.
- If the investigation concludes that no branch meets the numerical
  floor without loosening `ACCURACY`: flip to `Accepted` with the
  decision being "accept current perf; document why";
  simultaneously open a separate ADR on the tolerance question.
- If a downstream consumer materially changes the workload (e.g.
  tilings with ≥ 10 000 vertices become routine), the perf bar in
  the acceptance criteria may need to tighten and the candidate
  paths re-evaluated.

## Related

- ADR-0005 (exact arithmetic via `BigDecimal` and Spire `Rational`)
  — the decision being revisited. Its "Revisit if" clause
  explicitly anticipates this ADR.
- ADR-0003 (safe / `Unsafe` method pairs) — the `Unsafe` lane
  already concedes allocation cost but does not currently concede
  arithmetic cost; a `Double` path on validation would be
  orthogonal to the safe/unsafe distinction.
- ADR-0007 (cross-platform targets) — JS is the consumer driving
  this investigation; any JVM↔JS bitwise-parity downgrade
  interacts with ADR-0007's guarantees.
- ADR-0008 (JMH benchmarks) — the regression guard the methodology
  above relies on; needs a new `ValidationBenchmark` alongside the
  existing `UniformityBenchmark`.
