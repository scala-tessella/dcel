# ADR-0005: Exact arithmetic via `BigDecimal` and Spire `Rational`

- **Status:** Accepted
- **Date:** 2026-04-21

## Context

Unit-side regular polygons produce coordinates whose closed-form
expressions involve `√2`, `√3`, and trig functions of nice angles
(30°, 45°, 60°). Two operations collapse if those values drift under
floating-point accumulation:

1. **Vertex-coincidence tests.** When a new polygon is added on the
   boundary, `TilingAddition.findSharedEdges` must decide whether a
   newly computed point equals an existing vertex. Under `Double`,
   this requires an epsilon dance that becomes unstable as tilings grow.
2. **Angle sums.** `TilingValidation.validateGeometrically` sums
   interior angles around each vertex and around the boundary; the sum
   must equal 360° (or a known multiple) **exactly**, not approximately.

Precision has to be deterministic across JVM and Scala.js. Native
`Double` semantics are close but not identical, and some trig primitives
behave differently on the JS runtime.

## Decision

**Use `BigDecimal` for coordinates and Spire `Rational` for angles.**

- `opaque type BigPoint = (x: BigDecimal, y: BigDecimal)` with a small
  algebraic surface in `geometry.BigPoint` and helpers for
  "almost-equal" comparisons at a fixed `ACCURACY`
  (`geometry.BigDecimalGeometry.almostEqual`).
- `BigLineSegment`, `BigBox`, `BigRadian` are analogous `BigDecimal`
  wrappers.
- `AngleDegree` is backed by a Spire `Rational`; exact equality is
  therefore meaningful (a triangle's interior angle is literally
  `Rational(60)`, not `59.99999…`).
- Spire is used for `sin`/`cos` on `BigDecimal`
  (`spire.implicits.BigDecimalIsTrig`) at the configured precision.

## Consequences

**Positive**

- Vertex-coincidence tests and angle-sum checks can rely on equality,
  not tolerance. This makes `TilingValidation` tractable and keeps
  search algorithms (`TilingGenerator.findTilings`,
  `TilingEquivalency`) deterministic.
- JVM and Scala.js produce bitwise-identical results; tests can
  cross-verify.
- Rational angles compose cleanly: summing three `AngleDegree(60)` is
  exactly `AngleDegree(180)` with no rounding.

**Negative / tradeoffs**

- `BigDecimal`/`Rational` arithmetic is **one to two orders of magnitude
  slower** than `Double` for the same operation count. This is
  load-bearing context for ADR-0003 (keeping `Unsafe` fast paths) and
  ADR-0008 (running JMH benchmarks regularly).
- Spire 0.18 does not currently ship a Scala Native artifact. This
  blocks ADR-0007's Native target: even though the source code is
  cross-compilable, we can't resolve the Spire dependency on Native.
- Trigonometric `BigDecimal` operations return values at the
  configured precision, not infinite precision. We mitigate by keeping
  the precision well above the `ACCURACY` threshold used for comparisons.
- `BigDecimal` is implemented differently between the JVM (delegates to
  `java.math.BigDecimal`) and Scala.js (a polyfill). The JS path is
  noticeably slower than the JVM path; JS-side performance-sensitive
  operations should be kept shallow.

## How to apply

- New geometry primitives go in the `dcel.geometry` package and use
  `BigDecimal` by default. If you're tempted to use `Double` for
  performance, benchmark first and keep the `Double`-based fast path
  strictly local to the algorithm.
- Express angles in `AngleDegree` (Rational-backed). Only convert to
  `BigDecimal`/`Double` at the boundary to user-facing output (SVG
  coordinates).
- "Almost equal" comparisons use
  `BigDecimalGeometry.almostEqual(a, b, accuracy)`. The default
  `ACCURACY` constant lives in `BigDecimalGeometry` — prefer it to
  ad-hoc literals so we can tune globally.

## Alternatives considered

- **`Double` everywhere with epsilon tolerance.** Tried informally in
  early prototypes; angle-sum and vertex-coincidence checks became
  hard to stabilise as tilings grew. The drift accumulates.
- **Algebraic numbers via Spire's `Algebraic` type.** Exact for the
  operations we need, but even slower than `BigDecimal`, and harder to
  render into SVG without an explicit precision step.
- **Fixed-point integer scaling (e.g. pico-units).** Fast and exact for
  rational coordinates, but the same polygons translated to different
  origins would produce non-canonical integer forms unless we commit
  to a global grid. Rejected as extra discipline for little gain.
- **`spire.math.Real`.** Computable reals; neat but opaque for equality
  decisions (you never *know* two reals are equal, only that they're
  within ε).

## Revisit if

- Spire publishes a Native artifact (would unblock ADR-0007).
- A profile shows `BigDecimal` coordinates are the bottleneck in a hot
  path that could accept a local `Double` implementation with a
  re-validation step.
- We need precision tuning from a client (currently hard-coded in
  `BigDecimalGeometry`).

## Related

- ADR-0003 (safe/`Unsafe` pairs — the `Unsafe` lane saves on allocation
  but still pays for exact arithmetic).
- ADR-0007 (cross-platform targets — Spire blocks Native).
- ADR-0008 (JMH benchmarks — the regression guard for this tradeoff).
