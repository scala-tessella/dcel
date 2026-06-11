# ADR-0016: Scala Native stays blocked on Spire — keep waiting, don't replace

- **Status:** Accepted
- **Date:** 2026-06-11

## Context and problem statement

ADR-0007 left Scala Native scaffolded but unwired, with unblock criteria:
Spire publishes a Native artifact, the other deps already do, and the sbt
plugins get added. A 2026-06-11 investigation re-checked those criteria and
found them **out of date in a way ADR-0007 did not anticipate**: the Scala
Native ecosystem split into incompatible 0.4 / 0.5 binary generations, and
our dependencies straddle the split.

State of Maven Central (checked against repository metadata, not the search
API — which lagged and initially misreported ring-seq):

| Dependency | Native 0.4 | Native 0.5 |
|---|---|---|
| `ring-seq` 0.8.0 | — | ✓ |
| `iron` 3.2.3 | ✓ | ✓ |
| `scalatest` 3.2.19 | — (only 3.3.0-SNAP4) | ✓ |
| `scalacheck` 1.18.1 | — (only 1.17.1) | ✓ |
| `scalatestplus scalacheck-1-18` | — | ✓ |
| **`spire` 0.18.0** | ✓ | **—** |

So Spire is *technically* published for Native — but only for the 0.4
generation, where ring-seq and the test stack are absent. Neither
generation has a complete dependency set; **Spire remains the single
blocker**, now specifically for Native 0.5.

This was verified empirically, not just by metadata: enabling
`NativePlatform` with `sbt-scala-native` 0.5.12 +
`sbt-scala-native-crossproject` 1.3.2 resolves every dependency except one —
`dcelNative/update` fails on exactly
`org.typelevel:spire_native0.5_3:0.18.0 — Not found`.

Spire itself looks dormant: 0.18.0 is its latest release, with no sign of a
Native 0.5 publication. So "wait for Spire" is no longer a passive default —
it is a choice with an indefinite horizon, and the alternative (removing the
Spire dependency) is concrete enough to estimate.

### What dcel actually uses from Spire

- **Structural:** `spire.math.Rational`, backing the `AngleDegree` opaque
  type (the exact-arithmetic backbone, ADR-0005) and its serialisation in
  `SvgMetadata`.
- **Incidental, trivially replaceable:** `spire.math.{abs, min, max, pi}`
  and the `spire.implicits.*` / `spire.compat.numeric` ordering instances.

A self-contained `BigInt`-based `Rational` (normalised fraction; arithmetic,
comparison, `toBigDecimal`, string round-trip) is on the order of 150 lines
plus tests. Replacing Spire is therefore feasible — the question is whether
it is worth doing *now*.

## Decision

**Keep waiting. Scala Native stays unwired per ADR-0007, and Spire is not
replaced.** There is no urgent demand for a `dcel` Native artifact — ADR-0007
ranked Native "nice-to-have, not blocking anyone today", and nothing has
changed on the demand side. Undertaking a replacement of the
exact-arithmetic core (ADR-0005/ADR-0010 territory) without a consumer
asking for it would be risk without payoff.

The internal-`Rational` replacement is recorded here as the **standing
contingency**, to be executed when (not if) a revisit trigger fires.

## Revisit triggers

Reopen this decision when any of these holds:

1. **Concrete demand** for a Native `dcel` artifact (a real consumer, not
   speculation).
2. **The editor's JS bundle size becomes a problem.** Dropping Spire removes
   a heavyweight dependency from the Scala.js bundle the downstream editor
   ships — this benefit is independent of Native and could justify the
   replacement on its own.
3. **Spire publishes for Native 0.5** — then ADR-0007's mechanical steps
   apply directly (the plugin pair `sbt-scala-native` 0.5.12 +
   `sbt-scala-native-crossproject` 1.3.2 is verified to resolve; budget for
   shared-test runtime surprises on the new platform).
4. **Spire is archived** or otherwise visibly abandoned upstream.

Under triggers 1, 2 or 4, the path is the contingency: an ADR-0017 for the
internal `Rational`, replacing the structural uses first (`AngleDegree`,
`SvgMetadata`), the incidental ones mechanically, with exact test parity on
JVM + JS before wiring Native.

## Consequences

### Positive

- Zero effort and zero churn now; the exact-arithmetic core that every
  validation path depends on stays untouched.
- The investigation cost is banked: the dependency matrix, the verified
  plugin pair, and the Spire-usage audit make the eventual unblock (either
  route) a planned job instead of a discovery project.
- ADR-0007's scaffold-and-wait posture is reaffirmed with current evidence
  rather than silently rotting.

### Negative / risks

- The Native timeline is outside our control, and Spire's dormancy means
  "waiting" may be indefinite. The README and build comments must stay
  honest that Native is advertised-but-unshipped (ADR-0007's standing
  caveat).
- The JS bundle keeps carrying Spire in the meantime — the flagship editor
  pays for `Rational` plus everything Spire links in.
- ADR-0007's unblock criterion 1 ("Spire publishes a Scala Native artifact
  compatible with Scala 3") is now insufficient as written; the criterion
  that matters is *Native 0.5 specifically*. ADRs are append-only, so this
  refinement lives here rather than as an edit to ADR-0007.

## Alternatives considered

- **Replace Spire with an internal `Rational` now.** Rejected for now, kept
  as the contingency: it is real work and real risk in the most
  load-bearing arithmetic in the library, and no consumer is asking for
  what it buys. The moment one is (triggers above), this flips.
- **Pin Scala Native 0.4 to use Spire's existing artifact.** Rejected:
  ring-seq and `scalatestplus` have no 0.4 artifacts, scalatest/scalacheck
  only stale ones, and 0.4 is the ecosystem's past, not its future.
- **Vendor Spire's `Rational` sources.** Rejected: imports a large,
  optimised implementation (with its license obligations) when the library
  needs only a small slice; a minimal in-house `Rational` is less code than
  the vendored file.
- **Contribute Native 0.5 publishing to Spire upstream.** Not pursued: with
  a dormant maintainership the review/release latency is unbounded, so it
  cannot be the plan — though if upstream revives, trigger 3 fires anyway.

## Related

- ADR-0005 (exact arithmetic — why `Rational` is load-bearing).
- ADR-0007 (cross-platform targets — the scaffold this ADR re-validates).
- ADR-0010 (validation geometry on `Double` — shrank Spire's hot-path role,
  which is what makes the contingency small).
