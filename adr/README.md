# Architecture Decision Records

This directory captures the non-obvious architectural decisions behind this
codebase. Each ADR explains *why* a particular choice was made, its tradeoffs,
and what would have to change for it to be revisited.

## Format

Short [MADR](https://adr.github.io/madr/)-style markdown, one file per
decision. Each file has:

- **Status** — `Proposed`, `Accepted`, `Deprecated`, or `Superseded by ADR-NNNN`.
- **Date** — ISO-8601.
- **Context and problem statement** — what forced the decision.
- **Decision** — the one-liner.
- **Consequences** — positive and negative, as concretely as possible.
- **Alternatives considered** — what was rejected and why.

## Index

| #    | Title                                                              | Status                 |
|------|--------------------------------------------------------------------|------------------------|
| 0001 | [DCEL as the core representation](0001-dcel-core-representation.md)                          | Accepted               |
| 0002 | [Mutation via deep-copy on the public boundary](0002-deep-copy-on-mutation.md)               | Accepted               |
| 0003 | [Paired safe and `Unsafe` methods](0003-safe-unsafe-method-pairs.md)                         | Accepted               |
| 0004 | [`Either[TilingError, A]` with a sealed error ADT](0004-either-based-error-handling.md)      | Accepted               |
| 0005 | [Exact arithmetic via `BigDecimal` and Spire `Rational`](0005-exact-arithmetic.md)           | Superseded by ADR-0010 |
| 0006 | [Opaque `VertexId` / `FaceId` with `Prefixable`](0006-opaque-ids-and-prefixable.md)          | Accepted               |
| 0007 | [Cross-platform: JVM + Scala.js shipped, Native blocked](0007-cross-platform-targets.md)     | Accepted               |
| 0008 | [JMH benchmarks in an opt-in subproject](0008-jmh-benchmarks-subproject.md)                  | Accepted               |
| 0009 | [Validation geometry — precision vs. performance](0009-validation-geometry-precision-performance.md) | Superseded by ADR-0010 |
| 0010 | [Validation geometry on `java.lang.Math` + `Double`](0010-validation-geometry-double.md)     | Accepted               |
| 0011 | [Isometric copy operations (mirror / translate / rotate / glide reflect)](0011-isometric-copy-operations.md) | Accepted               |
| 0012 | [Materialise enclosed regions as faces during merge](0012-enclosed-region-faces-on-merge.md) | Accepted               |
| 0013 | [Pinch vertices and merge determinism](0013-pinch-vertices-and-merge-determinism.md) | Accepted               |
| 0014 | [Exact corner angles for multi-pinch enclosed regions](0014-multi-pinch-enclosed-region-angles.md) | Accepted               |
| 0015 | [Largest parallelogon contained in a tiling](0015-largest-contained-parallelogon.md) | Accepted               |
| 0016 | [Scala Native stays blocked on Spire — keep waiting, don't replace](0016-native-wait-for-spire.md) | Accepted               |
| 0017 | [`Tiling` — the certified, validated tiling type](0017-validated-tiling.md) | Accepted               |
| 0018 | [Replicating OEIS A068600 — the Krotenheerdt tilings](0018-krotenheerdt-enumeration.md) | Accepted               |
| 0019 | [Fixed-Λ toroidal enumeration engine (scaling to n ≥ 3)](0019-fixed-lattice-toroidal-engine.md) | Accepted (sound; completeness bounded) |
| 0020 | [Exact-coordinate torus engine (the ADR-0019 "different search")](0020-exact-coordinate-torus-engine.md) | Accepted (faster/safe; cost exponential in covolume — limit characterized) |
| 0021 | [Direct combinatorial torus-quotient enumeration (reaching n = 4–7)](0021-direct-torus-quotient-enumeration.md) | Accepted (goal); realization superseded by ADR-0022 |
| 0022 | [Intrinsic combinatorial-map (Delaney–Dress) enumeration — soundness by construction](0022-intrinsic-combinatorial-map-enumeration.md) | Proposed (n ≤ 3 done) |
| 0023 | [Euclidean wallpaper-orbifold generator (reaching n = 4–7)](0023-euclidean-orbifold-generator.md) | Proposed (scope) |
| 0024 | [Isocoronal-prune vertex-star grower (the n = 4–7 anti-scatter lever)](0024-isocoronal-prune-grower.md) | Rejected (r=2 prune measured a no-op) |
| 0025 | [Bucketed assembly — vertex-type-set × bounded fundamental domain](0025-bucketed-assembly-enumeration.md) | Proposed (the new plan) |
| 0026 | [Corona-stabilization growth (Local Theorem)](0026-corona-stabilization-growth.md) | Superseded by ADR-0028 |
| 0027 | [Incenter dual — Taganap method is k=1-only](0027-incenter-dual-and-the-k1-only-wall.md) | Rejected (k≥2 re-enters covolume) |
| 0028 | [Galebach free-growth via corona-stabilization](0028-galebach-growth-corona-stabilization.md) | Rejected (1D-stacking scatter = covolume) |
| 0029 | [Oriented-slice profiled at n=4 — the covolume verdict](0029-oriented-slice-profiling-and-the-covolume-verdict.md) | Accepted (measured) |
| 0030 | [Bounded-V at n=4 — feasibility verdict](0030-bounded-v-n4-feasibility-verdict.md) | Accepted (measured; partial ~25-28/33) |
| 0031 | [Complementary engines + the both-walled n=4 tilings (~30/33)](0031-complementary-engines-and-the-both-walled-n4-tilings.md) | Accepted (measured) |
| 0032 | [Symmetry-first geometric grower — the scatter wall falls](0032-symmetry-first-geometric-grower.md) | Accepted (n=1 complete, n=2 15/20; parallel) |
| 0033 | [Closing the depth residual — n≤7 feasibility + speed levers](0033-closing-the-depth-residual-feasibility-and-speed-levers.md) | Accepted (arc-prune lever built + measured ineffective, rejected) |
| 0034 | [Fairness principle — Galebach is a validation oracle, not a source](0034-fair-enumeration-principle-and-closure-directed-growth.md) | Accepted; Conjecture R DISCHARGED for n≤3 (open n=4-7) |
| 0035 | [Dedicated methods for the large-domain chiral C₂ residual](0035-dedicated-methods-for-the-large-domain-chiral-c2-residual.md) | Accepted (orbifold-growth de-risked → WRONG fix: grower growth-path gap; pursue type-targeted oracle #2) |
| 0036 | [Non-monotonic difficulty + top-down fair type-set derivation](0036-non-monotonic-difficulty-and-top-down-type-set-derivation.md) | Proposed (constraint-first, derive type-sets fairly; diagnose n=3 grower gap first) |

## When to add an ADR

Open an ADR when you're about to make a decision that future contributors
cannot re-derive from reading the code:

- A choice between comparable technical options (library, representation, protocol).
- A discipline the code depends on but doesn't enforce (naming conventions,
  mutation contracts, validation boundaries).
- A constraint imposed from outside (dependency that blocks a target, upstream
  breaking change, performance envelope).

Don't open an ADR to document what `git blame` already shows: a bug fix, a
rename, a one-line refactor.

## Lifecycle

- New ADRs start as `Proposed`. Flip to `Accepted` once the decision is in main.
- A decision that gets replaced stays in the repo as `Superseded by ADR-NNNN`
  — don't delete it. The trail is the point.
- If you change your mind about a detail, add a new ADR that references the
  older one. ADRs are append-only.
