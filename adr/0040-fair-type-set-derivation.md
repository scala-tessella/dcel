# ADR-0040: Fair top-down type-set derivation (ADR-0039 Phase 1)

- **Status:** Accepted (design; measured results appended below). 2026-07-07.
- **Follows:** [[0039-constraint-first-pivot-and-the-12n-bound]] (Phase 1 of the constraint-first plan);
  executes [[0036-non-monotonic-difficulty-and-top-down-type-set-derivation]] §2 with the Phase-0 correction
  (the n=8 ceiling is NOT a compatibility fact — 8–9 types coexist in n-uniform tilings — so this stage
  derives CANDIDATE inventories; ceilings on `n` are Phase-2 refutations).

## Context — the fairness gap being closed

Every type-targeted run so far read its candidate type-sets from `TilingReference` (ADR-0036 flagged this as
uncomfortably close to the answer key), and `VertexTypes.polygonSides` HARDCODES the alphabet {3,4,6,8,12}.
A fair enumeration must DERIVE both. Krötenheerdt's own documented method (ADR-0039 Phase 0) is exactly
this: vertex-figure compatibility case analysis. This ADR mechanizes it, answer-blind.

## Decision — one corona primitive, three derivation stages

New `TypeCompatibility` (generator/), reusing `VertexTypes.normalize` (bracelet) and exact rational angles.

### Stage A — the arithmetic figures (no alphabet assumption)

Enumerate ALL cyclic arrangements (bracelets) of k ∈ [3,6] regular polygons, `pᵢ ≥ 3` unbounded, with
interior angles summing to 360°: `Σ(1/2 − 1/pᵢ) = 1` (Egyptian-fraction enumeration over exact rationals;
the equation itself bounds `pᵢ ≤ 42`). Documented expectation: **21** figures (Kepler/Sommerville).

### Stage B — tiling-viable figures, by mechanized local refutation

The key primitive: **polygon-corona satisfiability**. Around a p-gon, the p neighbor tiles `t₁..t_p` must,
at each corner, form an unordered pair `{tᵢ, tᵢ₊₁}` that is the cyclic-neighbor pair of `p` in SOME allowed
figure (a small cycle-DP; a corner may be PINNED to a specific occurrence's pair). This is sound as a
NECESSARY condition (every tiling realizes each corona; unordered pairs stay orientation/chirality-safe).

Iterate to a fixpoint over the stage-A universe: kill a figure F when (i) some adjacent pair of F is
supported by no surviving figure (the far end of an edge shares the same two faces), or (ii) some pinned
corona of F is unsatisfiable over the survivors. The classic six (3.7.42, 3.8.24, 3.9.18, 3.10.15, 4.5.20,
5.5.10) die by odd-cycle corona parity — Sommerville's argument, mechanized. Expected survivors: **15**,
with derived alphabet **{3,4,6,8,12}** — turning `polygonSides` from an assumption into a theorem
(cross-validated against `VertexTypes.validSignatures`).

### Stage C — candidate type-sets per n

`candidates(n)` = subsets S of the survivors, |S| = n, passing the necessary filters:

1. **edge-closure:** every adjacent pair of every F ∈ S is supported within S (both ends of every edge have
   types in S);
2. **pinned coronas:** for every F ∈ S, every polygon occurrence in F has a satisfiable corona over S;
3. **connectivity:** the can-share-edge graph on S is connected (any two realized types are joined by an
   edge path in the tiling, and every type in S is realized).

All three are provably necessary ⇒ **no realizable type-set is ever pruned** (the load-bearing soundness
direction, tested against every reference/oracle type-set for n ≤ 5). The filters OVER-generate by design
(ADR-0036: necessary, not sufficient) — Phase 2 confirms/refutes each candidate. A possible future
strengthening (noted, not built): a type-frequency LP feasibility filter (angle/edge counting equations).

### Derived-for-free corollaries (fixture tests, documented provenance)

- **4.8² isolation** (Krötenheerdt 1969): among the 15 survivors only 4.8² contains an 8, so every edge at a
  4.8²-vertex leads to another 4.8²-vertex ⇒ connectivity forces the singleton — `candidates(n≥2)` never
  contains 4.8².
- **m ≤ 14 ceiling** (Krötenheerdt): only 15 survivors and 4.8² is isolated ⇒ no candidate set of size > 14.
- **candidates(1) = exactly the 11 Archimedean type-sets**: the four mix-only figures (3².4.12, 3².6²,
  3.4.3.12, 3.4².6) are refuted as singletons by triangle-corona parity (each forces a 2-colouring of the
  triangle's odd corona) — i.e. the Archimedean TYPE inventory is fully derived, answer-blind.

## Validation plan (test-first, ground-up per the project discipline)

Leaves first (`TypeCompatibilitySpec`): stage-A count + exotic members; neighbor-pair extraction on hand
cases; corona-DP on hand-verified cases (triangle refutes pure 3².6², hexagon does not; pentagon refutes
pure 5².10). Then the fixpoint (15, the exact six killed, derived alphabet = `polygonSides`, survivors =
`validSignatures`). Then candidates: the corollaries above, the n ≤ 5 no-false-negative sweep against
`TilingReference` (validation oracle — the direction ADR-0034 permits), and a pinned characterization of
`|candidates(n)|` for n = 1..7 (the measured over-generation Phase 2 must retire).

## MEASURED RESULTS (2026-07-07, `TypeCompatibility` + `TypeCompatibilitySpec`, 13 tests green, ~3 s)

Built test-first, all green on the first full run; every documented expectation was DERIVED, none assumed:

- **Stage A = 21** figures from the angle equation alone (max polygon 42; all cyclic-arrangement splits
  present: 3².4.12/3.4.3.12, 3².6²/3.6.3.6, 3³.4²/3².4.3.4, 3.4².6/3.4.6.4).
- **Stage B = 15**; the fixpoint kills EXACTLY Sommerville's six (3.7.42, 3.8.24, 3.9.18, 3.10.15, 4.5.20,
  5.5.10 — all by odd-cycle corona parity); **derived alphabet = {3,4,6,8,12} = `VertexTypes.polygonSides`**
  (the hardcoded assumption is now a theorem) and survivors = `validSignatures` figure-for-figure.
- **Stage C corollaries all derived:** `candidates(1)` = EXACTLY the 11 Archimedean type-sets (the four
  mix-only figures refuted as singletons by triangle-corona parity); 4.8² appears in no mixed candidate
  (Krötenheerdt's isolation lemma); the full 15-set is not a candidate (the m ≤ 14 ceiling).
- **No-false-negative sweep PASSED:** every reference type-set for n = 2..5 (n2 pairs + the audited
  Wikipedia rows) is a candidate — the filters pruned no realizable set.
- **The candidate inventory (pinned characterization):**

  | n | candidates | realized distinct type-sets |
  |--:|-----------:|----------------------------:|
  | 1 | **11** | 11 (exact!) |
  | 2 | 25 | 15 |
  | 3 | 95 | 36 |
  | 4 | 289 | 21 |
  | 5 | 686 | 12 |
  | 6 | 1224 | (10 tilings) |
  | 7 | 1624 | (7 tilings) |
  | 8 | 1617 | 0 tilings (rigidity, not incompatibility) |

  Over-generation grows with n, as ADR-0036 predicted — at n = 7 Phase 2 inherits ~1624 candidate sets for
  7 real tilings, so per-candidate refutation must be CHEAP (the solver's UNSAT side) and/or the filters
  strengthened (the noted frequency-LP is the natural next necessary condition if needed).

## Consequences

- **Positive:** closes the ADR-0036 fairness gap (type-sets and even the polygon alphabet are now derived);
  gives Phase 2 its fair per-type-set work-list; derives three documented theorems as fixtures; trivial
  compute (Σ C(15,n) subsets).
- **Negative / open:** the filters are necessary-only — the candidate inventory over-generates, and the size
  of that gap (measured below) is exactly the refutation work Phase 2 inherits; unordered pairs deliberately
  discard orientation/chirality strength (soundness over power).
