# ADR-0038: Profile automaton (cylinder transfer matrix) for the banded family

- **Status:** Accepted; engine core built + validated test-first. 2026-06-25 (updated 2026-06-28).
- **Build state (2026-06-28):** `ProfileAutomaton` implements the explicit combinatorial profile + fill-vertex
  surgery + cycle-detection close. `ProfileAutomatonSpec` (7 tests, green): Rung 1 — surgery correct on the
  square row (new top vertex's below-fan, neighbour absorption); Rung 2 — driving from the square-row profile
  CLOSES `4⁴` key-for-key vs the oracle (cycle ⇒ `CylinderAutomaton.close`); Rung 3 — finds `3⁶` (triangle row)
  and DISCOVERS `3³.4²` from the square-row seed's triangle-fill branch (not hand-fed). The DRY refactor
  (generalized grower `*By` + exposed primitives) is regression-clean (67 tests). REMAINING: automatic seed
  enumeration, the `c` sweep, and the n=3 gate (the 6 grower-missed cells).
- **Reframes:** [[0037-strip-stacking-enumerator-for-the-banded-family]]. The band *generator* (`StripBand.fillAbove`
  / `bandValid`, exact ℤ[ζ₁₂], test-verified) is kept, but its **organization** changes: bands are no longer
  catalogued in a vacuum (that produced 55 layer-types, most irrelevant), they become the **transitions of a
  finite automaton at a fixed cylinder circumference**.

## Why the band catalogue was the wrong object

A band only has meaning relative to a horizontal period `h`. The ADR-0037 catalogue (a) fixed no `h`, (b)
enumerated layers independently instead of as *transitions between matching profiles*, and (c) never pruned
dead-end fans (300° notches, `3⁸` rows, …). It conflated *every conceivable layer* with *layers that participate
in a tiling*. Relevance only emerges from **connectivity into a cycle** — which is what an automaton encodes.

## Decision — the exploitation

A banded tiling is exactly **a tiling invariant under one short translation `h`**. Quotient the plane by `⟨h⟩`
→ an infinite **cylinder** of circumference `|h|`. Tiling a cylinder is a **1-D transfer-matrix / automaton**
problem; a doubly-periodic (banded) tiling is a **cycle** in it. This is 1-D (cost linear in stack height, not
exponential in covolume) — directly defeating both the grower's anisotropy bias and the fixed-Λ covolume wall,
**in the long direction where the missed cells live**.

### Definitions

- **Cylinder.** Fix `h ∈ ℤ[ζ₁₂]` horizontal (WLOG, by the order-24 point group), realizable as a sum of unit
  edges, `|h| ≤ Hmax`. Identify `x ≡ x + h`.
- **Profile (automaton state).** The growth **front**: the boundary between tiled (below) and untiled (above),
  a closed `h`-periodic polyline of unit edges plus, at each front vertex, the **below-fan** (the contiguous
  occupied 30°-slots / CCW polygon sizes already placed beneath it — a proper partial vertex fan). Canonicalised
  mod `h`-translation and mod vertical offset (front shape relative to its own min-y). This is `StripBand`'s
  `Profile`, carrying below-fans.
- **Transition.** From a profile, pick the **lowest–leftmost incomplete front vertex** (deterministic, MRV-style)
  and branch over every **valid completion** of its 360° (below-fan ∪ added-fan ∈ `validSignatures`), placing the
  1–4 polygons that finish it, gated by cylinder planar-consistency (`isConsistent` mod `h`: no overlap,
  edge-to-edge). Each completion yields a successor profile. This is exactly `growByCompletion` on the cylinder.
  A **band** is the coarse interpretation = one full revolution of the front (it has no privileged role in the
  search; it is the validation label).
- **Closing (cycle).** Hash each canonical front. When a front **shape + below-fans recurs** at higher `y`, the
  strip between the two equal fronts is a fundamental vertical period `Δ`. Close into a torus cell with periods
  `(h, Δ)`; gate with `KrotenheerdtTorusMapSearch.verifyCell(faces, h, Δ, maxN)`; accept iff it tiles with
  **exactly `n`** vertex types; key by the returned **D-symbol key** (so results dedup with the oracle / grower /
  bounded-V). Bound the height by `Vmax` (the analogue of `maxFaces`); fronts that never recur are discarded.

### Completeness (for `(Hmax, Vmax)`)

1. Every banded n-uniform tiling `T` has a shortest in-band translation `h_T` (anisotropy ⇒ it exists and is
   short); rotate it horizontal, `|h_T| ≤ Hmax`.
2. `growByCompletion` is sound **and complete** (it explores every valid completion of the forced vertex); on
   the cylinder it respects `⟨h_T⟩` via `isConsistent` mod `h`. So it reaches `T`'s fronts.
3. `T` is vertically periodic with some `Δ_T ≤ Vmax`, so its front recurs ⇒ the detector closes `T`.
4. Sweep `h` over horizontal translations `|h| ≤ Hmax`; union over `h` = the whole banded family. `(Hmax, Vmax)`
   is a monotone completeness caveat (like the oracle's `maxSize`), checked by **stabilization** (counts stop
   growing as the bounds rise).

**Soundness** is inherited verbatim from `verifyCell` (overlap-free, tiles the torus, all fans complete, ≤ n
types) — identical guarantee to every other engine.

### h-sweep bound

Horizontal module vectors satisfy `a₂ = 0, a₁ = −2a₃` (zero `y`-embedding); enumerate those with `|h| ≤ Hmax`
directly (lengths `1, √3, 2, 2√3/… ≤ Hmax`). `Hmax ≈ 4`, `Vmax ≈ 12` faces suffice for n=3; both grow modestly
with `n`.

**Empirically confirmed (`MissedPeriodProbe`, 2026-06-27).** Measured `|h|` (shortest lattice vector, via
`realizeCell`'s exact Λ) of every grower-missed n=3 cell: all 6 realizable banded misses have **`|h| ∈ {1, 2}`**
— inside `Hmax=4` with margin (even `Hmax=2` covers them). They are strongly **anisotropic** (height/`|h|` up to
**5.6:1**), confirming both the small-`h` assumption and the diagnosis (elongated cells the compact-disk grower
can't reach). The two unmeasured misses are the known non-banded **C₆ `D=11` outlier** (out of scope) and one
marginal cell bounded-V didn't realize at `maxV=16`. Grower-*reached* cells in the same type-sets have similar
`|h|` but low anisotropy (≈ 3.0–3.7), explaining the reach split.

## Validation plan (gate before scaling)

1. **State finiteness / determinism:** the profile set at a fixed small `h` is finite and stable; the canonical
   front key is translation/rotation-invariant (unit-test).
2. **Transition soundness:** every successor is cylinder-consistent and completes exactly the chosen vertex.
3. **Cycle ⇒ verifyCell:** every closed cell passes `verifyCell`; no spurious.
4. **THE success test:** the `h`-sweep at n=3 reproduces the banded family **including the 6 grower-missed n=3
   tilings** (compare by D-symbol key against the oracle — same key space, so key-for-key, not type-set).
5. Then push `Hmax/Vmax` to stabilization at n=3, and extend to n=4.

## Spike findings (2026-06-27, `CylinderAutomaton` + `CylinderAutomatonSpec` + `CylProbe`)

A vertical-slice spike grew tilings on a fixed-`h` cylinder reusing the grower's machinery (the planar
`isPlanarConsistent`/`isSound`/`growByCompletionPlanar` were generalised to `*By(vid, …)` taking a vertex-identity
— planar `identity`, cylinder mod-⟨h⟩ — so there is ONE growth implementation; regression-clean across 45 grower
tests). Results:

- **B (soundness) — VALIDATED.** `isConsistentBy` with the mod-⟨h⟩ identity accepts valid layers, rejects
  overlaps.
- **C (closing ⇒ oracle key) — VALIDATED, the key result.** Strips grown on the circumference-2 cylinder close
  via `verifyCell` (gate) + `torusMapClassify` (D-symbol key on the `primitiveBasis`) to the EXACT keys the
  oracle assigns, for ALL FOUR period-≤2 banded 1-uniform tilings: `4⁴`, `3³.4²`, `3.6.3.6`, `3⁶`. The
  grow→close→key pipeline works in the shared key space (unlike `enumerateBanded`'s disjoint geometric key).
- **Wrapping caveat.** At circumference ≤ a polygon's horizontal extent a polygon's edge wraps onto itself (its
  endpoints fold to one vertex), which the place-one-polygon-per-fan-gap growth can't express. Grow instead at
  `C = k·|h|` above the max extent and let `primitiveBasis` recover the primitive period (a |h|=1 cell is found
  as a period-2 strip and reduced back).
- **A (finiteness) — the spike's patch-growth does NOT bound the state space**, and the spike shows why:
  completing the lowest vertex advances the front in ONE direction but leaves the seed's far side permanently
  incomplete, so the tracked front spans an unbounded y-range (`CylProbe`: 57→118→178→291→440→1244 as the face
  budget rises — accelerating, not saturating). **Resolution (sharpens the Decision):** the transfer-matrix
  state must be the advancing **top profile** ([[0037…]]'s `StripBand.Profile` — a *combinatorial* edge-sequence
  + per-vertex fans), grown one-directionally with the completed region **forgotten**. Combinatorial profiles at
  fixed `C` are finite; a geometric front fingerprint over the whole patch is not. So the full engine grows
  PROFILES (StripBand) and closes them with the spike's validated `CylinderAutomaton.close` path — the two
  ADR-0037/0038 threads converge: **profiles are the states, the cylinder close is the acceptor.**

- **Why a patch CANNOT be the state (sharpened, 2026-06-27).** Two follow-ups confirmed the patch representation
  cannot bound the state regardless of heuristic: (i) restricting growth to UP-FACING frontier vertices (advance
  one way) reduced but did NOT stop the front-count growth (41→75→…→304) AND dropped a tiling (`3.6.3.6` stopped
  closing); (ii) the forward-reachable set of *partial* profiles is genuinely infinite (aperiodic partial
  growths). Root cause: a face-based patch always retains the bottom cut's OPEN-BELOW vertices, so the state
  carries a growing trailing edge — a patch cannot express *"below the profile is external/done."* Only the
  combinatorial profile `(edges, belowFans)` seals the bottom (every vertex's open arc is above; below is an
  abstract fan, not empty faces). ⇒ the engine MUST use the explicit combinatorial profile as the state, with
  transitions = local surgery (fill the lowest vertex, splice the fan's upper boundary into the polyline, update
  the two neighbours' below-fans) and tilings = CYCLES in the finite profile graph — NOT forward-reachability
  saturation. The spike's `verifyCell` + `torusMapClassify` close path is reused unchanged to key each cycle.

## Consequences

- **Positive:** an original, sound, 1-D enumerator for exactly the class that breaks the grower; bands appear
  only as front-revolutions on a path to a cycle (no irrelevant layers); reuses `growByCompletion` + `verifyCell`
  (sound by construction); shared D-symbol key (composes into the union); scales in the long direction.
- **Negative / open:** the `(Hmax, Vmax)` bound is a completeness caveat (mitigated by stabilization). The
  front-state count per `h` must be empirically bounded (the finiteness lemma rests on taut, lowest-front
  growth). Non-banded misses (the single C₆ outlier) remain out of scope — handled elsewhere.
- The ADR-0037 `StripBand` catalogue is **demoted** from "the search" to a **test oracle**: the transition
  alphabet at small `h`, used to check the automaton's transitions against hand-verified canonical bands.
