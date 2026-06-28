# ADR-0038: Profile automaton (cylinder transfer matrix) for the banded family

- **Status:** Accepted; engine core built + validated test-first. 2026-06-25 (updated 2026-06-28).
- **Build state (2026-06-28):** `ProfileAutomaton` implements the explicit combinatorial profile + fill-vertex
  surgery + cycle-detection close. `ProfileAutomatonSpec` (7 tests, green): Rung 1 — surgery correct on the
  square row (new top vertex's below-fan, neighbour absorption); Rung 2 — driving from the square-row profile
  CLOSES `4⁴` key-for-key vs the oracle (cycle ⇒ `CylinderAutomaton.close`); Rung 3 — finds `3⁶` (triangle row)
  and DISCOVERS `3³.4²` from the square-row seed's triangle-fill branch (not hand-fed). The DRY refactor
  (generalized grower `*By` + exposed primitives) is regression-clean (67 tests).
- **Build state (cont.):** automatic seeds done (StripBand band tops, `ProfileAutomaton.seeds`); efficient
  cycle-finder done. The naive path-local DFS explodes at n=3 (cycles ~10–40 fills, exponential branching). FIX =
  a transfer-matrix structure: **per target n-type-set**, build the PLAIN profile graph restricted to fills of
  that type-set (finite & small; the type-set is the bound), then find cycles COVERING all n types (BFS over
  `(profile, types-used)`) and close each (`enumerateForTypeSet`/`enumerate`). GATE (`ProfileGateProbe`,
  c∈{2,4}) on the 4 banded gap type-sets: the engine closes oracle cells **key-for-key with ZERO spurious**
  (sound!), but RECALL is low (2/13 gap cells): (a) shortest-covering-cycle-per-node finds only one cell per
  node — need multi-cycle enumeration; (b) hexagon type-sets need `c = √3·k` (non-integer) — at integer `c`
  hexagons wrap/don't fit. ⇒ NEXT: enumerate multiple covering cycles per type-set + add √3 circumferences +
  tune bounds. Soundness (the hard part) is proven.
- **Recall progress (2026-06-28, cont.):** (a) multiple covering cycles per type-set via **BFS over
  `(profile, types-used)`** — a DFS to `maxLen` explodes and found 0 (regression); BFS is bounded and efficient;
  (b) start cycles from EVERY graph node (a cell's cycle need not pass a band-top seed); (c) generalize `c` to
  any horizontal ℤ[ζ₁₂] vector for the **√3-family** (`seedsC`/`buildGraphForC`/`enumerateForTypeSetC`); hexagons
  need `c = 2√3` (minimal above their extent 2). GATE (`ProfileGateProbe`, c∈{2,4,2√3}, maxNodes 8000): **6/13
  banded gap cells matched key-for-key, ZERO spurious** (up from 2/13): the square/triangle gap sets give 2/4
  each, the hexagon-heavy `3⁶;3⁵.6;3.3.6.6` gives 2/3; the pure-hexagon `3.3.6.6;3.6.3.6;6.6.6` is still 0
  (every type is √3-period; needs larger/finer √3 circumferences than `2√3@8000`). REMAINING LEVERS (recall, not
  soundness): more circumferences (3, 3√3, …) + higher `maxNodes`/`capPerNode`/`maxLen`; PERFORMANCE — the `2√3`
  graph *build* (`fillLowest`×nodes) is the slow path (a full 4-set sweep at maxNodes 30000 doesn't finish in
  ~18min; 8000 does). Soundness holds throughout.
- **Perf + recall diagnosis (2026-06-28, cont.):** `fillLowest` was dominated by BigDecimal `toBigPoint`
  (`foldPos`'s while-loop + `lowestIndex`); replaced with a fast Double embedding (`xD`/`yD`) and an O(1) exact
  `foldPos` — a full **6-circumference** sweep ({2,3,4,6,2√3,3√3}) now runs in **~3.5 min** (was: 3 circs didn't
  finish in 18min). BUT recall PLATEAUED at **6/13** even at maxNodes 40000 / capPerNode 48 / maxLen 72 — so the
  7 holdouts are STRUCTURAL, not bounds. `pa.debug` per (type-set, c) shows: the pure-hexagon set
  `3.3.6.6;3.6.3.6;6.6.6` GROWS only **12 nodes / 0 cycles at its correct `c=2√3`** (growth stalls), while the
  hexagon-MIXED `3⁶;3⁵.6;3.3.6.6` grows fine there (63 nodes / 76 cycles / 2 emitted); integer `c` just cap at
  12000 nodes with 0 cycles (wrong period). ⇒ the holdout is **hexagon-heavy growth/seed coverage at √3** (the
  restricted fills or StripBand hexagon seeds can't continue a `6.6.6`/`3.6.3.6` tiling), NOT √3-precision and
  NOT bounds. NEXT: debug hexagon seeds/fills at `2√3` (why growth stalls after 12 profiles). Soundness intact.
- **CORRECTED finding (2026-06-28, `HexStallProbe` + `GapClassifyProbe`):** (i) growth does NOT stall — pure
  `6.6.6`/`3.6.3.6` cuts grow fine and `fillLowest` CAN mix types; the "12 nodes / 0 cycles" means the reachable
  graph has no cycle COVERING all 3 types. (ii) a WIDER circumference (`4√3`, `6√3`) does NOT help either. (iii)
  the earlier "isotropic" hypothesis is WRONG: `GapClassifyProbe` (lattice aspect = orthogonal-extent/|h|)
  classifies EVERY drawable gap cell as BANDED (aspect 1.7–5.6, isotropic 0/0); the engine reaches **5–6 of the
  11–13 banded cells**, missing genuinely-banded ones incl. the most anisotropic (`D=13` asp 4.6, `D=15` asp 5.6,
  both |h|=1) and the hexagon `D=9` (asp 2.0). Since escalating bounds AND widening circumferences both fail on
  `|h|∈{1,√3}` cells whose circumferences ARE swept, the gap is **REACHABILITY / SEED COVERAGE**: the missing
  cells' profiles aren't reached from the StripBand band-top seeds via type-restricted growth. ⇒ NEXT: enrich
  seeds (the StripBand catalogue lacks cuts of the harder banded cells) and/or make growth reach more profiles.
  Soundness intact (zero spurious throughout).
- **Seed enrichment attempt + confirmed cause (2026-06-28):** seeded from the UNdeduplicated band set
  (`StripBand.allBands`, vs the deduped `catalogue` which keeps only the smallest band per type) — **recall
  unchanged at 6/13** (honest negative). The cause is now pinned by `pa.debug`: at `c=2` for `{3⁶;3³.4²;4⁴}` the
  type-restricted up-facing growth reaches only **68 profiles total (NOT capped)**, and the missing cells' cuts
  are not among them. So up-facing taut growth is DIRECTIONAL and reaches only a *subset* (one connected
  component) of the profile graph; the missing banded cells' cut-profiles live in unreached components, and
  `StripBand.fillAbove` doesn't generate them as seeds. ⇒ the real fix is a more COMPLETE answer-blind
  seed/profile generator (direct enumeration of valid profiles at `c`, or bidirectional growth to connect
  components) — a focused sub-task. Added `ProfileAutomaton.enumerateFromSeeds` (explicit-seed hook) to later
  confirm seed-coverage by feeding a known cell's own cut-profile. Soundness intact.
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

## Recall ceiling: NOT seed coverage (decisive, 2026-06-28)

The engine plateaus at **6/13 banded gap cells**. Two independent seed-enrichment attempts were measured and
**both failed to lift recall**, which rules out seed coverage as the cause:

1. **Undeduped band-tops** (`StripBand.allBands`, every fillAbove of every polyline, not just the smallest per
   type): no change.
2. **The complete direct profile enumerator** (`ProfileAutomaton.enumerateProfiles` / `completeSeeds`):
   enumerates EVERY profile at circumference `c` — every x-monotone polyline summing to `c` (any rotation) ×
   every edge-consistent below-fan over the type-set's polygons, reaching the irreducible period-`c` profiles
   that band-top replication (sub-period) cannot. It generates **vastly** more seeds (e.g. `c=6`: 46 k vs 23)
   yet recall is **unchanged at 6/13**.

`pa.debug` localises the wall: at `c=2` for `{3⁶;3³.4²;4⁴}` the COMPLETE 12-profile seed set grows to only
**14 nodes / 7 cycles**, and the high-aspect cells `D=13` (aspect 4.6) and `D=15` (5.6) are never closed — their
~9–11-profile covering cycles **do not grow** even from a comprehensive seed set. So the ceiling lives in
**growth / cycle-finding for high-aspect cells, not seed generation**:

- the taut *lowest-vertex, up-facing* `fillLowest` growth, from a comprehensive seed set, does not reach the full
  cycle of the tallest (highest-aspect) banded cells — its forward-reachable, type-restricted graph stays small;
- `completeSeeds` also **explodes at large `c`** (46 k–72 k seeds, exceeding `maxNodes` so the BFS cannot grow at
  all), so it is *worse* than band-tops as a default. ⇒ the default seed source stays `seedsC` (band-tops);
  `enumerateProfiles`/`completeSeeds` are kept as **diagnostic** entry points, NOT wired in.

**Decisive next diagnostic (not yet run): cut-and-feed.** Realize a known-missing cell (e.g. `D=13`), cut its
exact ℤ[ζ₁₂] tiling at circumference `c=2|h|` into a profile, feed it via `enumerateFromSeeds`, and observe
whether the engine *closes* it. This isolates the two remaining hypotheses — (a) `enumerateProfiles` still does
not generate that specific cut (enumerator incompleteness), vs (b) the cut IS reachable as a seed but the
taut growth / covering-cycle BFS cannot close its cycle (growth-rule limitation). Until that test runs, the
plateau is attributed to growth/cycle-finding, NOT seeds.
