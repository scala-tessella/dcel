# ADR-0023: Euclidean wallpaper-orbifold generator (reaching n = 4–7)

- **Status:** Architecture A CLOSED OUT (= ADR-0025's bounded-V assembler, covolume wall). **Stage 1
  ORIENTED-slice generator BUILT and VALIDATED SOUND** — restricts generation to the rotation orbifolds
  (oriented closed D-sets), recovers mirror tilings as oriented doubles via `minimalSymbol`; generate tree is
  32×→167× smaller than generate-all (gap growing), finds REAL tilings beyond the oracle's reach. Escapes both
  walls (covolume + hyperbolic-universe). Remaining: push budget to n = 4–7 + tame `minimalSymbol` cost.
- **Date:** 2026-06-20

## Context — where ADR-0022 lands and why it stops

`DelaneySymbols` (ADR-0022) enumerates the Krotenheerdt tilings as minimal euclidean Delaney–Dress symbols,
filtered to regular `{3,4,6,8,12}`-gons with valid 360° vertices. It is **validated element-for-element**:
n = 1 = 11 and n = 2 = 20 exactly (vertex-type sets match the reference, zero spurious / missing); n = 3 = 38/39
with all 26 distinct type-sets present and none spurious (the one gap is a geometric duplicate sharing a
type-set). It also independently cross-checks the reference (engine and Wikipedia agree on the 26 n = 3
type-sets — and on **26**, not tessella3's untrusted 25).

It stops at the **D-set generation wall**. Measured facts that frame this ADR:

- **Minimal-symbol size grows ~linearly with n** (chambers): n = 1 → 1–10, n = 2 → 5–~22, n = 3 → 6–~24. So
  n = 7 is ≈ 45–60 chambers.
- **"Dress complexity" = chamber count.** The prebuilt genDSyms / Tegula databases (all euclidean ≤ complexity
  24) therefore cover **only n ≤ 3** — they cannot supply n = 4–7.
- ADR-0022's `DSetGenerator` enumerates ALL 2-manifold D-sets up to `maxSize`, then filters euclidean +
  regular-polygon. The sound prunes (r-values, max-curvature gate) reach n ≤ 3 in minutes, but cost grows ~5×
  per +2 chambers; ~50 chambers is hopeless. The general Huson/Delgado-Friedrichs euclidean enumerator
  (Tegula/Gavrog) is efficient for ALL euclidean tilings, but those are still an astronomical superset of our
  135 regular-polygon ones — the regular-polygon constraint must be woven INTO generation, not applied after.

## Decision (proposed) — generate by symmetry type, regular-polygon-native

Enumerate the tilings by building, for each **euclidean orbifold** (the 17 wallpaper symmetry types), the
**fundamental-domain map** out of regular polygons directly — never the hyperbolic universe, never the
non-regular euclidean tilings. The minimal Delaney–Dress symbol of each result is the canonical key
(deduplication) and the soundness certificate (intrinsic ⇒ the 3.3.6.6-style false-period overlaps cannot
arise — ADR-0022). `k`-uniform = `k` vertex orbits in the minimal symbol.

The 17 euclidean orbifolds (Conway): `o ×× ** *× 2222 22× 22* *2222 2*22 442 *442 4*2 333 *333 3*3 632 *632`.
Each fixes a bounded cone/mirror structure with cone orders ∈ {2,3,4,6} (crystallographic restriction).

### Two architectures (recommend A; B is the fallback if A's bucketing proves wrong)

**A. Torus map ("o") + minimization (recommended).** Every periodic tiling is, under its translation subgroup,
a **flat torus** tiling ("o" orbifold) — a finite combinatorial map of regular polygons with every vertex a
valid 360° type. Enumerate THOSE (the ADR-0021 torus-map idea, but built **intrinsically as a combinatorial
map / D-symbol**, so it is sound by construction — no coordinates, no guessed Λ, no overlap test). Then take
each map's **minimal** D-symbol (full symmetry, via the proven `isMinimal`/minimal-image) and bucket by its
vertex-orbit count `k`. This needs only ONE generator (the torus map), and minimization recovers the true `k`
(e.g. the honeycomb's 2 translation-orbits collapse to the 1 orbit of 6.6.6). Bounded by torus-cell size
(translation-orbit count ≈ k … ~2–3k), i.e. ~10–20 vertices for n = 7 — not ~50 chambers.

**B. Per-orbifold (fallback).** A direct generator per orbifold: place regular tiles in the orbifold's
fundamental domain consistent with its cones/mirrors. Avoids minimization, but is 17 cases with intricate
cone/mirror bookkeeping.

### Reuse vs new

- **Reuse from `DelaneySymbols`:** the D-symbol structure, orbit/curvature machinery, `isMinimal` /
  minimal-image, the regular-polygon + valid-vertex filter, vertex-config reconstruction, and the `Frac`
  curvature algebra — all become the **verify / canonical-key tail**.
- **New core:** the regular-polygon **fundamental-domain map grower** — vertex-completion growth that places
  only `{3,4,6,8,12}` tiles, completes every vertex to a valid 360° type, and closes the map (edge-pairings).
  For architecture A this is the torus-map grower; the genuine new work is the gluing/closure search.

## The crux risk (state it plainly)

The hard part is **tractable fundamental-domain map enumeration** — the "which boundary edge does a new edge
glue to" branching (ADR-0021's documented crux). The D-symbol framing removes the *soundness* and
*duplication* problems but NOT the branching. Controls (from ADR-0021, now on the intrinsic map): MRV
vertex-completion, antiparallel-only gluings, canonical-map dedup of partial states, and a measurement gate
(states-per-cell must track cell SIZE, not blow up) before any high-n run. This is the genuine research
unknown; everything else is engineering.

## Measurement spike (2026-06-20) — GO, conditional

Instrumented the existing geometric map grower (`KrotenheerdtTorusMapSearch`, vertex-completion + boundary
gluing) to count search STATES per closed cell and the cell's face count:

| run | states | cells | states/cell | closing-cell faces (min/avg/max) |
|-----|--------|-------|-------------|----------------------------------|
| n=1 covol≤2.6 | 81 | 4 | 20 | 3 / 4.5 / 6 |
| n=1 covol≤6 | 99 | 6 | 17 | 3 / 5.0 / 8 |
| n=2 covol≤2.6 | 525 | 4 | 131 | 3 / 4.5 / 6 |
| n=2 covol≤4 | 884 | 7 | 126 | 3 / 4.9 / 8 |

Findings:
- **The fundamental cells are tiny** — 3–8 faces at BOTH n=1 and n=2. The target objects are small; size is not
  the problem.
- **states/cell is BOUNDED per n** (n=2: 126 vs 131 across covolumes ⇒ it tracks cell COUNT linearly, not
  blowing up *within* a uniformity level). This is the gate's core property.
- **CAVEAT:** the states/cell CONSTANT grows ~7× from n=1 (≈17) to n=2 (≈130). That growth is the *grow-cover
  scatter* — aperiodic two-type patches grown to the face cap before being abandoned. Architecture A's
  **early gluing closes at the first valid gluing and never grows those patches**, so the real constant should
  be far smaller — but that is UNMEASURED (needs the early-gluing core).
- The geometric engine's ~0.6 s/state (BigDecimal overlap check + closure attempt on every state) is a
  geometric-engine artifact; architecture A (intrinsic D-symbol, no overlap test, cheap combinatorial ops) has
  cheap per-state cost.

**Verdict — GO, conditional.** The cells are small and the per-n branching is bounded, so the approach is
sound to pursue. Proceed to build architecture A's early-gluing grower, then **re-run this exact measurement
as a HARD gate**: if the early-gluing states/cell constant stays roughly flat across n (not ~7×/n), continue
to n = 4–7; if it still grows multiplicatively, fall back to Galebach's vertex-star method.

## Build plan — the early-gluing core (geometric realization)

First realization is **geometric** (reuse `KrotenheerdtTorusMapSearch`'s `FaceZ` / `ZetaPoint` / `verifyCell`
/ `boundaryHalfEdges` / `completions`), because the soundness hole that motivated the D-symbol pivot is
already closed by `tilesWithoutOverlap` — and with EARLY gluing that overlap test runs only at *closure*
(rare), not per state, so its cost is amortised away. The pure-intrinsic D-symbol form remains the fallback if
per-state cost still dominates.

- **State** `(faces: List[FaceZ], gens: Gens)` where `Gens = Rank0 | Rank1(g) | Rank2(g1,g2)` is the partial
  deck lattice (ℤ[ζ₁₂] vectors), developed in ONE plane frame.
- **Move EXTEND** — complete the most-constrained (MRV) boundary vertex by placing new faces
  (`growByCompletionPlanar`), kept iff planar-consistent (rank 0/1) or residue-consistent mod Λ (rank 2) and
  sound (`isSound`: every vertex a valid/extendable type, ≤ maxN completed types is NOT imposed — we bucket by
  k after).
- **Move GLUE** — at the MRV vertex, identify a boundary half-edge `(p1,b)` with an antiparallel one
  `(p2,b+6)`; deck vector `t = p1 + step(b) − p2`. `gens.add(t)`: `Rank0+t→Rank1(t)`;
  `Rank1(g)+t→Rank2(g,t)` if independent else require `t ∈ ⟨g⟩`; `Rank2+t→` keep iff
  `t.congruentMod(0,g1,g2)` else PRUNE (a third independent period ⇒ not a torus). Antiparallel-only =
  orientation-preserving (the ADR-0022 flatness condition); glue-at-MRV-only bounds the branch factor.
- **Closure** — once `Rank2`, gate by `distinctArea ≈ covolume` then run `verifyCell` (which already does the
  overlap test, fan/orbit checks, and the canonical key). `verifyCell = Some ⇒` emit and stop the branch.
- **Dedup** — visited-set on `canonicalKey(faces)` augmented with the (canonicalised) `gens`.
- **Re-measurement gate** — re-run the spike's states/cell measurement; proceed to n = 4–7 only if it stays
  flat across n (vs the grow-cover ~7×/n).

### Build outcome (2026-06-20) — geometric early-gluing BUILT, but the gate is NEGATIVE

`KrotenheerdtTorusMapSearch.enumerateByGluing` implements the above (the `Gens` rank-0/1/2 lattice with the
`congruentMod` prune, candidate glue vectors, Λ-consistent extend after rank-2, gated closure). It is
FUNCTIONAL and SOUND for n = 1 (finds the cells, no 3.3.6.6). **But it is SLOWER than grow-cover** (n = 1
mf = 16: ~43 s vs ~2 s) and shows the same residual issues (a non-primitive duplicate; `4.6.12` past the face
cap). Root cause: **EXTEND *before* rank-2 grows the planar patch with no Λ constraint — the same scatter as
grow-cover, now multiplied by the gluing branches.** Early gluing closes n = 1 cells from the corona, but
reaching n ≥ 2 cells needs pre-Λ extension, which scatters; the deck lattice cannot constrain growth until it
is discovered, and discovering it needs the growth. This is the genuine crux, and the geometric realization
does NOT resolve it.

**Revised recommendation:** switch the core to **Galebach's vertex-star gluing** (the proven 2002 method,
which reached n ≤ 6). It avoids free planar growth entirely: it grows the tiling by attaching whole
vertex-stars and tracking the symmetry orbits directly, so there is no aperiodic-patch scatter to prune. Keep
`DelaneySymbols` as the canonical-key / minimal-image tail (soundness + dedup) and as the n ≤ 3 oracle; keep
`enumerateByGluing` as a reference/cross-check at n = 1. The `o`-orbifold-via-minimization framing still holds;
only the *grower* changes from free-planar-extend to vertex-star-attach.

## Architecture A is now CLOSED OUT (ADR-0025): the per-orbifold route B is the remaining path

Architecture A (torus map "o" + minimization) was built and pushed hard under ADR-0025 as the bounded-`V`
dart assembler (intrinsic combinatorial map, sound, complete, exactly-identified, with ordered-port +
partial-map-canonical + face-closure pruning). Verdict: it works and is cheap for cells that FIT, but the
**torus-cell vertex count `V` IS the covolume**, so it hits the SAME exponential-in-covolume wall as the
fixed-Λ engines — only 6 of 20 two-uniform cells fit `V ≤ 6`, cost ≈ `c^V`, n = 4–7 unreachable. So "o +
minimization" is decided: NOT the route. **Architecture B (per-orbifold, small symmetric symbols) is what
remains.**

## Per-orbifold generator — STAGE 0 de-risk + infrastructure BUILT (2026-06-20)

Before committing to the intricate 17-orbifold build, measured whether the premise holds — that the regular
euclidean symbols are FEW and the generate-all engine wastes its effort on the hyperbolic universe (which the
per-orbifold route skips by construction). New, validated infrastructure on `DelaneySymbols` (reuses its
proven internals, so correct): `enumerateSymbols` (returns the minimal symbols), `orbifoldSignature` (the
cone/mirror/orbit shape), `generationStats` (the generate-all waste). Probe `OrbifoldInventoryProbe`.

**Finding 1 — the waste is huge and GROWS with size** (`generationStats`, n ≤ 3):

| maxSize | total D-sets walked | euclidean-feasible | euclidean % | regular-euclidean symbols |
|---------|--------------------|--------------------|-------------|---------------------------|
| 10 | 809 | 327 | 40.4% | 31 |
| 12 | 3 395 | 709 | 20.9% | 38 |
| 14 | 10 716 | 1 482 | 13.8% | 44 |
| 16 | 43 482 | 3 270 | 7.5% | 56 |
| 18 | 172 218 | 7 407 | 4.3% | 62 |

The total tree grows ≈ 4–5× per +2 chambers (the wall); the euclidean slice only ≈ 2×; the **regular**
targets grow nearly linearly (31 → 62). At maxSize 18 generate-all walks 172 k D-sets to surface 62 regular
tilings — **99.96 % waste, and the euclidean fraction keeps falling** (40 % → 4.3 %). At the ~30–60-chamber
sizes n = 4–7 needs, the euclidean fraction is well under 1 %: an orbifold-directed generator that visits only
the euclidean slice is the structural win, and it grows with n. **Premise CONFIRMED.**

**Finding 2 — the orbifold structure (what B must build):** of the 65 regular symbols at n ≤ 3, **63 are
mirror orbifolds** (`mir=true`) — only 2 are mirror-free — with cone/corner orders ∈ {2,3,4,6} (the
crystallographic restriction), chamber sizes 1–20. So B is dominated by the MIRROR orbifolds (the `*XYZ`,
`X*Y`, `XY*` families): place regular tiles in a fundamental polygon with mirror edges and cones of order
{2,3,4,6}. The orbifold signatures are near-distinct per tiling (the regular + cone structure is what selects,
not a few buckets each with many tilings) — so B enumerates each orbifold's regular triangulations, bounded by
domain size, not by the hyperbolic D-set count.

## Stage 1 — the ORIENTED-slice generator BUILT and VALIDATED (2026-06-20)

Rather than encode 17 orbifolds with mirror/cone/chain bookkeeping, Stage 1 takes the **oriented slice** and
lets minimization recover the rest. `OrientedDSetGenerator` (in `DelaneySymbols`) is `DSetGenerator` restricted
to CLOSED, ORIENTED D-sets — no `σ_i` fixed points (no mirror boundaries), consistent 2-colouring — i.e. the
rotation orbifolds `o/2222/333/442/632`. A mirror tiling is recovered as its **oriented double cover** (≤ 2×
the chambers of its mirror minimal symbol); the A068600 `n` is then read off the FULL `minimalSymbol` (whose
automorphisms include the orientation-reversing reflections), so the mirror symmetry is not lost — only the
*generation* is restricted. `orientedRegularSymbols` runs the slice → euclidean v-values → regular filter →
minimal key + dedup, reusing every validated downstream piece.

**Validated SOUND.** Every result passes the same `isEuclidean` + `regularPolygonVertices` filters as the
oracle and keys (via `minimalSymbol`) identically. Cross-check (`OrbifoldStage1Probe`): of the oriented results
keyed by minimal symbol, **zero are truly spurious** — the 2 that were absent from the generate-all oracle at
maxSize 18 are REAL tilings present at maxSize 24 (`{3⁶;3⁴.6}`, which is in `TilingReference.n2`, and a
3-uniform `{3².4.3.4; 3³.4²; 4⁴}`). So the oriented generator finds real tilings BEYOND the generate-all
oracle's reach — exactly the large-translation-cell / small-orbifold tilings the bounded-V assembler
(ADR-0025) could not reach.

**The structural win — the tree is far smaller and the gap grows:**

| maxSize | generate-all D-sets | oriented D-sets | ratio |
|---------|---------------------|-----------------|-------|
| 10 | 809 | 25 | 32× |
| 12 | 3 395 | 68 | 50× |
| 14 | 10 716 | 103 | 104× |
| 16 | 43 482 | 260 | **167×** |

At oriSize 30 it recovers 11/11 (n=1), 18/19 (n=2), 21/35 (n=3) — the gap is purely budget (oriented doubles
need ~2× chambers, affordable because the tree is 167× cheaper and the ratio rises). This is the first engine
in the project that is sound, complete-in-principle, AND escapes both walls (covolume and hyperbolic-universe)
— because it is bounded by the SMALL oriented orbifold symbol, not the translation cell or the 2-manifold
D-set count.

**It CROSSES n = 3 — the project's first.** `OrbifoldPushProbe`, counts (got / A068600) that GROW with the
chamber budget:

| n | oriSize 32 (124 s) | oriSize 34 (286 s) |
|---|--------------------|--------------------|
| 1 | **11 / 11 ✓** | **11 / 11 ✓** |
| 2 | 19 / 20 | 19 / 20 |
| 3 | 28 / 39 | 29 / 39 |
| 4 | **3 / 33** | **5 / 33** |
| 5 | **1 / 15** | **1 / 15** |
| 6 | 0 / 10 | 0 / 10 |
| 7 | 0 / 7 | 0 / 7 |

So it reaches **n = 4 and n = 5** — genuine 4- and 5-uniform tilings, which generate-all (n ≤ 3 wall) and the
bounded-V assembler (covolume wall) both cannot produce — and the counts rise monotonically with budget
(n = 4: 3 → 5 over +2 chambers). The counts are LOWER BOUNDS: a tiling appears once its oriented double fits
`oriSize`, and the low-symmetry n ≥ 3 doubles exceed these budgets.

**Status: Stage 1 oriented-slice BUILT, VALIDATED, and REACHING n = 4–5.**

### Profiling + optimization (2026-06-20) — and a corrected diagnosis

The earlier guess that `minimalSymbol` dominates was WRONG. `orientedProfile` (oriSize 28): of 9.4 s, the
**GENERATION is 8.8 s (94 %)**; `minimalSymbol`/`canonicalKey`/`DSymGen` are ≈ 0. The per-child
`isWeaklyOriented` was an O(size) BFS run on every node. Replaced with an **O(1) incremental 2-colouring**
threaded through the generator state (each `σ_i` pairing must join opposite colours; the manifold-closure edge
too), redundant per-child `isLoopless` dropped (kept only as a completed-symbol backstop). Result: generation
**8.8 s → 5.5 s** at oriSize 28, identical 52 835-D-set search, validated (same results, 0 truly-spurious,
n = 1 11/11).

### The real scaling limit (measured)

The oriented D-set tree grows ≈ 2.4× per +2 chambers, and ACCELERATES past that at larger sizes (oriSize 36
did not finish in 12 min where ~5 was projected). Combined with the **2× chamber penalty of the oriented
double** (a tiling's oriented symbol is up to twice its mirror minimal), this means: the oriented-double route
gets the HIGH-symmetry n = 4–7 tilings cheaply, but the LOW-symmetry ones (large mirror symbols → ~60-chamber
doubles) are out of budget. So it REACHES n = 4–7 with growing partial counts, but COMPLETING them this way is
compute-bound.

**Consequence for the plan.** To complete n = 4–7, the decisive remaining win is to drop the 2× penalty by
generating the **mirror minimal symbols directly** — i.e. the per-orbifold mirror generator (architecture B
proper) after all, OR interleaving v-assignment with generation so the euclidean/regular constraint prunes the
~96 % non-euclidean D-sets during generation (the generation, not minimization, is the wall). The oriented
generator stands as the validated engine that first crossed n = 3; the next build is halving the chamber count.

## Validation ladder

1. **Oracle cross-check:** the existing `DelaneySymbols` engine is correct through n = 3 — the new generator
   must reproduce it EXACTLY (counts and vertex-type sets) for n ≤ 3, by symmetry type where possible.
2. **Reference:** then `TilingReference` counts for n = 4, 5, 6, 7 (33, 15, 10, 7) and, where the
   appendix has them, the n = 4/5 vertex-type sets.
3. **Measurement gate:** instrument states-per-cell at n = 2/3; proceed to n = 4–7 only if it tracks cell size.

## Consequences

- The only route to n = 4–7 (≈ 45–60-chamber symbols) within the project: the cost tracks fundamental-domain
  size, not the hyperbolic D-set universe.
- Larger and higher-risk than ADR-0022 (a new search, with the ADR-0021 branching risk re-incurred — but now
  sound and canonical via D-symbols).
- `DelaneySymbols` stays as the validated oracle for n ≤ 3 and the verify/key tail.

## Alternatives considered

- **Prebuilt genDSyms/Tegula database (≤ complexity 24).** Covers only n ≤ 3 (chamber-count wall). Rejected for
  the goal; already subsumed by ADR-0022.
- **Reuse Tegula/Gavrog's euclidean enumerator.** JVM and efficient, but enumerates ALL euclidean tilings —
  filtering our regular-polygon needle from that haystack at ~50 chambers is itself intractable, plus
  integration risk. Rejected as the core; possibly useful as an independent oracle for small n.
- **Galebach's vertex-star gluing (2002).** The proven method for exactly this problem (reached n ≤ 6) and
  regular-polygon-native — but it is NOT D-symbol-based, so it forgoes the canonical-key / soundness reuse and
  would re-introduce a bespoke dedup/soundness layer. A strong alternative if architecture A's torus-map
  branching proves intractable; worth a literature pass before committing.
- **Stop at n ≤ 3.** `DelaneySymbols` already gives validated n ≤ 3; ship that and defer n = 4–7. The honest
  default if the branching risk is judged too high for the payoff.
