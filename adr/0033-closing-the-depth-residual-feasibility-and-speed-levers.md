# ADR-0033: Closing the depth residual — n≤7 feasibility and the speed levers

- **Status:** Accepted (decision: build the partial-fan arc constraint first). 2026-06-23.
- **Date:** 2026-06-23
- **Follows:** [[0032-symmetry-first-geometric-grower]] (the engine, the per-state/memory wins, and the
  `canonicalKey` over-pruning fix that made the grower sound + deterministic).

## Context — where we are

After the `canonicalKey` over-pruning fix (ADR-0032, 2026-06-23), the rotation-first engine is **sound,
deterministic, and complete-for-its-depth**, and the union with bounded-V stands at (all rotation-only, zero
rotation-free observed at every level):

| level | count | of | how |
|---|---|---|---|
| n=2 | **20** | 20 | quiesced, trivial |
| n=3 | **36** | 39 | constrained deep sweep, maxFaces=96, 55 min |
| n=4 | **27** | 33 | union, maxFaces=64, quiesced 54 min |

The residual is now a **single, clean cause: fundamental-domain DEPTH.** Every short cell is a
`3.4.4.6`/snub-`3.3.3.3.6`-rich **C₂** tiling whose domain (cell/2) exceeds the `maxFaces` used. The measured
cost curve is **diminishing returns**: n=3 recovered 33→35→36 as maxFaces went 58→96 (~1 cell per depth step,
~55 min/sweep), because each next cell has a larger domain.

This ADR records the two strategic questions this raises, with the pros/cons of each option, and the decision.

## Question A — does this path finish n=1–7 in ≤ 1 week?

**Verdict: n≤4 yes (days); n=5 probably; n=6–7 not with today's *completion-only* constraint — they need a
stronger prune first.** It is a compute/algorithm question, not correctness: the rotation-only thesis is sound
(every Krötenheerdt tiling has ≥1 rotation centre — confirmed n=2/3/4, zero rotation-free), so completeness is
reachable in principle. The binding cost is the long tail of large-domain C₂ cells, whose search grows with n
(more vertex types ⇒ bigger domains). Galebach did n≤6 in ~1 month on 2002 hardware; we have ~100–1000× the
compute and a better algorithm, so n≤6 in a week is plausible **iff** the deep-cell search is made competitive.
As-is, the curve likely stalls around n=5–6 within the week.

## Question B — the speed levers (pros / cons)

### 1. Stronger constraint: partial-fan ARC pruning  ← CHOSEN
Prune a growth child whose *partial* fan at a vertex cannot be a contiguous **arc** of any target vertex
figure (today the type-set constraint only fires when a vertex *completes* off-target).
- **Pros:** cuts per-step branching from ~4 ({3,4,6,12}) to ~1–2 ⇒ **exponential** reduction in tree size at
  depth — the deep C₂ cells go from "10-min cap, still short" to "closes fast"; contained change to
  `growBySymmetry`; soundness is provable + testable (a valid T-tiling's every partial fan IS an arc of one of
  its vertex types, so the prune never removes a valid path); composes with every other lever; the single
  lever most likely to decide n=5–7.
- **Cons:** needs a correct cyclic sub-arc test over vertex figures incl. orientation/reflection (care to keep
  it sound); a buggy/too-aggressive version could prune valid paths (mitigate with the no-valid-loss test on a
  reached type-set, like the existing constraint test); modest implementation risk.

### 2. Closure-directed (best-first) growth order
Order the frontier by closeness-to-closing (boundary/area ratio) instead of MRV/DFS.
- **Pros:** reaches the closing patch with far fewer states; composes with #1.
- **Cons:** more invasive (frontier becomes a priority queue, complicates the work-stealing pool); heuristic —
  may not help uniformly; medium effort.

### 3. Grower `targetCount` early-stop
Stop growing a type-set once its multiplicity is union-reached (bounded-V + grower).
- **Pros:** trivial; no wasted time confirming cells already in hand.
- **Cons:** small absolute win (the cost is the *unreached* deep cells, which have no early-stop).

### 4. Exact-integer `primitiveBasis`
Same ℤ[ζ₁₂]-exact treatment that gave `tilesWithoutOverlap` ~120×.
- **Pros:** removes the last BigDecimal hotspot; free-ish once #1/#2 shrink the rest.
- **Cons:** now only ~10–20% (paid on closing candidates after the verifyCell reorder), so low priority;
  touches shared `KrotenheerdtLatticeSearch` code (wider blast radius).

### 5. Distribution across machines
Fan the per-type-set deep sweeps over cloud workers (embarrassingly parallel).
- **Pros:** linear speedup in worker count; no algorithm change.
- **Cons:** infra, not algorithm; only helps if single-box *compute* (not the tree size) is the wall — better
  to shrink the tree first (#1) so each worker does less. Last resort.

## Decision

Build **#1 — the partial-fan arc constraint** next. It is the highest-leverage, soundness-testable, contained
change, and it directly determines whether n=5–7 fits in the week. If it delivers the expected exponential
pruning, "n≤7 in a week" moves from *uncertain* to *likely*; if not, that is the signal that n=6–7 needs
distribution (#5). Levers #2–#4 are follow-ups that compose on top.

## Consequences

- **Positive:** attacks the only remaining cause (deep-cell search cost) at its root; keeps the engine sound
  (the prune is provably non-lossy); reuses the existing constrained-grower plumbing + its no-valid-loss test.
- **Negative / risk:** a subtle bug in the arc test could silently prune valid paths — mitigated by the
  no-valid-loss regression (constrained ⊇ unconstrained-on-target) before any deep run; and the arc test adds
  per-growth-step cost (must stay cheap relative to the branching it saves).

## MEASURED OUTCOME (2026-06-23): the partial-fan arc constraint did NOT help — lever rejected

Built the partial-fan arc constraint (sound: constrained-growth "no valid loss" test green; deterministic:
`DeterminismProbe` reached=1 across par=1/15, maxFaces 80/96; no regression: 32 tests green). Then ran the
apples-to-apples payoff test — the n=3 constrained deep sweep at maxFaces=96, same config as the pre-prune run.

**Result: 36/39 in 54 min — IDENTICAL to the pre-prune 36/39 in 55 min**, same three deficit cells
(`{3².6²;3.4².6;3.6.3.6}` 2/3, `{3².6²;3.6.3.6;6³}` 1/2, `{3⁶;3.3.3.3.6;3².6²}` 2/3), per-set times unchanged.

**Diagnosis — the prune barely fires.** For n=3 the target vertex figures share many common sub-arcs (lots of
3s and 6s), so almost every partial fan that already passes `isExtendableFan` is *also* a contiguous arc of
some target ⇒ the arc constraint rarely prunes. The per-vertex polygon branching it targets was already
well-constrained; the actual cost is **reaching and verifying the deep (>96-face) patches**, which the prune
does not touch. So lever #1 is **sound but practically ineffective** for this problem — rejected as a speed
lever (kept in the code, harmless, may help type-sets with more dissimilar vertex figures).

**Revised verdict on n≤7 in ≤1 week:** more doubtful. The biggest hoped-for algorithmic lever failed, so the
deep-cell cost stands. Honest standing: the SCIENCE is solid (rotation-only thesis confirmed; engine sound +
deterministic; residual is pure fundamental-domain depth; zero rotation-free at every n), and the counts are
sound LOWER BOUNDS (n=2 20/20, n=3 36/39, n=4 27/33). Closing the *full* counts for n=6-7 within a week is
unlikely without either (a) a genuinely different growth strategy — **closure-directed / best-first** growth
(reach the closing patch via the shortest path, not breadth — the remaining untested algorithmic idea), or
(b) **distribution** across machines (brute compute on the embarrassingly-parallel per-type-set sweeps). The
per-vertex-constraint family is exhausted.
