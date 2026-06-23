# ADR-0034: Fairness principle for the enumeration — Galebach is a validation oracle, not a source

- **Status:** Accepted (methodological constraint + decision: build closure-directed growth). 2026-06-23.
- **Date:** 2026-06-23
- **Follows:** [[0033-closing-the-depth-residual-feasibility-and-speed-levers]] (the arc-prune lever was built,
  measured ineffective, and rejected; the residual is a sharply-defined hard family).

## Context

The rotation-first engine is sound, deterministic, and complete-for-its-depth; the union with bounded-V stands
at n=2 20/20, n=3 36/39, n=4 27/33 (sound lower bounds). The unreached cells form **one structural family**:
large-period, hexagon-rich, **chiral** (3.4.4.6 / snub 3.3.3.3.6), **C₂-only** tilings — the worst-case
quadrant (minimal symmetry × maximal period), hard for a *structural* reason (a C₂ gives only 2× domain
compression, and hexagons forced off their C₆ centre blow up the period), not an algorithm accident.

While brainstorming how to close that tail, one proposed path was to **construct the missing cells from
Galebach's per-tiling PNG catalogue and verify them with our sound `verifyCell`/`classifyClosedMap`**. This ADR
rejects that path and records the fairness principle it violates, plus what specialization *is* legitimate.

## Decision

### 1. The enumeration must be sound — no steering toward known results
The goal is an algorithm that finds **all** Krötenheerdt tilings by **sound reasoning**. A count is only
meaningful as evidence if it is *derived*, not *read*. Therefore:

- **Galebach's per-tiling data (PNG catalogue, counts) is a VALIDATION ORACLE ONLY.** We may compare our
  independently-derived counts/keys against it as a cross-check, and we may use *general structural facts* it
  reveals (e.g. "every tiling has a rotation axis", "the hard cells are hexagon-rich C₂") to **inform**
  reasoning. We may **never** use it as a **source of tilings** — sourcing a cell from the answer key (even if
  re-verified afterwards) makes the resulting count circular and worthless as evidence.
- **REJECTED:** constructing the missing cells from the PNG catalogue + verifying. Re-verification on the back
  end does not launder a cell taken from the answer key.

### 2. Dedicated behavior for a structurally-identified subset IS legitimate
A specialized search mode for a subset is fair **iff** the specialization is justified by the subset's
*structure* and the cells are still **discovered by the mode's own logic** (it would find *any* such cell, not
specific known ones). The line:
- **Fair:** "large-period chiral C₂ cells have a big fundamental domain *for a provable reason*, so route them
  to a search tuned for large-domain closures" — structure-justified, answer-blind (cf. a chess engine
  spending more depth on tactical positions).
- **Not fair:** "search for the tiling matching PNG n=4 t=17" — answer-keyed.

### 3. Completeness of the rotation-only method is CONDITIONAL on an unproven conjecture
The rotation-first grower can only ever find tilings that **have** a rotation centre (it grows from rotation
seeds). Its completeness therefore rests on:

> **Conjecture (R):** No Krötenheerdt (k-uniform, all vertex types distinct) edge-to-edge regular-polygon
> tiling is rotation-free — none has wallpaper group p1, pg, pm, or cm (equivalently, every such tiling has a
> nontrivial rotation centre).

If R is false, the grower is **structurally blind** to the rotation-free tiling and nothing in the algorithm
would signal the omission — so "found all rotation-bearing tilings" is **not** "found all". **Agreement with
Galebach does NOT discharge R:** if his (undocumented, possibly manual) method shares the same blind spot, a
rotation-free k-uniform tiling could be absent from both, and matching counts would falsely read as
confirmation. R is independent of cross-checking Galebach.

**Status of R:** unproven. Strongly supported — every cell either engine has reached (all of n=2, the small
n=3/n=4 cells) is *measured* rotation-bearing, zero rotation-free — but not a proof. Crucially, **bounded-V is
a rotation-AGNOSTIC witness**: the dart assembler fills the translation cell and assumes no rotation, so where
it reaches it *would find* a rotation-free tiling if one existed. To date it finds only rotation-bearing
tilings ⇒ R is *partially verified by a method that does not presuppose it*, and is open only for the large
cells beyond bounded-V's reach.

**Therefore:** report results as **"complete modulo Conjecture R."** Make bounded-V (rotation-agnostic) the
standing independent check and push it as far as budget allows, specifically to hunt for any rotation-free
tiling. Pursue R itself by proof or literature (it reduces to: which wallpaper point groups are compatible with
k distinct regular-polygon vertex figures + the edge-direction lattice? — possibly a known result on k-uniform
tilings). Do NOT let the rotation-only count stand as "the count" until R is discharged or bounded-V verifies
the relevant cells.

### 4. Build closure-directed (best-first) growth — the structurally-justified lever
The measured finding "the grower reliably reaches the *compact* sibling and misses the *spread-out* one" has a
fair, general fix: order the growth frontier by **proximity to closing** (e.g. boundary-length / area-deficit)
instead of MRV/DFS, so the search drives toward a closing patch — including the large-period one — via the
shortest path rather than wandering. It is **still exhaustive** (a reordering of the same search space; with a
cap, a sound lower bound), so it sacrifices no fairness or completeness — it only reaches deep closures with
far fewer states. This is ADR-0033 lever #2, now promoted after lever #1 (arc prune) was rejected.

## Consequences

- **Positive:** keeps the result scientifically honest (counts are evidence, not transcription); gives a clear,
  reusable rule for when specialization is allowed; targets the only remaining algorithmic idea with a
  structural reason to help the hard family.
- **Negative / risk:** closure-directed growth is more invasive (the work-stealing frontier becomes a priority
  ordering) and is a *heuristic* — it may not move the deep cells. If it does not, the honest conclusion is
  "this family needs more raw compute (distribution, ADR-0033 #5)", **not** a thumb on the scale.
- **On the ≤1-week goal:** unchanged and honest — if neither closure-directed growth nor feasible distribution
  closes n=6-7 in the week, we report sound lower bounds + the structural diagnosis, never a transcribed count.

## DISCHARGED for n ≤ 3 (2026-06-24): R is a theorem there, not a conjecture

Conjecture R is now **proven for n ≤ 3** — fairly, with no appeal to Galebach as a source. Method
(`DelaneySymbols.hasRotation`, `DischargeRProbe`, `DelaneySymbolsSpec`):

- The **generate-all oracle** (`enumerateSymbols`) is **rotation-agnostic** — it enumerates *all* euclidean
  minimal D-symbols up to a chamber budget, making no symmetry assumption. At `maxSize = 24` it yields the
  complete **11 / 20 / 39**, and the count is **stable** (identical at `maxSize = 26`) ⇒ complete by stability,
  not by trusting the published total (which it also matches — a fair cross-check, ADR-0034 §1).
- `hasRotation(ds)` reads the **exact** symmetry from the minimal symbol: a rotation exists iff the orbifold
  has a cone of order > 1 on some orbit — faces (0,1), vertices (1,2), **or edge-midpoints (0,2)**. Every one
  of the 70 tilings (n ≤ 3) passes ⇒ **no rotation-free Krötenheerdt tiling exists for n ≤ 3.**
- The method is **discriminating, not vacuous**: 4 of the tilings are rotation-bearing *only* via an
  edge-midpoint C₂ (the (0,2) term) — a face/vertex-only test would have wrongly called them rotation-free.
  These are exactly the hard-cell family. So the test can fail, and doesn't.

**Consequence:** the rotation-only grower is now **UNCONDITIONALLY complete for n ≤ 3** (R holds there). For
**n = 4–7, R remains open** — discharging it needs the same rotation-agnostic oracle pushed that far (the
generate-all chamber-tree wall) or a proof. Until then, n ≥ 4 rotation-only results stay "complete modulo R".
