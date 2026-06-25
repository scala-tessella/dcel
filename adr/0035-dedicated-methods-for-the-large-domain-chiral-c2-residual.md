# ADR-0035: Dedicated methods for the large-domain chiral C₂ residual

- **Status:** Accepted (options enumerated + de-risked). **Orbifold-domain growth (#1) measured the WRONG fix
  — the n=3 residual is a grower growth-path gap, not depth (see Spike conclusion). Decision: pursue the
  type-targeted combinatorial oracle (#2) next.**
- **RETRACTION (2026-06-25):** the premise "the residual is *large-domain chiral C₂*" was an ARTIFACT of
  `TilingReference` transcription errors + measuring the grower against wrong multiplicities. The reference
  audit (oracle vs reference, n≤3) found 2 compensating n=3 errors; corrected, the grower's true n=3 reach is
  **32/39** (not 36), and the 7 missed cells are mostly **SMALL** (D=11–18, all with seedable C₂/C₃/C₆ rotations
  + mirrors), not large. ⇒ the grower has a real, NOT-depth completeness bug to be investigated (not an
  abandon-the-grower conclusion). The orbifold-growth de-risk conclusion below still stands (depth is not the
  cause); only the "large-domain" characterization is withdrawn.
- **Date:** 2026-06-25
- **Follows:** [[0034-fair-enumeration-principle-and-closure-directed-growth]] (closure-directed best-first
  growth built + measured ineffective + rejected; the residual is a sharply-defined hard family).

## Context

Every engine — the symmetry grower, the generate-all D-symbol oracle, the fixed-Λ engines, bounded-V — closes
the same set of n ≤ 3 tilings and stalls on the **same handful of cells**: large fundamental domain, **chiral**
(3.4.4.6 / snub 3.3.3.3.6 vertices ⇒ no mirror), **C₂-only** point symmetry (wallpaper p2/pgg). Standing sound
lower bounds: n=2 20/20, n=3 36/39, n=4 27/33, zero rotation-free throughout.

We have a precise structural fingerprint of the eluding class but, so far, no method that closes it. Before
declaring it a pure compute wall, this ADR audits **what is actually closed vs what we have never tried**, so
the "we can't develop dedicated methods" worry is answered with evidence, not resignation.

### Genuinely closed (do not re-litigate)

- **Seed completeness.** A C₂ centre can only sit at a face centre, edge midpoint, or vertex; the two faces
  across a C₂ edge-midpoint are necessarily congruent (a rotation preserves polygon size), so the
  congruent-pair seed catalogue covers every C₂ centre (`SeedCatalogueProbe`, 22 seeds). Not a seed gap.
- **Growth ordering.** Closure-directed best-first measured strictly worse than DFS (ADR-0034 §4); committed
  DFS with closure-directed child ordering (completion order + compact-disk MRV tie-break) is already correct.
- **Brute depth.** Raising `maxFaces` is exponential and counterproductive (7 h / maxFaces=96 reached *fewer*
  than the quiesced maxFaces=64).

These three are all variants of ONE paradigm: grow a planar patch, discover the lattice. The other paradigms
are barely touched.

## Decision

Enumerate the unexplored dedicated methods, with an HONEST payoff analysis, and de-risk the cheapest credible
one before committing.

### 1. Orbifold fundamental-domain growth — grow the C₂ *sector*, not the whole cell
Today the grower places the full C_m orbit each step, so its stored patch is the whole translation cell ≈ **2×
the fundamental domain** for C₂, and `maxFaces` bounds that. Growing only the orbifold sector and closing via
the wallpaper-group identifications would store half as many faces.

> **HONEST RE-ANALYSIS (the correction).** This is a **~2× constant factor, NOT an exponential win.** The
> current grower already places the full orbit each step, so its partial patches are C_m-symmetric — and a
> partial symmetric patch is in bijection with its partial sector. So the **search tree (the set of partial
> sectors explored) is IDENTICAL** to what sector-only growth would explore; orbifold-domain growth does not
> remove a single node. It only makes each node ~m× cheaper (store/verify the sector, not the whole cell) and
> halves the `maxFaces` number needed to reach a given cell. Valuable *iff* the tree to reach the residual
> cells is already tractable (then a 2× speedup tips it over); useless if that tree is itself exploding
> (sector growth explores the same exploding tree). ⇒ **must be gated on a reachability-at-depth measurement**
> (does the existing grower, given enough depth, reach the n=3 residual at all?), not built on faith.

### 2. Type-targeted *combinatorial* oracle — a different tree
The grower constrains by type-set *geometrically*; the D-symbol oracle is always *generate-all*. We have never
constrained the oracle's chamber generation to the target vertex types, which fixes the (0,1)/(1,2)-orbit sizes
(polygon sizes and vertex degrees) and prunes the chamber tree **high up** — unlike the curvature prune that
only bit at the leaves (ADR-0034). This is a genuinely DIFFERENT search tree from geometric growth (it
enumerates chamber structures, not planar patches), is **rotation-agnostic** (so it doubles as the Conjecture-R
witness), and could reach a specific deep cell the generate-all wall blocks. Medium effort; reuses the oracle.

### 3. SAT / ILP / CP exact-cover solver — a different *search engine*
We have only ever used hand-rolled backtracking. "Tile a torus of bounded covolume with these polygons,
C₂-symmetric, every vertex ∈ this type-set" is a finite constraint-satisfaction problem — precisely what modern
SAT/ILP/CP solvers (with clause learning / LP relaxation) crush, including instances where naive backtracking's
tree explodes. The heavyweight, most-different tool; an external solver dependency. For a *single* stubborn cell
it is likely the strongest option, and it can *prove UNSAT* (no such tiling) — a certification, not just a hit.

### 4. Fundamental-domain size bound (a theorem) — the certification path
If the period of a Krötenheerdt tiling with a given type-set is provably bounded (≤ f(n) faces), exhaustive
search to that bound **certifies** the count (turns every lower bound into a proof) and bounds the search. This
is the path that connects to proving Conjecture R, and the only one that yields *certified* counts rather than
sound-lower-bounds-matching-Galebach. Math/literature, not code.

## Plan (original — then overtaken by the spike below)

De-risk **#1** first because it is cheapest and reuses the grower — the de-risk being a
**reachability-at-depth measurement** with the *existing* grower (run the n=3 deficit type-sets at high
`maxFaces`): if depth alone reaches them, orbifold-domain growth is a worthwhile 2× and we build it; if the tree
explodes first, no growth method works and we move to **#2** (type-targeted oracle, the different tree) and/or
**#3** (SAT, the different engine). **#4** runs in parallel as the route to certified (not just matched) counts.

**Outcome of that de-risk (see Spike sections below): #1 ABANDONED — the residual is a grower growth-path gap,
not depth, so a depth/cap lever cannot help. The rotation-agnostic oracle already has these cells. ⇒ Revised
plan: pursue #2 (type-targeted oracle) next; #3 (SAT) is the fallback if #2 hits the chamber wall; #4 remains
the certification route.**

## Spike result (2026-06-25): the residual SPLITS into cap-bound vs tree-walled

`DeepReachProbe` ran the existing DFS grower, constrained, on the three n=3 deficit type-sets at **maxFaces=160,
12 min/set** (the reachability-at-depth gate). **None closed at 160**, but for two distinct reasons:

| deficit type-set | maxFaces=96 | maxFaces=160 | diagnosis |
|---|---|---|---|
| {3².6²; 3.4².6; 3.6.3.6} | 2/3 | 2/3, **quiesced 14 s** | **cap-bound** — tiny tree, cell simply > 160 faces |
| {3².6²; 3.6.3.6; 6³} | 1/2 | 1/2, hit 12-min cap | **tree-walled** — tree did not quiesce |
| {3⁶; 3.3.3.3.6; 3².6²} | 2/3 | 2/3, hit 13-min cap | **tree-walled** |

**Interpretation.** The cap-bound set (1) *exhaustively* searched its (tiny) tree at maxFaces=160 and the
missing cell wasn't there ⇒ its translation cell exceeds 160 faces; the tree is small, so the only barrier is
the face cap. **This is precisely the case orbifold-domain growth fixes** — storing the C₂ *sector* (cell/2)
reaches cells up to ~2× the face cap at the same tiny tree, so orbifold-growth has a concrete, evidence-backed
shot at the cap-bound sub-class. The tree-walled sets (2, 3) did not quiesce in ~12 min; orbifold-growth's 2×
per-state is only marginal there — they need a different *tree* (#2 type-targeted oracle) or *engine* (#3 SAT).

⇒ **The residual is not one wall but two.** A complete answer needs (a) orbifold-domain growth for the
cap-bound sub-class AND (b) #2/#3 for the tree-walled sub-class. Caveat for #2: these cells have large minimal
D-symbols (chambers ∝ domain size), so a type-targeted oracle may itself hit the chamber-tree wall — making #3
(SAT, which solves rather than enumerates, and can prove UNSAT) the stronger bet for the tree-walled cells.

### Spike CONCLUSION (2026-06-25): orbifold-growth is the WRONG fix — it's a grower growth-path gap

Pushed the cap-bound set ({3².6²; 3.4².6; 3.6.3.6}) much further to distinguish "deep but reachable" from
"never reached":

- **maxFaces ladder 220 / 300 / 380:** quiesced at **2/3 every time** (31 s / 61 s / 123 s), growing patches
  right up to the cap (~296 faces at 300). A real n=3 translation cell cannot plausibly exceed ~40 faces, let
  alone 380 ⇒ **this is NOT depth.**
- **Widening closure candidates** (`boundaryGlueBases` `take(8)→take(40)`, sound, 34 grower/union tests still
  green): **still 2/3**, and 66× slower ⇒ **NOT a closure-detection / candidate-limit gap either.** Reverted.

Both depth and closure ruled out ⇒ the grower **structurally never builds the 3rd cell's patch** — a
growth-path / germination gap (confirmed not a constraint artifact: the *unconstrained* grower also stalls at
36/39). **Orbifold-domain growth — a depth/cap lever — therefore cannot help, and is abandoned.**

**The reframe that matters:** the rotation-AGNOSTIC generate-all oracle *already finds all 39 n=3 tilings*
(R discharged, 11/20/39 at maxSize 24-26) — including these three. So they are not intrinsically hard to find;
they are hard *for the grower specifically*. For n ≤ 3 the count is already complete via the oracle; the
grower's 36/39 is a grower-only incompleteness. For **n ≥ 4** (where the generate-all oracle is walled) this
same growth-path gap is the real barrier — so the right lever is the **rotation-agnostic, growth-path-free
method: the TYPE-TARGETED ORACLE (#2)**, the very machinery that already nails n ≤ 3, constrained to the target
types to push past the chamber wall. ⇒ **Pursue #2 next, not orbifold-growth.**

## Consequences

- **Positive:** replaces "it's a compute wall, give up" with a ranked menu of genuinely-unexplored paradigms and
  an honest payoff for each; keeps the measure-before-building discipline (the #1 re-analysis already saved us
  from over-investing in a 2× believing it was exponential).
- **Negative / risk:** #2 and #3 are real builds with uncertain payoff; #3 adds an external solver. None is
  guaranteed to crack the residual — but "we tried the other paradigms and measured them" is a far stronger
  position than "we only ever grew patches."
- **Fairness:** all four are answer-blind (they find *any* tiling of the structure, not specific known ones) and
  type-set targeting is the already-accepted ADR-0034 §2 specialization.
