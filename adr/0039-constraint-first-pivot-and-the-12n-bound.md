# ADR-0039: Full-project reassessment — the 12n bound and the constraint-first (SAT/CP) pivot

- **Status:** Accepted (user-approved 2026-07-07; strategic pivot — build gated on the Phase-0/1/2 plan below).
- **Date:** 2026-07-07
- **Follows:** [[0038-profile-automaton-cylinder-transfer-matrix]] (band engine: sound, n-independent, 8/13 of
  the n=3 banded gap at `maxBand=2`, remaining blockers diagnosed and BANKED). Executes what
  [[0035-dedicated-methods-for-the-large-domain-chiral-c2-residual]] (#2 type-targeted generation, #3 SAT/CP,
  #4 size-bound theorem) and [[0036-non-monotonic-difficulty-and-top-down-type-set-derivation]]
  (constraint-first architecture, fair type-set derivation) proposed and accepted **but never built**.

## Context — a from-first-principles reassessment, not a resume

After ~40 ADRs the honest scoreboard is: n=1,2 exact; n=3 complete via the `DelaneySymbols` oracle
(39/39, stable at maxSize 24–26); n=4 ≈ 30/33 (bounded-V ∪ oriented-slice practical ceiling, ADR-0031);
n=5 partial lower bound; **n=6, n=7 = 0 by every method**. The user mandated a step-all-the-way-back
reassessment: re-derive the strategy, find genuinely untried levers, and justify the next step against the
scoreboard — do not inherit the band-engine line by default.

### Finding 1 — every measured wall is a SEARCH-STRATEGY wall, not an object-size wall

| engine family | its state space | measured wall |
|---|---|---|
| fixed-Λ / bounded-V assembly (0019/0020/0025/0030) | translation cell, ~c^V states | V unbounded per run; `seen` dedup OOMs at V=19 |
| D-symbol tree generation, all variants (0022/0023/0029/0034) | all partial D-sets, tree 3–5.8×/+2 | euclidicity is a CLOSURE condition — no generation order prunes partials; `checkCanonicity` O(size²)/node |
| free / corona growth (0018/0024/0026/0028) | partial planar patches | 1D-aperiodic scatter, inherent to patch growth |
| oriented-slice (0023/0029) | oriented D-set tree | same tree; low symmetry ⇒ symbol ≈ covolume |
| symmetry grower (0032–0035) | patches from rotation seeds | growth-path gap (unexplained) + fundamental-domain depth |
| band / profile automaton (0037/0038) | profiles at a fixed circumference | **bounded** — but covers only the banded class |

The band engine's real lesson generalizes: an engine whose state space is bounded by something *intrinsic*
beats the covolume wall. Its bound (the cylinder circumference) covers only the anisotropic class. The bound
that covers everything is the minimal-symbol size — see Finding 2.

### Finding 2 — the 12n bound (the overlooked theorem): the object space is small and n-bounded

A Krötenheerdt n-uniform tiling has exactly n vertex orbits under its full symmetry group G. Every chamber
(flag orbit) of the minimal Delaney symbol sits at exactly one vertex orbit; a vertex of degree d carries 2d
flags, and a wallpaper group acts freely on chambers, so orbit i contributes 2dᵢ/|stabᵢ| chambers; degree ≤ 6
(the all-triangle vertex). Hence:

> **Theorem (size bound).** minimal-symbol size ≤ Σᵢ 2dᵢ ≤ **12n** — ≤ 36 at n=3, ≤ 48 at n=4, **≤ 84 at n=7**.
>
> **Corollary (covolume bound).** V_cell = Σᵢ [G:Λ]/|stabᵢ| ≤ 12n, since wallpaper point groups have order
> ≤ 12 (p6m). The bounded-V sweep has a provable stopping rule after all.

Sanity checks against measured data: n≤3 complete at maxSize 24 (< 36 ✓); the recorded "n=7 ≈ 45–60
chambers" estimate (< 84 ✓); 4.6.12's cell V=12 at n=1 (= 12·1, p6m, tight ✓); the pinned hard n=4 cell
V ≥ 19 (< 48 ✓). Phase 0 turns this into a written proof + a property test over every realized cell.

Consequences of the bound:
- **The whole n ≤ 7 problem lives in D-symbols of ≤ 84 elements.** No covolume appears in the bound. Every
  wall so far was the cost of *searching a superset* of this space, never the space itself.
- **It is a certification bound.** Enumerating all euclidean regular-polygon minimal symbols with exactly n
  vertex orbits up to size 12n *proves* the count — upgrading any complete level from
  "matches Galebach (validation)" to "certified", i.e. ADR-0035's #4 theorem path, free in this frame.

### Finding 3 — the accepted-but-never-built levers are exactly the ones matching the measured root causes

ADR-0034 pinned why D-symbol tree generation fails: canonicity cost per node on an accelerating tree, plus
euclidicity's lack of early-firing power *in any generation order*. Both failure modes are precisely what
CDCL constraint solvers address: no fixed generation order, global propagation of the (integer-linear)
curvature constraint, clause learning. Isomorph-free exhaustive generation via SAT is a mature technique —
**SAT Modulo Symmetries** (Kirchweger–Szeider, 2021+) integrates CDCL with dynamic lex-minimality symmetry
breaking and emits DRAT proofs. ADR-0035 #3 proposed SAT/CP and it was never tried; ADR-0036's stage 1 (fair
top-down type-set derivation) closes a standing fairness gap (type-targeted runs currently read candidate
sets from `TilingReference` — uncomfortably close to the answer key) and was never built either. The pivot
to the banded family (0037/0038) was locally justified by de-risks but left the strongest paradigm untouched.

### Finding 4 — external feasibility witnesses (fair: methods, not cells)

- **Krötenheerdt derived 11, 20, 39, 33, 15, 10, 7 BY HAND, with proofs** ("Die homogenen Mosaike n-ter
  Ordnung in der euklidischen Ebene" I–III, 1969–70) — a *documented* method, unlike Galebach's program, and
  never read for this project (only Lenngren's 2009 survey is catalogued). If a human can execute the case
  analysis, the instance sizes are solver-trivial; reading his *method* is fair under ADR-0034 §1 (structural
  reasoning, same status as the Local Theorem).
- Galebach: exhaustive k ≤ 6 in ~1 month of 2002 compute; Čtrnáct k ≤ 12. Feasibility is not the question;
  the search strategy is.

### Finding 5 — the union-of-complementary-engines frame is the wrong PRIMARY frame

It has no coverage theorem ("every tiling falls in some engine's reachable class" is exactly the kind of
unproven assumption the fairness discipline refuses elsewhere — cf. Conjecture R handling); it structurally
generates open-ended engine-patching (the band engine is the third engine built to patch the second engine's
blind spot); and it has produced zero tilings at n=6,7. The engines remain valuable as *validation witnesses*
(bounded-V is the rotation-agnostic Conjecture-R probe; the oracle is the n≤3 fixture), not as the route to
completeness.

## Decision

**Pivot to a constraint-first enumeration over the ≤ 12n D-symbol space, per fairly-derived type-set, with a
SAT/CP solver as the search engine.** Deliverable redefinition is DEFERRED until this paradigm is measured —
it is the last untried one, and the only one whose cost model is bounded by n rather than covolume.

### The plan (staged, test-first per the project discipline)

- **Phase 0 (½–1 day):** read Lenngren 2009 in full + Krötenheerdt I–III (method extraction only — ADR-0034
  fairness); write the 12n size/covolume bound as a proof section + property test against every realized cell.
- **Phase 1 (1–2 days):** fair top-down type-set compatibility enumerator (ADR-0036 stage 1), test-first.
  Validate the derived per-n type-set inventory for n ≤ 3 against the ORACLE (not Galebach); check it derives
  the n=8 ceiling (∅) — a standalone deliverable.
- **Phase 2 (2–4 days):** the D-symbol constraint engine spike. Encoding: per candidate type-set, the n
  vertex-star chamber cycles are determined by the types (m₀₁ sequences fixed); variables are the inter-star
  gluings (σ₂, face closures); constraints = involutions, valid signatures, integer-linear euclidicity
  (curvature = 0 after LCM scaling), connectivity, exactly-n distinct vertex orbits; enumerate via blocking
  clauses / lex-minimality; dedup survivors with the existing `minimalSymbol` (survivor counts are tiny — the
  0.69M→90 redundancy lived in the tree, not the solution set). Soundness tail UNCHANGED: `isEuclidean` +
  regular-polygon filter + realize/verify, keyed in the shared D-symbol key space.
- **Gates (each cheap, hours not days):** the 70 known n ≤ 3 minimal symbols are a perfect fixture — every
  one must SATISFY the encoding (property tests) before any enumeration run. Then:
  **G1** n=1 = 11 at size ≤ 12; **G2** n=2 = 20 at ≤ 24; **G3** n=3 = 39 at ≤ 36 — *certified* by the bound,
  which already beats every engine built so far; **G4** n=4 vs 33 at ≤ 48 — decides the project.
- **Decision gate:** G3 pass ⇒ push n=4–7 per-type-set. G3/G4 wall after an honest, tested attempt ⇒ THEN
  discuss redefining the deliverable from strength (certified n ≤ 3 + the 12n theorem + measured walls across
  six paradigms + the partial unions).
- **Solver (JVM, CI on Java 17):** OR-Tools CP-SAT Java bindings or SAT4J (pure JVM), or a DIMACS pipe to
  kissat/cadical. Fallback encoding if D-symbol SAT proves awkward: SAT over `BucketAssembly`'s dart-matching
  at bounded V ≤ 12n (constraint structure + op→key bridge already built and validated; a solver proves
  exhaustion by refutation, with no `seen` set to OOM).

### Fairness ruling (extends ADR-0034)

- The **solver is a component**, not the algorithm: the algorithm = the 12n bound + the fair type-set
  derivation + the encoding + the enumeration/certification scheme. External-solver dependency accepted by
  the user (2026-07-07), as ADR-0035 #3 anticipated.
- Krötenheerdt's papers and Lenngren's survey are used for **method only** — never as a source of cells or
  of candidate type-sets (those are derived by Phase 1).
- Galebach remains a validation oracle only.

### What is PARKED (not dead)

- **Band engine (0037/0038):** the glide-closure fix and the sets-#1/#4 diagnosis stay banked. Its standalone
  ceiling on the n=3 gate is ~8/13 and n=3 is already complete via the oracle; its remaining value is as a
  validation witness and as the fallback anisotropic specialist if the pivot fails.
- **n=4 grinding with the existing union** (the ~30/33 → 33/33 push): no credible existing-engine path to the
  both-walled 4.6.12-mixed cells; superseded by G4.
- The grower / bounded-V / oriented-slice stay as cross-checks in the shared key space.

## Consequences

- **Positive:** attacks all n at once, including the n=6/7 zeros — where the difficulty INVERSION helps
  (7 mutually-compatible distinct types is the most over-constrained instance: hardest for a grower, easiest
  for a solver, ADR-0036 §1); certification for free via the 12n bound; cheap staged de-risk with a perfect
  oracle fixture; executes decisions already accepted on their merits; closes the type-set fairness gap.
- **Negative / risk:** encoding correctness (orbit counting, connectivity, minimality) is subtle — mitigated
  by the fixture-first discipline (all 70 oracle symbols must satisfy every constraint before any run) and by
  the unchanged soundness tail; AllSAT isomorph handling at size 48–84 is the open performance question — the
  gates measure it before any large investment; an external native solver adds a build/CI dependency (SAT4J
  is the pure-JVM escape hatch).
- **If the paradigm walls:** the honest position is then "six paradigms measured" and the redefinition
  discussion happens with complete evidence — never a transcribed count.

## Phase-0 findings (2026-07-07, same session — literature read, method extracted, one ADR-0036 correction)

Lenngren 2009 read END-TO-END (diva-portal FULLTEXT01, saved locally); A068600 verified against OEIS (JSON
API) AND Galebach's own site table. Krötenheerdt's originals (Wiss. Z. Halle 18/19, 1969–70) are NOT freely
digitized — the survey-level method description below is what we have unless the user obtains scans.

### Krötenheerdt's documented method (via Lenngren §9)

- His "n-homogeneous" = edge-to-edge + n non-empty vertex classes by TYPE + classes coincide with symmetry
  ORBITS — i.e. exactly n-uniform AND n-Archimedean = the A068600 condition ✓ (definitions align).
- **1969:** proves the number of n-homogeneous tilings is FINITE, via "regulärer Vieleckskomplex" — an
  INDUCTIVE growth of regular vertex figures attached along edges (a corona-style induction); enumerates
  n=1,2. **1970a/b:** enumerates n=3..7 "using a rather intricate division into various cases"; proves NO
  n-homogeneous tilings for n > 7.
- His scaffolding theorems (all derivable, fair to reuse as METHOD): no n-uniform m-Archimedean with n < m;
  **no m-Archimedean tiling with m > 14** (only 15 vertex figures can occur in tilings at all, and 4.8² only
  in its own 1-uniform tiling ⇒ ≤ 14 types can ever coexist).
- So the documented human method IS vertex-figure compatibility case analysis + inductive growth — validating
  the Phase-1 architecture as a faithful (and mechanizable) reconstruction.

### Structural facts Phase 1 must DERIVE (now with documented provenance, still answer-blind)

21 arithmetic vertex figures (angle equation Σ(1−2/nᵢ)=2, 3 ≤ k ≤ 6 polygons); 6 of them cannot occur in ANY
edge-to-edge regular-polygon tiling (Sommerville 1905 — adjacency propagation forces impossible angles);
4 more (3².4.12, 3².6², 3.4.3.12, 3.4².6) cannot tile ALONE but occur in mixed tilings; 4.8² occurs ONLY in
its 1-uniform tiling (Krötenheerdt 1969). These are the fixture tests for the compatibility enumerator.

### CORRECTION to ADR-0036 (important, from Galebach's table + OEIS)

A068600(n≥8) = 0 is CONFIRMED (OEIS comment + Galebach's table diagonal: (8,8)=0). **But ADR-0036's stated
mechanism — "n=8 is a combinatorial ceiling: no 8 compatible types coexist" — is WRONG.** Galebach's
(m-Archimedean × n-uniform) table shows 8 and 9 distinct types DO coexist in n-uniform tilings:
(m=7,n=8)=20, (m=8,n=9)=8, (m=8,n=10)=27, (m=9,n=11)=1. The n=8 zero comes from the **types-must-equal-
orbits rigidity** (no tiling has 8 types AND only 8 vertex orbits), NOT from type-set compatibility.
⇒ Phase 1's "derive n=8 = ∅" gate CANNOT fall out of the compatibility enumerator alone — deriving the
ceiling needs the realization stage (Phase 2) refuting every 8-type candidate set at 8 orbits. The Phase-1
deliverable is reframed: derive the candidate type-set inventory (with the m ≤ 14 ceiling and the ~15-figure
facts as fixture tests); the n=8 ceiling is a Phase-2 (solver / UNSAT) result, as is honest — Krötenheerdt
himself needed the full case analysis for n > 7, not just compatibility counting.

### Originality + literature to pull for the 12n bound

- Delgado-Friedrichs (pers. comm. in Lenngren, 2009): the Delaney–Dress approach "has yet to be applied to
  k-uniform tilings" — corroborating this project's originality claim for the D-symbol route.
- **Chavey (1984b), "Periodic tilings and tilings by regular polygons I: Bounds on the number of orbits of
  vertices, edges and tiles" (Mitt. Math. Seminar Giessen 164)** — literature exactly in the 12n bound's
  family; check it (and his 1984a thesis, online) when writing the bound's proof section. Chavey 1984a is
  also the documented 3-uniform enumeration (edge-type lemmata, strip/dissection arguments) — a second
  documented method source beyond Krötenheerdt.

## Alternatives considered (this session)

- **Finish the band engine first** (glide fix + sets #1/#4): reaches ≤ 2 + unknown n=3 gap cells, core-engine
  closure-guard change with full regression risk, no n ≥ 5 story. Parked, not chosen.
- **Grind n=4 to 33/33 with the existing union:** the gap cells are both-walled (ADR-0031); weeks for ≤ 5
  cells, zero n ≥ 5 story. Rejected.
- **Redefine the deliverable now:** premature — the infeasibility evidence covers tree/growth/assembly/
  transfer-matrix, not constraint solving. Deferred to the decision gate.
