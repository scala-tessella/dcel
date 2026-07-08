# ADR-0041: Generalizing the assembler to m-Archimedean n-uniform tilings (Galebach's question marks)

- **Status:** Proposed (user-requested; design settled, build not started). 2026-07-08.
- **Follows:** [[0039-constraint-first-pivot-and-the-12n-bound]] (the SAT symbol assembler, complete A068600),
  [[0040-fair-type-set-derivation]] (the candidate machinery this generalizes).

## Context — the unpublished cells

Galebach's table (probabilitysports.com/tilings.html, bottom) counts **m-Archimedean, n-uniform** tilings
(n vertex ORBITS, m distinct vertex TYPES, m ≤ n). Known: all cells for n ≤ 6, the near-diagonal for
n = 7..11 ((7,7)=7, (8,7)=20, (9,8)=8, (10,8)=27, (11,9)=1, zeros to their right), and row totals for n ≤ 6
(A068599: 11, 20, 61, 151, 332, 673). **Unknown ("?"): every off-diagonal cell for n ≥ 7** — e.g. the number
of 7-uniform 2-Archimedean tilings — and all row totals for n ≥ 7. These appear to be genuinely unpublished
(Čtrnáct's k ≤ 12 extension is also undocumented).

## Decision — the generalization is structurally small

The ADR-0039 engine assumes the Krötenheerdt condition (orbits = types, each type exactly one orbit). The
(m, n) generalization relaxes exactly one thing: **type-MULTISETS** — m distinct compatible types with
multiplicities summing to n.

1. **Candidates (Phase-1 reuse):** the ADR-0040 filters (edge-closure, pinned coronas, connectivity) apply
   verbatim to the SUPPORT set — they never used distinctness. Candidate space = {support ∈ candidates-style
   m-sets} × {compositions of n into m positive parts}.
2. **Frames:** one star per ORBIT — a type with multiplicity k contributes k stars, each with its own
   folding choice. σ₀ SAT (m₀₁-equality, σ₂-equivariance, face closure, star-cut connectivity) unchanged.
3. **Classify:** relax the final filter from `orbitCount == typeCount` to
   `(distinct types, orbit count) == (m, n)`; everything else (euclidean, regular, MINIMAL, canonical key,
   dedup) unchanged.
4. **Size bound:** the 12n chamber bound is orbit-based, so it holds verbatim — (7, 2) instances are no
   larger than the completed n=7 run; row 11 means ≤ 132 chambers, row 14 ≤ 168.

**Performance note:** repeated identical stars introduce star-SWAP symmetry (duplicate enumeration up to
the multiset permutations, on top of per-star automorphisms). Canonical-key dedup absorbs this for
correctness; if floods appear, the levers are (a) lex-ordering between identical stars (sound symmetry
breaking: fix a canonical order of the k copies' σ₀ signatures), then (b) per-star automorphism lex breaking.

## PRIOR-ART CHECK (2026-07-08, user-mandated before building) — Galebach's "?" are largely STALE

- **Čtrnáct's k ≤ 12 extension fills the breakdown** (published on Wikipedia's "Euclidean tilings by convex
  regular polygons" table; raw wikitext verified): row 7 = 175/572/426/218/74/**7**; row 8 =
  298/1037/795/537/203/**20**; row 9 = 424/1992/1608/1278/570/80/**8**; row 10 = 663/3772/2979/2745/1468/
  212/**27**; row 11 = 1086/7171/5798/5993/3711/647/52/**1**; row 12 = 1607/13762/11006/12309/9230/1736/
  129/15. Row totals to k=13 are in OEIS **A068599** (…, 1472, 2850, 5960, 11866, 24459, 49794, 103082).
  The diagonal-adjacent values match Galebach's page ((8,7)=20, (9,8)=8, (10,8)=27, (11,9)=1) ✓.
- **⚠ Wikipedia's ROW 8 DOES NOT SUM: 298+1037+795+537+203+20 = 2890 ≠ 2850** (= the row total AND
  A068599(8)). Rows 7 and 9–12 sum exactly (each verified by hand). So the published data carries an
  isolated 40-tiling inconsistency — either one breakdown cell or the total is wrong. **Settling this is a
  genuinely decidable open question and the first live target.**
- No OEIS sequences exist for the columns (searched; zero results for the 2-Archimedean column
  22, 33, 74, 100, 175, …) — candidate new sequences if our numbers confirm.
- Čtrnáct's method (like Galebach's) is UNDOCUMENTED — no published algorithm or completeness argument. So
  even for the cells his data fills, an independent, documented, sound derivation has scientific value
  (agreement between two independent methods effectively certifies both; disagreement finds a bug in one).
- Genuinely unenumerated: the **k=13 breakdown** (all unknown on Wikipedia; only the total 103082 and
  (13,10)=0 are recorded) and **everything k ≥ 14**.

## Validation plan (the gate ladder — revised after the prior-art check)

Known cells now give a ~35-gate ladder. Order: the ten cells of rows 3–6 ((3,2)=22, (4,2)=33, (4,3)=85,
(5,2)=74, (5,3)=149, (5,4)=94, (6,2)=100, (6,3)=284, (6,4)=187, (6,5)=92 — the n ≤ 3 cells also key-for-key
vs the oracle relaxed the same way), then row 7 (175/572/426/218/74) as the scale gate. **Then the live
targets in value order: (a) recompute ROW 8 independently and settle the 2890-vs-2850 discrepancy; (b) rows
9–12 as independent certification of Čtrnáct's undocumented data (as budget allows — counts reach 10⁴); (c)
the k=13 breakdown (new numbers, if row-12-scale costs permit).** ADR-0042's DRAT certification applies to
the terminal-UNSAT exhaustiveness of each cell.

**Scale expectation:** identical stars introduce a swap-symmetry flood factor up to k! for a type repeated k
times ((7,2) has up to 6 copies ⇒ ≤ 720×) on top of per-star automorphisms. Frame-level dedup (foldings as
combinations-with-repetition for identical stars) is free; σ₀-level lex breaking between identical star
blocks is the prepared lever if the gate runs measure floods.

## BUILD + GATE RESULTS (2026-07-08, running log)

- **Built:** `solveMultiset` (one star per orbit; identical-star foldings as combinations-with-repetition) +
  `solveCell(n, m)` (fair supports × positive multiplicity compositions) + SOUND lex-leader symmetry
  breaking (x ≤lex x∘π over the pair-vars, eq-prefix chains) for every star automorphism (forced propagation
  from chamber 0's image) and adjacent identical-star swap — the identical-star floods had capped (3,1) and
  (4,2); with breaking the whole fast spec runs ~20 s.
- **Bonus derivation:** (n,1) = 0 for n = 2, 3 — Krötenheerdt's "no n-uniform 1-Archimedean beyond n=1"
  theorem, as solver refutations.
- **Gate ladder (all EXACT, zero caps):** (3,2)=22, (4,2)=33, (4,3)=85 (spec, ~20 s); (5,2)=74, (5,3)=149,
  (5,4)=94 (~1 min each); (6,2)=100, (6,3)=284, (6,4)=187, (6,5)=92 (~5–11 min each). Diagonal regression
  (2,2)=20, (3,3)=39 intact. **11/11 known cells so far.** Row 7 running; row 8 (the live target) queued.

## Consequences

- **Positive:** fills genuinely unpublished cells with the same soundness/fairness standard as the A068600
  result; near-zero new theory (multisets + one relaxed filter); a strong ten-cell validation ladder exists.
- **Negative / risk:** counts grow (row 6 sums to 673) — enumeration output and dedup volume scale up;
  star-swap floods may need the lex levers; rows ≥ 10 sizes are untested territory (≤ 168 chambers — likely
  fine, measure first); no ground truth exists for the "?" cells, so the evidential standard is
  "matched every known cell + sound-by-construction", as for n = 6, 7.
