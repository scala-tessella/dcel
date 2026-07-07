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

## Validation plan (the gate ladder — no unknown is touched before it)

The known off-diagonal cells are ten exact gates: (3,2)=22, (4,2)=33, (4,3)=85, (5,2)=74, (5,3)=149,
(5,4)=94, (6,2)=100, (6,3)=284, (6,4)=187, (6,5)=92. All ten must reproduce EXACTLY (count level; the n ≤ 3
cells also key-for-key vs the oracle relaxed the same way) before any "?" cell is computed. Then row 7
left-to-right ((7,2)...(7,6)), then rows 8+. Row totals for n = 7+ extend A068599 — a publishable result if
it holds; ADR-0042's DRAT certification applies to the terminal-UNSAT exhaustiveness of each cell.

## Consequences

- **Positive:** fills genuinely unpublished cells with the same soundness/fairness standard as the A068600
  result; near-zero new theory (multisets + one relaxed filter); a strong ten-cell validation ladder exists.
- **Negative / risk:** counts grow (row 6 sums to 673) — enumeration output and dedup volume scale up;
  star-swap floods may need the lex levers; rows ≥ 10 sizes are untested territory (≤ 168 chambers — likely
  fine, measure first); no ground truth exists for the "?" cells, so the evidential standard is
  "matched every known cell + sound-by-construction", as for n = 6, 7.
