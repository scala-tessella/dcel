# ADR-0036: Difficulty is non-monotonic in n — derive viable type-sets top-down (fairly), realize by constraint

- **Status:** Proposed (strategic reframing; diagnosis of the n=3 grower gap to run first).
- **Date:** 2026-06-25
- **Follows:** [[0035-dedicated-methods-for-the-large-domain-chiral-c2-residual]] (orbifold-growth measured the
  wrong fix; the residual is a grower growth-path gap; pursue the type-targeted oracle #2).

## Context — a reframing prompted by the shape of A068600

A068600 is **11, 20, 39, 33, 15, 10, 7, 0, 0, …** — it PEAKS at n=3 and declines to n=7, then is identically
zero. The structural reason: a Krötenheerdt tiling has *exactly n distinct* vertex types, drawn from the ~21
admissible ones (the ways regular polygons close 360°). Edge-to-edge adjacency forces compatibility between the
types at an edge's two ends, so as n grows you need ever more *mutually-compatible* distinct types coexisting in
one periodic tiling. By n=7 only 7 tilings thread that needle; **n=8 is a combinatorial CEILING** (no 8
compatible types coexist), not an empty search.

## Decision / insight

### 1. Difficulty is NON-MONOTONIC in n, and it INVERTS with the method
- For the **geometric grower** (depth-bound): n distinct types ⇒ ≥ n vertex orbits ⇒ a *larger* fundamental
  domain. So high-n cells are the BIGGEST — exactly what the grower chokes on. n=7 is *hard* for the grower
  (few cells, but each large).
- For a **constraint-driven method** (type-targeted oracle / SAT): difficulty = size of the solution space, and
  "7 distinct types must coexist" is enormous pruning. **High-n is the most over-constrained ⇒ potentially the
  EASIEST to enumerate exhaustively**, despite large cells.
- ⇒ Difficulty peaks in the **hard middle (n≈3–4)** — most tilings, only moderate constraint — with *both ends
  easy* (n=1 trivial; n=7 rigidly constrained). This is a strong further argument to **abandon the grower for a
  constraint method**: it literally inverts the residual from "hopeless" to "most-pruned."

**Seeding is not the high-n bottleneck** — high-n tilings still have rotation centres (R holds), so seeds exist;
the grower's high-n problem is domain *size*, not germination. "Attacking in reverse" does not fix seeding — it
sidesteps the grower.

### 2. Derive the viable type-sets TOP-DOWN, fairly (the real prize of "reverse")
**Fairness gap exposed:** every type-targeted run today reads its candidate type-sets from `TilingReference` —
i.e. from Galebach's list. That is uncomfortably close to *sourcing from the answer key* (ADR-0034). A fair
method must **derive** which type-sets are viable. The top-down framing is exactly how: enumerate the
**vertex-type compatibility structure** (which subsets of the ~21 types can mutually coexist edge-to-edge) and
walk it from the largest sets down. This yields, independently of Galebach:
- the candidate type-sets for every n (fairly derived — closes the ADR-0034 fairness gap);
- the **ceiling for free** — n=8 comes out *empty* (no 8 compatible types), validating the method AND proving
  the top of the sequence;
- the high-n sets first, where structure is most rigid and enumeration cheapest.

### 3. Two-stage, constraint-first architecture (likely a faithful, fair Galebach reconstruction)
1. **Derive viable type-sets top-down** by type-compatibility combinatorics (fair, finite, ceiling-proving).
2. **Realize each** by a constraint method (#2 type-targeted oracle / #3 SAT): produce the tiling(s) or prove
   none (UNSAT = certification).
This dissolves both standing problems at once — the fairness gap (stop reading type-sets from the answer) and
the residual (large high-n cells become tightly-constrained solver instances, not deep geometric grows).

## Consequences

- **Positive:** a fair, difficulty-inverted attack where the scary high-n cases are the easy ones; closes the
  "type-sets read from Galebach" fairness gap; the n≤7 ceiling becomes a *derived* result, not an input.
- **Negative / risk:** type-compatibility is **necessary but not sufficient** — stage 1 over-generates
  candidate type-sets, so stage 2 must confirm/refute each (refute = a SAT UNSAT proof, itself a certification).
  Realizing large high-n cells is still real work — "fewer + more constrained" makes it tractable, not free.
- **On the n=3 grower gap (ADR-0035):** orthogonal to this, and rendered *moot* by going constraint-first (the
  grower leaves the critical path). But understanding *why* a sound-looking engine missed 3/39 is cheap
  insurance before leaning the whole count on a different engine ⇒ **diagnose the 3 cells FIRST** (realise them
  from the oracle — our own sound engine, fair as debugging — and read their intrinsic symmetry).

## Plan

1. **Diagnose** the 3 n=3 grower-gap cells (next; cheap, insurance).
2. Then **build stage 1**: the vertex-type compatibility enumerator (fair top-down type-set derivation), and
   confirm it reproduces the per-n type-set inventory AND derives n=8 = ∅.
3. Then **stage 2**: realize via #2 (type-targeted oracle), #3 (SAT) as fallback for the tree-walled cells.
