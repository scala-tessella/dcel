# ADR-0026: Corona-stabilization growth — the Local Theorem path to n ≤ 7

- **Status:** Proposed — the strategic reset after the D-symbol-generation family was measured DEAD (ADR-0022/0023)
  and the cell-assembly family hit the covolume wall (ADR-0020/0025). Research-informed (Dolbilin–Schattschneider–
  Senechal Local Theorem).
- **Date:** 2026-06-20

## The map of walls (all MEASURED this session)

| family | engines | wall |
|--------|---------|------|
| **D-symbol generation** | generate-all (0022), oriented-slice, interleaved-360°, corona-first, orbit-count | the **partial-D-set tree** grows ~4×/+2; NO prune cuts it (euclidean = closure condition; orbit-count fires too late). CONCLUSIVELY DEAD for n ≥ 4. |
| **cell assembly** | fixed-Λ (0019/0020), bounded-V dart (0025) | size = **covolume**, exponential; reaches n ≤ 2 cells (V ≤ ~7) only |

Best surviving engine: the **oriented-slice generator** (ADR-0023) — 167× over generate-all, first to cross
n = 3, reaches n = 4 = 5/33, n = 5 = 1/15 (partial, budget-bound). It is the *ceiling* of the D-symbol family.

## Feasibility is PROVEN (the decisive external fact)

Galebach exhaustively enumerated **k ≤ 6** k-uniform tilings — ≈ **1 month on 2002 hardware**
(probabilitysports.com/tilings.html; A068600 = 11,20,39,33,15,10,7). With ~1000× modern single-thread + cores,
that is **hours**, and n = 7 (~2× the work) is **days** — within the ≤ 1-week budget. His program is a **growth**
method and is **unpublished** ("a scientific description is lacking"). The published D-symbol tools (genDSyms /
Tegula) are the *size-bounded* generator we already ported — they wall at Dress-complexity 24 = **n ≤ 3**. So
the route to n ≤ 7 is a **growth** method, not D-symbols.

## The research finding — what controls the growth "scatter"

Earlier growth attempts (ADR-0018/0023/0024) drowned in **scatter** (exponentially many aperiodic partial
patches). The deep-research pass (stopped after search+verify) recovered the documented control:

- **Dolbilin–Schattschneider–Senechal Local Theorem.** A locally-finite face-to-face tiling is periodic
  (crystallographic) **iff its corona isomorphism-class count STABILIZES** — the number of distinct k-th coronae
  stops growing past a **determining radius**. (IUCr primary sources; survived adversarial verification.)
- In **E² the determining radius is SMALL**: classically *"a tiling by convex polygons is regular (transitive)
  iff all its first coronae are equivalent"*; *"a monohedral tiling is isohedral iff all first tile coronae are
  congruent."*
- A **published derivation framework** exists: **k-isocoronal tilings from tile-s-transitive tilings**, *Acta
  Cryst.* A75 (2019) — a systematic step-by-step method (TO FETCH AND STUDY).

### The critical caveat (verification caught it)

**k-isocoronal (orbits of CORONAE) ≠ k-uniform (orbits of VERTICES = A068600).** They differ in general;
corona-orbit count = vertex-orbit count only *at* the determining radius. So the 2019 framework is adjacent,
not identical — we enumerate corona structure and read off vertex-orbits at the determining radius.

### Why ADR-0024's corona prune was a no-op, and the fix

ADR-0024 pruned by "≤ n distinct coronae at a **fixed radius r = 2**" → no-op (aperiodic patches also have ≤ n
2-coronae for a while; the prune fired after the scatter). The Local Theorem says the right criterion is NOT a
fixed radius but **STABILIZATION**: grow coronae level by level and prune when the distinct-corona-class count
*exceeds k* OR *fails to stabilize*. Aperiodic patches never stabilize (their class count keeps climbing), so
this is the principled scatter-killer the fixed radius lacked.

## Decision (proposed) — corona-gluing enumerator, bounded by k

Enumerate sets of **≤ k vertex-coronae** (complete vertex-stars, so 360° is built in and there is no
partial-corona problem) that **glue consistently** and **stabilize** under the Local Theorem (⇒ a genuine
periodic k-uniform tiling). Bounded by k (corona-orbit count), NOT by covolume (corona-level) and NOT by the
partial-D-set tree (complete coronae). Verify / canonical-key / dedup reuse `DelaneySymbols` (minimal symbol)
and the proven `verifyContentAnyN` tail; cross-check counts against the validated n ≤ 3 oracle, then
`TilingReference` for n = 4–7.

## Risks / open questions

- The exact **determining radius / stabilization criterion** for vertex-k-transitive regular-polygon tilings is
  the research gap (ADR-0024 proved a fixed r = 2 insufficient). The 2019 paper may settle it.
- **Branching of corona-gluing**: is the set of ≤ k consistent stabilizing corona-sets small? UNMEASURED — a
  de-risk spike (does it stay small for a known n = 4 tiling?) gates the full build.
- Whether to enumerate vertex-coronae directly or via the dual (tile-coronae / k-isohedral) per the 2019 method.

## Keepers / status

- `DelaneySymbols` (n ≤ 3 oracle + minimal-symbol canonical key/dedup) — the verify/key tail for any engine.
- Oriented-slice generator — validated, the D-symbol ceiling (partial n ≤ 5), kept as cross-check.
- `BucketAssembly` — validated low-V torus engine, kept as cross-check.
- Next: fetch *Acta Cryst.* A75 (2019); then de-risk the corona-stabilization prune (the exact thing ADR-0024
  got wrong); then build.
