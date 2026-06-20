# ADR-0026: The dual / low-index-subgroup construction — the documented route to n ≤ 7

- **Status:** Proposed — the strategic reset after the D-symbol-generation family was measured DEAD (ADR-0022/0023)
  and the cell-assembly family hit the covolume wall (ADR-0020/0025). **Grounded in a COMPLETE published
  algorithm**: Taganap & De Las Peñas, "k-Isocoronal tilings", *Acta Cryst.* A75 (2019) 94–106 (Theorems 3.1,
  3.2, 4.1). The Dolbilin–Schattschneider–Senechal Local Theorem was the right intuition (bounded determining
  radius); this paper supplies the actual constructive, complete, orbit-bounded method. NOTE: the file is named
  for the earlier corona-stabilization idea, now superseded by the §"DOCUMENTED ALGORITHM" below.
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

## THE DOCUMENTED ALGORITHM — Taganap & De Las Peñas, *Acta Cryst.* A75 (2019) 94–106

Fetched and read the paper. It gives a **complete, proven, orbit-bounded** construction of k-uniform tilings —
the corona-growth above is SUPERSEDED by this (it's the rigorous version of what Galebach did).

**Theorem 3.2 (the regular-polygon construction).** An edge-to-edge **k-uniform** tiling `T*`
`(v₁₁.v₁₂…; …; vₖ₁.vₖ₂…)` by regular polygons ⟺ an edge-to-edge **tile-k-transitive** tiling `T` by
**tangential polygons with regular vertices**, with the SAME symmetry group. Construction (Theorem 3.1 with
incenters):
1. `xᵢ` := the **incenter** of each tile-orbit representative `tᵢ` (`i = 1..k`).
2. `P` := `Gxᵢ` over all `i` — the incenters of every tile.
3. **Connect by an edge the incenters of any two adjacent tiles** of `T`.
4. Each **vertex `z` of `T` of valence `v`** becomes a **regular `v`-gon of `T*`** (vertices = incenters of the
   tiles around `z`). Regular because `T`'s tiles are tangential (equal inradius ⇒ equal edge lengths) and `z`
   is a regular vertex (equal angles ⇒ equal vertex angles). So `T*` is k-uniform.

`T*` is the **dual** of `T` (incidence-reversing: T*-vertices ↔ T-tiles, T*-tiles ↔ T-vertices).

**Theorem 4.1 + the method (Section 4).** In E² there are **exactly 87** edge-to-edge isocoronal (k=1) tilings
by convex polygons (20 tile-transitive). The method that PROVES completeness:
1. **Seeds**: the **tile-transitive tilings by convex polygons of Schattschneider & Dolbilin (1998)** (the
   isohedral types). For the regular-polygon world they are the tile-transitive tilings by tangential
   regular-vertex polygons (the Laves/dual side of the Archimedean world).
2. **Engine**: for each seed `T` with wallpaper group `G`, enumerate the **finite-index subgroups `H ≤ G`**
   (the paper used **GAP**). `H` partitions the tiles into `k` orbits.
3. For each `H`: construct `T*` by the incenter dual (choosing `xᵢ` per `Stab_H(tᵢ)` — a rotation centre /
   point on a reflection axis / incenter, exhausting symmetry-group variants `S ≤ D_n` of the resulting
   polygons).
4. **Regular-polygon `T*` = k-uniform**; bucket by `k` distinct vertex types ⇒ **A068600**.

**This is bounded by the SUBGROUP INDEX ≈ k**, not chamber count or covolume. A tile-transitive seed (1
tile-orbit under `G`) gives `k` tile-orbits under `H` with `[G:H]` ≈ `k` (small): k ≤ 7 ⇒ index ≲ 12 ⇒ a
SMALL, fast low-index-subgroup enumeration (seconds–minutes per seed) — easily within the week budget, and
**complete with proof** (no scatter, no determining-radius guesswork — the corona/Local-Theorem analysis above
was the right intuition but this paper supplies the actual algorithm).

## Decision — build the dual / low-index-subgroup construction

Implement: (a) the fixed seed list (tile-transitive tilings by tangential regular-vertex polygons + their
wallpaper groups `G`); (b) **finite-index subgroup enumeration** of the 17 wallpaper groups (the
Reidemeister–Schreier / low-index-subgroups algorithm — or its perfect-colouring equivalent: a subgroup `H`
giving `k` tile-orbits ↔ a `k`-colouring of the tiles perfect under `H`); (c) the **incenter dual**
construction `T → T*`; (d) the **regular-polygon test** + `k`-distinct-type (Krötenheerdt) filter; (e) dedup +
verify via `DelaneySymbols` minimal symbol, cross-checked against the n ≤ 3 oracle then `TilingReference`
(n = 4–7 = 33,15,10,7).

## Risks / open questions

- **Subgroup-enumeration engine**: reimplement low-index subgroups of wallpaper groups (well-understood;
  Reidemeister–Schreier on the 17 fundamental groups), OR the perfect-colouring formulation on the seed tiling
  (likely cleaner combinatorially, stays in tessella's wheelhouse). De-risk: reproduce Theorem 4.1's **87**
  and the **11** Archimedean (k=1) first.
- **Seed list**: pin the exact tile-transitive-by-tangential-regular-vertex seeds (paper cites Schattschneider
  & Dolbilin 1998 + Taganap 2017 thesis for details) — may need one more fetch.
- **Choice-of-`xᵢ` variants** (`S ≤ D_n` per Section 4) — exhaust them so no regular tiling is missed.

## Keepers / status

- `DelaneySymbols` (n ≤ 3 oracle + minimal-symbol canonical key/dedup) — the verify/key tail for any engine.
- Oriented-slice generator — validated, the D-symbol ceiling (partial n ≤ 5), kept as cross-check.
- `BucketAssembly` — validated low-V torus engine, kept as cross-check.
- Next: fetch *Acta Cryst.* A75 (2019); then de-risk the corona-stabilization prune (the exact thing ADR-0024
  got wrong); then build.
