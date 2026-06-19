# ADR-0022: Intrinsic combinatorial-map (Delaney–Dress) enumeration — soundness by construction

- **Status:** Proposed — supersedes the *realization* of ADR-0021 (the "discovered-Λ propagation" prototype),
  keeping ADR-0021's goal and analysis. The fixed-Λ engines (ADR-0019/0020) remain the validated reference
  on the cells they reach.
- **Date:** 2026-06-20

## Context — what the ADR-0021 prototype taught us

ADR-0021 proposed reaching n = 4–7 (and the dodecagon cells) by enumerating the **finite torus quotient**
directly, with Λ *discovered* from a developed patch rather than swept. The prototype
(`KrotenheerdtTorusMapSearch`) realized this as: grow one planar patch per seed corona in exact ℤ[ζ₁₂],
read a candidate deck lattice Λ off the patch (repeated-vertex periods, or antiparallel boundary-edge
gluings), and hand the closed cell to the proven verify tail (`verifyContentAnyN` + `torusContentKey`).

Three findings came out of building and exercising it:

### 1. A soundness defect shared by ALL THREE engines

Above covolume ≈ 4, every torus engine — the DCEL `KrotenheerdtLatticeSearch`, the ζ-coordinate
`KrotenheerdtTorusSearch`, and the new `KrotenheerdtTorusMapSearch` — emits **spurious n = 1 "tilings"
3.3.6.6 and 3.4.4.6**. These vertex configurations satisfy the 360° angle condition but are **not**
Archimedean: no 1-uniform tiling of either exists (they occur only as orbits of n ≥ 2 tilings). The defect
was invisible because every prior test caps covolume at ≤ 2.6, and the n = 2 test cross-checks an engine
against *itself* (`enumerate` vs `enumerateCombined`), never against ground truth.

**Root cause.** All three engines wrap a planar patch onto a torus via a *candidate* Λ and certify it with
**residue/slot consistency mod Λ** (plus fan-completeness and `distinctArea == covol`). None of those detect
a **false translation period**: an all-360°, edge-paired, genus-1 *combinatorial* map can still have
nontrivial **rotational holonomy** (a flat cone / Klein-type identification — exactly ADR-0021's stated
orientability caveat). Its development then **overlaps**, and the overlap is between faces that share **no
corner** (a corner lands strictly inside another face), so the corner-keyed slot check is blind to it, and
`distinctArea == covol` is satisfied with a compensating gap. Confirmed geometrically: for the 3.3.6.6 cell,
point-sampling the Λ-development shows real overlaps where genuine tilings show none.

### 2. The "duplicate" signatures are not a bug — the vertex-type set is a lossy label

`3.3.3.4.4` (3³.4²) and `3.3.4.3.4` (3².4.3.4) are **distinct** Archimedean tilings sharing the multiset
`{3,3,3,4,4}`; a diagnostic that *sorts* the signature conflates them. More generally, the vertex-type *set*
under-determines a tiling: several n ≥ 2 type-sets correspond to **multiple** distinct tilings (e.g. four
distinct 3-uniform tilings share `{3.4².6; 3.6.3.6; 4⁴}`). The discriminating invariant is the tiling's
**geometry/adjacency**, captured by `torusContentKey` (and, canonically and coordinate-free, by a
Delaney–Dress symbol).

### 3. The grow-cover search is sound (post-fix) but incomplete for the hardest cells

An exact overlap gate (`tilesWithoutOverlap`: reject if any face corner is strictly inside another face under
the Λ-development) was added to `KrotenheerdtTorusMapSearch.verifyCell`. It restores **soundness**: at n = 1
the engine then yields only genuine Archimedean tilings (9/10 at covol ≤ 16, zero spurious). But the
"grow planar cover, read off Λ" *search* is **incomplete** for complex cells — it never assembles/closes
`4.6.12` (dodecagon + hexagons + squares, covol ≈ 19.4) within budget, and misses the mixed n = 2 cells —
and the per-state overlap check is costly. Completeness, not soundness, is the open problem.

## Ground truth and the validation principle

Reference data is checked in as `generator/src/test/.../TilingReference.scala`:

- **Counts (n = 1..7)** — authoritative OEIS **A068600** = 11, 20, 39, 33, 15, 10, 7 (Krotenheerdt: n-uniform
  with exactly n distinct vertex types).
- **n = 1** (11 Archimedean) and **n = 2** (20 vertex-type pairs) — verified explicit configs.
- **n = 3 (39), n = 4 (33), n = 5 (15)** — rows from Wikipedia "List of k-uniform tilings"; counts match
  A068600 (n = 5 completed by hand — tooling dropped the dodecagon row). Multiplicities for n = 3/4 are not
  independently verified, so these are a documented appendix, not a strict oracle.
- **n = 6 (10), n = 7 (7)** — counts only; no machine-readable per-tiling source exists (Wikipedia's tables
  stop at n = 5; Galebach's catalogue at probabilitysports.com/tilings.html is rendered images, no
  signatures). The tessella3 list is **not** treated as ground truth.

**Validation principle (the decisive consequence).** With a **sound** engine (rejects every non-tiling) that
**deduplicates** by a canonical key, returning exactly `A068600(n)` tilings *proves* it returned exactly the
true set — a sound, deduplicated subset whose size equals the known total is the whole set. So "the right
tilings, not just the right count" needs no fragile per-signature table for n ≥ 3; it follows from
**soundness + dedup + count**.

## Decision

**Enumerate the quotient as an *intrinsic combinatorial map* (a Delaney–Dress symbol, or an equivalent
flag/edge-pairing structure) — never via a guessed Λ and plane residues.** Build the partial map by gluing
regular-polygon faces edge-to-edge with the existing valid-vertex / ≤ n-type pruning; close when it is a
connected, orientable, genus-1 map with every vertex a complete 360° type; deduplicate by the canonical
(minimal) D-symbol; bucket by the A068600 condition (vertex orbits = types = n).

### Why this fixes the prototype's problems *by construction*

- **Soundness is automatic — no overlap check.** A finite, orientable, genus-1 map with every vertex exactly
  2π has **no cone points**: it is a *smooth* flat torus, whose developing map onto E² is a **bijection** (the
  deck group is a translation lattice). So a closed intrinsic map *cannot* overlap. Equivalently, one simply
  *cannot construct* a pure-3.3.6.6 torus intrinsically — the gluing gets stuck. The 3.3.6.6/3.4.4.6 defect
  was purely an artifact of the non-intrinsic (guessed-Λ + residue) construction.
- **No duplication.** The minimal D-symbol is a complete canonical invariant: one tiling ↔ one symbol; the
  (a)/(b) tilings differ by construction.
- **No covolume wall.** Cost is bounded by **face count per cell** (tens of faces for n ≤ 7), not covolume —
  the property ADR-0021 needs and the fixed-Λ engines (ADR-0020) lack.

### What it does NOT solve on its own

The **enumeration branching** — the extend-vs-glue / which-edge-to-glue choice (ADR-0021's "gluing branch
factor" crux). The D-symbol formalism makes construction *canonical and sound*; it does not by itself bound
the search. Mitigation: this is the **proven** route — Galebach (2002) and Delgado-Friedrichs' D-symbol
enumeration reached n ≤ 6 on far older hardware — controlled by MRV ordering, the ≤ n-type prune, canonical
dedup of partial maps, and the measurement gate below.

## Architecture

Replace the "grow a planar cover, guess Λ, read periods" core of `KrotenheerdtTorusMapSearch` with an
**intrinsic interleaved-gluing map enumerator**:

- **State** — a partial map: regular-polygon faces `{3,4,6,12}` plus a partial edge-pairing, tracked
  combinatorially (flags / σ-involutions), with **no plane coordinates and no Λ**.
- **Moves** at the most-constrained open vertex (MRV): attach a new face, **or** glue an open edge to a
  compatible open edge. A glue is admissible only if combinatorially consistent (cannot over-fill a vertex
  beyond 360°, preserves orientability).
- **Close** — no open edges, every vertex a valid complete type, χ = V − E + F = 0, orientable ⇒ a
  guaranteed-real flat tiling (no overlap test needed).
- **Dedup** — by the canonical minimal flag/D-symbol key (coordinate-free; replaces `torusContentKey`).
- **A068600 filter** — n vertex orbits (map automorphisms) = n distinct vertex types = n.
- **Cross-check** — during bring-up, also realize the closed map in exact ℤ[ζ₁₂] and run the proven geometric
  tail (`verifyContentAnyN`) and `TilingReference` counts; once the combinatorial path is trusted, drop the
  geometric crutch.

The proven verify tail, `ZetaPoint`, `VertexTypes`, and `TilingReference` are reused; the fixed-Λ engines are
left untouched until the new engine reproduces their counts (and the soundness fix is then ported to them as a
separate task, since they are *complete* but currently *unsound* at high covolume).

## Plan

1. **Gavrog reuse spike (cheap, possibly decisive).** Gavrog/3dt + Systre (Olaf Delgado-Friedrichs) enumerate
   periodic tilings via D-symbols and are **JVM/Java** — same platform. Investigate (≈1–2 h) whether
   `org.gavrog` can enumerate 2D Euclidean tilings constrained to `{3,4,6,12}` regular polygons with n vertex
   types. If it fits, wire it in; if not (it leans 3D/nets), build the intrinsic enumerator below.
2. **Build the intrinsic interleaved-gluing enumerator** with the canonical map key.
3. **Validate** against `TilingReference`: n = 1 → 2 → 3 (counts 11/20/39, key-equivalence to the fixed-Λ
   engine where it reaches).
4. **Measurement gate** (ADR-0021's): instrument states-per-cell at n = 2/3 and confirm it tracks cell
   *size*, not covolume, before attempting n = 4–7. If it tracks covolume, tighten controls first.
5. **Push to n = 4–7** and the dodecagon cells; assert each count equals A068600(n).

## Consequences

- The only route consistent with both **soundness** (your requirement: exactly the right tilings, not just
  the right count) and the **covolume wall** (ADR-0020): correctness becomes structural, and cost scales with
  cell size.
- A genuinely new engine and a coordinate-free canonical key — larger and higher-risk than the overlap-patch,
  but it removes two whole bug classes (false-period overlaps, type-set duplication) by construction.
- Interim state preserved: `KrotenheerdtTorusMapSearch` is sound at n = 1 (overlap gate) but search-incomplete
  for the hardest cells; the fixed-Λ engines stay the complete-but-currently-unsound reference until the
  overlap fix is ported.

## Alternatives considered

- **Keep the geometric engine + overlap check.** Sound, but the grow-cover search is incomplete for complex
  cells and the per-state overlap test is costly; duplication-as-display persists; does not change the
  construction's fragility. Rejected as the long-term core (kept as the n = 1 sanity engine).
- **Port the overlap fix into the fixed-Λ engines and stop there.** Makes them sound *and* they are already
  complete, so it would deliver exact n = 1/2 quickly — but it inherits the ADR-0020 **covolume-exponential
  wall**, so it cannot reach n = 4–7 or the dodecagon cells. Worth doing for a validated low-n reference, but
  not the route to the goal.
- **Reuse Gavrog wholesale.** Potentially the fastest path to correct results if the API fits; uncertain it
  covers this exact constrained problem. Resolved by the spike in step 1 rather than assumed.
