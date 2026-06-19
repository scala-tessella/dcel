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

1. **Gavrog reuse spike — DONE (2026-06-20).** Outcome: *reuse the D-symbol theory and data, build the
   enumerator.* Findings:
   - **Gavrog** (`org.gavrog`, JVM) has the D-symbol data structures + algorithms but ships as a **3D-net**
     tool (Systre); reusable as a library for the `DSymbol` type, not as a turnkey 2D regular-polygon
     enumerator.
   - **Tegula** (Huson/Delgado-Friedrichs) is the dedicated **2D periodic-tiling** explorer via Delaney–Dress
     symbols (Java/JavaFX, open source) — but GUI-oriented, no clean "all k-uniform `{3,4,6,12}` tilings" API.
   - **genDSyms** (Julia) ships **databases of all Euclidean tilings up to Dress complexity 24** — the most
     directly useful asset: a ready D-symbol dataset to **filter** (tile m-values ∈ {3,4,6,12}, χ = 0, n
     vertex orbits = n types) as an **independent ground-truth oracle** for n = 3–7 (which Wikipedia/Galebach
     could not give as text).
   - **`gavrog.org/TCS.pdf`** (Delgado-Friedrichs, "Data Structures and Algorithms for Tilings I") is the
     build blueprint.
   Decision: do NOT bolt a 3D GUI app into the pipeline; build the enumerator in-stack per TCS.pdf, and use the
   genDSyms Euclidean D-symbol database as the n = 3–7 cross-check oracle (filling the gap left in
   `TilingReference`, where n = 6/7 were count-only).

   **genDSyms "database" pull — DONE (2026-06-20).** There is **no bundled database** to pull: the repo
   `github.com/odf/julia-dsymbols` is **generator source only** (Julia, ~1300 LOC), and the published
   databases are giant SQLite files (≈2.4 billion tilings, all Euclidean+spherical ≤ Dress complexity 24) via
   the Tegula download site — impractical to fetch here, and purely *combinatorial* (a "6-edge tile" is not
   necessarily a regular hexagon), so they would need our `{3,4,6,12}` + 360° realizability filter regardless.
   What we DID pull is the **generator algorithm**, which is the more useful asset: a generic `BackTracker` +
   `DSetGenerator(2, maxSize)` (enumerate the σ-involution D-sets up to `maxSize` chambers) →
   `DSymGenerator` (assign the v/m-values) → curvature/orientation properties. A D-symbol is `op[D,i]` (the
   three involutions σ₀,σ₁,σ₂) plus `v[D,i]` (m-values: tile edge-count m₀₁ and vertex degree m₁₂). This ports
   cleanly to Scala and IS the ADR-0022 engine core; run at a modest `maxSize` with our filter (tiles ∈
   {3,4,6,12}, every vertex a valid 360° regular-polygon type, k tile/vertex orbits = k distinct types) it
   yields the n = 1–7 Krotenheerdt tilings natively — no giant download. (genDSyms is external/licensed; we
   port the algorithm, not vendor the source.)
2. **Build the enumerator by porting the genDSyms generator** (`backTracker` → `DSetGenerator` →
   `DSymGenerator` → properties) to Scala, plus the regular-polygon filter (tiles ∈ {3,4,6,8,12}, all vertices
   valid 360° types, k orbits = k types). The minimal D-symbol is the canonical map key. This subsumes the
   "intrinsic interleaved-gluing" idea — enumerating D-symbols IS the intrinsic, coordinate-free construction —
   and gives the n = 3–7 oracle and the engine in one.
   - **DONE (2026-06-20):** `DelaneySymbols.scala` ports the generator (Frac curvature, DSet + orbits +
     orientation + automorphisms, `DSetGenerator`, `DSymGenerator`, the regular-polygon/Krotenheerdt filter)
     plus a **minimality** filter (keep only the maximal-symmetry symbol per tiling) and a vertex-config
     reconstruction that unfolds the symbol's symmetry (`m₁₂ = r₁₂·v₁₂`). **n = 1 = exactly 11 Archimedean,
     including `4.8.8`** (which the ζ engines cannot do), in ~0.5 s, sound (3.3.6.6 / 3.4.4.6 cannot be
     constructed). Tested by `DelaneySymbolsSpec` (Frac laws, n=1 exact, octagon, soundness, A068600 bound,
     monotonicity, n=2 ⊆ the 20).
   - **Pruning (2026-06-20):** two SOUND prunes restrict the search to the flat regular-polygon world:
     (a) **regular-polygon r-values** — a closed 01-orbit (tile) must have `r ∈ {1,2,3,4,6,8,12}` and a closed
     12-orbit (vertex) `r ∈ {1,2,3,4,5,6}` (so `m = r·v` can be a `{3,4,6,8,12}`-gon / a degree-3–6 vertex);
     a closed orbit is final, so a partial D-set violating this is dropped in `DSetGenerator.children`.
     (b) **euclidean-feasibility** — skip a complete D-set's whole `DSymGenerator` unless its MAXIMAL curvature
     (all v at `minV`) is ≥ 0 (otherwise every v-assignment is hyperbolic). Effect: `maxSize 16` went from
     time-out to 1.7 s; `maxSize 20` runs in ~42 s. Counts: n = 1 = 11 (complete), n = 2 = 19/20, n = 3 = 35/39
     at `maxSize 20` (still climbing with size).
   - **OPEN — the remaining wall for n ≥ 4:** the euclidean gate is on COMPLETE D-sets, so the D-set GENERATION
     tree is still fully walked; cost still grows ~4–5× per `+2` chambers. n = 2 → 20 needs `maxSize ≈ 22–24`
     (minutes); n = 4–7 need much larger symbols. The next lever is a **partial euclidean-curvature prune** in
     `DSetGenerator` (cut a partial branch once no in-budget completion can reach curvature 0) or a smarter
     euclidean-orbifold generator (Delgado-Friedrichs' methods) — both non-trivial; the crude per-chamber
     curvature bound is too loose to prune, so a tighter r-value-aware bound is needed.
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
