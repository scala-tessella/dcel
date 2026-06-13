# ADR-0018: Replicating OEIS A068600 — the Krotenheerdt tilings

- **Status:** Accepted
- **Date:** 2026-06-13

> Implemented in the `generator/` subproject (`VertexTypes`, `PatchCanonical`,
> `TilingCertifier`, `KrotenheerdtSearch`, `KrotenheerdtApp`) with the supporting
> library method `TilingLattice.validatedPeriods`. Results below are from the runs
> recorded in `generator/results/`.

## Context and problem statement

[OEIS A068600](https://oeis.org/A068600) counts the **Krotenheerdt tilings**: the
edge-to-edge tilings of the plane by regular polygons that are *n*-uniform (their
vertices fall into exactly *n* transitivity classes under the symmetry group)
**and** have exactly *n* distinct vertex types. The sequence is

| n | 1 | 2 | 3 | 4 | 5 | 6 | 7 | ≥8 |
|---|---|---|---|---|---|---|---|----|
| A068600(n) | 11 | 20 | 39 | 33 | 15 | 10 | 7 | 0 |

The *n* = 1 row is the 11 Archimedean (uniform) tilings; the higher rows are
Krotenheerdt's and Galebach's results, mirrored on
[probabilitysports.com/tilings.html](https://probabilitysports.com/tilings.html).

The goal of this work is to **independently confirm and replicate** these counts
with the `dcel` machinery — not to take them on faith but to enumerate the tilings
from scratch, certify each as a genuine infinite periodic tiling, and check the
totals. The decided scope is to **build the full apparatus and certify n ≤ 3 now**,
with the longer n = 4…7 runs left as parameterised invocations for later.

Only the polygons `{3, 4, 6, 8, 12}` can appear (others cannot close 360° with
regular partners), and the octagon appears only in `4.8.8`. We keep the octagon in
the alphabet so the search **re-derives** its exclusion for n ≥ 2 rather than
assuming it.

## Decision

A research-grade pipeline in the JVM-only `generator/` subproject, in four stages:

1. **Vertex types** (`VertexTypes`). Enumerate the 15 valid vertex configurations
   over `{3,4,6,8,12}` (interior angles summing to exactly 360°, length 3–6,
   bracelet-normalised), plus the table of contiguous *partial* fans used to prune
   the search.

2. **Canonical-growth search** (`KrotenheerdtSearch`). Depth-first growth of patches:
   at each step the deterministically chosen boundary vertex (nearest the patch
   centroid) is filled in **every** geometrically possible way. Any infinite tiling
   containing the patch must fill that vertex with one of the branched polygons, so
   completeness is preserved. Search states are merged only when **congruent**
   (`PatchCanonical.congruenceKey`, a reflection-invariant canonical form), which
   never loses a continuation. Sound prunes drop a branch only when no infinite
   regular tiling could contain it (a partial fan outside the valid table, an
   irregular auto-filled hole, more than *n* inner types, …).

3. **Certification** (`TilingCertifier`). A horizon patch is certified as evidence
   of an infinite *n*-uniform, *n*-type tiling, or rejected with a structured
   reason. The pipeline (detailed below) detects the translation lattice, exhibits a
   ≥ 2×2 block of identical fundamental cells (a constructive periodicity proof, per
   ADR-0015), checks a fully-interior witness cell shows exactly *n* types, and
   measures the vertex-orbit count by corona refinement to confirm uniformity = *n*.

4. **Identity** (`torusKey`). Each certified tiling is reduced to a canonical
   descriptor of one fundamental cell, so distinct infinite tilings are counted once
   regardless of which finite window certified them, and tilings sharing a vertex-type
   composition are still told apart.

`KrotenheerdtApp` is the `runMain` driver: it runs the search and persists each
certified tiling as an SVG (for eyeball comparison against Galebach's gallery) plus
round-trippable metadata and an index line.

## The certification pipeline, concretely

A periodic tiling is certified from a finite patch as follows.

- **Translation lattice.** `TilingLattice.validatedPeriods` returns the candidate
  period vectors that survive landing validation over the patch interior, each
  tagged with its mismatch count. The certifier selects the basis itself
  (`selectBasis`): Gauss-reduced candidate pairs are tried in increasing cell-area
  order, and the first whose anchored grid yields a ≥ 2×2 block of **area-complete
  cells with identical reduced face content covering every face size** present in
  the patch wins. This replaced an earlier all-or-nothing detector (see
  *Alternatives*).

- **Parallelogon block.** The winning block's boundary is a parallelogon, which
  tiles the plane by translation carrying its faces (ADR-0015). With ≥ 2 cells in
  each direction every seam of the infinite tiling is witnessed inside the patch —
  a periodicity proof that, unlike translate-and-merge, does not depend on the
  patch outline.

- **Witness cell.** The deepest cell whose vertices are all patch-interior, so
  every type in it is measured. Its type set must be exactly the *n* patch-interior
  types.

- **Uniformity.** The witness-cell representatives are refined into vertex orbits by
  corona equivalence (`getDcelAtVertex` + `groupByBoundaryEquivalency`) at growing
  radius, comparing only coronas that stay clear of the patch boundary (a face-hull
  truncation guard — a dodecagon spans graph distance 6, so vertex-distance clearance
  alone is unsound). `classes == types == n` forces uniformity = gonality = *n*, the
  Krotenheerdt condition.

### The torus key and chirality

The torus key expresses one fundamental cell's faces and typed vertices in lattice
coordinates and takes the lexicographic minimum over the choice of cell origin and
over the candidate bases `±v, ±w, ±(v±w)`. Two subtleties, both found by the
cross-check overcounting:

- **Basis must be Gauss-reduced first.** `primitiveBasis` returns *some* primitive
  basis, and a doubled cell halved by it comes out skewed; two windows of the same
  tiling could then arrive with differently-shaped primitive bases that the small
  candidate set could not reconcile, yielding two keys for one tiling. Reducing to
  the two successive minima (`gaussReduced`) fixes this.

- **Reflection is automatic.** A068600 classifies tilings up to *all* isometries,
  so a chiral tiling and its mirror image (the only chiral 2-uniform tiling is
  `3.12.12 + 3.4.3.12`) count once. The key needs no explicit mirror pass: it
  quotients over every basis including orientation-reversing ones, and
  `descriptor(reflect P, reflect B) = descriptor(P, B)`, so it is reflection-invariant
  by construction once the basis is reduced.

## Results

- **n = 1 → 11.** Exactly the 11 Archimedean tilings, including `4.8.8` (octagon)
  and the three 3-polygon types `6.6.6`, `3.12.12`, `4.6.12`. `KrotenheerdtSpec`
  locks the count and the composition set. (~45 s.)

- **n = 2 → 20.** Exactly the 20 published 2-uniform Krotenheerdt tilings, with the
  composition multiset matching the published table: five vertex-type compositions
  occurring twice and ten once. The five doubled compositions are `{3⁶ , 3⁴.6}`,
  `{3⁶ , 3³.4²}`, `{3³.4² , 3².4.3.4}`, `{3³.4² , 4⁴}`, `{3.4².6 , 3.6.3.6}` — each a
  pair of genuinely distinct (non-mirror) tilings, kept apart by their torus keys.
  The chiral `3.12.12 + 3.4.3.12` is counted once.

`KrotenheerdtSpec` checks the n = 2 result on the discriminating cases that exhaust
quickly (a genuine pair → two keys; the chiral tiling → one key). The full
enumeration is reproduced by `KrotenheerdtApp` and recorded under
`generator/results/`.

## Assumptions and how they are checked

- **Horizon adequacy / type-ball radius.** The search certifies patches at a finite
  vertex horizon, and a growth gate requires every fully-interior vertex to see all
  *n* types within a small ball. Both could in principle drop a true tiling with a
  large fundamental cell. They do not for n ≤ 2: **every** published tiling is found
  at the radius-3 gate, so none has a type-ball radius exceeding 3 — the assumption
  is verified by the result, not merely posited. Each higher *n* must re-confirm
  this against its published count.

- **Corona equivalence ≈ orbit equivalence.** Uniformity is measured by local-DCEL
  boundary-equivalence of coronas, a local proxy for the global orbit relation,
  bounded by the depth at which coronas remain witnessed. Sound as a lower bound
  (distinct witnessed coronas ⇒ distinct orbits); the matching upper bound rests on
  this proxy and is cross-checked by the published counts.

- **Aperiodic look-alikes.** Triangle fields decorated with hexagons share a
  Krotenheerdt composition while being aperiodic for every decoration spacing
  (sparse, dense-row-stacked, or rim-placed). They are killed by sound final
  verdicts — `NoPeriodEvidence` (no two independent near-zero-mismatch period
  candidates ⇒ at best 1-D periodic) and `SubTilingPeriod` (a rounding-clean period
  whose cell misses a face size ⇒ only a sub-tiling is periodic) — plus the corona
  orbit lower bound as a veto on retries. These are what make the search terminate
  with the right count rather than drowning in look-alikes.

## Consequences

- A068600 is independently replicated for n ≤ 2 from first principles: each tiling
  is enumerated, certified as genuinely periodic and *n*-uniform, and identified up
  to isometry. The library's lattice/parallelogon machinery (ADR-0015) is exercised
  as the certification backbone.
- The search and certifier are parameterised (`enumerate(n, maxVertices, …,
  parallelism)`), with a work-stealing parallel scheduler, so n ≥ 3 is a matter of
  compute. n = 3…7 are not yet run.
- New library surface: `TilingLattice.validatedPeriods` (the validated candidate
  periods with mismatch counts) and a handful of `private[dcel]` lattice helpers
  reused by the certifier.
- The certifier's gates encode empirical thresholds (ball radius, mismatch budget)
  validated only up to n = 2; they are the most likely thing to need revisiting at
  higher *n*.

## Alternatives considered

- **Translate-and-merge wallpaper test** (repeat the patch along the basis and
  re-validate the merge). Rejected: false negatives on spiral patches and OOM via
  deferred validation of broken pseudo-period intermediates. The parallelogon-block
  proof is outline-independent.
- **All-or-nothing lattice detection.** A strict (zero-defect) period test is
  rounding-brittle at scale (one rounded-key flip among hundreds of landings kills a
  true period); a tolerant one admits sublattice false periods. Resolved by exposing
  all validated candidates and letting **content evidence** (cell-equality + face-size
  coverage) pick the basis.
- **Global `uniformityTree` for the orbit count.** It parks boundary-truncated
  vertices in spurious leaves on growth patches; replaced by corona refinement of a
  witnessed interior cell.
- **Reusing `TilingGenerator`.** Its `validSignatures` made the 4th polygon
  mandatory (dropping the three 3-polygon types) and its state key was not canonical;
  replaced wholesale.
