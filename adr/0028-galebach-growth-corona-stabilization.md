# ADR-0028: Reconstructing Galebach's growth — corona-stabilization on the geometric grower

- **Status:** Proposed (chosen direction, 2026-06-20). After the incenter-dual route was measured k=1-only
  (ADR-0027), the user chose to reconstruct Galebach's undocumented **growth** method — the only route
  *proven feasible* to n ≤ 6 (Galebach ~1 month, 2002; Čtrnáct later reproduced k ≤ 6 and pushed to k ≤ 12,
  also undocumented).
- **Date:** 2026-06-20

## What the literature gives us (this session's research)

- **Galebach (2002–03)** and **Čtrnáct (k ≤ 12)** exhaustively enumerated k-uniform tilings by **growth**;
  **no method is published and no completeness proof exists** (Lenngren 2009 survey confirms; reconfirmed
  2026). Counts of *all* k-uniform tilings: 20, 61, 151, 332, 673 (k=2..6). Our target A068600 is the
  **Krötenheerdt** subset (exactly k *distinct* types): 11, 20, 39, 33, 15, 10, 7.
- **GomJau-Hogg notation + ANTWERP v3.0** (Symmetry 2021; open source `HHogg/antwerp`,
  `FlorisSteenkamp/gomjau-hogg`, app at antwerp.hogg.io): a **documented, deterministic, O(n)** way to BUILD a
  periodic regular-polygon tiling from a compact placement notation (seed polygon + hyphen-separated
  placement stages + `/m` mirror, `/r` rotation operations that generate the symmetry group). It is a
  **no-scatter growth primitive and a notation library**, NOT an exhaustive enumerator (it renders a supplied
  notation). Useful as (a) a build/validate primitive and (b) ground-truth catalogue to cross-check n=4–7
  (which `TilingReference` lacks as text for n=6,7). Exact placement semantics to be lifted from the
  open-source code at implementation time.
- **Dolbilin–Schattschneider–Senechal Local Theorem** ([[reference_local_theorem_corona]]): a locally-finite
  tiling is periodic with k orbit-types **iff its corona isomorphism-class count stabilizes at k past a
  (small, in E²) determining radius**. This is the documented scatter-control.

## The diagnosis of past growth failures

- **ADR-0018 geometric grower (`KrotenheerdtSearch.scala`)** — EXISTS and works for n ≤ 3. It fills the
  deterministically chosen boundary vertex in all valid ways, prunes by `validPartialFans`, merges congruent
  states (`PatchCanonical.congruenceKey`), certifies horizons (`TilingCertifier`) and dedups by torus key. Its
  only periodicity prune is **"#distinct vertex TYPES ≤ n"**. It scatters at n ≥ 4 (~850 states already at
  n=2).
- **Why the type-count prune is too weak (the key insight):** two vertices of the **same type** can lie in
  **different orbits** (different coronae). An aperiodic patch can keep #types ≤ n while #corona-classes
  (orbits) climbs without bound. The type-count prune never fires on that scatter; the **corona-class** count
  does. For a Krötenheerdt tiling #types = #orbits = n, so at the target they coincide — but *during growth*
  the corona-class count is the discriminating invariant.
- **Why ADR-0024's corona prune was a no-op:** it counted distinct coronae at a **fixed radius r = 2**.
  Aperiodic patches also have ≤ n distinct 2-coronae for a while (they diverge only at larger radius), so it
  never fired. **The Local Theorem requires STABILIZATION at GROWING radius, not a fixed radius.**

## Decision — instrument the existing grower with a growing-radius corona-stabilization prune

Build on `KrotenheerdtSearch` (it already has a `typeBallRadius` corona-ball mechanism), adding:

1. **Corona-class invariant.** For each *fully determined* vertex (whose r-corona lies entirely inside the
   patch), compute a canonical key of its r-corona (the labelled local patch up to isometry). Partition such
   vertices into corona-classes at the **maximal radius r supported by the current patch** (growing, not
   fixed).
2. **The prune (the scatter-killer).** Reject a patch as soon as its corona-class count **exceeds n** at any
   supported radius. Monotone in radius, so it fires increasingly hard as the patch grows — aperiodic patches
   (class count → ∞) are killed; genuine k-uniform patches stabilize at exactly n.
3. **Termination / emit.** When corona-classes have **stabilized** at exactly n over an increment of radius
   AND a translational period is detected (the determining-radius condition), extract the fundamental domain,
   wrap to a torus, and verify/key via `DelaneySymbols.classifyClosedMap` (sound: closed all-360° map ⇒ genuine
   flat tiling, ADR-0022) — dedup by canonical key, filter to exactly n distinct types.

**De-risk spike (do first, measure):** add the corona-class count prune and MEASURE whether the n=4 search
stays bounded (states do not blow up) while n ≤ 3 still reproduces the oracle exactly. This is the decisive
test of the growth path — if the growing-radius stabilization prune kills scatter, n ≤ 7 is in reach; if it
doesn't, growth is walled too and we have a measured negative.

## Validation

- n ≤ 3: exact, key-for-key vs the `DelaneySymbols` oracle.
- n = 4–7: sound engine + canonical dedup + matching `TilingReference.counts` ⇒ the exact set (project
  principle). Cross-check configs against the ANTWERP/GomJau-Hogg library and Wikipedia n ≤ 5.

## Risks / open questions

- **Branching before stabilization** may still be exponential even though the determining radius is bounded —
  the open risk. Galebach controlled it; whether the corona prune alone suffices is what the spike measures.
- **Determining radius** may exceed the patch radius for some high-k tilings (the k-isocoronal ≠ k-uniform
  caveat); the stabilization-over-an-increment test guards against premature emit.
- **Corona-key cost** (canonical local-patch keys per vertex per step) — must be incremental, not recomputed
  whole each step.

## Keepers

- `KrotenheerdtSearch` (geometric grower) — the spike target.
- `DelaneySymbols` oracle + `classifyClosedMap` + `dualSymbol` — verify/key/dedup tail.
- GomJau-Hogg/ANTWERP — build primitive + ground-truth catalogue (to lift at implementation time).
