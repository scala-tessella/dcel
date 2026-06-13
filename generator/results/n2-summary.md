# n = 2 replication summary (OEIS A068600(2) = 20)

The canonical-growth search certified **exactly the 20 published 2-uniform
Krotenheerdt tilings**, with the composition multiset matching the published
2-uniform table: five vertex-type compositions occur twice (pairs of distinct
tilings sharing a composition) and ten occur once. The chiral
`3.12.12 + 3.4.3.12` is counted once (mirror images identified). No 21st tiling
appeared.

Run: `KrotenheerdtApp 2 80 … 12` (horizon 80, hard-cap factor 4.5, early-type
gate 60, type-ball radius 3, 12 workers). All 20 surfaced within ~5.1M search
states; the search was stopped after the full set was found and held with no
further tiling through 6.2M states (a pre-fix run independently held at 20 + the
since-fixed chiral duplicate through 7M states).

## Certified compositions

| count | composition |
|-------|-------------|
| ×2 | `3.3.3.3.3.3 ; 3.3.3.3.6` (3⁶ , 3⁴.6) |
| ×2 | `3.3.3.3.3.3 ; 3.3.3.4.4` (3⁶ , 3³.4²) |
| ×2 | `3.3.3.4.4 ; 3.3.4.3.4` (3³.4² , 3².4.3.4) |
| ×2 | `3.3.3.4.4 ; 4.4.4.4` (3³.4² , 4⁴) |
| ×2 | `3.4.4.6 ; 3.6.3.6` (3.4².6 , 3.6.3.6) |
| ×1 | `3.3.3.3.3.3 ; 3.3.4.3.4` (3⁶ , 3².4.3.4) |
| ×1 | `3.3.4.3.4 ; 3.4.6.4` (3².4.3.4 , 3.4.6.4) |
| ×1 | `3.3.3.4.4 ; 3.4.6.4` (3³.4² , 3.4.6.4) |
| ×1 | `3.4.4.6 ; 3.4.6.4` (3.4².6 , 3.4.6.4) |
| ×1 | `3.4.6.4 ; 4.6.12` (3.4.6.4 , 4.6.12) |
| ×1 | `3.3.3.3.3.3 ; 3.3.4.12` (3⁶ , 3².4.12) |
| ×1 | `3.12.12 ; 3.4.3.12` (3.12.12 , 3.4.3.12 — chiral, once) |
| ×1 | `3.3.3.3.3.3 ; 3.3.6.6` (3⁶ , 3².6²) |
| ×1 | `3.3.3.3.6 ; 3.3.6.6` (3⁴.6 , 3².6²) |
| ×1 | `3.3.6.6 ; 3.6.3.6` (3².6² , 3.6.3.6) |

Total: **20** (5 × 2 + 10 × 1). Matches A068600(2).

Per-tiling renders, metadata and torus keys are produced by re-running
`KrotenheerdtApp` (see this directory's README); the `index.tsv` it writes lists
each tiling's torus key and basis.
