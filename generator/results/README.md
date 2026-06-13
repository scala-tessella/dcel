# Enumeration results

Artifacts produced by `KrotenheerdtApp` (see [ADR-0018](../../adr/0018-krotenheerdt-enumeration.md)).
Each run writes, under `n<N>/`:

- `index.tsv` — one line per distinct certified tiling: composition, torus key, basis, certifying patch size;
- `tiling-<key>.svg` / `tiling-<key>.xml` — a render and the round-trippable patch metadata.

Regenerate with, e.g.:

```
sbt "generator/runMain io.github.scala_tessella.dcel.KrotenheerdtApp 2 80 generator/results/n2 12"
```

`n2-summary.md` records the n = 2 replication (20 tilings, composition multiset
matching the published 2-uniform table).
