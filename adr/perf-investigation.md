# DCEL — Validation performance investigation

- **Date:** 2026-04-23
- **Author:** @mario-callisto
- **Scope:** `io.github.scala-tessella.dcel` — `TilingSVG.fromMetadata`
  and the validation it triggers.
- **Status:** Findings handoff. Implementation to be done in the dcel
  repo.

## TL;DR

Importing SVG templates into Tessella Editor is dominated by
`TilingValidation.validateGeometrically`, which in turn is dominated by
`BigPoint.fromPolar` calling `spire.std.BigDecimalIsTrig.{cos,sin}`.
Spire's `BigDecimal` trigonometry evaluates a `spire.math.Real`
Cauchy-sequence at arbitrary precision on every call — ~100–1000×
slower than `Math.cos`/`sin` on `Double`.

The recommended fix is **library-side**: switch the validation path's
`fromPolar` to `Double` arithmetic with epsilon comparison. Expected
outcome: aperiodic/domino template import from **~6500 ms → < 500 ms**;
regular templates from ~700 ms → < 50 ms.

## Context

Tessella Editor ships 18 SVG templates under `public/templates/`
(3 regular, 8 semiregular, 7 aperiodic). Each template carries a
`<tessella:tessella-dcel>` metadata element with full vertex /
half-edge / face data. On import, `TilingSVG.fromMetadata` parses the
metadata and runs the library's validation pipeline. Templates were
produced by the editor's own SVG export (via `TilingSVG` + the editor's
`SvgExporter`) and are therefore already guaranteed to be valid DCELs
at the time they were written.

The editor observed import times of several seconds on the largest
templates. This document records what was measured, what the
bottleneck is, and what to do about it.

## Measurements

Browser: Firefox. Production build (`npm run preview`, Vite
`fullLinkJS`), `localhost:4173`. Timing via
`dom.console.time`/`timeEnd` bracketing the single call to
`TilingSVG.fromMetadata` in the editor's `SvgImporter.parseTiling`.

**Raw samples** — 4× each of the largest aperiodic template (`domino.svg`,
1023 V / 3004 HE / 481 F) and the smallest regular template
(`regular_6-6-6.svg`, 54 V / 144 HE / 20 F), alternated to rule out
caching or JIT-warmup effects:

| Template | Run 1 | Run 2 | Run 3 | Run 4 | Median |
|---|---|---|---|---|---|
| `domino.svg` | 6506 | 6558 | 6678 | 6530 | **6544 ms** |
| `regular_6-6-6.svg` | 715 | 729 | 701 | 706 | **711 ms** |
| `domino.svg` (2nd set) | 6503 | 6733 | 6689 | 6628 | — |
| `regular_6-6-6.svg` (2nd set) | 691 | 706 | 711 | 714 | — |

**Scaling model** — a linear fit in vertex count:

```
T(n) ≈ 370 ms + 6 ms/vertex
```

Variance is <1% across runs. There is no warmup effect: repeat loads
of the same template take the same time as the first load.

**Immediately rules out**:
- O(n²) / O(n³) on vertex count (would be >20 s on domino)
- JIT / class-init / parser warmup (load 2 = load 1)
- A template-specific input pathology (domino is simply the largest;
  every template's time is consistent with the linear model)

## Profile

Firefox Profiler recording of one steady-state `domino` load,
development build (`npm run dev`, `fastLinkJS`, readable Scala.js
names). The hot path is a **single linear chain** — no branching with
comparable percentages — which means one sub-tree owns the cost.

Stack walk, running % of total, down to the leaves that matter:

```
promise callback …                                              88%
└─ … editor wiring …
   └─ TilingDCEL$.fromUntrusted(List, List, List, Face) : Either 91%
      └─ TilingValidation$.validate(TilingDCEL) : Either          91%
         └─ TilingValidation$.validateGeometrically(TilingDCEL)  67%
            └─ SimplePolygon$.fromUntrusted(Vector) : Either     66%
               ├─ BigPoint$.fromPolar(BigDecimal, BigDecimal): T2 43%
               │  ├─ spire.std.BigDecimalIsTrig.cos(BigDecimal)  22%
               │  │  └─ BigDecimalIsTrig.fromReal(Real)          15%
               │  │     └─ Real$Inexact.apply(Int): SafeLong     15%
               │  │  and Real$.cos(Real): Real                   6.2%
               │  └─ spire.std.BigDecimalIsTrig.sin(BigDecimal)  21%
               └─ BigPoint$.isSimplePolygon(List): Boolean       21%
```

Minor contributors visible below the main chain (all <1% each):
`BigPoint.angleTo`, `BigPoint.distanceTo`, `BigPoint.$plus`,
`AngleDegree.toBigRadian`, `AngleDegree.sumExact`,
`AngleDegree.isFullCircle`, `Rational.apply(Double)`,
`Vector.map`, `ListBuffer.addOne`, a small slice of GC.

## Root-cause analysis

The bottleneck is a **single primitive** running on an **expensive
numeric stack**:

1. **`BigPoint.fromPolar(radius, angle): (BigDecimal, BigDecimal)`**
   is called for every vertex-like computation during geometric
   validation. Appears to reconstruct expected `(x, y)` from edge
   length + angle, presumably to verify that the stored coordinates
   are consistent with the stored topology.

2. It computes `x = radius · cos(angle)` and `y = radius · sin(angle)`
   using `spire.std.BigDecimalIsTrig`, which is:

   - Exact (`BigDecimal` precision preserved end-to-end).
   - Implemented by lifting `BigDecimal` → `spire.math.Real` (a lazy
     Cauchy sequence), calling `Real.cos` / `Real.sin`, then forcing
     the result at a requested precision via
     `Real$Inexact.apply(Int): SafeLong` and converting back to
     `BigDecimal`.
   - ~100–1000× slower than `java.lang.Math.cos(double)`, because the
     Cauchy-sequence evaluator allocates and reduces repeatedly per
     call.

3. For `domino` (1023 vertices), this path runs thousands of times.
   Accumulated cost: **~2.8 s of cos + ~1.4 s of sin = ~4.2 s** out of
   6.5 s total.

4. A secondary **21% / ~1.4 s** sits in
   `BigPoint.isSimplePolygon(List)`, likely an O(n²) edge-pair
   non-intersection check, also using `BigDecimal` arithmetic.

5. The remaining ~9% is topological validation, parsing, graph
   construction, and GC — all non-issues at this scale.

The important architectural observation: **validation correctness is
not what's expensive**. What's expensive is that a geometric primitive
used *inside* validation runs on an exact-arithmetic stack where a
`Double` would be more than precise enough to decide the question
being asked.

`fromPolar` in validation is answering *"is the stored (x, y)
consistent with (r, θ)?"* — a comparison-with-tolerance. `Double` has
~15 significant decimal digits, vastly exceeding any realistic
tolerance for tiling geometry at screen scale.

## Recommendations

Ranked by expected impact and simplicity.

### 1. `BigPoint.fromPolar` → `Double` + epsilon in the validation path

**Scope:** `BigPoint.fromPolar` and the callers in
`TilingValidation.validateGeometrically` /
`SimplePolygon.fromUntrusted`. Leave exact arithmetic in place for
any callers that genuinely need it (angle-sum checks on `Rational`,
etc. — already fast, per the profile).

**Change:**

```scala
// proposed signature/path inside validation
def fromPolarD(radius: Double, angle: Double): (Double, Double) =
  (radius * Math.cos(angle), radius * Math.sin(angle))

// comparison
def ~=(a: (Double, Double), b: (Double, Double), eps: Double = 1e-9): Boolean =
  math.abs(a._1 - b._1) <= eps && math.abs(a._2 - b._2) <= eps
```

The stored coordinates in the metadata (`BigDecimal`) convert to
`Double` via `.doubleValue` for the comparison.

**Expected outcome:**
- `BigPoint.fromPolar` cost drops from ~2800 ms to single-digit ms
- `validateGeometrically` drops from ~4400 ms to ~200 ms (the
  `isSimplePolygon` 1.4 s remains unless #3 is also done)
- `domino.svg` import: 6500 ms → ~500 ms
- `regular_6-6-6.svg` import: 711 ms → <50 ms
- All other validation semantics preserved: the same comparison is
  made, just with looser-but-more-than-adequate tolerance.

**Risks:**
- Epsilon choice. `1e-9` is conservative for screen-scale tilings
  (typical edge length 1, so 1e-9 is a billionth of an edge). Tune
  downward only if a specific test case demands it.
- Unit mismatch in polar input. If angle is currently supplied as a
  `BigRadian` from `Rational`, the conversion to `Double` is lossy —
  but only in the 16th digit, well below epsilon.

### 2. Memoize `cos`/`sin` results by angle (fallback if #1 is rejected)

If exact arithmetic is load-bearing somewhere I can't see, a smaller
win comes from caching:

```scala
private val cosCache = collection.concurrent.TrieMap[BigDecimal, BigDecimal]()
private val sinCache = collection.concurrent.TrieMap[BigDecimal, BigDecimal]()

def cachedCos(angle: BigDecimal): BigDecimal =
  cosCache.getOrElseUpdate(angle, trig.cos(angle))

def cachedSin(angle: BigDecimal): BigDecimal =
  sinCache.getOrElseUpdate(angle, trig.sin(angle))
```

**Expected outcome:**
- Regular / semiregular templates: huge win (a hexagonal tiling uses
  3–6 distinct angles). Probably 80%+ reduction in trig time.
- Aperiodic (`domino`, `penrose_*`): modest win. More distinct
  angles, but many faces still share angle values.
- Not as good as #1. Use only if #1 is blocked.

### 3. `BigPoint.isSimplePolygon` on `Double`

Secondary — 21% / 1.4 s on `domino`. Same treatment: perform the
edge-pair intersection checks on `Double` with epsilon, since the
underlying question ("do these two line segments cross?") tolerates
the same imprecision. Worth doing after #1 lands.

### 4. Integrity-checked fast path in `TilingSVG`

Previously considered as a way to short-circuit validation for
known-good input (e.g. templates the editor itself produced) via a
hash-verified `fromTrustedMetadata(meta: String, sha256: String)`.

**Re-assessment after this investigation:** becomes tactical rather
than strategic. If #1 lands and domino imports drop to ~500 ms,
there's no user-facing problem left to solve. The fast path is still
worth having for re-imports of previously-exported user work (same
"we already validated these bytes" story) but can be deferred.

## Out of scope but noted

The metadata coordinates are serialised at 34-digit precision (e.g.
`0.8660254037844385546600717137097877`). This is orders of magnitude
more precision than any screen-scale geometry needs, and the parsing
and arithmetic overhead is real. A separate, backward-compatible
change to emit coordinates at ~10 digits would shrink template files
(domino is 515 KB, mostly coordinate payload) and further speed up
parsing. Independent of the validation fix above.

## Verification plan

Once the dcel change lands and a new snapshot is published (or
vendored to `lib-repo/`):

1. Keep the `dom.console.time` bracket in the editor's
   `SvgImporter.parseTiling` (it was added for this investigation —
   revert at the end).
2. Re-run the alternating 4×domino / 4×regular protocol on
   `npm run preview`. Post the new numbers here.
3. Expected pass criteria:
   - `domino.svg` median < 1000 ms (soft), < 500 ms (target)
   - `regular_6-6-6.svg` median < 100 ms (soft), < 50 ms (target)
4. Confirm on a mid-range Android device (Firefox for Android, via
   `chrome://inspect` on desktop pointed at the dev server) — the
   Android/F-Droid packaging work in
   [ADR-005](adr/005-android-packaging-fdroid.md) depends on this
   validation cost being reasonable on mobile CPUs.

## Appendix — reproduction steps

1. Clone tessella-editor, `npm install`, `npm run build`, `npm run preview`.
2. In `src/main/scala/io/github/scala_tessella/editor/utils/file/SvgImporter.scala`,
   bracket the `TilingSVG.fromMetadata(metadataStr)` call with
   `dom.console.time("TilingSVG.fromMetadata")` and
   `dom.console.timeEnd("TilingSVG.fromMetadata")`.
3. Open `http://localhost:4173` in Firefox, DevTools → Console.
4. Templates → Aperiodic → `domino` (×4), Templates → Regular →
   `6.6.6` (×4), alternating.
5. Copy the timing lines from the console.

For the profile:

1. `npm run dev`.
2. `http://localhost:5173`, F12 → Performance → Start Recording.
3. Load `domino` once.
4. Stop recording.
5. Call Tree → sort by Running Time (or Total), drill into the
   `promise callback` subtree.
6. The single linear hot chain down to `BigDecimalIsTrig.{cos,sin}`
   is visible within ~10 levels.
