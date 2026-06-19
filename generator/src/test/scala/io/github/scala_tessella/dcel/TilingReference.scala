package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.VertexSignature

/** Ground-truth reference for the Krotenheerdt enumeration (OEIS A068600: n-uniform tilings with exactly n
  * DISTINCT vertex types). Used to validate that every engine returns *exactly* the right tilings — not
  * merely the right quantity.
  *
  * RELIABILITY / PROVENANCE (deliberately explicit — see ADR-0021 session):
  *   - `counts` (n = 1..7) are authoritative: OEIS A068600 = 11, 20, 39, 33, 15, 10, 7.
  *   - `n1` (the 11 Archimedean) and `n2` (the 20 two-uniform vertex-type pairs) are reliable: n=1 is
  *     universally documented; n=2 was transcribed from Wikipedia "List of k-uniform tilings" and its count
  *     checks (20). These are the TESTED oracle.
  *   - n = 3..5 per-tiling configurations (from Wikipedia "List of k-uniform tilings") are kept in
  *     [[rawWikipediaN3to5]]; their ROW COUNTS match A068600 (39, 33, 15), but their exact MULTIPLICITIES
  *     (how many distinct tilings share a vertex-type set) are not independently verified, so they are a
  *     documentation appendix, not a strict pass/fail oracle. n = 6, 7 are NOT available as text anywhere we
  *     found (Wikipedia's tables stop at n=5; Galebach's catalogue probabilitysports.com/tilings.html is
  *     image-only) — only the counts (10, 7) are known.
  *
  * THE VALIDATION PRINCIPLE this enables: a SOUND engine (rejects every non-tiling — e.g. the overlapping
  * false-period cells 3.3.6.6 / 3.4.4.6, see ADR-0021) that DEDUPLICATES by canonical key and returns exactly
  * `counts(n)` tilings must be returning *exactly* the true set — a sound, deduplicated subset whose size
  * equals the known total is the whole set. So for n ≥ 3, "exactly the right tilings" is certified by
  * soundness + dedup + matching `counts(n)`, without needing a fragile per-signature table.
  */
object TilingReference:

  /** OEIS A068600 — number of Krotenheerdt n-uniform tilings (n distinct vertex types). Authoritative. */
  val counts: Map[Int, Int] =
    Map(1 -> 11, 2 -> 20, 3 -> 39, 4 -> 33, 5 -> 15, 6 -> 10, 7 -> 7)

  /** Of the 11 1-uniform (Archimedean) tilings, the one needing the octagon (`4.8.8`, ℤ[ζ₂₄]) is out of the
    * `{3,4,6,12}` world the exact-coordinate engines cover; so those engines target 10.
    */
  val n1CountNoOctagon: Int = 10

  private def sig(s: String): VertexSignature = VertexTypes.normalize(s.split('.').map(_.toInt).toList)

  /** The 11 Archimedean vertex configurations (n = 1). Authoritative. */
  val n1: Set[VertexSignature] =
    Set(
      "3.3.3.3.3.3",
      "3.3.3.3.6",
      "3.3.3.4.4",
      "3.3.4.3.4",
      "3.4.6.4",
      "3.6.3.6",
      "3.12.12",
      "4.4.4.4",
      "4.6.12",
      "4.8.8",
      "6.6.6"
    ).map(sig)

  /** The 10 Archimedean reachable in the `{3,4,6,12}` world (4.8.8 excluded). */
  val n1NoOctagon: Set[VertexSignature] = n1 - sig("4.8.8")

  /** The 20 two-uniform (n = 2) tilings, each as its set of vertex types (the two orbits). Transcribed from
    * Wikipedia "List of k-uniform tilings" (raw wikitext); count = 20. NOTE several vertex-type *pairs* occur
    * TWICE as distinct tilings (different adjacency) — so this is a multiset of 20 entries, NOT 20 distinct
    * type-sets. A tiling is distinguished by its geometry (canonical key), not by its type-set alone.
    */
  val n2: List[Set[VertexSignature]] =
    List(
      Set("3.3.3.3.3.3", "3.3.4.3.4"),
      Set("3.4.6.4", "3.3.4.3.4"),
      Set("3.4.6.4", "3.3.3.4.4"),
      Set("3.4.6.4", "3.4.4.6"),
      Set("4.6.12", "3.4.6.4"),
      Set("3.3.3.3.3.3", "3.3.4.12"),
      Set("3.12.12", "3.4.3.12"),
      Set("3.3.3.3.3.3", "3.3.6.6"),
      Set("3.3.3.3.3.3", "3.3.3.3.6"), // [3⁶; 3⁴.6]₁
      Set("3.3.3.3.3.3", "3.3.3.3.6"), // [3⁶; 3⁴.6]₂
      Set("3.3.6.6", "3.3.3.3.6"),
      Set("3.6.3.6", "3.3.6.6"),
      Set("3.4.4.6", "3.6.3.6"),       // [3.4².6; 3.6.3.6]₂
      Set("3.4.4.6", "3.6.3.6"),       // [3.4².6; 3.6.3.6]₁
      Set("3.3.3.4.4", "3.3.4.3.4"),   // [3³.4²; 3².4.3.4]₁
      Set("3.3.3.4.4", "3.3.4.3.4"),   // [3³.4²; 3².4.3.4]₂
      Set("4.4.4.4", "3.3.3.4.4"),     // [4⁴; 3³.4²]₁
      Set("4.4.4.4", "3.3.3.4.4"),     // [4⁴; 3³.4²]₂
      Set("3.3.3.3.3.3", "3.3.3.4.4"), // [3⁶; 3³.4²]₁
      Set("3.3.3.3.3.3", "3.3.3.4.4")  // [3⁶; 3³.4²]₂
    ).map(_.map(sig))

  /** The distinct vertex types appearing across the Krotenheerdt tilings (the search alphabet, octagon
    * aside): all 14 of the {3,4,6,12} angle-valid types — including the four (3.3.6.6, 3.4.4.6, 3.3.4.12,
    * 3.4.3.12) that CANNOT tile 1-uniformly but DO occur as orbits of n ≥ 2 tilings.
    */
  val vertexTypes: Set[VertexSignature] =
    List(
      "3.3.3.3.3.3",
      "3.3.3.3.6",
      "3.3.3.4.4",
      "3.3.4.3.4",
      "3.4.6.4",
      "3.6.3.6",
      "3.12.12",
      "4.4.4.4",
      "4.6.12",
      "6.6.6",
      "3.3.6.6",
      "3.4.4.6",
      "3.3.4.12",
      "3.4.3.12"
    ).map(sig).toSet

  /** Raw Wikipedia "List of k-uniform tilings" strings for n = 3..5 — DOCUMENTATION ONLY, not a tested oracle
    * (transcription is not trustworthy; see class doc). Compact notation: `3^6` = 3.3.3.3.3.3, `3^2.4.3.4` =
    * 3.3.4.3.4, `3.4^2.6` = 3.4.4.6, `3^2.6^2` = 3.3.6.6, `3.12^2` = 3.12.12, `4.6.12`, `3^4.6` = 3.3.3.3.6,
    * `3^3.4^2` = 3.3.3.4.4, `3^2.4.12` = 3.3.4.12, `3.4.3.12`, `3.4.6.4`, `3.6.3.6`, `4^4`, `6^3`.
    */
  val rawWikipediaN3to5: Map[Int, List[String]] =
    Map(
      3 -> List(
        "3.4^2.6; 3.6.3.6; 4.6.12",
        "3^6; 3^2.4.12; 4.6.12",
        "3^2.4.12; 3.4.6.4; 3.12^2",
        "3.4.3.12; 3.4.6.4; 3.12^2",
        "3^3.4^2; 3^2.4.12; 3.4.6.4",
        "3^6; 3^3.4^2; 3^2.4.12",
        "3^6; 3^2.4.3.4; 3^2.4.12",
        "3^4.6; 3^3.4^2; 3^2.4.3.4",
        "3^6; 3^2.4.3.4; 3.4^2.6",
        "3^6; 3^2.4.3.4; 3.4.6.4",
        "3^6; 3^3.4^2; 3.4.6.4",
        "3^6; 3^2.4.3.4; 3.4.6.4",
        "3^6; 3^3.4^2; 3^2.4.3.4",
        "3^2.4.12; 3.4.3.12; 3.12^2",
        "3.4.6.4; 3.4^2.6; 4^4",
        "3^2.4.3.4; 3.4.6.4; 3.4^2.6",
        "3^3.4^2; 3^2.4.3.4; 4^4",
        "3.4^2.6; 3.6.3.6; 4^4",
        "3.4^2.6; 3.6.3.6; 4^4",
        "3.4^2.6; 3.6.3.6; 4^4",
        "3.4^2.6; 3.6.3.6; 4^4",
        "3^3.4^2; 3^2.6^2; 3.4^2.6",
        "3^2.6^2; 3.4^2.6; 3.6.3.6",
        "3^2.6^2; 3.4^2.6; 3.6.3.6",
        "3^4.6; 3^3.4^2; 3.4^2.6",
        "3^2.6^2; 3.4^2.6; 3.6.3.6",
        "3^2.6^2; 3.6.3.6; 6^3",
        "3^2.6^2; 3.6.3.6; 6^3",
        "3^4.6; 3^2.6^2; 6^3",
        "3^6; 3^2.6^2; 6^3",
        "3^6; 3^4.6; 3^2.6^2",
        "3^6; 3^4.6; 3^2.6^2",
        "3^6; 3^4.6; 3^2.6^2",
        "3^6; 3^4.6; 3.6.3.6",
        "3^6; 3^4.6; 3.6.3.6",
        "3^6; 3^4.6; 3.6.3.6",
        "3^6; 3^3.4^2; 4^4",
        "3^6; 3^3.4^2; 4^4",
        "3^6; 3^3.4^2; 4^4"
      ), // 39 rows as fetched (multiplicities unverified)
      4 -> List(
        "3^2.4.3.4; 3^2.6^2; 3.4^2.6; 6^3",
        "3^3.4^2; 3^2.6^2; 3.4^2.6; 4.6.12",
        "3^2.4.3.4; 3^2.6^2; 3.4^2.6; 4.6.12",
        "3^6; 3^3.4^2; 3^2.4.3.4; 3^2.4.12",
        "3^6; 3^2.4.3.4; 3^2.4.12; 3.12^2",
        "3^6; 3^2.4.3.4; 3.4.3.12; 3.12^2",
        "3^6; 3^3.4^2; 3^2.4.3.4; 3.4.6.4",
        "3^6; 3^3.4^2; 3^2.4.3.4; 3.4.6.4",
        "3^6; 3^2.4.3.4; 3.4.6.4; 3.4^2.6",
        "3^4.6; 3^2.6^2; 3.6.3.6; 6^3",
        "3^4.6; 3^2.6^2; 3.6.3.6; 6^3",
        "3^2.4.12; 3.4.3.12; 3.4.6.4; 4.6.12",
        "3^3.4^2; 3^2.4.12; 3.4.3.12; 3.12^2",
        "3^3.4^2; 3^2.4.12; 3.4.3.12; 4^4",
        "3^3.4^2; 3^2.4.12; 3.4.3.12; 3.12^2",
        "3^6; 3^3.4^2; 3^2.4.3.4; 4^4",
        "3^2.4.3.4; 3^2.6^2; 3.4.6.4; 3.4^2.6",
        "3^6; 3^3.4^2; 3.4^2.6; 3.6.3.6",
        "3^6; 3^4.6; 3.4^2.6; 3.6.3.6",
        "3^6; 3^4.6; 3.4^2.6; 3.6.3.6",
        "3^6; 3^4.6; 3^3.4^2; 3.4^2.6",
        "3^6; 3^4.6; 3^3.4^2; 3.4^2.6",
        "3^6; 3^4.6; 3^2.6^2; 6^3",
        "3^6; 3^4.6; 3^2.6^2; 6^3",
        "3^6; 3^4.6; 3^2.6^2; 6^3",
        "3^6; 3^4.6; 3^2.6^2; 6^3",
        "3^6; 3^4.6; 3^2.6^2; 3.6.3.6",
        "3^3.4^2; 3^2.6^2; 3.4^2.6; 6^3",
        "3^3.4^2; 3^2.6^2; 3.4^2.6; 6^3",
        "3^2.6^2; 3.4^2.6; 3.6.3.6; 4^4",
        "3^2.6^2; 3.4^2.6; 3.6.3.6; 4^4",
        "3^2.6^2; 3.4^2.6; 3.6.3.6; 4^4",
        "3^2.6^2; 3.4^2.6; 3.6.3.6; 4^4"
      ), // 33 rows as fetched (multiplicities unverified)
      5 -> List( // all 15, user-supplied from Wikipedia (the dodecagon row #7 is the one tooling had dropped)
        "3^2.4.3.4; 3^2.6^2; 3.4.6.4; 3.4^2.6; 6^3",
        "3^6; 3^4.6; 3^2.6^2; 3.6.3.6; 6^3",
        "3^6; 3^4.6; 3^3.4^2; 3.4^2.6; 4.6.12",
        "3^4.6; 3^3.4^2; 3^2.4.3.4; 3.4^2.6; 4^4",
        "3^6; 3^2.4.3.4; 3.4.6.4; 3.4^2.6; 3.6.3.6",
        "3^6; 3^4.6; 3.4.6.4; 3.4^2.6; 3.6.3.6",
        "3^2.4.3.4; 3^2.4.12; 3.4.6.4; 3.12^2; 4.6.12",
        "3^6; 3^4.6; 3.4^2.6; 3.6.3.6; 4^4",
        "3^6; 3^4.6; 3.4^2.6; 3.6.3.6; 4^4",
        "3^6; 3^4.6; 3.4^2.6; 3.6.3.6; 4^4",
        "3^6; 3^4.6; 3.4^2.6; 3.6.3.6; 4^4",
        "3^6; 3^3.4^2; 3.4^2.6; 3.6.3.6; 4^4",
        "3^6; 3^4.6; 3^3.4^2; 3.4^2.6; 4^4",
        "3^6; 3^3.4^2; 3^2.6^2; 3.4^2.6; 3.6.3.6",
        "3^6; 3^4.6; 3^3.4^2; 3^2.6^2; 3.4^2.6"
      ) // 15 rows (count matches A068600(5))
    )
