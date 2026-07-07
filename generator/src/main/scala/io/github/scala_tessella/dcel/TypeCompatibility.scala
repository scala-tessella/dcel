package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.DelaneySymbols.Frac
import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Fair top-down type-set derivation (ADR-0040 = ADR-0039 Phase 1).
  *
  * Derives, answer-blind, the candidate vertex-type-sets for the Krotenheerdt enumeration — including the
  * polygon ALPHABET itself, which [[VertexTypes.polygonSides]] hardcodes:
  *
  *   - Stage A [[arithmeticFigures]]: every bracelet of 3–6 regular polygons (p ≥ 3 UNBOUNDED) whose interior
  *     angles sum to 360° — the 21 figures of Kepler/Sommerville, from the angle equation alone.
  *   - Stage B [[viableFigures]]: the figures that can occur in SOME edge-to-edge tiling — a fixpoint of two
  *     provably-necessary local checks (edge-pair support, pinned polygon-corona satisfiability).
  *     Sommerville's six impossible figures die by odd-cycle corona parity, mechanized; the surviving
  *     alphabet is derived.
  *   - Stage C [[candidates]]: the size-n subsets passing the same necessary checks restricted to the set,
  *     plus connectivity — the fair per-type-set work-list for the Phase-2 realizer.
  *
  * Every filter is a NECESSARY condition of an edge-to-edge regular-polygon tiling realizing the set (each
  * scaladoc states why), so no realizable type-set is ever pruned; the inventory over-generates by design
  * (ADR-0036) and Phase 2 confirms/refutes each candidate.
  */
object TypeCompatibility:

  private def ordered(a: Int, b: Int): (Int, Int) = if a <= b then (a, b) else (b, a)

  /** The unordered pairs of polygons meeting across each edge at a vertex of this type: adjacent cyclic pairs
    * of the figure. At the OTHER end of an edge the same two faces meet, so that vertex's type must contain
    * the same pair — the edge-closure necessity used throughout.
    */
  def adjacentPairs(sig: VertexSignature): Set[(Int, Int)] =
    val n = sig.size
    (0 until n).map(i => ordered(sig(i), sig((i + 1) % n))).toSet

  /** The unordered neighbor pair of each OCCURRENCE of polygon `p` in the cyclic figure — i.e. the two tiles
    * flanking that specific p-gon around the vertex. Each occurrence at a realized vertex is an actual p-gon
    * whose corona must complete, so each pair is a legitimate corona pin.
    */
  def occurrenceNeighborPairs(sig: VertexSignature, p: Int): Set[(Int, Int)] =
    val n = sig.size
    (0 until n)
      .filter(i => sig(i) == p)
      .map(i => ordered(sig((i - 1 + n) % n), sig((i + 1) % n)))
      .toSet

  /** NECESSARY corona test. Around a p-gon the `p` neighbor tiles `t₁..t_p` must, at each of the `p` corners,
    * form an unordered pair `{tᵢ, tᵢ₊₁}` that is an occurrence-neighbor pair of `p` in SOME allowed figure
    * (the corner vertex contains the p-gon flanked by exactly those two tiles). `pinned` fixes corner 1 to a
    * specific pair (a known occurrence). Unordered pairs make the test orientation/chirality-safe: it can
    * only under-constrain, never wrongly refute. Solved by a reachability DP around the cycle.
    */
  def coronaSatisfiable(p: Int, figures: Set[VertexSignature], pinned: Option[(Int, Int)] = None): Boolean =
    val cornerPairs: Set[(Int, Int)] = figures.flatMap(f => occurrenceNeighborPairs(f, p))
    val firstPairs                   = pinned.fold(cornerPairs)(pp => if cornerPairs(pp) then Set(pp) else Set.empty)
    def step(t: Int): Set[Int]       = cornerPairs.collect {
      case (a, b) if a == t => b
      case (a, b) if b == t => a
    }
    firstPairs.exists: (a, b) =>
      Set((a, b), (b, a)).exists: (t1, t2) =>
        // corner 1 = (t₁,t₂); corners 2..p−1 advance t₂ → t_p; corner p closes the cycle back to t₁
        var reach = Set(t2)
        for _ <- 2 until p do reach = reach.flatMap(step)
        reach.exists(tp => cornerPairs(ordered(tp, t1)))

  /** Stage A: all vertex figures from the angle equation `Σ(1/2 − 1/pᵢ) = 1` alone — k ∈ [3,6] polygons (≥ 3
    * by definition of a vertex, ≤ 6 because no regular angle is below the triangle's 60°), sides `pᵢ ≥ 3`
    * UNBOUNDED (the equation itself caps them at 42). Multisets by Egyptian-fraction descent, then all
    * distinct bracelet arrangements. Exactly 21 (tested).
    */
  val arithmeticFigures: Set[VertexSignature] =
    def multisets(rem: Frac, terms: Int, minP: Int): List[List[Int]] =
      if terms == 0 then if rem.isZero then List(Nil) else Nil
      else if rem.signum <= 0 then Nil
      else
        // need Σ 1/pᵢ = rem over `terms` non-decreasing values: 1/p ≤ rem and terms·(1/p) ≥ rem
        val lo = math.max(minP.toLong, (rem.den + rem.num - 1) / rem.num) // ceil(den/num)
        val hi = terms.toLong * rem.den / rem.num                         // floor(terms·den/num)
        (lo to hi).toList.flatMap: p =>
          multisets(rem - Frac.make(1, p), terms - 1, p.toInt).map(p.toInt :: _)
    (3 to 6)
      .flatMap(k => multisets(Frac.make(k.toLong - 2, 2), k, 3))
      .flatMap(_.permutations)
      .map(normalize)
      .toSet

  /** Stage B: the figures that can occur in some edge-to-edge regular-polygon tiling — the fixpoint of two
    * necessary checks over the surviving universe: (i) every adjacent pair of F is an adjacent pair of some
    * survivor (edge-closure), and (ii) every polygon occurrence of F has a satisfiable pinned corona over the
    * survivors. Both only shrink as the universe shrinks, so iterating to a fixpoint stays necessary-sound.
    * Kills exactly Sommerville's six (tested); the derived alphabet is exactly {3,4,6,8,12} (tested against
    * the previously-hardcoded [[VertexTypes.polygonSides]]).
    */
  val viableFigures: Set[VertexSignature] =
    def consistent(f: VertexSignature, survivors: Set[VertexSignature]): Boolean =
      adjacentPairs(f).forall(pr => survivors.exists(g => adjacentPairs(g)(pr))) &&
        f.distinct.forall(p =>
          occurrenceNeighborPairs(f, p).forall(pi => coronaSatisfiable(p, survivors, Some(pi)))
        )
    var survivors                                                                = arithmeticFigures
    var changed                                                                  = true
    while changed do
      val next = survivors.filter(consistent(_, survivors))
      changed = next.size != survivors.size
      survivors = next
    survivors

  /** The polygon alphabet DERIVED (stage B), not assumed. */
  val derivedPolygonAlphabet: Set[Int] = viableFigures.flatten

  /** True when types `a` and `b` can be the two ends of some edge: they share an adjacent pair. Necessary for
    * adjacency; the REALIZED adjacency graph of a tiling containing both is a subgraph of this relation.
    */
  def canShareEdge(a: VertexSignature, b: VertexSignature): Boolean =
    (adjacentPairs(a) & adjacentPairs(b)).nonEmpty

  /** Stage C membership test — every check is necessary for an edge-to-edge tiling whose vertex-type set is
    * EXACTLY `s`: (1) edge-closure within `s`; (2) every polygon occurrence's pinned corona satisfiable over
    * `s`; (3) the [[canShareEdge]] graph on `s` connected (all types are realized and the tiling is
    * connected, so realized adjacency — a subgraph — connects them).
    */
  def isCandidate(s: Set[VertexSignature]): Boolean =
    def closure   = s.forall(f => adjacentPairs(f).forall(pr => s.exists(g => adjacentPairs(g)(pr))))
    def coronas   =
      s.forall(f =>
        f.distinct.forall(p => occurrenceNeighborPairs(f, p).forall(pi => coronaSatisfiable(p, s, Some(pi))))
      )
    def connected =
      val seen = collection.mutable.Set(s.head)
      val todo = collection.mutable.Queue(s.head)
      while todo.nonEmpty do
        val cur = todo.dequeue()
        for nxt <- s if !seen(nxt) && canShareEdge(cur, nxt) do
          seen += nxt
          todo.enqueue(nxt)
      seen.size == s.size
    s.nonEmpty && closure && coronas && connected

  /** Stage C: the fairly-derived candidate type-sets with exactly `n` distinct types — the Phase-2 work-list.
    * Over-generates (necessary filters only); never misses a realizable set (tested against every reference
    * type-set for n ≤ 5).
    */
  def candidates(n: Int): Set[Set[VertexSignature]] =
    viableFigures.subsets(n).filter(isCandidate).toSet
