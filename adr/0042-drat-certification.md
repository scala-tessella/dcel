# ADR-0042: DRAT certification of the solver refutations

- **Status:** Proposed (user-requested; not started). 2026-07-08.
- **Follows:** [[0039-constraint-first-pivot-and-the-12n-bound]] (the SAT assembler; the n=8 = 0 ceiling and
  every enumeration's exhaustiveness rest on solver-reported UNSAT).

## Context — the one black box left in the trust chain

The completed A068600 result has two kinds of load-bearing NEGATIVE claims: (a) **n=8 = 0** — 1617
per-type-set refutations; (b) the **terminal UNSAT** ending every enumeration (what makes each level's count
"all of them" rather than "the ones found"). Both currently mean "SAT4J said UNSAT". Everything else in the
chain is either mathematics documented in the ADRs (filter necessity, the 12n bound, star-model
completeness) or code exercised by the oracle fixtures — but the solver itself is trusted blindly.

## What DRAT is

DRAT ("Deletion Resolution Asymmetric Tautology") is the standard machine-checkable proof format for UNSAT:
the CDCL solver logs every learned-clause addition (each RUP/RAT-checkable in polynomial time) and deletion,
ending in the empty clause. A small independent checker — `drat-trim`, or formally verified checkers
(`cake_lpr` in CakeML, GRAT) — validates the log against the original CNF **without trusting the solver**.
This is the standard by which modern computer-assisted proofs (Pythagorean triples, Keller's conjecture,
Schur number 5) are audited.

## Why it is useful here

- Upgrades the n=8 ceiling and every exhaustiveness claim from "our program says so" to an independently
  checkable proof artifact — the natural rigor bar before publishing the counts or the ceiling.
- Shrinks the trusted base to: the encoding generator (fixture-tested + necessity arguments in ADR-0039/40),
  the classify tail (the oracle's own validated filters), and a tiny proof checker.
- Cheap: our instances are small (≤ ~170 chambers even for ADR-0041 rows); proof logging overhead and
  checking time are negligible at this scale.

## Decision — the route (when built)

**SAT4J does not emit DRAT.** The certification path is a DIMACS pipe:

1. Emit each frame's CNF to a DIMACS file (the encoding already streams clauses — tee them to a writer;
   cardinality `exactly-1` must be expanded to CNF (pairwise/binomial at our sizes) since DRAT is clausal).
2. For each UNSAT obligation — a refuted frame, or an enumeration's terminal state (original CNF + all
   blocking clauses of the found models) — run kissat/CaDiCaL with `--proof`, producing a DRAT file.
3. Check with `drat-trim` (and optionally a verified checker for the headline n=8 artifacts); store
   (CNF, proof, checker verdict) as durable artifacts per obligation.
4. Cross-check discipline: the kissat verdicts must agree with SAT4J's everywhere (a free two-solver
   consistency check); the SAT side needs no certificates (each found model is verified by our own
   `sigma0Valid` + classify tail — models are self-certifying).

External dependency note: kissat/CaDiCaL + drat-trim are native binaries (not JVM); they would be dev/CI
tools, not library dependencies — the enumeration engine stays pure-JVM SAT4J.

## Consequences

- **Positive:** publication-grade auditability for the negative claims; two-solver agreement as a bonus
  correctness net; artifacts are reproducible and small.
- **Negative / risk:** native tooling in the loop (packaging/CI); the exactly-one expansion slightly changes
  the emitted CNF vs the SAT4J instance (must re-verify equivalence — same models — on the gate levels);
  certificates cover the SOLVER link only — the encoding's faithfulness remains a code/mathematics claim
  (mitigated by the oracle fixtures and the ten-gate ladders).
