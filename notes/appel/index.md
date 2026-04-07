# Appel — *Compiling with Continuations* (1992)

Reference: Andrew W. Appel, Cambridge University Press.
Compiler described: Standard ML of New Jersey (SML/NJ).

This directory contains chapter-by-chapter notes on the book, focused on how
each chapter's content applies to the FunQ → OpenQASM/QIR compilation pipeline.
See also [../pipeline.md](../pipeline.md) for the high-level strategy document and [../index.md](../index.md) for the full knowledge index.

---

## FunQ Compilation Pipeline → Appel Chapter Map

| Stage | Description | Appel chapter(s) | Status |
|---|---|---|---|
| 1 | Parse + scope resolve | FunQ-specific | done |
| 2 | Lower to λ-calculus-like form; desugar patterns/data | Ch 4 (§4.1–4.2, §4.5–4.8) | done |
| 3 | Convert to CPS | Ch 5 | done |
| 4 | Recursion check + tail-loop recognition | FunQ-specific | done |
| 5 | Record-shape analysis + interface flattening | FunQ-specific | done |
| 6 | Gate/def classification | FunQ-specific | done |
| 7 | Closure conversion (eliminate free variables) | Ch 10 | done |
| 8 | Defunctionalization | FunQ-specific | done |
| 9 | Qubit hoisting | FunQ-specific | done |
| 10 | Local record flattening | FunQ-specific | done |
| 11 | Emit OpenQASM | Replaces Chs 13–14 | done (first cut) |
| — | β-contraction, constant folding | Ch 6 §6.1–6.2 | deferred |
| — | Beta expansion (inlining) | Ch 7 | deferred |
| — | Hoisting | Ch 8 | deferred |
| — | CSE | Ch 9 | deferred |
| — | Register spilling | Ch 11 | deferred |

The pipeline mostly follows Appel's CPS story, with FunQ-specific passes
inserted around it. Stage 6 is an early OpenQASM-facing classification pass
inserted before closure conversion so it can inspect clean interfaces. Stages
9–11 replace Appel's classical backend with the current OpenQASM lowering and
emission path.

---

## Chapter Relevance

### High relevance — read carefully

| Chapter | Title | Why relevant |
|---|---|---|
| 1 | Overview | CPS motivation, advantages over λ/QUAD/SSA, compiler structure |
| 2 | Continuation-passing style | The CPS IR datatype we will implement |
| 3 | Semantics of the CPS | Formal correctness; needed to reason about linearity |
| 5 | Conversion into CPS | The core algorithm: FunQ AST → CPS |
| 6 | Optimization of the CPS | β-contraction and eta are the workhorse optimizations |
| 10 | Closure conversion | Required to produce flat, emittable code |
| 11 | Register spilling | Maps to qubit/classical register allocation |

### Moderate relevance — read selectively

| Chapter | Title | Why relevant |
|---|---|---|
| 4 | ML-specific optimizations | Pattern matching (§4.2) and data constructors (§4.1) apply directly to FunQ; module system (§4.8) less so |
| 7 | Beta expansion | Inlining quantum subroutines; must respect linearity (no double-expansion of qubit args) |
| 8 | Hoisting | Lifting FIX definitions; relevant to circuit subroutine structure |
| 9 | Common subexpressions | Classical sub-computation deduplication only; quantum states cannot be duplicated |
| 12 | Space complexity | Qubit liveness analysis is a form of space analysis |

### Low relevance — skim or skip

| Chapter | Title | Notes |
|---|---|---|
| 13 | The abstract machine | Classical register machine; replaced by QASM/QIR emitter |
| 14 | Machine-code generation | VAX/MIPS/SPARC specific; not applicable |
| 15 | Performance evaluation | SML/NJ benchmarks; not directly applicable |
| 16 | The runtime system | GC, heap layout; not applicable (quantum backends manage memory differently) |
| 17 | Parallel programming | Coroutines/multiprocessor; may become relevant for parallel quantum circuits |
| 18 | Future directions | §18.2 (type information) and §18.6 (state threading) may be interesting |

### Appendices

| Appendix | Title | Notes |
|---|---|---|
| A | Introduction to ML | Reference only; FunQ syntax is similar |
| B | Semantics of the CPS | Formal denotational semantics; useful for proving linearity preservation |
| C | Obtaining SML/NJ | Not applicable |
| D | Readings | Bibliography guide |

---

## Reading Progress

| Chapter | Status | Notes file |
|---|---|---|
| 1 | Read | [ch01-overview.md](ch01-overview.md) |
| 2 | Read | [ch02-cps-datatype.md](ch02-cps-datatype.md) |
| 4 | Read | [ch04-lambda-language.md](ch04-lambda-language.md) |
| 5 | Read | [ch05-cps-conversion.md](ch05-cps-conversion.md) |
| 10 | Read | [ch10-closure-conversion.md](ch10-closure-conversion.md) |
| 11 | Read | [ch11-register-spilling.md](ch11-register-spilling.md) |
| 3, 6–9, 12–18 | Not started | — |
