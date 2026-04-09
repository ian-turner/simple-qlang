# Linear Types and Proto-Quipper — Reading List

This note collects the literature on linear type theory and the Proto-Quipper family of
languages. The goal is to inform FunQ's type system, which will take heavy inspiration
from Proto-Quipper's parameter/state distinction and `!` modality.

---

## Reading order (recommended)

Read in the order below. Each paper builds on the previous ones.

---

### 1. Girard — *Linear Logic* (1987)

**Venue:** Theoretical Computer Science 50:1–102  
**URL:** https://www.sciencedirect.com/science/article/pii/0304397587900454

The origin of linear logic. Introduces the core connectives (`⊗`, `⊸`, `!`, `&`, `⊕`)
and the key idea that propositions are *resources* — they must be used exactly once unless
marked with `!` (the "of course" / exponential modality, which permits duplication and
discarding). Everything in Proto-Quipper ultimately traces back to this paper.

**Key concepts for FunQ:** the `!` modality; resource sensitivity; why `A ⊸ B` ("A
lollipop B") is the right type for functions that consume their argument linearly.

---

### 2. Wadler — *Linear Types can Change the World!* (1990)

**Venue:** Programming Concepts and Methods (IFIP TC 2)  
**URL:** https://www.semanticscholar.org/paper/Linear-Types-can-Change-the-World!-Wadler/24c850390fba27fc6f3241cb34ce7bc6f3765627

Translates Girard's logic into a practical type system for functional languages. Shows
that linear types allow safe destructive array update and eliminate the need for reference
counting — without sacrificing referential transparency. The paper that demonstrated
linear types are a *compiler tool*, not just a logic curiosity.

**Key concepts for FunQ:** how `!A` means "A can be freely copied/discarded"; the
distinction between linear and unrestricted function types.

---

### 3. Selinger and Valiron — *A Lambda Calculus for Quantum Computation with Classical Control* (2006)

**Venue:** Mathematical Structures in Computer Science 16(3):527–552  
**arXiv:** https://arxiv.org/abs/cs/0404056

The foundational quantum lambda calculus. Defines a call-by-value typed λ-calculus
where `Qubit` is a linear type (no-cloning enforced by the type system) and classical
control (`if/else`, recursion) operates on unrestricted data. Gives a type system derived
from affine intuitionistic linear logic, an operational semantics, and safety theorems.

**Key concepts for FunQ:** how `Qubit` is made linear; how classical Bool/Int sit at
type `!Bool`, `!Int`; the interplay between quantum and classical types in a single
language. This paper is the direct predecessor of the Proto-Quipper family.

---

### 4. Fu, Kishida, Ross, Selinger — *A Categorical Model for a Quantum Circuit Description Language* (2017)

**Venue:** QPL 2017 (Quantum Physics and Logic)  
**arXiv:** https://arxiv.org/abs/1706.02630

Introduces the categorical semantics that underlies Proto-Quipper-M. Defines the
**parameter/state distinction** precisely:

- **Parameter**: a value known at *circuit generation* time — can be freely duplicated
  and discarded. Lives in the unrestricted fragment; given type `!A`.
- **State**: a value known at *circuit execution* time — subject to no-cloning, must be
  used linearly. `Qubit` is the canonical state type.

The model is a pair of categories connected by a functor, capturing the two-phase
structure: a classical outer layer that generates circuits, and a quantum inner layer
that runs them.

**Key concepts for FunQ:** this is where the `!` modality gets its quantum-computing
meaning. A function typed `!(A ⊸ B)` can only manipulate parameters — it cannot
perform quantum operations, because quantum operations are inherently linear and
non-duplicable. This is the formal justification for the discussion in the conversation
about why `!(Int -> Int)` cannot internally allocate/measure qubits.

---

### 5. Fu, Kishida, Ross, Selinger — *Linear Dependent Type Theory for Quantum Programming Languages* (2020)

**Venue:** LICS 2020  
**arXiv:** https://arxiv.org/abs/2004.13472  
**ACM:** https://dl.acm.org/doi/10.1145/3373718.3394765

The core Proto-Quipper-M paper. Combines linear types (from the 2017 model) with
dependent types (types that can depend on values), enabling:

- Circuit families indexed by classical parameters (e.g., an n-qubit QFT)
- Type-safe higher-order functions over qubit lists
- Formal statement and proof of type safety

The type system uses a **graded/fibred** structure: the linear and dependent layers are
kept separate and composed via a fibration. The `!` modality lives in the "parameter"
layer (unrestricted, classical); the quantum state lives in the "state" layer (linear).

**Key concepts for FunQ:** the full type system we are modeling. Read carefully: the
distinction between `A` (linear/state) and `!A` (unrestricted/parameter), the typing
rules for `box`/`unbox`, and how dependent types index quantum data structures.

---

### 6. Fu, Kishida, Ross, Selinger — *A Tutorial Introduction to Quantum Circuit Programming in Dependently Typed Proto-Quipper* (2020)

**Venue:** Reversible Computation 2020  
**arXiv:** https://arxiv.org/abs/2005.08396

A programmer-facing companion to paper 5. Works through concrete examples: building
quantum circuit families, uncomputing ancilla qubits safely, and using dependent types
to enforce structural invariants on qubit lists. Less formal than the LICS paper; better
for building intuition.

**Key concepts for FunQ:** practical examples of how `!` shows up in real programs;
how `box` and `unbox` work; how qubit lists are typed.

---

### 7. Fu, Kishida, Ross, Selinger — *Proto-Quipper with Dynamic Lifting* (2023)

**Venue:** PACMPL (POPL 2023)  
**ACM:** https://dl.acm.org/doi/abs/10.1145/3571204  
**arXiv:** https://arxiv.org/abs/2204.13041

Extends Proto-Quipper-M with *dynamic lifting* — the ability to read a measurement
result at circuit execution time and branch on it (i.e., mid-circuit measurement with
classical feedback). This is exactly what FunQ's `meas` + `if` combination does.

The paper introduces a **modal type system** with two modalities: `!` (classical,
duplicable) and a new modality for dynamic values (known only at runtime). The
operational semantics carefully separates circuit generation from circuit execution.

**Key concepts for FunQ:** this is the most directly relevant paper for FunQ's
measurement semantics and the `CSwitch` on measurement results. The modality
structure here directly addresses how to type `meas` and the if/else branching on its
result.

---

### 8. Fu et al. — *Proto-Quipper with Reversing and Control* (2025)

**arXiv:** https://arxiv.org/abs/2410.22261

Extends Proto-Quipper with circuit reversal (`rev`) and classical control of circuits.
Formalizes `with-computed` (apply a circuit, compute something, then uncompute the
circuit). Relevant if FunQ ever adds adjoint/inverse gate operations.

**Key concepts for FunQ:** future reference for adjoint gates and controlled-circuit
combinators. Not immediately needed.

---

## Summary table

| # | Paper | Year | Key contribution | Priority for FunQ |
|---|---|---|---|---|
| 1 | Girard, *Linear Logic* | 1987 | Foundation: `!`, `⊸`, resource sensitivity | Background |
| 2 | Wadler, *Linear Types can Change the World!* | 1990 | Linear types as a PL tool | Background |
| 3 | Selinger–Valiron, *Quantum Lambda Calculus* | 2006 | Linear types + quantum; `Qubit` as linear | **High** |
| 4 | Fu et al., *Categorical Model* (Proto-Quipper-M) | 2017 | Parameter/state distinction; `!` semantics | **High** |
| 5 | Fu et al., *Linear Dependent Type Theory* | 2020 | Full Proto-Quipper-M type system | **High** |
| 6 | Fu et al., *Tutorial* (Proto-Quipper-D) | 2020 | Practical examples; dependent types | Medium |
| 7 | Fu et al., *Dynamic Lifting* | 2023 | Measurement + classical feedback modality | **High** |
| 8 | Fu et al., *Reversing and Control* | 2025 | Adjoint gates, controlled circuits | Low (future) |
