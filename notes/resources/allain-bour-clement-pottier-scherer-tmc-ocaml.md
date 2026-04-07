# Allain et al. — *Tail Modulo Cons, OCaml, and Relational Separation Logic*

Allain et al. Status: **initial notes**.

See also: [index.md](index.md), [leijen-lorenzen-trmc.md](leijen-lorenzen-trmc.md), [../future/bounded-recursion.md](../future/bounded-recursion.md), [../passes/recursion.md](../passes/recursion.md)

---

## Why this paper matters after Leijen and Lorenzen

Leijen and Lorenzen give the broad theory: recursive calls under residual
contexts can be exposed and turned into tail-recursive workers. This paper is
the useful next read because it shows what that idea looks like in a production
compiler with real user-facing constraints:
- which recursive positions are worth recognizing
- how to expose them predictably to users
- how to keep the generated code efficient enough to replace hand-written
  accumulator code

For FunQ, this makes the paper less of a new theory paper and more of a
concrete implementation reference for list-building recursion.

---

## Core claim

OCaml implements an explicit, opt-in **tail modulo constructor** (TMC)
transformation for functions whose recursive calls occur under a mix of:
- tail contexts
- constructor contexts

The compiler translates such functions into a pair of variants:
- a direct-style entry function with the original interface
- a destination-passing-style (DPS) worker that writes its result into a caller
  provided destination

The key operational move is:
- allocate a partial constructor with a hole
- pass the hole as the destination to the recursive call
- let the recursive call fill the remaining structure in tail position

This removes stack recursion for list-building code such as `map`, while
preserving the direct-style source program. The paper also proves correctness of
the core transformation and shows that the transformed code can match or beat
complex hand-written tail-recursive implementations once a small amount of
unrolling is added.

---

## What the OCaml implementation actually does

### 1. It gives a precise notion of TMC position

The paper characterizes optimizable calls as those that appear under an
arbitrary composition of:
- tail frames (`let`, `if`, and other ordinary tail contexts)
- constructor frames

This is the practical recognition rule behind the implementation. It is more
general than "the recursive call must appear immediately under one constructor";
nested constructor layers and intervening tail contexts can still qualify.

### 2. It transforms only known calls

The implementation is deliberately first-order:
- it only turns direct calls to known TMC-transformed functions into DPS calls
- it supports mutually recursive and locally known functions
- it does not expose DPS variants across higher-order or module boundaries

That restriction keeps the feature predictable and keeps the transformed calling
convention local to the compiler.

### 3. Ambiguity is surfaced to the user instead of guessed

For terms like a binary-tree node with two recursive children, the compiler may
have multiple incomparable choices about which recursive call to make tail.
OCaml treats this as a user-facing design point:
- ambiguous cases are rejected
- users disambiguate with `[@tailcall]` or `[@tailcall false]`

This is important. The authors do not present TMC as a silent heuristic; they
present it as an expert-facing performance tool whose behavior should be
predictable.

### 4. Constructor compression matters in practice

A naive DPS translation of nested constructors creates too many intermediate
destinations and writes. The implementation therefore keeps a stack of delayed
constructor applications and reifies it only when needed.

This "constructor compression" has two direct consequences:
- generated code is clearer and cheaper
- effectful constructor arguments must be let-bound before compression, to avoid
  changing evaluation order

That second point is especially relevant for FunQ, where measurements and
classical computations cannot be reordered casually.

### 5. The implementation is structured around compositional "choices"

The transformation needs both:
- top-down information about whether a subterm is in a TMC-eligible context
- bottom-up information about whether transforming it is useful

Instead of repeatedly rescanning subterms, the compiler computes a compositional
"choice" summary for each subterm containing:
- the direct-style transform
- the DPS transform
- metadata about whether a useful TMC call was found
- metadata for ambiguity diagnostics

This avoids a quadratic traversal strategy.

---

## Performance and engineering lessons

The paper's benchmark story is narrower than the theory paper, but very useful:
- the simple TMC-transformed `map` performs very well on large lists
- without unrolling it loses to the best hand-written versions on small lists
- a lightly unrolled TMC version matches the best of both worlds and is still
  much simpler than the large hand-optimized library implementations

The adoption story also matters:
- by spring 2024 the OCaml standard library was already using the feature for
  several list-building functions such as `map`, `mapi`, `map2`, `init`,
  `filter_map`, `take`, and `of_seq`
- the annotation had also spread to third-party libraries and a small number of
  non-list datatype builders

The appendix also explains why they did **not** use a generic CPS
transformation instead:
- on OCaml's runtime it has a much worse constant factor
- TMC can only be done safely inside the compiler anyway

That distinction matters for FunQ because our setting is the reverse: CPS is
already our middle-end IR, so the paper is more useful for its recognition and
engineering discipline than for its anti-CPS motivation.

---

## Limits and non-transferable parts

### 1. TMC fixes stack usage, not static output bounds

This paper does not solve the problem that matters most for FunQ: proving a
static bound on aggregate size and qubit allocation. A TMC-transformed worker
may be tail-recursive and still hide an output-size dependency that OpenQASM
cannot represent dynamically.

### 2. The runtime representation does not transfer to OpenQASM

OCaml's implementation relies on:
- partially initialized constructors
- single-assignment mutation of those constructors
- a heap-level destination representation

OpenQASM does not have an analogue of "allocate a cell with a hole and fill it
later", so FunQ cannot import the runtime strategy directly.

### 3. Only some recursive branches can become tail calls

In multi-branch recursive structures, TMC may optimize only one recursive path.
That is ideal for list builders and one-sided nesting, but only a partial win
for balanced-tree-style recursion.

### 4. The implementation is intentionally first-order and annotation-driven

This is a feature for OCaml, not a limitation to "fix" in FunQ. But it does
mean the paper is best read as a practical bounded transformation, not as a
general-purpose recursion eliminator.

---

## FunQ-specific takeaways

### 1. Read this as the production-compiler counterpart to TRMC

Leijen and Lorenzen explain how to expose residual recursive context in general.
Allain et al. show how a compiler can turn a useful subset of that idea into an
implementable, predictable feature.

### 2. The useful import is the recognition story, not destination passing

For FunQ, the interesting transferable idea is:
- identify recursive calls under constructor-like residual work
- make that residual work explicit
- preserve evaluation order while doing so

The non-transferable part is the target representation: FunQ should lower to
explicit loop-carried state and statically bounded aggregates, not heap
destinations with holes.

### 3. Constructor compression suggests a normalization step for list builders

If FunQ recognizes a chain of residual list/tuple construction around a
recursive call, it should probably compress that residual work before lowering
to `CFor`. That should reduce the number of loop-carried temporary aggregates
and make later list-erasure easier.

### 4. Ambiguous recursive positions should probably produce diagnostics

If a recursive function has several incomparable recursive positions, silently
choosing one may make later lowering fragile. The OCaml paper is a good
argument for surfacing that ambiguity explicitly instead of hiding it behind a
heuristic.

### 5. Effect preservation is non-negotiable

The OCaml implementation has to preserve effect order when constructor
compression moves work around. The same issue is sharper in FunQ because
measurement and classical control are observable and constrain the legality of
any recursion normalization pass.

---

## Concrete implementation guidance for FunQ

Near-term lessons for bounded-recursion work:
- use the paper's "tail frames plus constructor frames" view as a recognition
  model for list-building recursion
- preserve evaluation order by let-binding classical and measurement-dependent
  subterms before extracting residual recursive context
- do not implement runtime DPS or mutable destination cells; lower recognized
  patterns into explicit loop state plus statically known bounds
- treat nested constructor work as something to compress before `CFor` lowering
- emit a clear diagnostic when there are several incomparable recursive
  positions and the compiler cannot justify one choice as obviously better

This paper strengthens the current FunQ plan rather than replacing it. It is a
good reference for **how to operationalize TRMC-style recognition in a real
compiler**, but it does not remove the need for:
- static shape inference
- early rejection of dynamic qubit-growing recursion
- explicit `CFor` / `while` loop IR
- list and aggregate erasure before OpenQASM emission
