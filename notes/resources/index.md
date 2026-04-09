# Resource Notes

This directory is the note hub for external reading material: Appel's book,
individual paper notes, and the download manifest for the PDFs kept in the
repo-root `resources/` directory.

Current focus:
- Appel chapter notes live in [appel/index.md](appel/index.md)
- Standalone paper notes so far: [kelsey-ssa-cps.md](kelsey-ssa-cps.md), [maurer-compiling-without-continuations.md](maurer-compiling-without-continuations.md), [kennedy-compiling-with-continuations-continued.md](kennedy-compiling-with-continuations-continued.md), [cong-whatever.md](cong-whatever.md), [leijen-lorenzen-trmc.md](leijen-lorenzen-trmc.md), [allain-bour-clement-pottier-scherer-tmc-ocaml.md](allain-bour-clement-pottier-scherer-tmc-ocaml.md), [peyton-jones-specconstr.md](peyton-jones-specconstr.md), [ohori-sasano-fixed-point-promotion.md](ohori-sasano-fixed-point-promotion.md)

Downloaded PDFs live in the repo-root `resources/` directory (git-ignored). The
download manifest is `resources/papers.txt`, read by `scripts/get_papers.sh`.

---

## Reading tracker

| Resource | PDF | Why relevant | Status | Notes |
|---|---|---|---|---|
| **Linear types and Proto-Quipper** (reading list) | see note | Foundation for FunQ's type system | not started | [linear-types-proto-quipper.md](linear-types-proto-quipper.md) |
| Appel, *Compiling with Continuations* | `resources/appel-continuations.pdf` | Foundation for the CPS IR and the current middle end | in progress | [appel/index.md](appel/index.md) |
| Kelsey, *A Correspondence between CPS and SSA* | `resources/kelsey-ssa-cps.pdf` | Most direct guide for treating join continuations as backend blocks | initial notes | [kelsey-ssa-cps.md](kelsey-ssa-cps.md) |
| Maurer et al., *Compiling without Continuations* | `resources/maurer-compiling-without-continuations.pdf` | Join points as explicit, second-class control nodes | initial notes | [maurer-compiling-without-continuations.md](maurer-compiling-without-continuations.md) |
| Kennedy, *Compiling with Continuations, Continued* | `resources/kennedy-compiling-with-continuations-continued.pdf` | Contification and staged CPS backend improvement | initial notes | [kennedy-compiling-with-continuations-continued.md](kennedy-compiling-with-continuations-continued.md) |
| Cong et al., *Compiling with Continuations, or without? Whatever.* | `resources/cong-whatever.pdf` | Design-space survey for an incremental backend refactor | initial notes | [cong-whatever.md](cong-whatever.md) |
| Leijen and Lorenzen, *Tail recursion modulo context* | `resources/leijen-lorenzen-trmc.pdf` | Primary bounded-recursion reference for turning structural recursion into loops | initial notes | [leijen-lorenzen-trmc.md](leijen-lorenzen-trmc.md) |
| Allain et al., *Tail Modulo Cons, OCaml, and Relational Separation Logic* | `resources/allain-bour-clement-pottier-scherer-tmc-ocaml.pdf` | Production-compiler reference for list-building recursion | initial notes | [allain-bour-clement-pottier-scherer-tmc-ocaml.md](allain-bour-clement-pottier-scherer-tmc-ocaml.md) |
| Peyton Jones, *Call-pattern Specialisation for Haskell Programs* | `resources/peyton-jones-specconstr.pdf` | Static boundedness and recursive specialization | initial notes | [peyton-jones-specconstr.md](peyton-jones-specconstr.md) |
| Ohori and Sasano, *Lightweight Fusion by Fixed Point Promotion* | `resources/ohori-sasano-fixed-point-promotion.pdf` | Recursive fusion / fixed-point rewriting reference | initial notes | [ohori-sasano-fixed-point-promotion.md](ohori-sasano-fixed-point-promotion.md) |

---

## Manifest format

```
filename | title | url
```

- One paper per line; blank lines and `#`-comment lines ignored
- Downloaded to `resources/<filename>`
- The script logs the human-readable title while downloading
- The downloader tries `wget` first and falls back to `curl` for sources that reject plain `wget`

---

## PDF manifest

| File | Paper | Why relevant |
|---|---|---|
| `appel-continuations.pdf` | Appel, *Compiling with Continuations* | Reference for the CPS IR and all middle-end passes |
| `kelsey-ssa-cps.pdf` | Kelsey, *A Correspondence between CPS and SSA* | Backend refactor: turning join continuations into basic blocks |
| `maurer-compiling-without-continuations.pdf` | Maurer et al., *Compiling without Continuations* | Making join points explicit and second-class |
| `kennedy-compiling-with-continuations-continued.pdf` | Kennedy, *Compiling with Continuations, Continued* | Contification; staged CPS→join-point migration |
| `cong-whatever.pdf` | Cong et al., *Compiling with Continuations, or without? Whatever.* | Design-space survey; scope control for refactor |
| `leijen-lorenzen-trmc.pdf` | Leijen and Lorenzen, *Tail recursion modulo context: An equational approach (extended version)* | Primary reference for turning non-tail structural recursion into tail-recursive workers via contexts; directly relevant to bounded recursion lowering and CPS-based loopification |
| `allain-bour-clement-pottier-scherer-tmc-ocaml.pdf` | Allain et al., *Tail Modulo Cons, OCaml, and Relational Separation Logic* | Production-compiler account of tail-modulo-constructor transformation; concrete implementation reference for list-building recursion |
| `peyton-jones-specconstr.pdf` | Peyton Jones, *Call-pattern Specialisation for Haskell Programs* | Shape-based specialization of recursive functions; useful for static boundedness, eliminating redundant matches, and inlining-adjacent specialization |
| `ohori-sasano-fixed-point-promotion.pdf` | Ohori and Sasano, *Lightweight Fusion by Fixed Point Promotion* | Recursive inlining/fusion technique for pushing consumers through fixed points without ad hoc rewrites |

See [../future/backend-refactor.md](../future/backend-refactor.md) and
[../future/bounded-recursion.md](../future/bounded-recursion.md) for how these
resources map onto planned compiler work.
