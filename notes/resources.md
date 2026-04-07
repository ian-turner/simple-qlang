# Reference Papers

Downloaded papers live in `resources/` (git-ignored). The download manifest is
`resources/papers.txt`, read by `scripts/get_papers.sh`.

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

## Papers in the reading list

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

See [future/backend-refactor.md](future/backend-refactor.md) for how each paper
maps onto the planned emitter improvement work.
