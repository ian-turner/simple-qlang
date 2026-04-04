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

---

## Papers in the reading list

| File | Paper | Why relevant |
|---|---|---|
| `appel-continuations.pdf` | Appel, *Compiling with Continuations* | Reference for the CPS IR and all middle-end passes |
| `kelsey-ssa-cps.pdf` | Kelsey, *A Correspondence between CPS and SSA* | Backend refactor: turning join continuations into basic blocks |
| `maurer-compiling-without-continuations.pdf` | Maurer et al., *Compiling without Continuations* | Making join points explicit and second-class |
| `kennedy-compiling-with-continuations-continued.pdf` | Kennedy, *Compiling with Continuations, Continued* | Contification; staged CPS→join-point migration |
| `cong-whatever.pdf` | Cong et al., *Compiling with Continuations, or without? Whatever.* | Design-space survey; scope control for refactor |

See [future/backend-refactor.md](future/backend-refactor.md) for how each paper
maps onto the planned emitter improvement work.
