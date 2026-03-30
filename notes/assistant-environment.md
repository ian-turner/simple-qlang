# Assistant Environment Notes

This note records repo-local environment files intended to keep agent guidance
close to the codebase.

## Files

- `AGENTS.md`: shared instructions for coding agents working in this repo
- `CODEX.md`: Codex-specific guidance focused on pipeline boundaries and
  verification expectations
- `CLAUDE.md`: existing Claude-oriented project summary

## Purpose

These files are meant to reduce repeated rediscovery of the same project
constraints:

- the middle end should stay backend-neutral
- measurement consumes qubits
- recursion checking happens before closure conversion
- the first qubit-allocation strategy is one slot per `init`

If these decisions change, update both the environment files and the design
notes they point to.
