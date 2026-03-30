# CLI output now emits only OpenQASM

The `funq` executable now prints only the generated OpenQASM program on
success.

This keeps the command-line behavior aligned with the current role of the
executable as a backend driver rather than an IR debugging tool.

Implications:

- Compiler diagnostics still surface as fatal errors.
- Intermediate IR dumps are no longer part of the default CLI output.
- Future debugging output should probably live behind an explicit flag rather
  than the default execution path.
