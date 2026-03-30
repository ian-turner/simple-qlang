# Float Constants

This note records the first pass of symbolic float-constant support in FunQ.

## Representation

- Surface syntax now stores float literals as `String` rather than Haskell
  `Float`.
- The parser treats both decimal literals such as `1.5` and the built-in
  constant `pi` as the same symbolic float-literal form.
- The middle end preserves those strings through `ConcreteSyntax`, `Syntax`,
  `LambdaIR`, and CPS.

## Why

OpenQASM has symbolic constants such as `pi`, so evaluating float literals into
 host-language numeric values too early loses information we want to preserve in
 the emitted program text.

Keeping float constants symbolic also keeps the middle end backend-neutral. A
future QIR backend can lower symbolic constants differently without changing the
 front half of the compiler.

## Current Backend Behavior

- `src/OpenQASM.hs` now carries symbolic float constants as classical values and
  renders them directly into emitted QASM expressions.
- OpenQASM output declarations are now type-aware, so homogeneous float outputs
  can emit as `array[float[64], n] output;` and mixed outputs still fall back
  to per-leaf declarations instead of assuming every output is a `bit`.
- Integer-only paths such as constructor tags and `switch` scrutinees still
  require integer classical values.

## Follow-Up

- Introduce a backend-specific constant-lowering step before final emission so
  OpenQASM and future QIR backends can interpret symbolic constants according to
  their own rules.
- Add a real type checker so float and integer arithmetic are validated before
  backend emission rather than inferred opportunistically in the emitter.
