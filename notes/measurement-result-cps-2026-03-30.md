# Measurement As A Single-Result CPS Primitive

Measurement no longer lowers to a two-continuation `CPrimOp`.

The earlier encoding modeled `meas` as a branching primitive:

- `CPrimOp PMeas [q] [] [zeroCont, oneCont]`

That matched one traditional CPS presentation, but it was a poor fit for the
current OpenQASM backend because every measurement directly created two backend
branches. In practice this caused the emitter to duplicate downstream code even
when the source program only wanted a measured `Bool` value and would branch
later through an ordinary `if`.

The current lowering instead treats `meas` like a normal single-result
primitive:

- `CPrimOp PMeas [q] [b] [cont]`

where `b` is the measured classical result.

This has two benefits for the current pipeline:

- measurement stays aligned with the FunQ surface type `Qubit -> Bool`
- backend branching now happens through explicit `CSwitch` on the measured
  classical value rather than being forced at every measurement site

The OpenQASM emitter correspondingly emits measurement as a simple `bit m =
measure q[i];` statement and threads that bit into the continuation
environment.
