## Attributes

- id: WI-20260924-RR5QR-regression-tests-for-scala-3-4
- created: 2026-09-24T09:03:18Z

- status: Open
- status_agent: user
- status_at: 2026-09-24T09:03:18Z

- acceptance: sbt-test

- tags: scala-3.9

## Description

Regression tests for Scala 3.4-3.9 syntax inside async: better-fors, named tuples, new context-bound/given syntax

Probed on 2026-09-24 against 3.9.0: all of these work inside async[ComputationBound], but no test covers them (tests were written for 3.3 LTS). All are stable in 3.8.4 too, so tests can go to shared/src/test without version-specific dirs.
- better-fors (default since 3.8): alias as the first enumerator (`a = await(x)`), alias followed by a guard with await, await in yield after alias+guard, trailing identity map elimination, alias in the middle using an awaited value.
- named tuples (3.7): build with await `(name = "a", age = await(..))`, `.field` selection on an awaited named tuple, pattern `case (name = n, age = a)`.
- context bounds / givens (3.6): local `def f[T: Ordering as ord]`, aggregate bounds `[T: {Ordering, Numeric}]`, local `given base: Int = await(..)` used by a using-parameter.
Known gap, not 3.9-specific: polymorphic lambdas (`[T] => (a: T) => a`) inside async are rejected with "language construction is not supported: Closure(...)".

