# Changelog

## 1.3.3 (2026-05-16)

### Features

*   Add `Breaks` support to the compiler plugin ([97e8e16](https://github.com/rssh/dotty-cps-async/commit/97e8e162))
*   Route `Breaks.break()` through `BreaksAsyncShift` end-to-end ([4532d27](https://github.com/rssh/dotty-cps-async/commit/4532d27d))
*   Match `Breaks` dispatch by qualifier type, not by qualifier symbol ([dd48f80](https://github.com/rssh/dotty-cps-async/commit/dd48f806))

### Bug Fixes

*   Fix `ArrayOpsAsyncShift.dropWhile` returning head instead of empty when all match ([b2fd197](https://github.com/rssh/dotty-cps-async/commit/b2fd1976))
*   Fix `EitherAsyncShift.forall` returning false for `Left` ([5cbdf7c](https://github.com/rssh/dotty-cps-async/commit/5cbdf7c3))
*   Fix `TryAsyncShift.recover`/`recoverWith` `MatchError` on unhandled exceptions ([83a331a](https://github.com/rssh/dotty-cps-async/commit/83a331ab))
*   Fix `UsingAsyncShift.apply` leaking the resource on the failure path ([88e0e33](https://github.com/rssh/dotty-cps-async/commit/88e0e333))
*   Fix `IndexedSeqAsyncShift.indexWhere` ignoring the `from` parameter ([cb47d0b](https://github.com/rssh/dotty-cps-async/commit/cb47d0b1))
*   Fix `BreaksAsyncShift.breakable` discarding the body computation ([b81e1fa](https://github.com/rssh/dotty-cps-async/commit/b81e1fab))
*   Fix `ArrayOpsAsyncShift.lastIndexWhere` not clamping `end` ([df9b25d](https://github.com/rssh/dotty-cps-async/commit/df9b25d0))
*   Catch synchronous throws in JS `ComputationBound.flatMapTry` ([fbe7cc3](https://github.com/rssh/dotty-cps-async/commit/fbe7cc3c))
*   `recoverWith`: use `applyOrElse` to avoid double pattern match ([51cc54d](https://github.com/rssh/dotty-cps-async/commit/51cc54d9))
*   Fix typo in `dropWhile` empty-array assertion message ([95545fb](https://github.com/rssh/dotty-cps-async/commit/95545fbd))

### Build

*   Update sbt to 1.12.11 ([8acc490](https://github.com/rssh/dotty-cps-async/commit/8acc4901))
*   Update sbt-scala-native to 0.5.11 ([c699209](https://github.com/rssh/dotty-cps-async/commit/c6992097))
