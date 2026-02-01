# Changelog

## 1.3.0 (2026-01-31)

### Features

*   Add `tailRecM` to `CpsMonad` for stack-safe monadic recursion ([cf4bf14](https://github.com/rssh/dotty-cps-async/commit/cf4bf141))
    - Enables stack-safe recursive computations in all CPS monads
    - Specialized stack-safe implementations for `IterableCpsMonad` ([475f3c9](https://github.com/rssh/dotty-cps-async/commit/475f3c99)), `EitherCpsMonad` ([5d8a90d](https://github.com/rssh/dotty-cps-async/commit/5d8a90dd)), and `LazyListCpsLogicMonad` ([37a845f6](https://github.com/rssh/dotty-cps-async/commit/37a845f6))
*   Add `limit` method to `CpsLogicMonad` for taking first N elements from a logic stream ([2e77cc9](https://github.com/rssh/dotty-cps-async/commit/2e77cc9a))
*   Upgrade `TailRec` monad to `CpsTryMonad` with try/catch/finally support ([850d194](https://github.com/rssh/dotty-cps-async/commit/850d194c))
*   Add `CpsLazyT` monad transformer for trampolining observer operations ([c0f90fb](https://github.com/rssh/dotty-cps-async/commit/c0f90ffb))
*   Add `SuspendableObserverProvider` and trampolined `lazyFsplit` for stack-safe `fsplit` in `LogicStreamT` ([5e1dcd2](https://github.com/rssh/dotty-cps-async/commit/5e1dcd26))
*   Add `withMsplit` to `CpsLogicMonad` to eliminate singleton `flatMap(msplit(...))` universally ([409df28](https://github.com/rssh/dotty-cps-async/commit/409df289))
    - For `LazyList` (Observer=Identity), `withMsplit` becomes a direct call, avoiding deep iterator chain buildup
    - For `LogicStreamT` (Observer=F), it goes through trampolined `fsplit`
    - Fixes `LazyList` `filter` stack overflow and removes the need for per-operation overrides

### Bug Fixes

*   Fix stack overflow in `mFoldLeftWhileObserveM` by rewriting to use `observerCpsMonad.tailRecM` ([c0f90fb](https://github.com/rssh/dotty-cps-async/commit/c0f90ffb))
*   Fix stack overflow in `LogicStream` interleave/fairFlatMap chains via trampolined `lazyFsplit` ([5e1dcd2](https://github.com/rssh/dotty-cps-async/commit/5e1dcd26))
*   Fix stack overflow in `LazyList` `filter`, `interleave`, `fairFlatMap`, `ifte`, `once`, `limit` via `withMsplit` ([409df28](https://github.com/rssh/dotty-cps-async/commit/409df289))
*   Propagate errors in `mObserveOne` instead of silently returning `None` ([6c10d75](https://github.com/rssh/dotty-cps-async/commit/6c10d75e))
*   Fix `mFoldM` type parameter order inconsistency in `LazyListCpsLogicMonad` ([32fd58f](https://github.com/rssh/dotty-cps-async/commit/32fd58f8))
*   Refactor `tailRecM` implementations to use tail recursion instead of deprecated `Either` methods ([c24e09d](https://github.com/rssh/dotty-cps-async/commit/c24e09d3), [6e3a6b9](https://github.com/rssh/dotty-cps-async/commit/6e3a6b99))

### Build

*   Add Scala 3.8.1 support for compiler plugin ([cbb3f17](https://github.com/rssh/dotty-cps-async/commit/cbb3f17f))
*   Scala.js 1.20.2 ([e2bba2e](https://github.com/rssh/dotty-cps-async/commit/e2bba2eb))
*   Scala Native 0.5.10 ([b345d4d](https://github.com/rssh/dotty-cps-async/commit/b345d4d4))
*   sbt 1.12.1 ([34e6b09](https://github.com/rssh/dotty-cps-async/commit/34e6b09d))
*   scalajs-junit-test-runtime 1.20.2

### Internal

*   Remove redundant `mObserveOne` and `mFoldLeftWhileObserveM` overrides from `CpsLogicStreamMonadBase` ([6c10d75](https://github.com/rssh/dotty-cps-async/commit/6c10d75e))
*   Disable debug logging in CompileIssue112 test ([1cfe4d8](https://github.com/rssh/dotty-cps-async/commit/1cfe4d8c))
*   Document Scala 3.3.7 scaladoc thread-safety bug workaround in CONTRIBUTING.md ([32fd58f](https://github.com/rssh/dotty-cps-async/commit/32fd58f8))
