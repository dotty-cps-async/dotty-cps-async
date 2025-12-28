# Changelog

## 1.2.0 (2025-12-28)

### Features

*   Add `CpsPreprocessor[F[_], C]` typeclass for preprocessing async block bodies before CPS transformation ([e3c9de1](https://github.com/rssh/dotty-cps-async/commit/e3c9de17a2516f63656372b0245a16828d8ff8e7))
    - Enables monad-specific AST transformations (e.g., Durable monad caching, tracing, STM savepoints)
    - Works with both macro path (`async[F] { }`) and direct style path (`def foo(using CpsDirect[F])`)
*   Add context type parameter to CpsPreprocessor for context-specific preprocessing ([fd27ffe](https://github.com/rssh/dotty-cps-async/commit/fd27ffea0c48de813457e6347902c977e2a66cec))
    - Change from `CpsPreprocessor[F]` to `CpsPreprocessor[F, C]` where C is the context type
    - Context is passed explicitly to preprocess method, enabling direct use of context-specific methods
*   Add CpsPreprocessor user documentation with complete tracing example ([3eece10](https://github.com/rssh/dotty-cps-async/commit/3eece1091e130a7aa9cfc519e476f7b90d454318))

### Bug Fixes

*   Fix CpsPreprocessor to expand before CPS transform using two-stage approach ([bbe33bb](https://github.com/rssh/dotty-cps-async/commit/bbe33bba18a8836e740b4c047267c758d3fd93db))
    - Restructured async macro into two stages to ensure preprocessor's transparent inline macro expands before CPS transformation analyzes the body
    - Allows preprocessors to insert await calls that are properly recognized and transformed
