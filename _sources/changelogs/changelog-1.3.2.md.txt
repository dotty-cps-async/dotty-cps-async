# Changelog

## 1.3.2 (2026-04-11)

### Features

*   Add `Option.filterNot` async shift ([d285d00](https://github.com/rssh/dotty-cps-async/commit/d285d009))
    - Enables using `await` inside `Option.filterNot`

### Bug Fixes

*   Fix `flattenObserver` stack overflow on Scala Native ([fdb3740](https://github.com/rssh/dotty-cps-async/commit/fdb37404))

### Build

*   Add Scala 3.8.3 to cross-compilation ([a3c5378](https://github.com/rssh/dotty-cps-async/commit/a3c5378f))
*   Update Scala.js to 1.21.0 ([7fdcaaa](https://github.com/rssh/dotty-cps-async/commit/7fdcaaa7))
*   Update sbt to 1.12.7 ([a8a49b2](https://github.com/rssh/dotty-cps-async/commit/a8a49b22))
