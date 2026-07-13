# Changelog

## 1.3.4 (2026-07-13)

### Build

*   Update Scala to 3.3.8, cross-versions to 3.8.3/3.8.4 ([594c5e4](https://github.com/rssh/dotty-cps-async/commit/594c5e44))
*   Update scala-native to 0.5.12 ([afd39c8](https://github.com/rssh/dotty-cps-async/commit/afd39c87))
*   Bump scala-js to 1.22.0, scalajs-env-nodejs to 1.6.0 ([d63748f](https://github.com/rssh/dotty-cps-async/commit/d63748f8))
*   Add `.sbtopts` with 8g heap: the sbt default of 1g GC-thrashes on the native build ([90797a4](https://github.com/rssh/dotty-cps-async/commit/90797a4c))

### Documentation

*   Drop the scaladoc `publishLocal` workaround: the parallel-doc bug is fixed in 3.3.8 ([aea0f48](https://github.com/rssh/dotty-cps-async/commit/aea0f480))
