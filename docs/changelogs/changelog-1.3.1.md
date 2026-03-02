# Changelog

## 1.3.1 (2026-03-02)

### Features

*   Add `collect`, `fold`, `exists`, `forall`, `find` to `OptionAsyncShift` ([8c121ab](https://github.com/rssh/dotty-cps-async/commit/8c121ab8), [e742b15](https://github.com/rssh/dotty-cps-async/commit/e742b151))
    - Enables using `await` inside `Option.collect`, `Option.fold`, `Option.exists`, `Option.forall`, `Option.find`

### Bug Fixes

*   Workaround for [scala3#17445](https://github.com/scala/scala3/issues/17445): preserve Select name kind in TreeMap ([1346d36](https://github.com/rssh/dotty-cps-async/commit/1346d364))
    - Fixes case class `copy` with named arguments inside `async` blocks when used with `-Xcheck-macros`

### Build

*   Update cross-compilation to Scala 3.3.7 / 3.8.2 (drop 3.3.6, 3.7.4, 3.8.1) ([e742b15](https://github.com/rssh/dotty-cps-async/commit/e742b151))

### Documentation

*   Add "Writing your own monad" chapter ([d40b574](https://github.com/rssh/dotty-cps-async/commit/d40b5744))
*   Move AutomaticColoring into Deprecated Features section ([c841a25](https://github.com/rssh/dotty-cps-async/commit/c841a25a))
