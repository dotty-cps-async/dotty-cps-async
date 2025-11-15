# Changelog

## 1.1.5 (2025-11-15)

### Features

*   Added cross-compilation of compiler plugin to Scala 3.7.4 ([0790808](https://github.com/rssh/dotty-cps-async/commit/0790808259d9bfd62d3800f18647699275bcdc9e))

### Bug Fixes

*   Attempt to fix potential race condition in CpsTryBlockTranformer ([1157072](https://github.com/rssh/dotty-cps-async/commit/115707286a2c5db663c8ffe07b04661d2ea06009))
*   Eliminated false positive match exhausive warning, by transforming sealed trait to enum ([b459be4](https://github.com/rssh/dotty-cps-async/commit/b459be4e63de717486fe1411ec02ec77c0a4742b))
*   Fix for issue #112, where async macro was not processing parameters of case classes during inline expansion.  ([e5416c3](https://github.com/rssh/dotty-cps-async/commit/e5416c3c2cc6cf4f2dfa0731a435d5a9b217247e))

### Other

*   Updated to scala-native 0.5.9 ([4042667](https://github.com/rssh/dotty-cps-async/commit/4042667a881cc8888f3a81a4878e0314f3ce1b72))
*   Updated to scala 3.3.7 ([1e9f565](https://github.com/rssh/dotty-cps-async/commit/1e9f565094ac2999e2bd18098dcc27d7c9674453))
*   Updated organization in logic monad artifact ([faa8823](https://github.com/rssh/dotty-cps-async/commit/faa8823b76f28894d7af325794ad7efac7f98368))
