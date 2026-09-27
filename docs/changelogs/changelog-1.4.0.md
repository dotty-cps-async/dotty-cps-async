# Changelog

## 1.4.0 (2026-09-27)

### Features

*   Add `MacroReflectScope` with `PatternAwareTreeMap` instead of `PatternMaps` wrapper ([d098160](https://github.com/rssh/dotty-cps-async/commit/d098160d))
*   Implement `PatternMaps#PatternTreeMap` instead of `TransformUtil.transformCaseDef/transformPattern` helpers ([a8f1169](https://github.com/rssh/dotty-cps-async/commit/a8f11691))

### Bug Fixes

*   Parse patterns top-down in macro TreeMaps: fix named patterns, `NonFatal` substitution under `Bind` ([dc2d563](https://github.com/rssh/dotty-cps-async/commit/dc2d5638))
*   Keep `@RuntimeChecked`/`@unchecked` on async match selector, fix plugin inlined binding with sync body ([6bf3e07](https://github.com/rssh/dotty-cps-async/commit/6bf3e076))
*   `ComputationBound`: don't restart on wait quant, fix busy wait ([97c5227](https://github.com/rssh/dotty-cps-async/commit/97c52272))

### Build

*   Update Scala to 3.9.0, cross-versions to 3.9.0/3.8.4 ([5d2b3a6](https://github.com/rssh/dotty-cps-async/commit/5d2b3a67))
*   Call `MethodTypeKind` API directly instead of reflection ([698a66a](https://github.com/rssh/dotty-cps-async/commit/698a66a6))
*   Update sbt to 2.0.9 ([f5cf0c5](https://github.com/rssh/dotty-cps-async/commit/f5cf0c5d))
*   Move Sphinx generation to `project/SphinxPlugin.scala` ([f0c1608](https://github.com/rssh/dotty-cps-async/commit/f0c1608a))
*   Run compiler-plugin tests after fixture-only changes ([96a784e](https://github.com/rssh/dotty-cps-async/commit/96a784ee))
