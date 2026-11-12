# tests-cli

Standalone reproductions, one directory per case, runnable with scala-cli: `scala-cli run tests-cli/<case>`.

## Running on CI

Selected cases are also run by `cc.TestTestsCli` (in `compilerPlugin`, `sbt compilerPlugin/testOnly cc.TestTestsCli`)
against the current build, see `compiler-plugin/src/test/scala/cc/TestsCli.scala`.
Each case is compiled in-process by dotc twice: via macro and via compiler plugin, then its `@main` method is run.

From the `//> using` directives:

- `options` / `scalacOptions` are passed to the compiler (e.g. `-Wunused:all -Werror`);
- `scala` and `jvm` are ignored: the case is compiled with the Scala version of the build (also with `++3.8.4`);
- `dep` on `io.github.dotty-cps-async::dotty-cps-async` is ignored, the current build is on the classpath;
- any other dependency or directive fails the test: such a case can't be run there.

To add a case: create the directory, then add a test method to `cc.TestTestsCli`.
Reproductions of compiler bugs or of integrations with other libraries stay scala-cli only.
