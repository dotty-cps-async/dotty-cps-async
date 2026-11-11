package cc

import org.junit.Test

/** Reproductions from `tests-cli/` which are run on CI, see [[TestsCli]]. */
class TestTestsCli {

  // https://github.com/dotty-cps-async/dotty-cps-async/issues/143
  val unusedContextParam = "t2026_11_11_unused_context_param"

  @Test def unusedContextParamMacro(): Unit =
    TestsCli.check(unusedContextParam, usePlugin = false, expectedOutput = Some("List(2, 2, 4, 4)\n"))

  @Test def unusedContextParamPlugin(): Unit =
    TestsCli.check(unusedContextParam, usePlugin = true, expectedOutput = Some("List(2, 2, 4, 4)\n"))

}
