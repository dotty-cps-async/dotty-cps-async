package cc

import org.junit.Test
import org.junit.Assert.*

/** Reproductions from `tests-cli/` which are run on CI, see [[TestsCli]]. */
class TestTestsCli {

  @Test def directiveValues(): Unit = {
    assertEquals(List("-Wunused:all", "-Werror"), TestsCli.directiveValues("-Wunused:all -Werror"))
    assertEquals(List("-Wunused:imports,privates", "-Werror"), TestsCli.directiveValues("\"-Wunused:imports,privates\" -Werror"))
    assertEquals(List("-Wconf:msg=unused import:s"), TestsCli.directiveValues("\"-Wconf:msg=unused import:s\""))
    assertEquals(List("-Xa", "-Xb"), TestsCli.directiveValues("\"-Xa\", \"-Xb\""))
  }

  // https://github.com/dotty-cps-async/dotty-cps-async/issues/143
  val unusedContextParam = "t2026_11_11_unused_context_param"

  @Test def unusedContextParamMacro(): Unit =
    TestsCli.check(unusedContextParam, usePlugin = false, expectedOutput = Some("List(2, 2, 4, 4)\n"))

  @Test def unusedContextParamPlugin(): Unit =
    TestsCli.check(unusedContextParam, usePlugin = true, expectedOutput = Some("List(2, 2, 4, 4)\n"))

}
