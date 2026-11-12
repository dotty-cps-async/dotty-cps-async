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

  @Test def harnessFormats(): Unit = {
    val c = TestsCli.readCase("t2026_11_12_harness_formats")
    assertEquals(List("-Wunused:imports,privates", "-Werror"), c.options)
    assertEquals(Some("example.probe"), c.mainClass)
    TestsCli.check("t2026_11_12_harness_formats", usePlugin = false, expectedOutput = Some("ok\n"))
  }

  // https://github.com/dotty-cps-async/dotty-cps-async/issues/143
  val unusedContextParam = "t2026_11_11_unused_context_param"

  @Test def unusedContextParamMacro(): Unit =
    TestsCli.check(unusedContextParam, usePlugin = false, expectedOutput = Some("List(2, 2, 4, 4)\n"))

  @Test def unusedContextParamPlugin(): Unit =
    TestsCli.check(unusedContextParam, usePlugin = true, expectedOutput = Some("List(2, 2, 4, 4)\n"))

  // https://github.com/dotty-cps-async/dotty-cps-async/issues/148
  val unusedPatvars = "t2026_11_11_unused_patvars"

  @Test def unusedPatvarsMacro(): Unit =
    TestsCli.check(unusedPatvars, usePlugin = false, expectedOutput = Some("List(3, 3, 4, 2, -1, 14)\n"))

  @Test def unusedPatvarsPlugin(): Unit =
    TestsCli.check(unusedPatvars, usePlugin = true, expectedOutput = Some("List(3, 3, 4, 2, -1, 14)\n"))

  // https://github.com/dotty-cps-async/dotty-cps-async/issues/153
  val asyncPfRebind = "t2026_11_12_async_pf_rebind"

  @Test def asyncPfRebindMacro(): Unit =
    TestsCli.check(asyncPfRebind, usePlugin = false, expectedOutput = Some("List(6, 4, 3, 2, 21)\n"))

  @Test def asyncPfRebindPlugin(): Unit =
    TestsCli.check(asyncPfRebind, usePlugin = true, expectedOutput = Some("List(6, 4, 3, 2, 21)\n"))

}
