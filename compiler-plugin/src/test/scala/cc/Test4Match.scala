package cc

import org.junit.Test
import dotty.tools.dotc.reporting.ErrorMessageID


//TODO: AsyncLambda in match
class Test4Match {



  @Test
  def testCompileExample4m1(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val reporter = dotcInvocations.compileFilesInDir(
      "testdata/set4/m1",
      "testdata/set4/m1-target")

    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

  }



  @Test
  def testCompileExample4m2(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val reporter = dotcInvocations.compileFilesInDir(
      "testdata/set4/m2",
      "testdata/set4/m2-target")

    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

  }



  @Test
  def testCompileAndRunFlatMappedMatch_4m3(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val (code, output) = dotcInvocations.compileAndRunFilesInDirJVM(
      "testdata/set4/m3",
      "testdata/set4/m3-target",
      "cpstest.s4.m3.Test4m3"
    )

    val reporter = dotcInvocations.reporter
    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

    println(s"output=${output}")
    assert(output == "1\n", "The output should be 1")

  }

  @Test
  def testCompileAndRunRuntimeCheckedMatch_4m4(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val (code, output) = dotcInvocations.compileAndRunFilesInDirJVM(
      "testdata/set4/m4",
      "testdata/set4/m4-target",
      "cpstest.s4.m4.Test4m4"
    )

    val reporter = dotcInvocations.reporter
    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

    // `.runtimeChecked` and `: @unchecked` on an async scrutinee should disable the exhaustivity check
    val exhaustivityWarnings = reporter.allWarnings.filter(_.msg.errorId == ErrorMessageID.PatternMatchExhaustivityID)
    assert(exhaustivityWarnings.isEmpty, s"There should be no exhaustivity warnings, we have: ${exhaustivityWarnings.map(_.pos)}")

    println(s"output=${output}")
    assert(output == "2 2 3 4 5 true true 6\n", s"Unexpected output: ${output}")

  }



}
