package cc

import org.junit.Test

class Test25DependentLambda {

  @Test
  def testCompileAndRunDependentLambda_m1(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val (code, output) = dotcInvocations.compileAndRunFilesInDirJVM(
      "testdata/set25dependentLambda/m1",
      "testdata/set25dependentLambda/m1-classes",
      "cpstest.Test25m1"
    )

    val reporter = dotcInvocations.reporter
    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

    println(s"output=${output}")
    assert(output == "List(3, 21) List(3, 21) List(1, 10) List(2, 11)\n", s"unexpected output: ${output}")
  }

}
