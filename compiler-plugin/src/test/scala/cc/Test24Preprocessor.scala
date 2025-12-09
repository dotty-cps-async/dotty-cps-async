package cc

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.duration.*

class Test24Preprocessor {

  def compileCommon(): Unit = {
    if (!Test24Preprocessor.commonCompiled) {
      DotcInvocations.succesfullyCompileFilesInDir("testdata/set24preprocessor/common")
      Test24Preprocessor.commonCompiled = true
      println("-----finish common compilation for set24-----")
    }
  }

  def compileAndRunTestAfterCommon(dirname: String, testClassName: String): Unit = {
    compileCommon()
    val classpath1 = s"testdata/set24preprocessor/common-classes:${System.getProperty("java.class.path")}"
    val secondInvokationArgs = DotcInvocationArgs(extraDotcArgs = List("-classpath", classpath1))
    DotcInvocations.succesfullyCompileFilesInDir(dirname, secondInvokationArgs)
    val classpath2 = s"${dirname}-classes:${classpath1}"
    val mainClass = testClassName
    val cmd = s"java -cp $classpath2 $mainClass"
    println(s"Running $cmd")
    val process = Runtime.getRuntime.exec(cmd)
    val timeout = 1.minute
    blocking {
      val exitCode = process.waitFor(timeout.toSeconds, java.util.concurrent.TimeUnit.SECONDS)
      if (exitCode) {
        val output = scala.io.Source.fromInputStream(process.getInputStream).mkString
        val errorOutput = scala.io.Source.fromInputStream(process.getErrorStream).mkString
        if (!output.endsWith("Ok\n")) {
          println(s"output=${output}")
          println(s"error=${errorOutput}")
          throw new RuntimeException(s"Process $cmd failed")
        } else {
          println(s"Test passed: $testClassName")
        }
      } else {
        val output = scala.io.Source.fromInputStream(process.getInputStream).mkString
        val errorOutput = scala.io.Source.fromInputStream(process.getErrorStream).mkString
        println(s"output=${output}")
        println(s"error=${errorOutput}")
        process.destroy()
        throw new RuntimeException(s"Process $cmd timed out")
      }
    }
  }

  @Test
  def testPreprocessorWithAsyncPlugin(): Unit = {
    val dirname = "testdata/set24preprocessor/m1"
    val testClassName = "cps.preprocessor.TestCpsPreprocessorPlugin"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }

  @Test
  def testPreprocessorWithDirectStyle(): Unit = {
    val dirname = "testdata/set24preprocessor/m2"
    val testClassName = "cps.preprocessor.TestCpsPreprocessorDirect"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }

}

object Test24Preprocessor {
  var commonCompiled = false
}
