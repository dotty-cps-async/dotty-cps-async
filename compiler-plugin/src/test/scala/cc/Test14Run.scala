package cc

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.duration.*

class Test14Run {

  def compileCommon(): Unit = {
    if (!Test14Run.commonCompiled) {
      DotcInvocations.succesfullyCompileFilesInDir("testdata/set14runtests/common")
      Test14Run.commonCompiled = true
      println("-----finish common compilation-----")
    }
  }

  def compileAndRunTestAfterCommon(dirname: String, testClassName:String): Unit = {
    compileCommon()
    val classpath1 = s"testdata/set14runtests/common-classes:${System.getProperty("java.class.path")}"
    val secondInvokationArgs = DotcInvocationArgs(extraDotcArgs = List("-classpath", classpath1))
    DotcInvocations.succesfullyCompileFilesInDir(dirname, secondInvokationArgs)
    val classpath2 = s"${dirname}-classes:${classpath1}"
    val mainClass = "testUtil.JunitMain"
    val cmd = s"java -cp $classpath2 $mainClass $testClassName"
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
  def testShiftIterableOps(): Unit = {
    val dirname = "testdata/set14runtests/m1"
    val testClassName = "cps.TestBS1ShiftIterableOps"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }


  @Test
  def testShiftUsing(): Unit = {
    val dirname = "testdata/set14runtests/m2"
    val testClassName = "cps.TestBS1ShiftUsing"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }


  @Test
  def testPELazyEffect(): Unit = {
    val dirname = "testdata/set14runtests/m3"
    val testClassName = "cps.pe.TestPELazyEffectM3"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }


  @Test
  def tesReturingExamples(): Unit = {
    val dirname = "testdata/set14runtests/m4"
    val testClassName = "cpstest.TestReturningExamples"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }


  @Test
  def tesCBReturning1(): Unit = {
    val dirname = "testdata/set14runtests/m5_1"
    val testClassName = "cpstest.TestCBSReturning"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }


  @Test
  def tesCBReturning2(): Unit = {
    val dirname = "testdata/set14runtests/m5_2"
    val testClassName = "cpstest.TestCBSReturning"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }



  @Test
  def tesCBS1Select(): Unit = {
    val dirname = "testdata/set14runtests/m6"
    val testClassName = "cps.TestCBS1Select"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }



  @Test
  def testCBS1Try(): Unit = {
    val dirname = "testdata/set14runtests/m7_1"
    val testClassName = "cps.TestСBS1Try"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }



  @Test
  def testFtFoldScan(): Unit = {
    val dirname = "testdata/set14runtests/m8"
    val testClassName = "cps.TestFbFoldScan"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }


  @Test
  def testCBS1CustomShift(): Unit = {
    val dirname = "testdata/set14runtests/m9"
    val testClassName = "cps.TestCBS1CustomShift"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }



  @Test
  def testFizzBuzz(): Unit = {
    val dirname = "testdata/set14runtests/m10"
    val testClassName = "cps.pe.TestFizzBuzz"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }




  @Test
  def testCBS2Dynamic(): Unit = {
    val dirname = "testdata/set14runtests/m11"
    val testClassName = "cps.TestCBS2Dynamic"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }



  @Test
  def testFutureRangeWithFilterShift(): Unit = {
    val dirname = "testdata/set14runtests/m12"
    val testClassName = "cpstest.TestFutureRangeWithFilterShift"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }


}


object Test14Run {

  var commonCompiled = false

}