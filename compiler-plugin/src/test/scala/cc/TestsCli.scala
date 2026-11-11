package cc

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

import org.junit.Assert.*

/** Runs a scala-cli reproduction from `tests-cli/` against the current build.
  *
  * The case is compiled in-process with dotc (see [[DotcInvocations]]) on the test classpath, so it uses the freshly compiled
  * dotty-cps-async, not a published version. From `//> using` directives:
  *   - `options` / `scalacOptions` are passed to the compiler;
  *   - `scala` and `jvm` are ignored: the case is compiled by the Scala version of the build;
  *   - `dep` on dotty-cps-async is ignored (the current build is on the classpath), any other dependency makes the case not
  *     runnable here.
  *
  * The case is compiled either via macro, or via compiler plugin (then a `given UseCompilerPlugin` is added to the sources), and
  * then the `@main` method (if any) is run.
  */
object TestsCli {

  val testsCliDir: Path = Paths.get("..", "tests-cli")

  private val Directive = """^\s*//>\s*using\s+(\S+)\s*(.*)$""".r
  private val Package = """^\s*package\s+([\w.]+)\s*$""".r
  private val MainMethod = """@main\s+def\s+(\w+)""".r

  private val optionKeys = Set("options", "option", "scalacOptions", "scalacOption")
  private val depKeys = Set("dep", "deps", "dependency", "dependencies")
  private val ignoredKeys = Set("scala", "jvm")
  private val cpsDeps = Set("io.github.dotty-cps-async::dotty-cps-async", "io.github.dotty-cps-async:dotty-cps-async_3")

  case class Case(name: String, sources: List[Path], options: List[String], mainClass: Option[String], packages: Set[String])

  def readCase(name: String): Case = {
    val dir = testsCliDir.resolve(name)
    assertTrue(s"directory ${dir} does not exist", Files.isDirectory(dir))
    val sources = scalaSources(dir)
    assertFalse(s"no scala sources in ${dir}", sources.isEmpty)
    val lines = sources.flatMap(f => Files.readAllLines(f).asScala)
    val options = lines.flatMap {
      case Directive(key, rest) =>
        val values = rest.split("[\\s,]+").toList.map(_.stripPrefix("\"").stripSuffix("\"")).filter(_.nonEmpty)
        if (optionKeys.contains(key)) values
        else if (depKeys.contains(key)) {
          for (dep <- values) {
            val module = dep.split(":").toList.filter(_.nonEmpty) match
              case org :: artifact :: _ if dep.contains("::") => s"${org}::${artifact}"
              case org :: artifact :: _                       => s"${org}:${artifact}"
              case _                                          => dep
            if (!cpsDeps.contains(module))
              failCase(s"${name}: dependency ${dep} is not on the test classpath, this case can't be run from TestsCli")
          }
          Nil
        } else if (ignoredKeys.contains(key)) Nil
        else failCase(s"${name}: unsupported directive '${key}'")
      case _ => Nil
    }
    val mainClasses = lines.flatMap(l => MainMethod.findAllMatchIn(l).map(_.group(1))).distinct
    assertTrue(s"${name}: more than one @main method: ${mainClasses}", mainClasses.size <= 1)
    val packages = lines.collect { case Package(p) => p }.toSet
    Case(name, sources, options, mainClasses.headOption, packages)
  }

  /** Compile the case and run its `@main` method.
    * @param expectedOutput
    *   if defined, the output of the run should be equal to it.
    */
  def check(name: String, usePlugin: Boolean, expectedOutput: Option[String] = None): Unit = {
    val c = readCase(name)
    val mode = if (usePlugin) "plugin" else "macro"
    val outDir = Paths.get("target", "tests-cli", s"${name}-${mode}")
    val enablers = if (usePlugin) writePluginEnablers(c, Paths.get("target", "tests-cli", s"${name}-enabler")) else Nil
    // only the options of the case (not DotcInvocations.defaultCompileOpts, -Ydebug enables debug warnings)
    val args = List("-d", outDir.toString, "-classpath", DotcInvocations.testClassPath) ++
      (if (usePlugin) List("-Xplugin:src/main/resources") else Nil) ++
      c.options ++
      List("-color:never", "-Ycheck:all")
    val dotc = new DotcInvocations(silent = true)
    val reporter = dotc.compileFilesWithFullArgs((c.sources ++ enablers).map(_.toString), outDir.toString, args)
    for ((kind, d) <- reporter.allWarnings.map(("warning", _)) ++ reporter.allErrors.map(("error", _))) {
      val pos = if (d.pos.exists) s"${d.pos.source}:${d.pos.line + 1}:${d.pos.column + 1}" else "<no position>"
      println(s"${name} (${mode}): ${kind} at ${pos}: ${d.msg}")
    }
    assertFalse(s"${name} (${mode}): compilation failed: ${reporter.summary}", reporter.hasErrors)
    for (mainClass <- c.mainClass) {
      val (code, output) = DotcInvocations.runJVMInClasspath(mainClass, s"${outDir}:${DotcInvocations.testClassPath}")
      assertEquals(s"${name} (${mode}): exit code of ${mainClass}", 0, code)
      for (expected <- expectedOutput)
        assertEquals(s"${name} (${mode}): output of ${mainClass}", expected, output)
    }
  }

  private def failCase(message: String): Nothing = throw new AssertionError(message)

  private def scalaSources(dir: Path): List[Path] = {
    val stream = Files.walk(dir)
    try
      stream
        .iterator()
        .asScala
        .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".scala"))
        .filterNot(p => dir.relativize(p).iterator().asScala.exists(part => part.toString.startsWith(".") || part.toString == "target"))
        .toList
        .sorted
    finally stream.close()
  }

  /** `async` uses the compiler plugin when `UseCompilerPlugin` is in the implicit scope: add it to each package of the case. */
  private def writePluginEnablers(c: Case, dir: Path): List[Path] = {
    Files.createDirectories(dir)
    val packages = if (c.packages.isEmpty) List("") else c.packages.toList.sorted
    packages.zipWithIndex.map { (pkg, i) =>
      val file = dir.resolve(s"CompilerPluginEnabler${i}.scala")
      val packageClause = if (pkg.isEmpty) "" else s"package ${pkg}\n\n"
      Files.writeString(
        file,
        s"${packageClause}given cps.macros.flags.UseCompilerPlugin.type = cps.macros.flags.UseCompilerPlugin\n"
      )
      file
    }
  }

}
