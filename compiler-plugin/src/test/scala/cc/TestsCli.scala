package cc

import java.io.File
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
  * The case is compiled either via macro, or via compiler plugin (then a `given UseCompilerPlugin` is added to the packages of the
  * sources, if the case does not define it itself), and then the `@main` method (if any) is run.
  */
object TestsCli {

  val testsCliDir: Path = Paths.get("..", "tests-cli")

  private val Directive = """^\s*//>\s*using\s+(\S+)\s*(.*)$""".r
  // `package p`, `package p:`, `package p {`, `package p // comment`, but not `package object`
  private val Package = """^\s*package\s+(?!object\b)([\w.]+)""".r.unanchored
  // also `@main()` and `@main` on its own line
  private val MainMethod = """@main\b(?:\s*\(\s*\))?\s+def\s+(\w+)""".r

  private val optionKeys = Set("options", "option", "scalacOptions", "scalacOption")
  private val depKeys = Set("dep", "deps", "dependency", "dependencies")
  private val ignoredKeys = Set("scala", "jvm")
  private val cpsDeps = Set("io.github.dotty-cps-async::dotty-cps-async", "io.github.dotty-cps-async:dotty-cps-async_3")

  /** @param packages
    *   package of each source ("" for the empty package)
    * @param definesPluginEnabler
    *   the sources already mention `UseCompilerPlugin`
    */
  case class Case(
      name: String,
      sources: List[Path],
      options: List[String],
      mainClass: Option[String],
      packages: Set[String],
      definesPluginEnabler: Boolean
  )

  def readCase(name: String): Case = {
    val dir = testsCliDir.resolve(name)
    assertTrue(s"directory ${dir} does not exist", Files.isDirectory(dir))
    val sources = new DotcInvocations()
      .scalaFilesIn(dotty.tools.io.Path(dir.toString), skipDir = d => d.startsWith(".") || d == "target")
      .sorted
      .map(Paths.get(_))
    assertFalse(s"no scala sources in ${dir}", sources.isEmpty)
    val texts = sources.map(f => f -> Files.readString(f)).toMap
    val options = sources.flatMap(f => texts(f).linesIterator).flatMap {
      case Directive(key, rest) =>
        val values = directiveValues(rest)
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
    // chained package clauses (`package a` `package b`) give `a.b`
    val filePackages = sources.map(f => f -> texts(f).linesIterator.collect { case Package(p) => p }.mkString(".")).toMap
    val mainClasses = sources.flatMap { f =>
      MainMethod.findAllMatchIn(texts(f)).map { m =>
        if (filePackages(f).isEmpty) m.group(1) else s"${filePackages(f)}.${m.group(1)}"
      }
    }.distinct
    assertTrue(s"${name}: more than one @main method: ${mainClasses}", mainClasses.size <= 1)
    Case(
      name,
      sources,
      options,
      mainClasses.headOption,
      filePackages.values.toSet,
      texts.values.exists(_.contains("UseCompilerPlugin"))
    )
  }

  /** Values of a directive: quoted strings are kept as they are (with spaces and commas inside), unquoted values are separated by
    * whitespace; a comma between values is a separator.
    */
  def directiveValues(rest: String): List[String] = {
    val values = List.newBuilder[String]
    var i = 0
    while (i < rest.length) {
      val c = rest.charAt(i)
      if (c.isWhitespace || c == ',') i += 1
      else if (c == '"') {
        val end = rest.indexOf('"', i + 1)
        if (end < 0) failCase(s"unclosed quote in directive value: ${rest}")
        values += rest.substring(i + 1, end)
        i = end + 1
      } else {
        var end = i
        while (end < rest.length && !rest.charAt(end).isWhitespace) end += 1
        values += rest.substring(i, end).stripSuffix(",")
        i = end
      }
    }
    values.result().filter(_.nonEmpty)
  }

  /** Compile the case and run its `@main` method.
    * @param expectedOutput
    *   if defined, the case should have a `@main` method, and the output of its run should be equal to it.
    */
  def check(name: String, usePlugin: Boolean, expectedOutput: Option[String] = None): Unit = {
    val c = readCase(name)
    if (expectedOutput.isDefined)
      assertTrue(s"${name}: expectedOutput is set, but no @main method is found", c.mainClass.isDefined)
    val mode = if (usePlugin) "plugin" else "macro"
    val scalaVersion = dotty.tools.dotc.config.Properties.versionNumberString
    val baseDir = Paths.get("target", "tests-cli", scalaVersion)
    val outDir = cleanDir(baseDir.resolve(s"${name}-${mode}"))
    val enablers =
      if (usePlugin && !c.definesPluginEnabler) writePluginEnablers(c, cleanDir(baseDir.resolve(s"${name}-enabler")))
      else Nil
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
      val classpath = s"${outDir}${File.pathSeparator}${DotcInvocations.testClassPath}"
      val (code, output) = DotcInvocations.runJVMInClasspath(mainClass, classpath)
      assertEquals(s"${name} (${mode}): exit code of ${mainClass}", 0, code)
      for (expected <- expectedOutput)
        assertEquals(s"${name} (${mode}): output of ${mainClass}", expected, output)
    }
  }

  private def failCase(message: String): Nothing = throw new AssertionError(message)

  /** Remove the directory with its content (classes from previous runs), and create it again. */
  private def cleanDir(dir: Path): Path = {
    if (Files.exists(dir)) {
      val stream = Files.walk(dir)
      try stream.iterator().asScala.toList.reverse.foreach(Files.delete)
      finally stream.close()
    }
    Files.createDirectories(dir)
  }

  /** `async` uses the compiler plugin when `UseCompilerPlugin` is in the implicit scope: add it to each package of the case. */
  private def writePluginEnablers(c: Case, dir: Path): List[Path] = {
    c.packages.toList.sorted.zipWithIndex.map { (pkg, i) =>
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
