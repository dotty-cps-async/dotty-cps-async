import sbt._
import sbt.Keys._

import com.typesafe.sbt.site.SitePlugin
import com.typesafe.sbt.site.SitePlugin.autoImport.siteMappings

/** Generates the Sphinx documentation and adds it to the site.
  *
  * sbt-site dropped its Sphinx generator, so this drives sphinx-build directly.
  */
object SphinxPlugin extends AutoPlugin {

  // after SitePlugin, which initializes siteMappings
  override def requires = SitePlugin
  override def trigger = noTrigger

  object autoImport {
    val sphinxSourceDirectory = settingKey[File]("Source directory of the Sphinx documentation.")
    val sphinxTarget = settingKey[File]("Output directory of the generated Sphinx HTML.")
    @transient
    val sphinxGenerate = taskKey[File]("Run sphinx-build to generate the HTML documentation.")
  }

  import autoImport._

  override def projectSettings: Seq[Setting[?]] = Seq(
    sphinxSourceDirectory := baseDirectory.value / "docs",
    sphinxTarget := target.value / "sphinx" / "html",
    sphinxGenerate := {
      val src = sphinxSourceDirectory.value
      val out = sphinxTarget.value
      val log = streams.value.log
      val release = version.value
      // conf.py takes `version` as the short x.y one, as the old sbt-site plugin passed it
      val shortVersion = release match {
        case VersionNumber(Seq(x, y, _*), _, _) => s"$x.$y"
        case _                                  => release
      }
      IO.createDirectory(out)
      val cmd = Seq(
        "sphinx-build",
        "-b",
        "html",
        "-D",
        s"version=$shortVersion",
        "-D",
        s"release=$release",
        src.getAbsolutePath,
        out.getAbsolutePath
      )
      log.info(cmd.mkString(" "))
      // sphinx-build writes its warnings to stderr; failure is signalled by the exit code
      val plog = scala.sys.process.ProcessLogger(log.info(_), log.warn(_))
      val rc = scala.sys.process.Process(cmd) ! plog
      if (rc != 0) sys.error(s"sphinx-build failed with exit code $rc")
      out
    },
    siteMappings ++= Def.uncached {
      val conv = fileConverter.value
      val out = sphinxGenerate.value
      Path.allSubpaths(out).toSeq.map { case (f, p) => (conv.toVirtualFile(f.toPath): xsbti.HashedVirtualFileRef, p) }
    }
  )
}
