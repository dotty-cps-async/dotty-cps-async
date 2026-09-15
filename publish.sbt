credentials += Credentials(Path.userHome / ".sbt" / "central_sonatype_credentials")

ThisBuild / organization := "io.github.dotty-cps-async"
ThisBuild / organizationName := "dotty-cps-async"
ThisBuild / organizationHomepage := Some(uri("https://github.com/dotty-cps-async"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    uri("https://io.github.dotty-cps-async/dotty-cps-async"),
    "scm:git@github.com:dotty-cps-async/dotty-cps-async.git"
  )
)

ThisBuild / developers := List(
  Developer(
    id = "rssh",
    name = "Ruslan Shevchenko",
    email = "ruslan@shevchenko.kiev.ua",
    url = uri("https://github.com/rssh")
  )
)

ThisBuild / description := "async/await macros based on optimised monadic cps transform"
ThisBuild / licenses := List(License.Apache2)
ThisBuild / homepage := Some(uri("https://github.com/rssh/dotty-cps-async"))

ThisBuild / pomIncludeRepository := { _ => false }
//ThisBuild / publishTo := {
//       val nexus = "https://central.sonatype.com/"
//       if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
//       else Some("releases" at nexus + "service/local/staging/deploy/maven2")
//}
ThisBuild / publishMavenStyle := true
