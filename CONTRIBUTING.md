

Build:

```
sbt compile
```

Note: the build needs a large JVM heap (the Scala Native toolchain runs inside
the sbt JVM). `.sbtopts` sets `-Xmx8g`; with sbt's 1G default, `sbt test` ends
up spending all its time in GC and looks like it hangs.

Regenerate docs:

```
sbt makeSite
sbt previewSite
```
(Sphinx should be installed)


Publish new docs on github:
```
sbt ghpagesPushSite
```

Publish locally (for testing with dependent projects):
```
sbt publishLocal
```

Note: up to Scala 3.3.7, this needed a workaround
(`sbt 'set every (Compile / doc) := (Compile / doc / target).value' publishLocal`)
because of a thread-safety bug in scaladoc: when sbt generated docs for several
subprojects in parallel, a `NullPointerException` was thrown in
`SignatureBuilder.content()` inside `MemberRenderer`. This no longer reproduces
on 3.3.8, so plain `publishLocal` works.

Publish new release:
```
//ensure that you have no old publish in target/sona-stagign
sbt +publishSigned
sbt sonaRelease
```

