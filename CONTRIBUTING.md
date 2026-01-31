

Build:

```
sbt compile
```

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
sbt 'set every (Compile / doc) := (Compile / doc / target).value' publishLocal
```

Note: The `set every (Compile / doc) ...` workaround is needed due to a
thread-safety bug in Scala 3.3.7 scaladoc. When sbt runs doc generation for
multiple subprojects in parallel, a `NullPointerException` occurs in
`SignatureBuilder.content()` inside scaladoc's `MemberRenderer`. Each
subproject's doc task succeeds when run in isolation, but the parallel
execution triggers shared mutable state corruption. This workaround replaces
the doc task with a no-op so `publishLocal` can complete.

Publish new release:
```
//ensure that you have no old publish in target/sona-stagign
sbt +publishSigned
sbt sonaRelease
```

