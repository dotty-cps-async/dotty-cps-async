

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

Publish new release:
```
//ensure that you have no old publish in target/sona-stagign
sbt +publishSigned
sbt sonaRelease
```

