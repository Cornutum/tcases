# How To Download Tcases Using Maven #

## Tcases Command Line Tools

To get the command line version of Tcases, download the Tcases binary distribution file from the Maven Central Repository, using the following procedure.

  1. Visit the [Central Repository search page](https://search.maven.org/search?q=tcases-shell).
  1. Search for "tcases-shell".
  1. You will see the most recent release of `tcases-shell`. (To see all N previous versions, select `(N)` under "Latest Version".)
  1. Use the :arrow_down: button to select the type of file you want to download. Choose either a ZIP file or a compressed tar file (tar.gz).
  1. See *The Complete Guide* for [tips on installation](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm#install).


## Tcases Libraries

You can download the following Tcases libraries as JARs from the Maven Central Repository. Depending on how you want to use Tcases, list one
of the following dependencies in your project POM.

#### tcases-maven-plugin
A Maven plugin to run Tcases. For details, see the [plugin documentation site](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/).

#### tcases-lib
The core models for Tcases objects. For the current version, see the [release notes](ReleaseNotes.md). Prior to 3.0.0, this JAR also contained all other Tcases APIs.

```xml
<dependency>
  <groupId>org.cornutum.tcases</groupId>
  <artifactId>tcases-lib</artifactId>
  <version>...</version>
</dependency>
```

#### tcases-io
(Since 3.0.0) Supports reading and writing Tcases objects from external documents. For the current version, see the [release notes](ReleaseNotes.md).

```xml
<dependency>
  <groupId>org.cornutum.tcases</groupId>
  <artifactId>tcases-io</artifactId>
  <version>...</version>
</dependency>
```

#### tcases-cli
(Since 3.0.0) Command line tools for running Tcases. For the current version, see the [release notes](ReleaseNotes.md).

```xml
<dependency>
  <groupId>org.cornutum.tcases</groupId>
  <artifactId>tcases-cli</artifactId>
  <version>...</version>
</dependency>
```

#### tcases-openapi
(Since 3.1.0) Converts an OpenAPI v3 specification into test cases for a REST-ful API. For the current version, see the [release notes](ReleaseNotes.md).

```xml
<dependency>
  <groupId>org.cornutum.tcases</groupId>
  <artifactId>tcases-openapi</artifactId>
  <version>...</version>
</dependency>
```

#### tcases-rest-assured
(Since 3.4.0) Provides a [`TestCaseWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/restassured/RestAssuredTestCaseWriter.html)
implementation for [REST Assured](https://github.com/rest-assured/rest-assured).
For the current version, see the [release notes](ReleaseNotes.md).

```xml
<dependency>
  <groupId>org.cornutum.tcases</groupId>
  <artifactId>tcases-rest-assured</artifactId>
  <version>...</version>
</dependency>
```

#### tcases-moco
(Since 3.4.0) Provides a [`TestWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/moco/MocoServerTestWriter.html)
implementation for JUnit API tests using a [Moco](https://github.com/dreamhead/moco) stub server.
For the current version, see the [release notes](ReleaseNotes.md).

```xml
<dependency>
  <groupId>org.cornutum.tcases</groupId>
  <artifactId>tcases-moco</artifactId>
  <version>...</version>
</dependency>
```

#### tcases-ant
(Since 2.0.0) Provides an Ant task for running Tcases. For the current version, see the [release notes](ReleaseNotes.md).

```xml
<dependency>
  <groupId>org.cornutum.tcases</groupId>
  <artifactId>tcases-ant</artifactId>
  <version>...</version>
</dependency>
```
