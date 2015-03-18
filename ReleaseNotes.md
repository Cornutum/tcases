# Release Notes #

## 1.3.1 ##

  * To better support embedding in other apps, add stream-based methods for generating ([Tcases.getTests](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/Tcases.html)) and exporting ([Tcases.writeTests](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/Tcases.html)) test case definitions.

## 1.3.0 ##

  * The **once** attribute applies only to a default 1-tuple for a single variable. But now there is now a more general way to define once-only exceptions to higher-order combinations, by adding [Once elements](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm#once-tuples) to your generator definition.

  * Tcases Maven Plugin: Use parameter=project to more easily select a single project.

## 1.2.2 ##

  * Tcases Guide: Rework to avoid inadvertent substitution of Maven properties.

## 1.2.1 ##

  * Reducer: Improvements to logging make it easier to track progress.

## 1.2.0 ##

  * Introducing the [Tcases Reducer](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm#reduce), which helps automate the search for a smaller set of test cases.

## 1.1.1 ##

  * For XML input files, Tcases now validates the attributes specified for all elements. Tcases reports an error if any attribute is unknown or invalid.

## 1.1.0 ##

  * Restructured as Maven project containing multiple modules, all downloadable from the Maven Central repository. See [HowToDownload](HowToDownload.md).

  * Now available as a [Maven plugin](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/), so you can use Maven to generate test cases directly from the system input definitions for your Maven project.

## 1.0.1 ##

  * If generating JUnit (using the -J option), use a more useful default output file name of the form **${projectName}Test.java**. If necessary, convert the **${projectName}** part of the file name into a valid Java identifier. Consequently, -J does not have to be accompanied by -f to get good results.

  * When reading a system input definition document, report a failure for any reference to an undefined property. Write a WARN log message for any property definition that is not actually used.
