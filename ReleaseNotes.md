# Release Notes #

## 3.1.0 ##

This release add two new capabilities to Tcases.

  * **[Cardinality conditions](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm#cardinalityConditions)**:
  This new family of conditions allows you to define when a value or a variable can be used based on
  how many times a certain property appears in a test case.

  * **[Tcases for OpenAPI](tcases-openapi/README.md)**: Use Tcases to automatically generate test cases for your REST-ful API,
  based on an OpenAPI v3 specification.

Also included in this release:

  * **Better backtracking reduces failures**: For some complex input models, Tcases must conduct a lengthy, trial-and-error search to find a combination
  of values that satisfies all constraints. That involves backtracking from preliminary value choices to try different choices. This
  release fixes a backtracking problem that could cause Tcases to give up the search prematurely and fail to complete a test case.

## 3.0.2 ##

This is a patch release to fix the following problems using JSON files.

  * System test definitions: Each function definition can have output annotations. Therefore, in the JSON format, a function should be
  specified by a JSON object with an optional "has" property and a "testCases" property containing the array of test case definitions.
  This is a change to the schema, which previously allowed a function to be defined by the test case array only. But for backward compatibility,
  documents conforming to the previous schema continue to be accepted.

  * Shell commands now apply the `-T `option (content type) correctly when the file type is undefined (for example, when reading a JSON system input definition from
  standard input).
  

## 3.0.1 ##

This is a patch release to fix an annoying problem that caused the shell commands `tcases` and `tcases-reducer` to reject the new `-T` option. For
example, when you run the command `tcases -T json`, Tcases should read a JSON system input definition from standard input and produce a JSON system
test definition on standard output. And now it does.

## 3.0.0 ##

This release introduces a major new feature: you can now use JSON for all of the documents that Tcases reads and produces.  At
the command line level (including Tcases Maven Plugin configuration parameters), not much has changed -- just start using
`*.json` files instead of `*.xml` files.  [Tcases: The JSON Guide](http://www.cornutum.org/tcases/docs/Tcases-Json.htm) explains
the new JSON file formats and other necessary details.

Why JSON? Many people just find this format easier to read and write. Also, the JSON format is especially useful if you are
accessing Tcases using a Web service. In fact, using JSON, there is a way to define all Tcases inputs in [a single
request](http://www.cornutum.org/tcases/docs/Tcases-Json.htm#web).

Another major change is that Tcase APIs have been reorganized into multiple JARs. The improvement in modularity allows API users to
streamline dependencies to only those components actually needed. But these changes are not fully compatible with previous releases,
so API users may need to make changes to their projects, as described below.

  * Programs that use CLI classes (`TcasesCommand`, etc.) must change dependencies to use `org.cornutum.tcases:tcases-cli`.
  
  * Programs that directly read or write Tcases documents (using classes like `SystemlnputDocReader`, `SystemTestDocWriter`, etc.)
must change dependencies to use `org.cornutum.tcases:tcases-io`.
  
  *  The main utility classes `TcasesCommand` and `Tcases` have been refactored, relocating methods that use stream IO directly to the new class `TcasesIO` in the `tcases-io` module. Also, the superfluous inheritance relationships among these classes have been removed.


## 2.1.2 ##

Minor improvements, including some documentation touch-ups and some refactoring using functional expressions to simplify and clarify the code.

The most extensive change: reworking all tests using Hamcrest matchers to clarify all assertions. This became doable after the recent release
of [`hamcrest-composites`](https://github.com/Cornutum/hamcrest-composites), which provides new matchers for comparing complex Java objects with better testability.

## 2.1.1 ##

This release is a quick patch for [Tcases 2.1.0](#210) to ensure that the `tcases-reducer` command correctly uses the new `ReducerCommand` class.

## 2.1.0 ##

Following release 2.0.0, this release adds further improvements to the Tcases API. If your application uses the Tcases API, some minor changes will be required.
But if you use Tcases only from the command line or with Maven, you won't see any differences.

  * Thanks to [Thibault Kruse](https://github.com/tkruse) for his suggestions and feedback on all of the key features of this release.

  * **The "name" of a variable value can be any Java object, including `null`**
    * The Java object that represents a variable value is no longer limited to a `String`. Accessor data types have changed from `String` to `Object`.
      Null values are now allowed.
    * This affects the public interface to `VarDef`, `VarValueDef`, and `VarBinding`.
    * When reading the XML representation of a system input definition (`*-Input.xml`), the `name` attribute of a `Value` element is automatically converted
      to an appropriate Java object.
      * A numeric value is converted to either an `Integer`, a `Long`, or a `BigDecimal` object.
      * The strings `"true"` and `"false"` (in any combination of upper- and lower-case) are converted to `Boolean` objects.
      * The string `"null"` is converted to a null `Object`.
      * All other values are converted to `String` objects.
    * When reading the XML representation of a system test definition (`*-Test.xml`), the same conversions are applied to the `value` attribute of a `Var` element.

  * **Reducer API**
    * The `Reducer` class now provides `reduce` methods that operate directly at the API level.
    * Command line support for the Reducer has been relocated to a new `ReducerCommand` class.
    * The command line option for defining a "generator factory" -- the `-G` option and corresponding Maven property `genFactory` -- is now obsolete and has been removed.

  * **Saving system input definitions**
    * The new `SystemInputDocWriter` class now makes it possible to save a system input definition as an XML document compatible with
      `SystemInputDocReader`.

  * **Builders! Matchers!**
    * For each of the major entities in the Tcases API there is now a `*Builder` class that provides a "fluent" API for simpler construction of a complex object.
    * Let's show some love for unit tests, too! For most Tcase entities, there is now a `*Matcher` class that enables more powerful test
      assertions. By using `Asserts.assertMatches()` with a `Matcher` instead of `assertEquals`, you get more than a yes/no answer. Instead, you
      see exactly which field in a complex object contains the discrepancy. By using `Asserts.assertSetEquals()` with a `Matcher`, you see exactly
      which elements of a collection of complex objects are wrong and why.
    * With detailed comparison now handled by the `Matcher` classes, the corresponding `equals`
      methods have changed to provide a narrower "primary key" definition of equality.
    
## 2.0.0 ##

  * This is a major release designed to open Tcases up to a much broader community of applications. The features included will be
    especially helpful for applications that use the Tcases API to embed Tcases capabilities into larger programs.
    Some changes are not compatible with previous releases -- details are listed in the **Compatibility** notes below.

  * :trophy: Major props go to [Thibault Kruse](https://github.com/tkruse), who proposed and contributed to all of the key features of this release.

  * **Reduced dependencies for `tcases-lib`**
    * Relocated Ant support to a new `tcases-ant` module
    * Relocated command line support from the `Tcases` class to a new `TcasesCommand` class
    * Removed direct dependency on `logback-classic` to allow different bindings for the `slf4j` API

  * **Expanded characters allowed in names and values**
    * Names for input model elements like `System`, `Function`, and `Var` can now contain any Unicode alphabetic character.
      (See [DefUtils.isIdentifier()](tcases-lib/src/main/java/org/cornutum/tcases/DefUtils.java#L22).)
    * Names for variable `Value` elements can now contain nearly any character -- only XML markup characters disallowed.
      Even empty strings are allowed.
      (See [DefUtils.isVarValue()](tcases-lib/src/main/java/org/cornutum/tcases/DefUtils.java#L56).)

  * **Property annotations**: The `Value` properties that characterize a test case can be useful metadata for further transformations of test case definitions.
    So Tcases now automatically attaches an output annotation named `properties` to each `TestCase`.

  * **"Not applicable" variables**: Tcases may designate a variable as ["not applicable"](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm#varConditions)
    to a test case. But the way this was done was a bit of a hack. Previously, such variables would just be assigned a special value of `"NA"`. But
    what if `"NA"` actually means something different in your input domain ("North America", maybe)? You could have used it as a `Value` in your system input
    definition, and Tcases wouldn't complain. But when you looked at the resulting test cases generated by Tcases, you couldn't tell which `"NA"`
    meant "not applicable" and which meant "North America". Not cool! But this release fixes that problem. Now, you can use `"NA"` as a value however
    you like, and Tcases will use a different XML markup to show when a variable is "not applicable". This change will affect existing XML test case documents --
    see the **Compatability** notes for details. This also changes how "not applicable" bindings are represented in the API.

  * **Compatibility**: The following changes are not backward compatible with previous releases.
    * Tcases API
      * The Tcases command line interface is now implemented by the `TcasesCommand` class. But programs using the Tcases API directly can continue
        to use the basic `Tcases` class.
      * Programs using `TcaseTask` API for Ant support must add a dependency on the `tcases-ant` module.
      * Programs that use the Tcases API must add a runtime dependency for some binding of the `slf4j` API. For equivalence with previous releases,
        add a dependency for `logback-classic`.
    * System test documents
      * To designate "not applicable" variables, replace all instances of `<Var name="..." value="NA"/>` with `<Var name="..." NA="true"/>`.
        Otherwise, for system test documents used to define base test case inputs, test cases containing `"NA"` values may not be
        preserved.
    * XSLT transforms
      * See previous note. System test documents now use a different XML markup to designate "not applicable" variable. Any transform
        that gives special treatment to `value="NA"` attributes should change to recognize the `NA="true"` attribute instead.

## 1.5.4 ##

  * Fixes the ```tcases``` and ```tcases-reducer``` commands to correctly support the ```-R``` option.

  * Refactor XML document readers to use ```org.xml.sax.Attributes.getQName()```. This is more correct than ```getLocalName()```,
    which can return an empty string in some JVM implementations, resulting in a parser failure.

## 1.5.3 ##

  * Fixes a defect that caused the ```tcases``` command to fail when reading a system input definition from standard input.

  * Reducer: When ```newSeed``` is true, ensure that a new seed is written even when the initial round is already minimal.

  * Suppose a variable only has N values, but the number of test cases generated is more than N. Assuming there are no constraints on these N values, you'd like
    for each of them to be used by roughly the same number of test cases. 
    This release fixes a defect that was blocking that even distribution of values.
    Why does this matter? Because this increases test case variety, which could make it more likely that some test case will expose a defect.

## 1.5.2 ##

Ever wonder if a different random seed might lead to more interesting test cases? Now it's easier to find out: try the ```-R``` option at the ```tcases``` command line. Or, if you're using the Tcases Maven Plugin, the ```newSeed``` parameter does the same thing. This is like the ```-r``` option (or the ```seed``` parameter), except that Tcases picks the seed value for you.

Similarly, when using the Tcases Reducer, you might wonder if a different seed might produce an equally small but more interesting set of test cases.
So you can also use ```-R``` (or ```newSeed```) with the Tcases Reducer, which tells it to ignore any previous random seed
in the current generator definition and to search for a new minimizing seed value.

## 1.5.1 ##

  * HTML test case reports: Tcases output is often used to guide manual testing. But let's face it -- reading XML is not a lot of fun. So now you can ask Tcases to produce test case definitions in the form of an [HTML report](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm#html).

## 1.5.0 ##

  * Better performance: Due to an improved constraint solver, Tcases generates results much faster, even for large and complex input models that previously caused Tcases to "freeze". Although some models may still face problematic performance, these cases are now better described in the [Troubleshooting FAQs](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm#trouble), along with suggestions for fixes. But note that this change is not completely backward-compatible. The test cases produced by this version may not be identical to those produced by previous versions of Tcases, although they will be equivalent in coverage. Also, as a result of the new algorithm, Tcases is slightly less likely to honor "once-only" hints, although this will not affect most input models.

  * Output transformations: For more flexibility when transforming the test definition hierarchy, replace "test case annotations" with "system annotations" and "function annotations".

  * Windows *.bat scripts: Fix initialization of logging control variables.

  * Reducer: Fix a defect that caused Reducer to ignore the actual generator definition during its test case generation rounds.
  
## 1.4.0 ##

  * Introducing [Output Annotations](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm#annotations): For an output transformation to produce concrete test cases, sometimes the basic information in the input model is not enough. You need to add extra information that is not important for generating the test cases but is necessary to form the final output. That's what output annotations are for.

  * JUnit output: To assist completion of test case code, Tcases now adds comments listing all input variable values in the Given section of each @Test method body. But if you prefer the previous output format, you can exclude these comments using the command line option `-p values=false`.
  
  * See [Troubleshooting FAQs](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm#trouble) for more help on what to do when things go wrong. This includes more info on how to control Tcases logging output, including how to redirect logging to standard output. Also, logging messages have been improved to make it easier to understand what's going on.

  * Reducer: For better performance, reduce test cases for each function independently

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
