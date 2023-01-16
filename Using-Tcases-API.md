# Using The Tcases API #

## Tcases In Code ##

The Tcases command line interface is implemented using a Java API. You can use the same API to do all of the same things in your own Java program,
including [creating a system input definition](#builders), [generating test cases](#test-cases), and [reading or writing Tcases documents](#documents).

## Builders ##

The [`tcases-lib`](https://search.maven.org/search?q=g:org.cornutum.tcases%20AND%20a:tcases-lib) JAR contains all of the core
classes needed to create a [system input definition](Tcases-Guide.md#defining-system-functions).  You can use methods from the
[`SystemInputs`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/SystemInputs.html) for a fluent interface to
the various builder classes needed to construct a complete [`SystemInputDef`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/SystemInputDef.html).
For a complete example, see the [`SystemInputBuilderTest`](./tcases-io/src/test/java/org/cornutum/tcases/io/SystemInputBuilderTest.java).

## Test Cases ##

The [`tcases-lib`](https://search.maven.org/search?q=g:org.cornutum.tcases%20AND%20a:tcases-lib) JAR all contains all of the interfaces needed to
generate a [system test definition](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/SystemTestDef.html)
that contains all of the [test cases](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/TestCase.html) for a system input definition.

To generate test cases, use the [`Tcases`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/Tcases.html) class. `Tcases` defines
several variations on the
[`getTests`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/Tcases.html#getTests-org.cornutum.tcases.SystemInputDef-org.cornutum.tcases.generator.IGeneratorSet-org.cornutum.tcases.resolve.TestCaseResolverFactory-org.cornutum.tcases.SystemTestDef-org.cornutum.tcases.generator.GeneratorOptions-)
method perform this task. The primary input parameter is the `SystemInputDef` but additional parameters define optional helper objects.
For these, you can use a `null` value to apply a default value.
For an example, see the [`SystemInputBuilderTest`](./tcases-io/src/test/java/org/cornutum/tcases/io/SystemInputBuilderTest.java).

For example, `Tcases:getTests` accepts a [generator definition](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/generator/IGeneratorSet.html)
object to define the required [coverage](Tcases-Guide.md#defining-higher-coverage) for test cases.
You can use methods from the [`Generators`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/generator/Generators.html) class for a
fluent interface to the various builder classes needed to construct a generator definition.
For a complete example, see the [`GeneratorsTest`](./tcases-io/src/test/java/org/cornutum/tcases/generator/io/GeneratorsTest.java).

To run the [Tcases Reducer](Tcases-Guide.md#reducing-test-cases-a-random-walk),
use the [`Reducer`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/Reducer.html) class.
`Reducer` defines several variations on the
[`reduce`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/Reducer.html#reduce-org.cornutum.tcases.SystemInputDef-org.cornutum.tcases.generator.GeneratorSet-org.cornutum.tcases.SystemTestDef-org.cornutum.tcases.ReducerOptions-)
method to perform this task.

## Documents ##

The [`tcases-io`](https://search.maven.org/search?q=g:org.cornutum.tcases%20AND%20a:tcases-io) JAR contains classes needed to handle
the following types of Tcases documents.

  * System input definition

    * [`SystemInputJsonReader`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/io/SystemInputJsonReader.html)
    * [`SystemInputJsonWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/io/SystemInputJsonWriter.html)
    * [`SystemInputDocReader`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/io/SystemInputDocReader.html)
    * [`SystemInputDocWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/io/SystemInputDocWriter.html)

  * Generator definition

    * [`GeneratorSetJsonReader`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/generator/io/GeneratorSetJsonReader.html)
    * [`GeneratorSetJsonWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/generator/io/GeneratorSetJsonWriter.html)
    * [`GeneratorSetDocReader`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/generator/io/GeneratorSetDocReader.html)
    * [`GeneratorSetDocWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/generator/io/GeneratorSetDocWriter.html)

  * System test definition

    * [`SystemTestJsonReader`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/io/SystemTestJsonReader.html)
    * [`SystemTestJsonWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/io/SystemTestJsonWriter.html)
    * [`SystemTestDocReader`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/io/SystemTestDocReader.html)
    * [`SystemTestDocWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/io/SystemTestDocWriter.html)

  * Project definition

    * [`ProjectJsonReader`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/io/ProjectJsonReader.html)
    * [`ProjectJsonWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/io/ProjectJsonWriter.html)
