# Running API Test Cases #

## Contents ##

  - [Overview](#overview)
  - [Generating executable tests](#generating-executable-tests)
    - [Getting started](#getting-started) 
    - [How does it work?](#how-does-it-work)
    - [Example: REST Assured and JUnit](#example-rest-assured-and-junit) 
    - [Example: Create a specific TestNG class](#example-create-a-specific-testng-class)
    - [Example: Create tests from examples](#example-create-tests-from-examples)
    - [Understanding the TestWriter API](#understanding-the-testwriter-api)    
  - [Generating request inputs](#generating-request-inputs)
    - [Instead of input descriptions...](#instead-of-input-descriptions)
    - [Get actual input values...](#get-actual-input-values)
    - [How does input resolution work?](#how-does-input-resolution-work)
  
## Overview ##

Ideally, Tcases for OpenAPI would produce a test program that you could immediately run. Ideally, this test program would
execute all API requests against an actual API server, applying a comprehensive set of request input data and automatically verifying
the expected responses. Bam! Job done!

But is this even possible? Yes, it is -- mostly.
Consider that any such test program must combine all of the following elements.

  1. :white_check_mark: The framework for organizing test execution, e.g. JUnit, etc.
  1. :white_check_mark: The interfaces for submitting HTTP requests and receiving responses
  1. :white_check_mark: The actual request inputs
  1. :x: The expected response outputs

One of the most complicated parts is #3. But Tcases for OpenAPI can automatically generate random request input values,
including valid values that satisfy the requirements of the OpenAPI spec as well as invalid values that test API error
handling. To learn how this works, see [*Generating request inputs*](#generating-request-inputs) below.

For parts #1 and #2, there are lots of choices to make. There are many different ways to construct an executable test
program. Tcases for OpenAPI has built-in support for many of the most common interfaces used by Java test developers. In
addition, Tcases for OpenAPI provides an extensible Java API for implementing the same support for other test frameworks.  For
details, see [*Generating executable tests*](#generating-executable-tests) below.

That leaves part #4: defining the expected responses. But that's the part of the test program you have to fill in yourself.
Tcases for OpenAPI has no way to predict what your API will do for any request, much less a request using random inputs.

## Generating executable tests ##
  
### Getting started  ###

You can generate executable tests directly from your shell command line. If you use `bash` or a similar UNIX shell, you can run
the `tcases-api-test` command. Or if you are using a Windows command line, you can run the `tcases-api-test.bat` command file,
using exactly the same syntax.  For details about `tcases-api-test` command syntax, see the Javadoc for the
[`ApiTestCommand.Options`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/ApiTestCommand.Options.html)
class.  To get help at the command line, run `tcases-api-test -help`.

`tcases-api-test` is included in the Tcases binary distribution file. For instructions on how to download and install it, see
[*Tcases: The Complete Guide*](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm#install). After installation, you can
find OpenAPI examples in the `docs/examples/openapi` subdirectory.

You can also generate tests with the [Tcases Maven Plugin](README.md#running-tcases-for-openapi-using-maven)
using the [`tcases:api-test`](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/api-test-mojo.html) goal.

### How does it work? ###

When Tcases for OpenAPI generates an executable test, the result is one or more source code files that represent the test
program. You can immediately build this source code and run the test.

To do this, Tcases for OpenAPI uses the [TestWriter API](#understanding-the-testwriter-api) to bring together the following
three elements:

  * A [request test definition](Request-Test-Definition.md) that defines the inputs for request test cases (and that is created
    automatically from an OpenAPI spec via [input resolution](#get-actual-input-values)),
  
  * a [`TestWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html) that is
    responsible for producing the code required for a specific [test framework](#test-framework),

  * and a [`TestCaseWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestCaseWriter.html)
    that is reponsible for producing the code that uses a specific [request execution interface](#request-execution) to submit
    API requests.

### Example: REST Assured and JUnit  ###

By default, Tcases for OpenAPI generates a JUnit test class that uses [REST Assured](https://github.com/rest-assured/rest-assured)
to execute requests.  The name of the test class, by default, is derived from the `title` of the OpenAPI spec.  The package
containing the test class can also be determined automatically from the destination directory if it follows Maven project
conventions.

```bash
# Generate JUnit tests for requests defined in 'petstore-expanded.yaml'.
# Write results to 'SwaggerPetstoreTest.java'.
tcases-api-test -o src/test/java/org/examples petstore-expanded.yaml
```

You can see a summary of the generation process in the `tcases-api-test.log` file:

```
12:48:35.769 INFO  o.c.t.openapi.ApiTestCommand - M.N.P (YYYY-MM-DD)
12:48:35.773 INFO  o.c.t.openapi.ApiTestCommand - Reading API spec from ./petstore-expanded.yaml
12:48:36.114 INFO  o.c.t.openapi.ApiTestCommand - Generating request test cases using random seed=275678033
12:48:36.119 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Preparing constraint info
12:48:36.130 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Generating test cases
12:48:36.137 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Extended 0 valid base test cases
12:48:36.173 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Created 5 valid test cases
12:48:36.174 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Extended 0 base failure test cases
12:48:36.181 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Created 6 failure test cases
12:48:36.187 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Completed 11 test cases
...
12:48:36.443 INFO  o.c.t.openapi.ApiTestCommand - Writing API test using JUnitTestWriter[] and RestAssuredTestCaseWriter[]
12:48:36.444 INFO  o.c.t.openapi.ApiTestCommand - Writing API test to src/test/java/org/examples/SwaggerPetstoreTest.java
```

And you can see the generated source code in `SwaggerPetstoreTest.java`:

```java
package org.examples;

import org.junit.Test;

import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class SwaggerPetstoreTest {

    @Test
    public void getPets_TagsDefined_Is_Yes() {
        given()
            .queryParam( "limit", "-736708634")
            .queryParam( "tags", "")
        .when()
            .request( "GET", "http://petstore.swagger.io/api/pets")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }
...
}
```
### Example: Create a specific TestNG class  ###

Tcases for OpenAPI also supports [TestNG](https://testng.org/doc/), another widely-used Java test framework.  You can also use
other options to control the name and package of the generated test class or to control the input resolution
process.


```bash
# Generate TestNG tests for requests defined in 'petstore-expanded.yaml'.
# Use a specific seed for the generation of random input values.
# Report a failure for any test method that runs longer than 10 seconds.
# Write results to './MyTests.java'.
tcases-api-test -t testng -r 345589 -n org.examples.testng.MyTests -b MyBaseClass -u 10000 petstore-expanded.yaml
```

You can see the difference in the `tcases-api-test.log` file:

```
14:20:18.570 INFO  o.c.t.openapi.ApiTestCommand - M.N.P (YYYY-MM-DD)
14:20:18.574 INFO  o.c.t.openapi.ApiTestCommand - Reading API spec from ./petstore-expanded.yaml
14:20:18.905 INFO  o.c.t.openapi.ApiTestCommand - Generating request test cases using random seed=345589
...
14:20:19.222 INFO  o.c.t.openapi.ApiTestCommand - Writing API test using TestNgTestWriter[] and RestAssuredTestCaseWriter[]
14:20:19.222 INFO  o.c.t.openapi.ApiTestCommand - Writing API test to ./MyTests.java
```

And also in the the generated source code in `MyTests.java`:

```java
package org.examples.testng;

import org.testng.annotations.Test;

import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class MyTests extends MyBaseClass {

    @Test(timeOut=10000)
    public void getPets_TagsDefined_Is_Yes() {
        given()
            .queryParam( "limit", "-889243316")
            .queryParam( "tags", "")
        .when()
            .request( "GET", "http://petstore.swagger.io/api/pets")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }
...
}
```

### Example: Create tests from examples  ###

By default, Tcases for OpenAPI generates test cases using the schemas defined by the OpenAPI spec.
But you can also create executable tests using the [examples](./README.md#how-does-it-work) defined for the API.

```bash
# Generate JUnit tests for requests defined by the examples in 'petstore-expanded.yaml'.
# Write results to 'PetstoreTestExamples.java'.
tcases-api-test -X -n org.examples.PetstoreExamplesTest petstore-expanded.yaml
```

You can see a summary of the result in the `tcases-api-test.log` file:

```
12:48:35.769 INFO  o.c.t.openapi.ApiTestCommand - M.N.P (YYYY-MM-DD)
12:48:35.773 INFO  o.c.t.openapi.ApiTestCommand - Reading API spec from ./petstore-expanded.yaml
12:48:36.114 INFO  o.c.t.openapi.ApiTestCommand - Generating request test cases using API examples
...
12:48:36.443 INFO  o.c.t.openapi.ApiTestCommand - Writing API test using JUnitTestWriter[] and RestAssuredTestCaseWriter[]
12:48:36.444 INFO  o.c.t.openapi.ApiTestCommand - Writing API test to ./PetstoreExamplesTest.java
```

### Understanding the TestWriter API ###

The `tcases-api-test` command is implemented using the TestWriter API. Using this Java API, you can extend Tcases for OpenAPI
to generate tests that use different test frameworks and even different programming languages.

#### The basics ####

Most API test programs are organized around two different kinds of interfaces.

First, there is the overall <A name="test-framework">"test framework"</A> that is used to run test cases and report results.  In
the Java world, [JUnit](https://junit.org/junit4/) and [TestNG](https://testng.org/doc/) are the most widely used examples of
such frameworks. For test developers, such frameworks generally define how to designate individual tests and how to control
their execution. And they are general-purpose, equally applicable for all kinds of testing. There is nothing about them that
specifically supports API testing. To create tests for a different test framework, create a new subclass of the basic
[`TestWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html) class.
Several [helpful utilties](#some-helpful-utilities) are available to make this easier.

Also, API tests must rely on an additional set of <A name="request-execution">"request execution"</A> interfaces. These are
the interfaces used to construct a request message, deliver it to an API server, and collect the resulting response. Here, too,
there are many alternatives, depending on the programming language and framework used for the test.  In the Java world, the
candidates range from basic APIs like [HttpClient](http://hc.apache.org/httpcomponents-client-ga/tutorial/html/fluent.html) to
domain-specific micro-languages like [REST Assured](https://github.com/rest-assured/rest-assured). To create tests that use a
different execution interface, create a new implementation of the
[`TestCaseWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestCaseWriter.html) interface.
`TestCaseWriter` implementations often start with a subclass of
[`TestCaseContentWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestCaseContentWriter.html).
Several [helpful utilties](#some-helpful-utilities) are available to make this easier.

#### Creating an API test, step-by-step ####

Here's a step-by-step outline of how to use the TestWriter API to convert an OpenAPI spec into an executable test.

  1. Create a [`TestCaseWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestCaseWriter.html) instance.
  1. Create a [`TestWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html) instance that uses this `TestCaseWriter`.
  1. Generate a request test definition for an OpenAPI spec.
     * Generate a Tcases system test definition for an OpenAPI spec using one of the `getRequestTests()` methods of
       the [`TcasesOpenApiIO`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/io/TcasesOpenApiIO.html)
       class.
     * Generate a request test definition using [`getRequestCases`()](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/resolver/RequestCases.html#getRequestCases-org.cornutum.tcases.SystemTestDef-org.cornutum.tcases.openapi.resolver.ResolverContext-).
  1. Create a [`TestSource`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestSource.html) to represent
     the parts of the request test definition to be tested.
  1. Create a [`TestTarget`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestTarget.html) that
     defines the test source file to be generated. Note that this `TestTarget` must be compatible with the `TestWriter`.
  1. Call [`TestWriter.writeTest()`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html#writeTest-S-T-).

#### Some helpful utilities ####

  - **To serialize a request parameter** according to the `style` and `explode` properties specified in the OpenAPI spec, use the public methods
    of the [`TestWriterUtils`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriterUtils.html) class,
    such as [`getQueryParameters()`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriterUtils.html#getQueryParameters-org.cornutum.tcases.openapi.resolver.ParamData-).

  - **To serialize form data** using the `application/x-www-form-urlencoded` media type, use the
    [`FormUrlEncoder`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/encoder/FormUrlEncoder.html).

  - **To serialize request body data** using another media type specified in the OpenAPI spec, use a
    [`DataValueConverter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/encoder/DataValueConverter.html).
    The `TestCaseContentWriter` class automatically registers converters for many common media types. Alternatively, create and register your own
    implementation.

## Generating request inputs ##

### Instead of input descriptions... ###

By default, Tcases for OpenAPI produces a JSON document that *describes* the input values for each test case, but only in a
general way.  It's left for you to choose the actual input values that match these descriptions. The following example uses the
[`tcases-api` command](README.md#running-tcases-for-openapi-from-the-command-line) to demonstrate the results produced in the
default case.

```
# Print test case descriptions to standard output
tcases-api -T yaml < petstore-expanded.yaml

{
  "system": "Swagger-Petstore",
  "has": {
    "server": "http://petstore.swagger.io/api",
    "version": "1.0.0"
  },
  "GET_pets": {
    "has": {
      "server": "http://petstore.swagger.io/api",
      "version": "1.0.0"
    },
    "testCases": [
      {
        "id": 0,
        "name": "tags.Defined='Yes'",
        "has": {
          "server": "http://petstore.swagger.io/api",
          "version": "1.0.0",
          "properties": "limit,limitValue,tags,tagsValue"
        },
        "query": {
        "tags.Defined": {
            "has": {
              "explode": "true",
              "style": "form"
            },
            "value": "Yes"
          },
          "tags.Type": {
            "value": "array"
          },
          "tags.Items.Size": {
            "value": "0"
          },
          "tags.Items.Contains.Type": {
            "NA": true
          },
          "tags.Items.Contains.Value.Length": {
            "NA": true
          },
          "tags.Items.Unique": {
            "NA": true
          },
          "limit.Defined": {
            "has": {
              "style": "form"
            },
            "value": "Yes"
          },
          "limit.Type": {
            "value": "integer"
          },
          "limit.Value": {
            "has": {
              "format": "int32"
            },
            "value": "< 0"
          }
        }
      },
      ...
    ]
  },
...
```

This describes a test case for the `GET /pets` request in which the `limit` parameter is a negative integer. It could be any negative integer, but you still
have to choose the one to use in your test.

### Get actual input values... ###

But compare the result of the previous command to the result of the command below. Specifying the `-D` option (or, when using
[Maven](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/api-mojo.html), `-DrequestCases=true`) produces an entirely
different result: a JSON document that describes actual values for each request input. This document is called a "request test
definition" (see details [here](Request-Test-Definition.md)) and it is created by ["resolving"](#how-does-input-resolution-work)
input descriptions into actual input values.

```
# Print a request test definition to standard output
tcases-api -T yaml -D < petstore-expanded.yaml

[
  {
    "id": 0,
    "name": "tags.Defined='Yes'",
    "server": "http://petstore.swagger.io/api",
    "version": "1.0.0",
    "path": "/pets",
    "operation": "GET",
    "parameters": [
      {
        "name": "limit",
        "in": "query",
        "style": "form",
        "explode": false,
        "data": {
          "type": "integer",
          "value": -736708634,
          "format": "int32"
        },
        "valid": true
      },
      {
        "name": "tags",
        "in": "query",
        "style": "form",
        "explode": true,
        "data": {
          "type": "array",
          "value": [
          ]
        },
        "valid": true
      }
    ]
  },
...  
```

### How does input resolution work? ###

"Resolving" a [test case description](http://www.cornutum.org/tcases/docs/Tcases-Json.htm#testCases) means generating an actual
value for each of the request input variables. To do this, Tcases for OpenAPI creates a random dummy value of the required type
that satisfies all of the requirements described for the test case. You can control the resolution process using the options
described below.

#### Generating random values ####

Typically, it's important for the results of `tcases-api -D` to be repeatable. So Tcases for OpenAPI uses a random number
generator with a consistent definition of its initial seed value. By default, the random seed value is a hash of the
[name](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#getName--) of the file containing your OpenAPI specification.

If you need more control over the random seed, use the `-r seed` option (or, when using
[Maven](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/api-mojo.html), `-Drandom=seed`). See example below.

But what happens when `tcases-api -D` reads the OpenAPI specification from standard input? In this case, no file name is given,
so Tcases for OpenAPI chooses a random seed for you. This can be useful if you want to see the result of using different seed values.

In any case, the random seed value used for resolution is always shown in the `tcases-api.log` file. See example below.

```
# Define a specific random seed
tcases-api -l stdout -D -r 1234567890 petstore-expanded.yaml
...
12:59:00.009 INFO  o.c.tcases.openapi.ApiCommand - Reading API spec from ./petstore-expanded.yaml
12:59:00.447 INFO  o.c.tcases.openapi.ApiCommand - Writing results to ./petstore-expanded-Request-Cases.json
12:59:00.448 INFO  o.c.tcases.openapi.ApiCommand - Generating request test cases using random seed=1234567890
...
```

#### Controlling resolution attempts ####

When a test case describes complex constraints on an input variable, resolving a satisfying value may require multiple
iterations. To prevent an infinite fruitless search, Tcases for OpenAPI places a limit on the maximum number of resolution
attempts made before giving up and reporting an error condition. By default, the limit on resolution attempts is 10000. But that limit is
arbitrary and there is no guarantee that it will be sufficient for your API spec. You can choose a different limit by using the `-m
limit` option (or, when using [Maven](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/api-mojo.html),
`-DmaxTries=limit`). See example below.

```
# Define a specific limit on resolution attempts
tcases-api -D -m 999999 petstore-expanded.yaml
```

Tcases for OpenAPI reports conditions in the OpenAPI spec or its corresponding test model that affect how input values are
resolved.  Warning conditions are reported with an explanation of the situation. Error conditions report input values that could
not be resolved. By default, conditions are reported by writing log messages. But you can choose to report resolution conditions by
throwing an exception (`fail`) or by ignoring them altogether (`ignore`). For `tcases-api`, use the `-c` option, which also
controls [input modelling conditions](README.md#semantic-linting-with-tcases-for-openapi) -- see example below.
When using [Maven](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/api-mojo.html), you can do the same thing with
separate values for `-DonModellingCondition` and `-DonResolverCondition`.

```
# Ignore all input modelling conditions but fail on any resolution condition
tcases-api -D -c ignore,fail petstore-expanded.yaml
```

