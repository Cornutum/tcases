# Running API Test Cases #

## Contents ##

  - [Overview](#overview)
  - [Generating executable tests](#generating-executable-tests)
    - [Getting started](#getting-started) 
    - [How does it work?](#how-does-it-work)
    - [Example: REST Assured and JUnit](#example-rest-assured-and-junit) 
    - [Example: Create a specific TestNG class](#example-create-a-specific-testng-class)
    - [Example: Select the API server used by generated tests](#example-select-the-api-server-used-by-generated-tests)
    - [Example: Handle an untrusted API server](#example-handle-an-untrusted-api-server)
    - [Example: Organize tests by API resource path](#example-organize-tests-by-api-resource-path)
    - [Example: Create tests from examples](#example-create-tests-from-examples)
    - [Example: Exclude response validation](#example-exclude-response-validation)
    - [Understanding the TestWriter API](#understanding-the-testwriter-api)    
  - [Running generated tests](#running-generated-tests)
    - [Set up test dependencies](#set-up-test-dependencies)
    - [Manage test resources](#manage-test-resources)
    - [Override the default API server](#override-the-default-api-server)
    - [Define credentials for request authorization](#define-credentials-for-request-authorization)
    - [Handle response validation conditions](#handle-response-validation-conditions)
    - [Handle `writeOnly` property validation](#handle-writeonly-property-validation)
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
  1. :ballot_box_with_check: The expected response outputs

One of the most complicated parts is #3. But Tcases for OpenAPI can automatically generate random request input values,
including valid values that satisfy the requirements of the OpenAPI definition as well as invalid values that test API error
handling. To learn how this works, see [*Generating request inputs*](#generating-request-inputs) below.

For parts #1 and #2, there are lots of choices to make. There are many different ways to construct an executable test
program. Tcases for OpenAPI has built-in support for many of the most common interfaces used by Java test developers. In
addition, Tcases for OpenAPI provides an extensible Java API for implementing the same support for other test frameworks.  For
details, see [*Generating executable tests*](#generating-executable-tests) below.

That leaves part #4: defining the expected responses. The form of API responses is defined by the OpenAPI definition, and Tcases
for OpenAPI will generate code to verify that response data matches those requirements. But what about the actual response data
*values*? Will generated tests verify that the response contains the values expected for the each API request executed? No,
that's the part of the test program you have to fill in yourself.  Tcases for OpenAPI has no way to predict what your API will
do for any request, much less a request using random inputs.

## Generating executable tests ##
  
### Getting started  ###

You can generate executable tests directly from your shell command line. If you use `bash` or a similar UNIX shell, you can run
the `tcases-api-test` command. Or if you are using a Windows command line, you can run the `tcases-api-test.bat` command file,
using exactly the same syntax.  For details about `tcases-api-test` command syntax, see the Javadoc for the
[`ApiTestCommand.Options`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/ApiTestCommand.Options.html)
class.  To get help at the command line, run `tcases-api-test -help`.

`tcases-api-test` is included in the Tcases binary distribution file. For instructions on how to download and install it, see
[*Tcases: The Complete Guide*](../Tcases-Guide.md#installing-the-tcases-distribution). After installation, you can
find OpenAPI examples in the `docs/examples/openapi` subdirectory.

You can also generate tests with the [Tcases Maven Plugin](README.md#running-tcases-for-openapi-using-maven)
using the [`tcases:api-test`](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/api-test-mojo.html) goal.

### How does it work? ###

When Tcases for OpenAPI generates an executable test, the result is one or more source code files that represent the test
program. You can immediately build this source code and run the test.

To do this, Tcases for OpenAPI uses the [TestWriter API](#understanding-the-testwriter-api) to bring together the following
three elements:

  * A [request test definition](Request-Test-Definition.md) that defines the inputs for request test cases (and that is created
    automatically from an OpenAPI definition via [input resolution](#get-actual-input-values)),
  
  * a [`TestWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html) that is
    responsible for producing the code required for a specific [test framework](#test-framework),

  * and a [`TestCaseWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestCaseWriter.html)
    that is responsible for producing the code that uses a specific [request execution interface](#request-execution) to submit
    API requests.

### Example: REST Assured and JUnit  ###

By default, Tcases for OpenAPI generates a JUnit test class that uses [REST Assured](https://github.com/rest-assured/rest-assured)
to execute requests.  The name of the test class, by default, is derived from the `title` of the OpenAPI definition.  The package
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
12:48:35.773 INFO  o.c.t.openapi.ApiTestCommand - Reading API definition from ./petstore-expanded.yaml
12:48:36.114 INFO  o.c.t.openapi.ApiTestCommand - Generating request test cases using random seed=275678033
12:48:36.119 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Preparing constraint info
12:48:36.130 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Generating test cases
12:48:36.137 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Extended 0 valid base test cases
12:48:36.173 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Created 5 valid test cases
12:48:36.174 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Extended 0 base failure test cases
12:48:36.181 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Created 6 failure test cases
12:48:36.187 INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[GET_pets]: Completed 11 test cases
...
12:48:36.443 INFO  o.c.t.openapi.ApiTestCommand - Writing API tests using JUnitTestWriter[] and RestAssuredTestCaseWriter[]
12:48:36.444 INFO  o.c.t.openapi.ApiTestCommand - Writing all API tests to src/test/java/org/examples/SwaggerPetstoreTest.java
12:48:36.446 INFO  o.c.t.openapi.ApiTestCommand - Writing API test resources to src/test/resources/org/examples
```

And you can see the generated source code in `SwaggerPetstoreTest.java`:

```java
package org.examples;

import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.junit.Test;
...
import io.restassured.response.Response;
...
import static io.restassured.RestAssured.*;
...

public class SwaggerPetstoreTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void getPets_TagsDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://petstore.swagger.io/api"))
                .queryParam( "limit", "-736708634")
                .queryParam( "tags", "")
            .when()
                .request( "GET", "/pets")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/pets", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/pets", response.statusCode(), responseHeaders( response));
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
14:20:18.574 INFO  o.c.t.openapi.ApiTestCommand - Reading API definition from ./petstore-expanded.yaml
14:20:18.905 INFO  o.c.t.openapi.ApiTestCommand - Generating request test cases using random seed=345589
...
14:20:19.222 INFO  o.c.t.openapi.ApiTestCommand - Writing API tests using TestNgTestWriter[] and RestAssuredTestCaseWriter[]
14:20:19.222 INFO  o.c.t.openapi.ApiTestCommand - Writing all API tests to ./MyTests.java
14:20:19.224 INFO  o.c.t.openapi.ApiTestCommand - Writing API test resources to .
```

And also in the the generated source code in `MyTests.java`:

```java
package org.examples.testng;

import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.testng.annotations.Test;
...
import io.restassured.response.Response;
...
import static io.restassured.RestAssured.*;
...

public class MyTests extends MyBaseClass {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test(timeOut=10000)
    public void getPets_TagsDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://petstore.swagger.io/api"))
                .queryParam( "limit", "-889243316")
                .queryParam( "tags", "")
            .when()
                .request( "GET", "/pets")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/pets", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/pets", response.statusCode(), responseHeaders( response));
    }
...
}
```

### Example: Select the API server used by generated tests ###

Generated tests must identify an API server to receive the requests submitted by test cases. By default, these tests use one of
the servers described in the OpenAPI definition. There is also a way to [override this setting when tests actually execute](#override-the-default-api-server).

An OpenAPI definition can describe multiple API `servers`, which can be used for the entire API, for a specific resource path,
or even for an individual path operation. By default, Tcases for OpenAPI selects the first element of each `servers` list to use
in generated tests. But there are command line options that allow you to select a different element of the `servers` list or even
to use a specific server that doesn't appear in the OpenAPI definition.

```bash
# Generate JUnit tests for requests defined in 'petstore-expanded.yaml',
# using the API server at index=2 in the servers list.
tcases-api-test -p org.examples -B index=2 petstore-expanded.yaml
```

```bash
# Generate JUnit tests for requests defined in 'petstore-expanded.yaml',
# using the API server with a 'description' containing a specific string.
tcases-api-test -p org.examples -B contains=Test petstore-expanded.yaml
```

```bash
# Generate JUnit tests for requests defined in 'petstore-expanded.yaml',
# using a specific API server URI.
tcases-api-test -p org.examples -B uri=http://api.myserver.com petstore-expanded.yaml
```

### Example: Handle an untrusted API server ###

When generated tests perform API requests using HTTPS, the API server is normally required to verify
that it can be trusted by presenting a valid server certificate. But during testing, this might not
be possible. For example, the test server might not have a certificate, or it may not be possible to
verify that the certificate is trusted in the test environment. To handle this situation, you can
generate tests by running `tcases-api-test` with the `-V` option (or, if using Maven, by running
`tcases:api-test` with the `-DtrustServer=true` parameter). This produces tests that connect to the
API without checking the server certificate.

```bash
# Generate JUnit tests for requests defined in 'petstore-expanded.yaml',
# using an untrusted test server
tcases-api-test -p org.examples -V -B uri=http://localhost petstore-expanded.yaml
```

### Example: Organize tests by API resource path ###

By default, Tcases for OpenAPI creates a single test source file that contains the test cases generated for all
resource paths defined by the OpenAPI definition. But that can be unwieldy for an extensive API that defines a large
number of endpoints. To better deal with this situation, you have the option to generate multiple test source files,
each containing the test cases for a single API resource path.

```bash
# Generate JUnit tests for requests defined by the examples in 'petstore-expanded.yaml'.
# Write test cases for each API path to a separate test source file
tcases-api-test -S -n org.examples.Petstore petstore-expanded.yaml
```

You can see a summary of the result in the `tcases-api-test.log` file:

```
16:51:05.341 INFO  o.c.t.openapi.ApiTestCommand - M.N.P (YYYY-MM-DD)
16:51:05.346 INFO  o.c.t.openapi.ApiTestCommand - Reading API definition from ./petstore-expanded.yaml
...
16:51:05.955 INFO  o.c.t.openapi.ApiTestCommand - Writing API test using JUnitTestWriter[] and RestAssuredTestCaseWriter[]
16:51:05.956 INFO  o.c.t.openapi.ApiTestCommand - Writing API tests for /pets to ./Petstore_PetsTest.java
16:51:05.958 INFO  o.c.t.openapi.ApiTestCommand - Writing API test resources to .
16:51:06.005 INFO  o.c.t.openapi.ApiTestCommand - Writing API tests for /pets/{id} to ./Petstore_PetsIdTest.java
16:51:06.010 INFO  o.c.t.openapi.ApiTestCommand - Writing API test resources to .
```

### Example: Create tests from examples  ###

By default, Tcases for OpenAPI generates test cases using the schemas defined by the OpenAPI definition.
But you can also create executable tests using the [examples](./README.md#how-does-it-work) defined for the API.

```bash
# Generate JUnit tests for requests defined by the examples in 'petstore-expanded.yaml'.
# Write results to 'PetstoreTestExamples.java'.
tcases-api-test -X -n org.examples.PetstoreExamplesTest petstore-expanded.yaml
```

You can see a summary of the result in the `tcases-api-test.log` file:

```
12:48:35.769 INFO  o.c.t.openapi.ApiTestCommand - M.N.P (YYYY-MM-DD)
12:48:35.773 INFO  o.c.t.openapi.ApiTestCommand - Reading API definition from ./petstore-expanded.yaml
12:48:36.114 INFO  o.c.t.openapi.ApiTestCommand - Generating request test cases using API examples
...
12:48:36.443 INFO  o.c.t.openapi.ApiTestCommand - Writing API tests using JUnitTestWriter[] and RestAssuredTestCaseWriter[]
12:48:36.444 INFO  o.c.t.openapi.ApiTestCommand - Writing all API tests to ./PetstoreExamplesTest.java
12:48:36.445 INFO  o.c.t.openapi.ApiTestCommand - Writing API test resources to .
```

### Example: Exclude response validation  ###

By default, Tcases for OpenAPI generates test cases that check the responses from API requests and verify that all requirements
specified in the OpenAPI definition are satisfied. For backward compatibility with previous releases that did not have this
feature, you may want to exclude response validation from generated tests, using the `-d false` option (or, if using
[Maven](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/api-test-mojo.html), `-DwithResources=false`).

```bash
# Generate JUnit tests that don't verify response requirements defined in 'petstore-expanded.yaml'.
tcases-api-test -p org.examples -d false petstore-expanded.yaml
```

This produces the following generated source code in `SwaggerPetstoreTest.java`:

```java
package org.examples;

import org.junit.Test;

import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class SwaggerPetstoreTest {

    @Test
    public void getPets_TagsDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://petstore.swagger.io/api"))
            .queryParam( "limit", "-736708634")
            .queryParam( "tags", "")
        .when()
            .request( "GET", "/pets")
        .then()
            .statusCode( isSuccess())
            ;
    }
...
}
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
Several [helpful utilities](#some-helpful-utilities) are available to make this easier.

Also, API tests must rely on an additional set of <A name="request-execution">"request execution"</A> interfaces. These are
the interfaces used to construct a request message, deliver it to an API server, and collect the resulting response. Here, too,
there are many alternatives, depending on the programming language and framework used for the test.  In the Java world, the
candidates range from basic APIs like [HttpClient](http://hc.apache.org/httpcomponents-client-ga/tutorial/html/fluent.html) to
domain-specific micro-languages like [REST Assured](https://github.com/rest-assured/rest-assured). To create tests that use a
different execution interface, create a new implementation of the
[`TestCaseWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestCaseWriter.html) interface.
`TestCaseWriter` implementations often start with a subclass of
[`BaseTestCaseWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/BaseTestCaseWriter.html).
Several [helpful utilities](#some-helpful-utilities) are available to make this easier.

#### Creating an API test, step-by-step ####

Here's a step-by-step outline of how to use the TestWriter API to convert an OpenAPI definition into an executable test.

  1. Create a [`TestCaseWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestCaseWriter.html) instance.
  1. Create a [`TestWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html) instance that uses this `TestCaseWriter`.
  1. Generate a request test definition for an OpenAPI definition.
     * Generate a Tcases system test definition for an OpenAPI definition using one of the `getRequestTests()` methods of
       the [`TcasesOpenApiIO`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/io/TcasesOpenApiIO.html)
       class.
     * Generate a request test definition using [`getRequestCases`()](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/resolver/RequestCases.html#getRequestCases-org.cornutum.tcases.SystemTestDef-org.cornutum.tcases.openapi.resolver.ResolverContext-).
  1. Create a [`TestSource`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestSource.html) to represent
     the parts of the request test definition to be tested.
  1. Create a [`TestTarget`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestTarget.html) that
     defines the test source file to be generated. Note that this `TestTarget` must be compatible with the `TestWriter`.
  1. Call [`TestWriter.writeTest()`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html#writeTest-S-T-).

#### Some helpful utilities ####

  - **To serialize a request parameter** according to the `style` and `explode` properties specified in the OpenAPI definition, use the public methods
    of the [`TestWriterUtils`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriterUtils.html) class,
    such as [`getQueryParameters()`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriterUtils.html#getQueryParameters-org.cornutum.tcases.openapi.resolver.ParamData-).

  - **To serialize form data** using the `application/x-www-form-urlencoded` media type, use the
    [`FormUrlEncoder`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/encoder/FormUrlEncoder.html).

  - **To serialize request body data** using another media type specified in the OpenAPI definition, use a
    [`DataValueConverter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/encoder/DataValueConverter.html).
    The `BaseTestCaseWriter` class automatically registers converters for many common media types. Alternatively, create and register your own
    implementation.

## Running generated tests ##

By default, certain dependencies are required to compile and execute tests generated by Tcases for OpenAPI. This section
describes the [setup](#set-up-test-dependencies) you will need.

Generated tests also depend on certain supporting resource files.  This section explains how ensure that these [resource
files](#manage-test-resources) are accessible at runtime.

This section also describes several runtime settings you can change to control the operation of generated tests.

### Set up test dependencies ###

(If you choose to [exclude response validation](#example-exclude-response-validation), no dependencies are needed.)

Generated tests that validate response requirements (the default) depend on classes defined in the `tcases-openapi-test` JAR. You
must ensure that this JAR appears on the class path when compiling or executing these tests. If you are using Maven or another similar build tool,
you can use the dependency definition shown [here](../HowToDownload.md#tcases-openapi-test). Alternatively, you can download the `tcases-openapi-test` JAR
directly from the [Maven Central Repository](https://search.maven.org/search?q=g:org.cornutum.tcases%20AND%20a:tcases-openapi-test).

### Manage test resources ###

(If you choose to [exclude response validation](#example-exclude-response-validation), you can ignore this section.)

Generated tests that validate response requirements (the default) must have access to the OpenAPI response definitions.
Therefore, for each generated test class, Tcases for OpenAPI also generates a corresponding *response definition file* that the
test can access as a resource at runtime. For example, for a test source file named `MyApiTest.java`, the corresponding
response definition file is named `MyApiTest-Responses.json`.

When you run the `tcases-api-test` command (or, if using Maven, the
[`tcases:api-test`](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/api-test-mojo.html) goal), the generated response
definition file is created in a *resource directory*, which by default is determined based on the location of the test class
source file. But you can specify the resource directory directly using the `-d` option (or, if using
[Maven](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/api-mojo.html), `-DresourceDir=path`). At any rate, the resource
directory path is always shown in the `tcases-api-test.log` file.

If you are using Maven with default settings, you should find that the generated response definition file is automatically
copied to the class path used to compile and execute the corresponding test class. Otherwise, you may have adjust the value of
command options or settings in your build project to ensure that response definition resource files are accessible to the
corresponding tests at runtime.

Alternatively, you can specify a different resource directory path when you run the test.  For Java tests, use the
`tcasesApiResourceDir` system property to override the normal resource directory setting. This property can be defined in the
`java` command that you run, either directly or via your IDE. Similarly, if you run tests using Maven, this setting can be
defined in the `mvn` command. For example:

```
# Run the 'SwaggerPetstoreTest', using response definitions located in a specific resource directory.
mvn test -Dtest=SwaggerPetstoreTest -DtcasesApiResourceDir=/myResources
```


### Override the default API server ###

By default, the API server used by generated tests is selected [when the test source code is generated](#example-select-the-api-server-used-by-generated-tests).
But you can override this setting when you actually run the tests. This can be helpful when no servers are listed in the original OpenAPI definition
or when you're running in your own personal test environment.

For Java tests, use the `tcasesApiServer` system property to set the API server URI used for all test requests. This setting can
be defined in the `java` command that you run, either directly or via your IDE. Similarly, if you run tests using Maven, this setting can
be defined in the `mvn` command. This setting overrides any API server specified in the test code.

```
# Run the 'SwaggerPetstoreTest', using a specific API server for all test requests.
mvn test -Dtest=SwaggerPetstoreTest -DtcasesApiServer=http://localhost
```

### Define credentials for request authorization ###

Execution of API requests may be subject to security requirements specified in the OpenAPI definition. To satisfy such requirements, you
must define the necessary authorization credentials when you run the tests.

For Java tests, authorization credentials are defined using Java system properties. These settings can
be defined in the `java` command that you run, either directly or via your IDE. Similarly, if you run tests using Maven, these settings can
be defined in the `mvn` command.

Depending on the security scheme, different settings are required. The following security schemes are supported.

  * API key

    ```
    # Run the 'SwaggerPetstoreTest', using an API key to authenticate test requests.
    mvn test -Dtest=SwaggerPetstoreTest -DtcasesApiKey=F81D4FAE-7DEC-11D0-A765-00A0C91E6BF6
    ```

  * HTTP Basic authentication

    ```
    # Run the 'SwaggerPetstoreTest', using a user id and password to authenticate test requests.
    mvn test -Dtest=SwaggerPetstoreTest -DtcasesApiUser=myUserId -DtcasesApiPassword=myLittleSecret
    ```

  * HTTP Bearer authentication

    ```
    # Run the 'SwaggerPetstoreTest', using a bearer token to authenticate test requests.
    mvn test -Dtest=SwaggerPetstoreTest -DtcasesApiBearer=eyJ0eXAi.eyJtZXNz.-yIVBD5b
    ```

### Handle response validation conditions ###

What is a response validation condition? Clearly, one such condition occurs whenever response data does not conform to the
requirements of its OpenAPI definition -- an "invalid response" condition. But other conditions are also possible. In some
cases, validating certain parts of the response may not be possible -- for example, when no schema is defined for the response
body or when the response content type is not supported by Tcases for OpenAPI. In such cases, an "unvalidated" condition occurs.

By default, an "invalid response" condition is handled by throwing an exception, while "unvalidated" conditions are ignored. But
you can change this behavior by defining your own
[`ResponseValidationHandler`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/test/ResponseValidationHandler.html).
For example, you could define a handler that writes a log message for all "unvalidated" conditions or that ignores invalid
responses for certain API paths or operations.

You can inject an instance of your own handler at runtime by defining the `tcasesApiValidationHandler` system property. This
setting can be defined in the `java` command that you run, either directly or via your IDE. Similarly, if you run tests using
Maven, these settings can be defined in the `mvn` command.

```
# Run the 'SwaggerPetstoreTest', using 'MyHandler' to handle response validation conditions.
# 'MyHandler' is assumed to be in the same package as 'SwaggerPetstoreTest'.
mvn test -Dtest=SwaggerPetstoreTest -DtcasesApiValidationHandler=MyHandler
```

Here's what you need to do to create a response validation handler named `MyHandler`

  * The `MyHandler` class must implement
    [`ResponseValidationHandler`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/test/ResponseValidationHandler.html).

  * The `MyHandler` class must define at least one of the following public constructors.

    1. A constructor with one `Class` argument. When called, this argument is set to the test class.
    1. A constructor with no arguments

    For example:

```java
public class MyHandler implements ResponseValidationHandler {

    // If defined, this constructor is used.
    public MyHandler( Class<?> testClass) {
        ...
    }

    // Otherwise, the no-arg constructor is used.
    public MyHandler() {
        ...
    }
    ...
```

  * The `MyHandler` class must be accessible on the class path when the test is executed.

  * If `MyHandler` is located in same package as the test class, the value of `tcasesApiValidationHandler` can be the simple
    class name. Otherwise, `tcasesApiValidationHandler` must be set to the fully-qualified name of the `MyHandler` class.

### Handle `writeOnly` property validation ###

According to the [OpenAPI spec](https://spec.openapis.org/oas/v3.0.2#fixed-fields-19), when response content is defined by an
object schema, any object properties that are designated as `writeOnly` "MAY be sent as part of a request but SHOULD NOT be sent
as part of the response." So is a response containing a value for a `writeOnly` property considered invalid? By default, yes --
this is reported as an "invalid response" condition.  But "SHOULD NOT" means this rule is not absolute and your API is allowed
to ignore it. In which case, you can disable validation for this rule when you run the tests.

For Java tests, use the `tcasesApiWriteOnlyInvalid` system property to disable (or enable) validation of `writeOnly`
properties. This setting can be defined in the `java` command that you run, either directly or via your IDE. Similarly, if you
run tests using Maven, this setting can be defined in the `mvn` command. 

```
# Run the 'SwaggerPetstoreTest', ignoring any `writeOnly` property values in API responses.
mvn test -Dtest=SwaggerPetstoreTest -DtcasesApiWriteOnlyInvalid=false
```


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

But compare the result of the previous command to the result of the command below. Specifying the `-D` option (or, if using
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
that satisfies all of the requirements described for the test case (with some [limitations](#resolution-limitations)). You can
control the resolution process using the options described below.

#### Generating random values ####

Typically, it's important for the results of `tcases-api -D` to be repeatable. So Tcases for OpenAPI uses a random number
generator with a consistent definition of its initial seed value. By default, the random seed value is a hash of the
[name](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#getName--) of the file containing your OpenAPI definition.

If you need more control over the random seed, use the `-r seed` option (or, if using
[Maven](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/api-mojo.html), `-Drandom=seed`). See example below.

But what happens when `tcases-api -D` reads the OpenAPI definition from standard input? In this case, no file name is given,
so Tcases for OpenAPI chooses a random seed for you. This can be useful if you want to see the result of using different seed values.

In any case, the random seed value used for resolution is always shown in the `tcases-api.log` file. See example below.

```
# Define a specific random seed
tcases-api -l stdout -D -r 1234567890 petstore-expanded.yaml
...
12:59:00.009 INFO  o.c.tcases.openapi.ApiCommand - Reading API definition from ./petstore-expanded.yaml
12:59:00.447 INFO  o.c.tcases.openapi.ApiCommand - Writing results to ./petstore-expanded-Request-Cases.json
12:59:00.448 INFO  o.c.tcases.openapi.ApiCommand - Generating request test cases using random seed=1234567890
...
```

#### Controlling resolution attempts ####

When a test case requires complex constraints on an input variable, resolving a satisfying value may require multiple
iterations. To prevent an infinite fruitless search, Tcases for OpenAPI places a limit on the maximum number of resolution
attempts made before giving up and reporting an error condition. By default, the limit on resolution attempts is 10000. But that limit is
arbitrary and there is no guarantee that it will be sufficient for your API definition. You can choose a different limit by using the `-m
limit` option (or, if using [Maven](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/api-mojo.html),
`-DmaxTries=limit`). See example below.

```
# Define a specific limit on resolution attempts
tcases-api -D -m 999999 petstore-expanded.yaml
```

#### Handling resolution conditions ####

Tcases for OpenAPI reports conditions in the OpenAPI definition or its corresponding test model that affect how input values are
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

#### Resolution limitations ####

  * **When unable to resolve a value** 

    When a test case requires complex constraints on an input variable, resolving a satisfying value may require multiple
    iterations. In some cases, the search for a satisfying value may be unsuccessful, in which case the you'll see the following
    error message. But note that there are [options you can use](#controlling-resolution-attempts) that may eliminate this failure.
```
... ERROR o.c.t.o.r.RequestCaseResolver - RequestCaseDef[...],...: Unable to resolve a value after 10000 tries. No request case created for this test.
```
    
  * **When a content media type has no schema** 

    <a name="content-no-schema"></a>
    Various elements of an OpenAPI definition can be described by a `content` property that defines one or more data media
    types. Some `content` media types represent unstructured data that can't be described in an OpenAPI definition, in which
    case the `schema` property is undefined. Tcases for OpenAPI accepts such media type definitions but reports a warning. Why?
    Because without a schema, Tcases for OpenAPI doesn't know how to generate actual input data for this content.

    Instead, input resolution will proceed using an empty schema, i.e. any type of non-null JSON data. The result is almost
    certainly not a valid value for your API, so you will need to modify the generated test cases accordingly.
