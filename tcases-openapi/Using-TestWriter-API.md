
# Using the TestWriter API #

## Contents ##

  * [Overview](#overview)
  * [Get Started](#get-started)
      * [Java? Nope. I need to produce tests in another language](#java-nope-i-need-to-produce-tests-in-another-language)
      * [Java tests are fine, but I don't use JUnit or TestNG](#java-tests-are-fine-but-i-dont-use-junit-or-testng)
      * [I use JUnit (or TestNG), but I want to replace REST Assured with something different](#i-use-junit-or-testng-but-i-want-to-replace-rest-assured-with-something-different)
      * [Since I'm adding extensions to Tcases, do I have to create my own fork?](#since-im-adding-extensions-to-tcases-do-i-have-to-create-my-own-fork)
      * [Can I get the `tcases-api-test` command to use my own TestWriter?](#can-i-get-the-tcases-api-test-command-to-use-my-own-testwriter)
  * [The TestWriter Lifecycle](#the-testwriter-lifecycle)
  * [What is a TestSource?](#what-is-a-testsource)
  * [What is a TestTarget?](#what-is-a-testtarget)
  * [Developer Requirements](#developer-requirements)
    * [TestWriter requirements](#testwriter-requirements)
    * [TestCaseWriter requirements](#testcasewriter-requirements)
    * [TestTarget requirements](#testtarget-requirements)
  * [TestCaseWriter Tips](#testcasewriter-tips)
    * [Using `IndentedWriter`](#using-indentedwriter)
    * [Using `BaseTestCaseWriter`](#using-basetestcasewriter)
    * [Understanding `RequestCase`](#understanding-requestcase)
    * [Validating responses](#validating-responses)
    * [Using `RequestCaseUtils`](#using-requestcaseutils)
  * [Testing Tips](#testing-tips)
    * [Testing with the CLI](#testing-with-the-cli)
    * [Testing with Maven](#testing-with-maven)


## Overview ##

When Tcases for OpenAPI generates an executable test for your API, the result is one or more source code files that implement
the test program. You can immediately build this source code and run the test.  To do this, Tcases for OpenAPI has to deal with
all of the issues of constructing an API test program.

* **Which language?** Which programming language are you using to write the test? Java? JavaScript? Something more exotic?

* **Which test framework?** Most test programs are built around a _test framework_ that is used to run test cases and report
  results.  For test developers, such frameworks generally define how to designate individual tests and how to control their
  execution.  Typically, they are general-purpose, equally applicable for all kinds of testing. Depending on the programming
  language, there are usually several test frameworks to choose from.

* **How will tests interact with an API server?** Any API test demands a set of _request execution interfaces_ of some kind.
  These are the interfaces used to construct a request message, deliver it to an API server, and collect the resulting response.
  Some test frameworks come with built-in request execution interfaces. In other cases, many different choices may be available.

Tcases for OpenAPI generates executable tests using the TestWriter API, which brings together the following three elements:

  * A [request test definition](Request-Test-Definition.md) that defines the inputs for request test cases (and that is created
    automatically from an OpenAPI definition via [input resolution](Running-Api-Test-Cases.md#get-actual-input-values)),
  
  * a [TestWriter](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html) that is
    responsible for producing the code required for a specific test framework,

  * and a [TestCaseWriter](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestCaseWriter.html)
    that is responsible for producing the code that uses a specific request execution interface to submit
    API requests and evaluate API responses.

If you want to generate a Java test program, it's likely that Tcases for OpenAPI already has everything you need. Tcases for
OpenAPI has built-in support for the two most common Java test frameworks (JUnit and TestNG) and for a powerful request
execution interface ([REST Assured](https://github.com/rest-assured/rest-assured)).  But what if you need something different?
In that case, you can use the TestWriter API to add extensions to Tcases for OpenAPI that produce the results you want.

## Get Started ##

#### Java? Nope. I need to produce tests in another language ####

You will need to create a new TestWriter to write the code for the target test framework in that language.
You will also need to create a new TestCaseWriter to write the code for the target request execution interface in that language.
To learn how, read all of the following sections, starting with [_The TestWriter Lifecycle_](#the-testwriter-lifecycle).

#### Java tests are fine, but I don't use JUnit or TestNG ####

You will need to create a new TestWriter to write the code for the target test framework.
To learn how, read all of the following sections, starting with [_The TestWriter Lifecycle_](#the-testwriter-lifecycle).
Be sure to learn about using the [`IndentedWriter`](#using-indentedwriter).

You can skip the [_TestCaseWriter requirements_](#testcasewriter-requirements) and [_TestCaseWriter Tips_](#testcasewriter-tips) sections
if you're happy with [REST Assured](https://github.com/rest-assured/rest-assured).
Otherwise, read [this](#i-use-junit-or-testng-but-i-want-to-replace-rest-assured-with-something-different), too.

#### I use JUnit (or TestNG), but I want to replace REST Assured with something different ####

You will need to create a new TestCaseWriter to write the code for the target request execution interface.
To learn how, start with the [_The TestWriter Lifecycle_](#the-testwriter-lifecycle) section.
Then read [_TestCaseWriter requirements_](#testcasewriter-requirements) and [_TestCaseWriter Tips_](#testcasewriter-tips)

#### Since I'm adding extensions to Tcases, do I have to create my own fork? ####

No! In fact, it's better to create your new TestWriter or TestCaseWriter as an independent package, with
[`tcases-openapi`](https://central.sonatype.com/artifact/org.cornutum.tcases/tcases-openapi/overview) as a dependency. By
decoupling from the Tcases source code, you won't have to modify your extension when a new Tcases release is published.
Better still: publish your new extension for everyone else to use!

#### How can I test my own TestWriter implementation? ####

You can test your new TestWriter or TestCaseWriter by plugging them into an execution of the `tcases-api-test` command.
To learn how, read [_Testing Tips_](#testing-tips).

#### Can I get the `tcases-api-test` command to use my own TestWriter? ####

Yes, you can! To learn how, read [_Testing with the CLI_](#testing-with-the-cli).

## The TestWriter Lifecycle ##

The work of a TestWriter is carried out via the _TestWriter lifecycle_. This lifecycle consists of a series of steps that
incrementally produce each part of a complete test program.

### Overview ###

The TestWriter lifecycle is an example of the [Template pattern](https://en.wikipedia.org/wiki/Template_method_pattern). The
template is provided by the abstract `TestWriter` class. A TestWriter must be implemented by a subclass of `TestWriter`.

The TestWriter lifecycle is invoked by calling the
[`writeTest`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html#writeTest-S-T-) method.
Each step of the lifecycle is implemented by a `TestWriter` method.  A lifecycle method may be abstract, in which case every
TestWriter implementation must provide its own implementation. Or, in other cases, a lifecycle method may act as a "hook" that a
new TestWriter subclass can choose to override, usually to add new actions before or after invoking the superclass method.
Completely replacing the behavior of a hook method is not recommended.

Here is an overview of the TestWriter lifecycle. Each abstract `TestWriter` lifecycle method is indicated by :small_blue_diamond:.

| Lifecycle Method  |     |                   | Purpose |
| ---:              | --- | :---              | --- |
| prepareTestCases  |     |                   | Set up the TestWriter for the request test definition |
| writeProlog       | :arrow_heading_down:  |                   | Write parts that precede test cases |
|                   | :small_blue_diamond:  | writeOpening      | Write the opening part of the test program |
|                   | :small_blue_diamond:  | writeDependencies | Write framework dependencies |
|                   | :small_blue_diamond:  | writeDeclarations | Write declarations of framework components |
| writeTestCases    | :arrow_heading_down:  |                   | Write all test cases |
|                   |     | writeTestCase     | Write a single test case |
| writeEpilog       | :arrow_heading_down:  |                   | Write parts that follow test cases |
|                   | :small_blue_diamond:  | writeClosing      | Write the closing part of the test program |
| writeResponsesDef |     |                   | Write [definitions](#what-is-a-testsource) for response validation |

### Delegation to TestCaseWriter ###

A TestWriter delegates part of its job to a TestCaseWriter, which is responsible for producing the code that executes a test case.
Consequently, the TestWriter lifecycle also orchestrates the interplay between TestWriter and TestCaseWriter responsibilities.
A TestCaseWriter must be an implementation of the `TestCaseWriter` interface and must provide an implementation for each of its
lifecycle methods.

Here is a overview of how a TestWriter delegates responsibilities to a TestCaseWriter.
Each `TestCaseWriter` lifecycle method is indicated by :small_orange_diamond:.

| Lifecycle Method  |     |                   |     |               | Purpose |
| ---:              | --- | :---              | --- | ---           | --- |
| prepareTestCases  | :arrow_heading_down:  |                   |     |               | Set up the TestWriter for the request test definition |
|                   | :small_orange_diamond:  | prepareTestCases  |     |               | Set up the TestCaseWriter for the request test definition |
| writeProlog       | :arrow_heading_down:  |                   |     |               | Write parts that precede test cases |
|                   | :small_blue_diamond:  | writeOpening      |     |               | Write the opening part of the test program |
|                   | :small_blue_diamond:  | writeDependencies |     |               | Write framework dependencies |
|                   | :small_orange_diamond:  | writeDependencies |     |               | Write request execution dependencies |
|                   | :small_blue_diamond:  | writeDeclarations |     |               | Write declarations of framework components |
|                   | :small_orange_diamond:  | writeDeclarations |     |               | Write declarations of request execution components |
| writeTestCases    | :arrow_heading_down:  |                   |     |               | Write all test cases |
|                   |     | writeTestCase     | :arrow_heading_down:  |               | Write a single test case |
|                   |     |                   | :small_orange_diamond:  | writeTestCase | Write the body of a single test case |
| writeEpilog       | :arrow_heading_down:  |                   |     |               | Write parts that follow test cases |
|                   | :small_orange_diamond:  | writeClosing      |     |               | Write request execution parts that follow test cases |
|                   | :small_blue_diamond:  | writeClosing      |     |               | Write the closing part of the test program |
| writeResponsesDef |     |                   |     |               | Write [definitions](#what-is-a-testsource) for response validation |

### The TestWriter lifecycle in action ###

To see how the TestWriter lifecycle works, let's look at an example using the standard `JUnitTestWriter`. This section shows the result produced
by each step of the lifecycle.

#### writeTests ####

This `TestWriter` method invokes the lifecycle, using a specified [TestSource](#what-is-a-testsource) and [TestTarget](#what-is-a-testtarget).

#### prepareTestCases ####

This hook method simply invokes the TestCaseWriter `prepareTestCases` method. Input to this method is the
[request test definition](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/resolver/RequestTestDef.html)
that describes the test cases generated from the OpenAPI definition.

#### writeProlog ####

This hook method invokes the lifecycle methods that produce the parts of the test program that precede the actual test cases.

#### writeOpening ####

This method writes the opening part of the test program. For example:

```java
package org.examples;
```

#### writeDependencies ####

This method writes framework-dependent dependencies. For example:

```java

import org.examples.util.BaseClass

import org.junit.Test;
```

#### writeDeclarations ####

This method writes framework-dependent declarations. For example:

```java

public class MyApiTestCase extends BaseClass {
```

#### writeTestCases ####

This hook method simply calls `writeTestCase` for each `RequestCase` in the
[request test definition](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/resolver/RequestTestDef.html).

#### writeTestCase ####

This hook method simply calls the TestCaseWriter `writeTestCase` method for a single `RequestCase`.
The `JUnitTestWriter` overrides this method to write the framework-dependent parts of the test case
that appear before and after the results of the superclass method. For example:

```java
    @Test
    public void deleteResource_IdDefined_Is_Yes() {
        ...
        ...
        ...
    }
```

#### writeEpilog ####

This hook method invokes the lifecycle methods that produce the parts of the test program that follow the actual test cases.

#### writeClosing ####

This method writes the closing part of the test program. For example:

```java
}
```

### The TestCaseWriter lifecycle in action ###

To see the role played by the TestCaseWriter in the lifecycle, let's look at an example using the standard `RestAssuredTestCaseWriter`.

#### prepareTestCases ####

This method sets up the TestCaseWriter for the
[request test definition](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/resolver/RequestTestDef.html).

#### writeDependencies ####

This method writes dependencies for the request execution interface. For example:

```java
import java.util.List;
import java.util.Map;
import static java.util.stream.Collectors.*;

import io.restassured.http.Header;
import io.restassured.response.Response;

import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;
```

#### writeDeclarations ####

This method writes declarations for the request execution interface. For example:

```java

    private ResponseValidator responseValidator = new ResponseValidator( getClass());
```


#### writeTestCase ####

This method writes the code for the body of a single test case. For example:

```java
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "Authorization", tcasesApiBasicCredentials())
                .queryParam( "id", "0")
            .when()
                .request( "DELETE", "/resource")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/resource", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/resource", response.statusCode(), responseHeaders( response));
```


#### writeClosing ####

This method write request execution parts that follow test cases. For the `RestAssuredTestCaseWriter`,
this includes the definition of several standard helper methods:

```java

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private static Matcher<Integer> isUnauthorized() {
        return is(401);
    }
    ...
    ...
    ...
```


## What is a TestSource? ##

One of the required inputs to the
[`TestWriter.writeTest`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html#writeTest-S-T-) method
is an instance of the `TestSource` class. A TestSource encapsulates all of the information that has been derived from the OpenAPI
definition in order to generate the test program. That includes not only the
[request test definition](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/resolver/RequestTestDef.html) but also the
[request response definitions](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/test/ResponsesDef.html) used to
validate the results of API requests. In addition, the `TestSource` can act as a filter that limits test generation to specific
API paths or operations.

The `tcases-api-test` command (or the Maven `tcases:api-test` goal) creates a TestSource using the
[TestSource.Builder](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestSource.Builder.html),
based on options provided in the command line. The request response definitions are derived from the OpenAPI definition
using the [getResponsesDef](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/io/TcasesOpenApiIO.html#getResponsesDef-java.io.File-) method.

## What is a TestTarget? ##

One of the required inputs to the
[`TestWriter.writeTest`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html#writeTest-S-T-) method
is an instance of the `TestTarget` class. A TestTarget defines the location and form of generated test program files.
Typically, a TestWriter depends on a designated `TestTarget` subclass that describes attributes of the test program that
are specific to the programming language and test framework supported by the TestWriter. For example, the `JUnitTestWriter` depends on the
[`JavaTestTarget`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/JavaTestTarget.html) class.

The `tcases-api-test` command (or the Maven `tcases:api-test` goal) creates a TestTarget using the
[JavaTestTarget.Builder](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/JavaTestTarget.Builder.html),
based on options provided in the command line. 

## Developer Requirements ##

To include the TestWriter API in your development project, add
[`tcases-openapi`](https://central.sonatype.com/artifact/org.cornutum.tcases/tcases-openapi/overview) as a dependency.

Alternatively, for better testing support, consider adding
[`tcases-cli`](https://central.sonatype.com/artifact/org.cornutum.tcases/tcases-cli/overview) as a dependency.
This makes it possible to run the [ApiTestCommand](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/ApiTestCommand.html)
directly in your IDE.

The following sections describe detailed requirements for implementing TestWriter API components.

### TestWriter requirements ###

Here are the requirements for implementing a new TestWriter.

- **Extend `TestWriter`**

  A TestWriter must be implemented by a subclass of the `TestWriter` class. For a TestWriter that supports a Java-based test framework, consider
  extending the [`JavaTestWriter`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/JavaTestWriter.html) class.

- **Define the standard constructor**

  The standard constructor has a `TestCaseWriter` instance as its single argument and must invoke the `super` constructor. For example:

  ```java
  /**
   * Creates a new JUnitTestWriter instance.
   */
  public JUnitTestWriter( TestCaseWriter testCaseWriter)
    {
    super( testCaseWriter);
    }
  ```

- **Use the `ApiTestWriter` annotation**

  <a name="apitestwriter"></a>
  For a TestWriter to be discovered at runtime by the `tcases-api-test` command (or the Maven `tcases:api-test` goal) the
  class definition must have the [`ApiTestWriter`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/ApiTestWriter.html)
  annotation. For example:

  ```java
  /**
   * Writes Java source code for a JUnit test that executes API requests.
   */
  @ApiTestWriter( name="junit", target="java")
  public class JUnitTestWriter extends AnnotatedJavaTestWriter
    {
    ...
    ...
    ...
    }
  ```

  The `ApiTestWriter` annotation has up to two arguments.

  - `name` (required)

    An identifier string. The `name` of this TestWriter is used in the command line to select this TestWriter for test case generation.
    
  - `target` (optional)

    An identifier string. The `target` of this TestWriter is the `name` of the [TestTarget implementation](#testtarget-requirements) that is required for this TestWriter.
    If omitted, the default is "java", which selects the `JavaTestTarget`.

- **Implement the TestWriter lifecycle**

  A TestWriter class must provide an implementation for all of the abstract [TestWriter lifecycle methods](#the-testwriter-lifecycle).

- **Use environment variables to customize properties**

  Properties that are unique to a TestWriter subclass cannot be initialized by the command line. Instead, such properties must be
  initialized by the standard constructor. To customize these properties at runtime, use well-documented environment variables.
  

### TestCaseWriter requirements ###

Here are the requirements for implementing a new TestCaseWriter.

- **Implement `TestCaseWriter`**

  A TestCaseWriter must implement the `TestCaseWriter` interface. To provide some helpful generic properties,
  consider extending the [`BaseTestCaseWriter`](#using-basetestcasewriter) class.

- **Define the standard constructor**

  The standard constructor is the default no-args constructor.

- **Use the `ApiTestCaseWriter` annotation**

  <a name="apitestcasewriter"></a>
  For a TestCaseWriter to be discovered at runtime by the `tcases-api-test` command (or the Maven `tcases:api-test` goal) the
  class definition must have the [`ApiTestCaseWriter`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/ApiTestCaseWriter.html)
  annotation. For example:

  ```java
  @ApiTestCaseWriter( name="restassured")
  public class RestAssuredTestCaseWriter extends BaseTestCaseWriter
    {
    ...
    ...
    ...
    }
  ```

  The `ApiTestCaseWriter` annotation has one argument.

  - `name` (required)

    An identifier string. The `name` of this TestCaseWriter is used in the command line to select this TestCaseWriter for test case generation.
    
- **Implement the TestCaseWriter lifecycle**

  A TestCaseWriter class must provide an implementation for all of the abstract [TestCaseWriter lifecycle methods](#delegation-to-testcasewriter).

- **Use environment variables to customize properties**

  Properties that are unique to a TestCaseWriter implementation cannot be initialized by the command line. Instead, such properties must be
  initialized by the standard constructor. To customize these properties at runtime, use well-documented environment variables.

### TestTarget requirements ###

Here are the requirements for implementing a new TestTarget.

- **Extend `TestTarget`**

  A TestTarget must be implemented by a subclass of the `TestTarget` class. For a TestTarget that supports a Java-based test framework, consider
  extending the [`JavaTestTarget`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/JavaTestTarget.html) class.

- **Define the standard constructor**

  The standard constructor is the default no-args constructor.

- **Use the `ApiTestTarget` annotation**

  For a TestTarget to be discovered at runtime by the `tcases-api-test` command (or the Maven `tcases:api-test` goal) the
  class definition must have the [`ApiTestTarget`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/ApiTestTarget.html)
  annotation. For example:

  ```java
  /**
   * Defines the target for output from a {@link JavaTestWriter}.
   */
  @ApiTestTarget( name="java")
  public class JavaTestTarget extends TestTarget
    {
    ...
    ...
    ...
    }
  ```

  The `ApiTestTarget` annotation has one argument.

  - `name` (required)

    An identifier string. The `name` of this TestTarget is referenced by the `ApiTestWriter` annotation to select this TestTarget
    for test case generation.

- **Use environment variables to customize properties**

  Properties that are unique to a TestTarget subclass cannot be initialized by the command line. Instead, such properties must be
  initialized by the standard constructor. To customize these properties at runtime, use well-documented environment variables.

## TestCaseWriter Tips ##

The fundamental task of a TestCaseWriter is to produce the test code that prepares an API request message, delivers it to an API server,
and (optionally) validates the resulting response message. This requires a thorough understanding of the OpenAPI definition and the
requirements it defines for serializing test case data. That can be a complicated job! But this section describes 
several utilities provided by Tcases for OpenAPI that can help out.

### Using `IndentedWriter` ###

All of the TestWriter and TestCaseWriter lifecycle methods that write test code use the
[`IndentedWriter`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/io/IndentedWriter.html).
This class provides a simple facade for a standard `PrintWriter` that handles code indentation. The
basic pattern for using `IndentedWriter` looks like this:

* `setIndent( width)`: Initialize the "tab width" for each level of indentation. The default _width_ is 2 spaces.
* `indent()`: Increment the current level of indentation.
* Write a line of code, using one of the following techniques.
  * The easy way
    * `println( string)`: Print a line containing the given _string_ at the current level of indentation.
  * The complicated way
    * `startLine()`: Begin a new empty line at the current level of indentation.
    * `print( string)`: Append the given _string_ to the current line. Repeat as needed to finish the content of the line.
    * `println()`: Append a newline to complete the current line.
* `unindent()`: Decrement the current level of indentation.

### Using `BaseTestCaseWriter` ###

To provide some helpful generic properties, consider extending the
[`BaseTestCaseWriter`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/BaseTestCaseWriter.html)
class. For example, `BaseTestCaseWriter` provides support for [test case dependencies](#test-case-dependencies) and
[data value converters](#data-value-converters) that serialize data values according to a specified
[media type](https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html).

#### Test case dependencies ####

Test case dependencies are boolean flags that indicate the how the generated test code must be prepared. A `BaseTestCaseWriter`
initializes test case dependencies when its `prepareTestCases` method is called. Test case dependencies are returned by the
`getDepends` method in the form of a [`Depends`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/BaseTestCaseWriter.Depends.html)
object.

Some dependencies are initialized according to standard options of the `tcases-api-test` command. For example,
[`trustServer`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/BaseTestCaseWriter.Depends.html#trustServer--)
indicates if generated HTTPS requests must accept an untrusted API server. Other dependencies are derived from the
[request test definition](Request-Test-Definition.md) that is given to `prepareTestCases`. For example,
[`dependsMultipart`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/BaseTestCaseWriter.Depends.html#dependsMultipart--)
indicates if any test cases require data encoded for the "multipart/form-data" media type.

#### Data value converters ####

The OpenAPI definition defines when request input data must encoded as a specific media type. To support these requirements, a `BaseTestCaseWriter`
maintains a mapping that associates a media type with a specific
[`DataValueConverter`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/encoder/DataValueConverter.html)
that serializes a test case `DataValue` as a string. By default, a `BaseTestCaseWriter` comes with converters for common media types
like "\*/\*", "text/plain", and "application/json".


### Understanding `RequestCase` ###

The central method for any TestCaseWriter implementation is `writeTestCase`. And the key input to this method is a
[`RequestCase`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/resolver/RequestCase.html)
object. A `RequestCase` defines all of the information needed to execute a specific API request with specific
test data.

A `RequestCase` defines the parameters and the body of the request, including the
[`DataValue`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/resolve/DataValue.html)
objects that are automatically generated by Tcases for OpenAPI. A `RequestCase` also indicates
if executing this request is expected to produce an authorization failure or some other type of error.

A `RequestCase` also defines properties of the request that are specified in the OpenAPI definition, such as the
API server URI, the location of parameter values, and the encoding styles used to serialize data values.
For a complete description of the semantics of such properties, see the [_OpenAPI Specification_](https://spec.openapis.org/oas/v3.0.2).

### Validating responses ###

By default, the test case code produced by a TestCaseWriter is expected to validate the response received after executing an API
request. This does _not_ mean verifying that a response contains the specific data values expected for the given request inputs. Instead,
response validation entails checking the response status code (i.e. does it indicate an expected failure?) and validating that the
form of the response meets the requirements specified in the OpenAPI definition.

#### Checking the status code ####

The [`RequestCase`](#understanding-requestcase) object specifies if the test case is expected to fail or not.

If
[`isFailure()`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/resolver/RequestCase.html#isFailure--)
is false, then the request is expected to succeed and return a status code in the 2xx range.

Otherwise, if
[`isAuthFailure()`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/resolver/RequestCase.html#isAuthFailure--)
is true, then the request is expected to return a 401 (Unauthorized) status code.

Otherwise, the request is expected to report a API client error by returning a different status code in the 4xx range.


#### Validating response definitions ####

Validating response definitions is optional. For a `BaseTestCaseWriter`, validation is expected if and only if `getDepends().validateResponses()` is
true.

The [TestSource](#what-is-a-testsource) given to the `TestWriter.writeTest` method specifies the _response definitions_ for each API request
defined in the OpenAPI definition. Response definitions are described by a
[`ResponsesDef`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/test/ResponsesDef.html)
object that is returned by the
[`TestSource.getResponses`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestSource.html#getResponses--)
method.

Calling `TestWriter.writeTest` invokes the TestWriter lifecycle. This eventually results in a call to `TestWriter.writeResponsesDef`, which
writes the `ResponsesDef` to a JSON resource file associated with the generated test program file, as described by the
[`TestTarget`](#what-is-a-testtarget). When the generated test program is executed, it must read this resource file to access the requirements
for each response specified by the OpenAPI definition.

The location of the response definition resource file is defined by the TestTarget. By default, this will be a file of form "\*-Responses.json"
in the same directory as the generated test program file.

### Using `RequestCaseUtils` ###

For each test case, a TestCaseWriter must produce code to compose the HTTP message for an API request. This requires an
understanding of the requirements specified by the OpenAPI definition for handling the values for all request inputs.  For each
request input, the `RequestCase` object defines not only the input value to be used but also the OpenAPI properties that
describe where this value must be located and how this value must be serialized.

OpenAPI serialization rules can be complicated, but the
[`RequestCaseUtils`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/RequestCaseUtils.html)
class provides methods to simplify the job.

| Method                  | Description                                                               |
| ---                     | ---                                                                       |
| [`getCookieParameters`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/RequestCaseUtils.html#getCookieParameters-org.cornutum.tcases.openapi.resolver.ParamData-)     | Returns the parameter bindings defined by a cookie parameter              |
| [`getHeaderParameterValue`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/RequestCaseUtils.html#getHeaderParameterValue-org.cornutum.tcases.openapi.resolver.ParamData-) | Returns the value of a header parameter                                   |
| [`getPathParameterValue`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/RequestCaseUtils.html#getPathParameterValue-org.cornutum.tcases.openapi.resolver.ParamData-)   | Returns the value of a path parameter                                     |
| [`getQueryParameters`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/RequestCaseUtils.html#getQueryParameters-org.cornutum.tcases.openapi.resolver.ParamData-)      | Returns the parameter bindings defined by a query parameter               |
| [`getHeaderValue`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/RequestCaseUtils.html#getHeaderValue-org.cornutum.tcases.openapi.resolver.HeaderData-)          | Returns a string representing the value of a header                       |
| [`formUrlEncoded`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/RequestCaseUtils.html#formUrlEncoded-org.cornutum.tcases.openapi.resolver.MessageData-boolean-)          | Returns encodings for the `application/x-www-form-urlencoded` media type  |
| [`toOctetStream`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/RequestCaseUtils.html#toOctetStream-org.cornutum.tcases.resolve.DataValue-)           | Returns a data value for the `application/octet-stream` media type        |


## Testing Tips ##

You can test your new TestWriter or TestCaseWriter by plugging them into an execution of the `tcases-api-test` command. You can
run this command in your Java development environment by using the [CLI](#testing-with-the-cli) or the
[Tcases Maven plugin](#testing-with-maven).

### Testing with the CLI ###

To run the CLI, you must add [`tcases-cli`](https://central.sonatype.com/artifact/org.cornutum.tcases/tcases-cli/overview) as a dependency.
You can then run the [ApiTestCommand](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/ApiTestCommand.html)
directly in your IDE.

To integrate you own TestWriter or TestCaseWriter, use the following command line arguments.

* `-t` _testWriterName_

  To select your new TestWriter, add the `-t` option using the `name` argument from the [`ApiTestWriter`](#apitestwriter) annotation.

* `-e` _testCaseWriterName_

  To select your new TestCaseWriter, add the `-e` option using the `name` argument from the [`ApiTestCaseWriter`](#apitestcasewriter) annotation.

* `-cp` _classPath_

  Use the `-cp` option to specify a _classPath_ containing the directories or JAR files that provide the implementation of your new TestWriter or TestCaseWriter.
  The _classPath_ must follow the same syntax conventions used by the `java` command on your platform.


### Testing with Maven ###

To test using Maven, you must add the [Tcases Maven plugin](https://www.cornutum.org/tcases/docs/tcases-maven-plugin/) to your Maven project.
You can then run tests using the [`tcases:api-test`](http://cornutum.org/tcases/docs/tcases-maven-plugin/api-test-mojo.html) goal.

To integrate you own TestWriter or TestCaseWriter, use the following `tcases:api-test` configuration parameters.

* `-DtestType=`_testWriterName_

  To select your new TestWriter, configure the `testType` using the `name` argument from the [`ApiTestWriter`](#apitestwriter) annotation.

* `-DexecType=`_testCaseWriterName_

  To select your new TestCaseWriter, configure the `execType` using the `name` argument from the [`ApiTestCaseWriter`](#apitestcasewriter) annotation.

* `-Dextensions=`_classPathElements_

  Configure `extensions` to specify the class path elements that provide the implementation of your new TestWriter or TestCaseWriter.
  For example, the following configuration will ensure that the `tcases:api-test` goal uses all of the class files produced by
  building your Maven project.

  ```xml
  ...
  ...
  ...
  <plugin>
    <groupId>org.cornutum.tcases</groupId>
    <artifactId>tcases-maven-plugin</artifactId>
    <version>${tcases.version}</version>
    <executions>
      <execution>
        <id>...</id>
        <phase>...</phase>
        <goals>
          <goal>api-test</goal>
        </goals>
        <configuration>
          <testType>myTestWriter</testType>
          <execType>myTestCaseWriter</execType>
          <extensions>
            <extension>${project.build.outputDirectory}</extension>
          </extensions>
        </configuration>
      </execution>
    </executions>
  </plugin>
  ...
  ...
  ...
  ```
