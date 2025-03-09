## Overview ##

When Tcases for OpenAPI generates an executable test for your API, the result is one or more source code files that implement
the test program. You can immediately build this source code and run the test.  To do this, Tcases for OpenAPI has to deal with
all of the issues of constructing an API test program.

* **Which language?** Which programming language are you using to write the test? Java? JavaScript? Some more exotic?

* **Which test framework?** Most test programs are built around a _test framework_ that is used to run test cases and report
  results.  For test developers, such frameworks generally define how to designate individual tests and how to control their
  execution.  Typically, they are general-purpose, equally applicable for all kinds of testing. Depending on the programming
  language, there are usually several test frameworks to choose from.

* **How will tests interact with an API server?** Any API test demands a set of _request execution interfaces_ of some kind.
  These are the interfaces used to construct a request message, deliver it to an API server, and collect the resulting response.
  Some test frameworks come with built-in request execution interfaces. In other cases, many different choices may be available.

Tcases for OpenAPI generates executable tests using the TestWriter API, which brings together the following three elements:

  * A [request test definition](Request-Test-Definition.md) that defines the inputs for request test cases (and that is created
    automatically from an OpenAPI definition via [input resolution](#get-actual-input-values)),
  
  * a [TestWriter](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html) that is
    responsible for producing the code required for a specific test framework,

  * and a [TestCaseWriter](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestCaseWriter.html)
    that is responsible for producing the code that uses a specific request execution interface to submit
    API requests and evaluate API responses.

If you want to generate a Java test program, it's likely that Tcases for OpenAPI already has everything you need. Tcases for
OpenAPI has built-in support for the two most common Java test frameworks (JUnit and TestNG) and for a powerful request
execution interface (REST Assured).  But what if you need something different? In that case, you can use the TestWriter API to
add extensions to Tcases for OpenAPI that produce the results you want.

## Get Started ##

#### Java? Nope. I need tests to be written in another language ####

#### Java tests are fine, but I don't use JUnit or TestNG ####

#### I use JUnit (or TestNG), but I want to use a different request execution interface ####

#### Since I'm adding extensions to Tcases, do I have to create my own fork? ####

### How can I test my own TestWriter implementation? ####

#### Can I get the `tcases-api-test` command to use my own TestWriter? ####

#### Is there a different way to generate test input values? ####


## The TestWriter Lifecycle ##

The work of a TestWriter is carried out via the **TestWriter lifecyle**. This lifecycle consists of a series of steps that
incrementally produce each part of a complete test program.

### Overview ###

The TestWriter lifecycle is an example of the [Template pattern](https://en.wikipedia.org/wiki/Template_method_pattern). The
template is provided by the abstract `TestWriter` class. A TestWriter must be implemented by a subclass of `TestWriter`.

The TestWriter lifecycle is invoked by calling the
[`writeTest` method](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html#writeTest-S-T-).
Each step of the lifecycle is implemented by a `TestWriter` method.  A lifecycle method may be abstract, in which case every
TestWriter implementation must provide its own implementation. Or, in other cases, a lifecycle method may act as a "hook" that a
new TestWriter subclass can choose to override, usually to add new actions before or after invoking the superclass method.
Completely replacing the behavior of a hook method is not recommended.

Here is an overview of the TestWriter lifecycle. Each abstract `TestWriter` lifecyle method is indicated by :small_blue_diamond:.

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

### Delegation to TestCaseWriter ###

A TestWriter delegates part of its job to a TestCaseWriter, which is responsible for producing the code that executes a test case.
Consequently, the TestWriter lifecyle also orchestrates the interplay between TestWriter and TestCaseWriter responsibilities.
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

### The TestWriter lifecycle in action ###

To see how the TestWriter lifecyle works, let's look at an example using the standard `JUnitTestWriter`. This section shows the result produced
by each step of the lifecycle.

#### writeTests ####

This `TestWriter` method invokes the lifecyle, using a specified [TestSource]() and [TestTarget]().

#### prepareTestCases ####

This hook method simply invokes the TestCaseWriter `prepareTestCases` method. Input to this method is the [request test definition]() that
describes the test cases generated from the OpenAPI definition.

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

This hook method simply calls `writeTestCase` for each `RequestCase` in the [request test definition]().

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

### The TestCaseWriter in action ###

To see the role played by the TestCaseWriter in the lifecycle, let's look at an example using the standard `RestAssuredTestCaseWriter`.

#### prepareTestCases ####

This method sets up the TestCaseWriter for the [request test definition]().

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


