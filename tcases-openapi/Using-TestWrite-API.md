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
  
  * a [`TestWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestWriter.html) that is
    responsible for producing the code required for a specific test framework,

  * and a [`TestCaseWriter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/TestCaseWriter.html)
    that is responsible for producing the code that uses a specific request execution interface to submit
    API requests and evaluate API responses.

If you want to generate a Java test program, it's likely that Tcases for OpenAPI already has everything you need. Tcases for
OpenAPI has built-in support for the two most common Java test frameworks (JUnit and TestNG) and for a powerful request
execution interface (REST Assured).  But what if you need something different? In that case, you can use the TestWriter API to
add extensions to Tcases for OpenAPI that produce the results you want.

## Get Started ##

#### Java? Nope. I need tests to be written in another language ####

#### JUnit tests are fine, but I want to use a different request execution interface ####

#### Since I'm adding extensions to Tcases, do I have to create my own fork? ####

#### How can I test my own `TestWriter` implementation? ####

#### Can I get the `tcases-api-test` command to use my own `TestWriter`? ####

#### Is there a different way to generate test input values? ####


## The TestWriter Lifecycle ##
