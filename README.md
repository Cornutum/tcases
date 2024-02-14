# Tcases: A Model-Based Test Case Generator #

[![Maven](https://img.shields.io/badge/maven-4.0.5-green.svg)](https://search.maven.org/search?q=tcases-shell)
[![Javadoc](https://img.shields.io/badge/javadoc-4.0.5-green.svg)](https://javadoc.io/doc/org.cornutum.tcases/tcases-shell)

## What's New? ##
  * The latest version ([Tcases 4.0.5](ReleaseNotes.md#405)) is now available at the Maven Central Repository.
    See [*How To Download Tcases*](HowToDownload.md) for download instructions.

  * Having trouble with Tcases? Check out [these tips](./Troubleshooting-FAQs.md).

  * Got a question? Need some guidance? Start a [discussion](https://github.com/Cornutum/tcases/discussions).

## What Does It Do? ##

Tcases is a tool for designing tests. It doesn't matter what kind of system you are testing -- UI, command line,
[REST-ful API](tcases-openapi/README.md#tcases-for-openapi-from-rest-ful-to-test-ful), or backend.  Nor does it matter
what level of the system you are testing -- unit, subsystem, or full system. You can use Tcases to design your tests in any of
these situations. With Tcases, you define the input space for your system-under-test and the level of coverage that you
want. Then Tcases generates a minimal set of test cases that meets your requirements.

Tcases is primarily a tool for black-box test design. For such tests, the concept of "coverage" is different from structural
testing criteria such as line coverage, branch coverage, etc. Instead, Tcases is guided by coverage of the input space of your
system.

Tcases gives you a way to define the input space for your system in a form that is concise but comprehensive. Then Tcases allows
you to control the number of test cases in your sample subset by specifying the level of coverage you want. You can start with a
basic level of coverage, and Tcases will generate a small set of test cases that touches every significant element of the input
space. Then you can improve your tests by selectively adding coverage in specific high-risk areas. For example, you can specify
pairwise coverage or higher-order combinations of selected input variables.

## How Does It Work? ##

First, you create a system input definition, a document that defines your system as a set of functions. For each system
function, the system input definition defines the variables that characterize the function input space. If you are testing a Web
service API, you can even [generate a system input definition automatically](tcases-openapi/README.md#tcases-for-openapi-from-rest-ful-to-test-ful)
from an OpenAPI definition.

Then, you can create a generator definition. That's another document that defines the coverage you want for each system
function. The generator definition is optional. You can skip this step and still get a basic level of coverage.

Finally, you run Tcases. Tcases is a Java program that you can run from the command line or using the
[Tcases Maven Plugin](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/). The command line version of Tcases comes with built-in
support for running using a shell script or an ant target. Using your input definition and your generator definition, Tcases
generates a system test definition. The system test definition is a document that lists, for each system function, a set of test
cases that provides the specified level of coverage. Each test case defines a specific value for every function input
variable. Tcases generates not only valid input values that define successful test cases but also invalid values for the tests
cases that are needed to verify expected error handling.

Of course, the system test definition is not something you can execute directly. (Unless it was
[derived automatically from an OpenAPI definition](tcases-openapi/README.md#how-do-you-run-generated-api-test-cases)!)
But it follows a well-defined schema, which means you can use a variety of transformation tools to convert it into a form that
is suitable for testing your system. For example, Tcases comes with a built-in transformer that converts a system test
definition into a Java source code template for a JUnit or TestNG test class.

## Get Started! ##

  * **The Lowdown**
    * [Tcases: The Complete Guide](./Tcases-Guide.md#tcases-the-complete-guide)
    * [Tcases for OpenAPI](tcases-openapi/README.md#tcases-for-openapi-from-rest-ful-to-test-ful): Testing a REST-ful API? Generate test cases directly from your OpenAPI v3 definition.
    * [The Tcases Maven Plugin](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/)

  * **Helpful Guides**
    * [How To Download Using Maven](HowToDownload.md)
    * [How To Setup a Tcases Web Service](./Tcases-Web-Service.md)
    * [Using The Tcases API](./Using-Tcases-API.md)
    * [Troubleshooting FAQ](./Troubleshooting-FAQs.md#troubleshooting-faqs)
    * [Release Notes](ReleaseNotes.md)

  * **More Info**
    * [Model-Driven Testing Using Tcases](ModelDrivenTestingForAgileTeams.md)
    * Javadoc: [Tcases API](http://www.cornutum.org/tcases/docs/api/index.html)

## Contributors ##

Thanks to the following people, who have contributed significant improvements to Tcases.

  * [Kerry Kimbrough](https://github.com/kerrykimbrough) (project founder)
  * [Juglar](https://github.com/juglar)
  * [Thibault Kruse](https://github.com/tkruse)
