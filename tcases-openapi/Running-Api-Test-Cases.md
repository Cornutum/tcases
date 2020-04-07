# Running API Test Cases #

## Contents ##

  - [Overview](#overview)
  - [Generating request inputs](#generating-request-inputs)
    - [Instead of this...](#instead-of-this)
    - [Do this...](#do-this)
    - [How does input resolution work?](#how-does-input-resolution-work)
  
## Overview ##

Ideally, Tcases for OpenAPI would produce a test program that you could immediately run. Ideally, this test program would
execute all API requests against an actual API server, applying a comprehensive set of request input data and automatically verifying
the expected responses. Bam! Job done!

Unfortunately, that is not possible. Consider that any such test program must combine all of the following elements.

  1. The framework for organizing test execution, e.g. JUnit, etc.
  1. The interfaces for submitting HTTP requests and receiving responses
  1. The actual request inputs
  1. The expected response outputs

Automatically generating any of these is complicated. For starters, different people want to do them differently. Also,
automatically generating expected outputs is infeasible -- generally speaking, it is mathematically undecidable.

But Tcases for OpenAPI provides options to automate some of the process. For now this is limited to #3, but look for
support for #1 and #2 in a future release.

## Generating request inputs ##

### Instead of this... ###

By default, Tcases for OpenAPI produces a JSON document that *describes* the input values for each test case, but only in a general way.
It's left for you to choose the actual input values that match these descriptions. For example, the following command demonstrates
the results produced in the default case.

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

### Do this... ###

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

