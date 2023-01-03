# Running Tcases As A Web Service #

This guide assumes you want to integrate Tcases into a Java-based Web service, submitting requests for Tcases services and getting generated
test cases in response.

## Defining a Tcases Project ##

You can describe all of the elements of Tcases project in a JSON document called a _project definition._. A project definition
contains a [system input definition](Tcases-Guide.md#modeling-the-input-space) (or a reference to its location). Optionally, a
project definition may also contain a [generator definition](Tcases-Guide.md#defining-higher-coverage) and a
[system test definition](Tcases-Guide.md#understanding-tcases-results) that supplies the
[base tests](Tcases-Guide.md#reusing-previous-test-cases) for generating new test cases.

### Defining Elements Directly ###

The simplest possible project definition consists of only a JSON system input definition.

```json
{ 
  "inputDef": { 
    ... 
  } 
} 
```


A more complete project definition could also include a JSON [generator definition](Tcases-Guide.md#defining-higher-coverage) and a set
of JSON [base test case definitions](Tcases-Guide.md#understanding-tcases-results).

```json
{ 
  "inputDef": { 
    ... 
  }, 
  "generators": { 
    ... 
  }, 
  "baseTests": { 
    ... 
  } 
} 
```

### Defining Element Locations ###

Alternatively, any of the elements of a Tcases project can be defined by a URL for the corresponding JSON document.

```json
{ 
  "inputDef": "http://www.cornutum.org/tcases/docs/examples/json/find-Input.json", 
  "generators": "http://www.cornutum.org/tcases/docs/examples/json/find-Generators.json", 
  "baseTests": "http://www.cornutum.org/tcases/docs/examples/json/find-Tests.json" 
} 
```


Document locations can either be absolute URLs or URIs relative to a specified "refBase" property.
A project definition can also contain a mix of actual JSON documents and URL references.

```json
{ 
  "refBase": "http://www.cornutum.org/tcases/docs/examples/json", 
  "inputDef": "find-Input.json", 
  "generators": "find-Generators.json", 
  "baseTests": { 
    ... 
  } 
} 
```

## Running a Tcases Project ##

Given an input stream that reads a JSON project definition, you easily generate project test cases and send the
resulting JSON system test definition document to an output stream, using methods of
the [`TcasesJson`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/TcasesJson.html) class.

```java
TcasesJson.writeTests 
  ( TcasesJson.getTests( projectInputStream), 
    testCaseOutputStream); 
```
