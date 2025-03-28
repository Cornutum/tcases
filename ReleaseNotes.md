# Release Notes #

## 4.1.0 ##

This release provides the following improvements to Tcases for OpenAPI.

  * **More info for TestWriter and TestCaseWriter authors**

    Want to add support for non-Java test cases or different test frameworks? There's some things you need to know. And now you can find them
    in the new guide to [_Using the TestWriter API_](tcases-openapi/Using-TestWriter-API.md).

  * **TestWriter annotations**

    To support test case generation for different test frameworks, new `TestWriter` implementations using the
    [`ApiTestWriter`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/ApiTestWriter.html)
    annotation can be integrated at runtime.
    Similarly, the
    [`ApiTestCaseWriter`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/ApiTestCaseWriter.html)
    and
    [`ApiTestTarget`](https://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/ApiTestTarget.html)
    annotations can be used to integrate new `TestCaseWriter` and `TestTarget` implementations, respectively. For details, see
    [_Using the TestWriter API_](tcases-openapi/Using-TestWriter-API.md).

  * **Exclude success or failure cases**

    It can be helpful to generate success cases and failure cases separately. For example, you might want to generate success cases
    based on OpenAPI examples but generate random failures based on OpenAPI schemas. (See discussion [318](https://github.com/Cornutum/tcases/discussions/318).)
    To exclude success cases, use the `-xs` option of the `tcases-api-test` command (or `-DexcludeSuccess=true` if using the Maven `tcases:api-test` goal).
    Similarly, to exclude failure cases, use the `-xf` option of the `tcases-api-test` command (or `-DexcludeFailure=true` if using the Maven `tcases:api-test` goal).

  * **Fixed generated base class import** [[313](https://github.com/Cornutum/tcases/issues/313)]

    Yikes! When the base class for the generated test class is in a different package, the generated `import` statement
    was missing a final ';'. But not anymore.
  
  * **Upgraded dependencies**

    Upgraded to swagger-parser 2.1.25.

## 4.0.5 ##

This release provides the following improvements.

  * **ResponseValidator: Rework to handle combined header values** [[293](https://github.com/Cornutum/tcases/issues/293)]

    To validate OpenAPI responses, generated tests now combine all values supplied for each header field name into
    a single comma-separated list. NOTE: To see the effect of this change, you must regenerate your OpenAPI tests.
    
  * **Speed up handling of numeric schemas with extremely large ranges**

    When a numeric schema specifies both `minimum` and `maximum` (i.e. a bounded range), Tcases calculates the number of values
    within the range that will be considered for test case generation, taking into account any `multipleOf` requirements. (This is for
    the purpose of checking consistency with other schema constraints.) Since OpenAPI allows numeric values to be arbitrarily large,
    a bounded range can be enormous! In previous releases, counting up the values for extremely large ranges was so slow that
    Tcases appeared to freeze. This release fixes that problem by speeding up the counting process.

## 4.0.4 ##

This release provides the following improvements.

  * **Decode `DecimalValue` from JSON correctly** [[289](https://github.com/Cornutum/tcases/issues/289)]

    A test case generated from an OpenAPI definition may specify a constant input value that is a JSON-encoded `object` or `array`.
    For example, this can occur when tests are generated from the `example` definitions. This release fixes a defect
    that caused non-integral numbers in such JSON constants to be decoded incorrectly as integers.
    
  * **Maven plugin dependencies**

    This release fixes a problem that caused some dependencies of the Tcases Maven Plugin to be incorrectly excluded.

## 4.0.3 ##

This release provides the following improvements.

  * **Generate "not multiple" failure cases correctly** [[285](https://github.com/Cornutum/tcases/issues/285)]

    When an input value is defined by a schema specifying a `multipleOf` requirement, Tcases will generate a failure
    case using a value that is _not_ a multiple. This release fixes a defect would sometimes cause Tcases to incorrectly omit this
    failure case or to spin forever trying to generate it. 
    
  * **Upgraded dependencies**

    Upgraded to swagger-parser=2.1.20, slf4j-api=2.0.11, logback-classic=1.3.12

## 4.0.2 ##

This release provides the following improvements.

  * **Skip "too large" failure cases for array variables when infeasible** [[276](https://github.com/Cornutum/tcases/issues/276)]

    You can use a [variable schema](./Tcases-Guide.md#defining-variable-schemas) to automatically generate value definitions.
    For a variable with a schema of type `array`, Tcases will normally generate a "too large" failure value definition, using
    an array value that exceeds the specified `maxItems`. But in some cases, this failure is infeasible. For example, consider
    the following definition for the `switches` array.

    ```json
    {
      "system": "MySystem",
      "MyFunction": {
        "arg": {
          "switches": {
            "type": "array",
            "maxItems": 2,
            "uniqueItems": true,
            "items": {
              "type": "boolean"
            }
          }
        }
      }
    }
    ```

    A "too large" failure value would require an array containing three or more boolean values. But such an array is not possible without
    violating the `uniqueItems` condition. Because a failure value must represent single distinct error, the "too large" failure is infeasible.

    In previous versions of Tcases, this situation would cause Tcases to throw an exception and give up. But in this release, Tcases will
    simply skip the infeasible "too large" case and keep going.

  * **Upgraded dependencies**

    Upgraded to swagger-parser 2.1.15.

## 4.0.1 ##

This release provides the following improvements.

  * **Using the Tcases API to generate tests**

    The Tcases command line interface is implemented using a Java API. You can use the same API to do all of the same things in
    your own Java program, including creating a system input definition, generating test cases, and reading or writing Tcases
    documents. For details, see [_Using The Tcases API_](./Using-Tcases-API.md).

    Thanks to [grzegorzgrzegorz](https://github.com/grzegorzgrzegorz) for inspiring improvements to make this API easier to use.

  * **`Tcases::getTests`: Resolve schemas by default**

    The [`Tcases::getTests`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/Tcases.html#getTests-org.cornutum.tcases.SystemInputDef-org.cornutum.tcases.generator.IGeneratorSet-org.cornutum.tcases.resolve.TestCaseResolverFactory-org.cornutum.tcases.SystemTestDef-org.cornutum.tcases.generator.GeneratorOptions-)
    method is the basic interface for generating test cases. It includes an optional
    [`TestCaseResolverFactory`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/resolve/TestCaseResolverFactory.html)
    parameter that defines how test case values are resolved. In this release, the default `TestCaseResolverFactory` uses the
    schemas defined by variable definitions and value definitions to resolve test case values.

  * **`Tcases::getEffectiveInputDef`: Normalize all schemas**

    In this release, the `Tcases::getEffectiveInputDef` method ensures that all schema definitions in the resulting system input
    definition have been "normalized". Schema normalization updates the original schema definition to resolve any
    inconsistencies. For example, a schema that defined both `"minimum": 0` and `"exclusiveMinimum": 1` would be normalized to
    define only `"minimum": 2`. Any change made during schema normalization is reported in a log message.

  * **Upgraded dependencies**

    Upgraded to swagger-parser 2.1.10.

## 4.0.0 ##

With this major release, JSON becomes the preferred format for all Tcases documents. From now on, unless otherwise stated, new
features will be supported only in JSON documents, although existing XML documents will continue to be supported as before.
[_Tcases: The Complete Guide_](./Tcases-Guide.md#tcases-the-complete-guide) has been updated accordingly.

This release also introduces an important new feature: [schema-based value definitions](./Tcases-Guide.md#defining-value-schemas).
Using standard JSON Schema keywords, you have the option to create a more concrete definition of a value class, which Tcases can
use to automatically generate test cases with actual representative values. You can even use a schema to generate all of the
value definitions for a variable automatically.

This release also introduces a new shell command -- `tcases-copy` -- and its Maven counterpart -- `tcases:copy`. You can use
[`tcases-copy`](./Tcases-Guide.md#copying-a-tcases-project) to copy a Tcases project to a new location or to convert it from XML to JSON.


## 3.8.6 ##

This release provides the following improvements to Tcases for OpenAPI.

  * **Generate tests from examples correctly for "composite" schemas** [[255](https://github.com/Cornutum/tcases/issues/255)]

    This release fixes a [problem](https://github.com/Cornutum/tcases/issues/255) when generating API tests using the
    [examples](./tcases-openapi/README.md#how-does-it-work) defined in an OpenAPI v3 definition. In previous versions,
    many valid test cases went missing, especially for any API input defined by a "composite" schema that combines subschemas
    using `oneOf` or `anyOf`. But in this release, all such test cases are generated correctly.

  * **Upgraded dependencies**

    Upgraded to swagger-parser 2.1.8.

## 3.8.5 ##

This release provides the following improvements to Tcases for OpenAPI.

  * **New option to handle untrusted API servers**

    When generated tests perform API requests using HTTPS, the API server is normally required to
    verify that it can be trusted by presenting a valid server certificate. But during testing, this
    might not be possible. For example, the test server might not have a certificate, or it may not
    be possible to verify that the certificate is trusted in the test environment. To handle this
    situation, you can generate tests by running `tcases-api-test` with the new `-V` option (or, if
    using Maven, by running `tcases:api-test` with the `-DtrustServer=true` parameter). This produces
    tests that connect to the API without checking the server certificate.

  * **Better message when schema type is inconsistent or ambiguous**

    Tcases for OpenAPI doesn't accept an OpenAPI definition that contains a [type-ambiguous
    schema](tcases-openapi/README.md#openapi-tips). For example, see [this
    discussion](https://github.com/Cornutum/tcases/discussions/251). But if one is found, you'll now
    see a more helpful error message that identifies the cause of the problem.

  * **Better handling of generated helper methods**

    The source code for generated tests includes certain helper methods, such as `isBadRequest()`, etc. In this
    release, such methods are included only if they are needed -- no more "unused private method" warnings.

  * **Upgraded dependencies**

    Upgraded to swagger-parser 2.1.6.

## 3.8.4 ##

This release provides the following improvements to Tcases for OpenAPI.

  * **Improved resolution for object array input values** [[245](https://github.com/Cornutum/tcases/issues/245)]

    For an API request input value that is an array, a test case definition will explicitly specify
    the data model only for one specific item, with annotations added to describe the values allowed
    for other array items. In previous versions, these annotations were not sufficient to correctly
    generate other array items of type `object`. Consequently, resolution of such input values
    produced invalid array items or (when items must be unique) resolution failures. But this release
    improves the resolution of object array values to avoid such failures.

  * **Handle resource directory correctly for response definitions** 

    For the `tcases-api-test` command, the `-d` option (or for the `mvn tcases:api-test` command,
    the `resourceDir` parameter) that is used to specify the location of API response definitions
    is now handled correctly.

  * **Upgraded dependencies**

    Upgraded to swagger-parser 2.1.5.
    
## 3.8.3 ##

This release provides the following improvements to Tcases for OpenAPI.

  * **Bearer tokens should not be base64-encoded** [[242](https://github.com/Cornutum/tcases/issues/242)]

    Tests generated by Tcases for OpenAPI contain code to handle credentials for the authorization scheme defined in the
    OpenAPI definition. Because of an ambiguity in the specification for OAuth bearer tokens, tests generated in previous
    releases incorrectly encoded user-specified bearer tokens using Base64. But in this release, no such encoding is applied
    to bearer tokens.

## 3.8.2 ##

This release provides the following improvements to Tcases for OpenAPI.

  * **Handle parameter and body input encodings correctly when "realizing" test cases**

    To avoid duplicate test cases, the `tcases-api-test` command (and, for Maven, the `tcases:api-test` goal) eliminates test
    cases that can't be uniquely "realized". For example, such duplicates can occur when null, empty, or undefined values are
    the same when serialized as strings. (See issue #[133](https://github.com/Cornutum/tcases/issues/133).) This release updates
    the "realization" logic to correctly identify duplicates, in particular when considering the encodings specified for media
    types like `application/x-www-form-urlencoded`.

  * **Handle null vs. empty inconsistencies correctly**

    Inconsistent test cases can emerge when null and empty values are serialized identically. (For details, see issue
    #[201](https://github.com/Cornutum/tcases/issues/201).)  This release now correctly resolves any such inconsistency with a
    warning, including for object values.

  * **Handle LWS in media types correctly** [[237](https://github.com/Cornutum/tcases/issues/237)]

    This changes fixes a defect in recognizing media type definitions, which can appear in OpenAPI definitions of
    requests and responses and also in the `Content-Type` headers received from API servers. Media type definitions
    containing "linear whitespace" (LWS) are now correctly decoded.

  * **Upgraded dependencies**

    Upgraded to swagger-parser 2.0.33.


## 3.8.1 ##

This release provides the following improvements to Tcases for OpenAPI.

  * **Apply the encodings specified for the properties of request/response content**

    For certain media types that describe complex objects, an OpenAPI definition can specify different serialization
    [encodings](https://spec.openapis.org/oas/v3.0.2#media-type-object) for each object property. For example, encodings can be
    defined for the properties of a request body or a response body that uses the `application/x-www-form-urlencoded` or
    `multipart/form-data` media types.

    Tcases for OpenAPI now recognizes these encodings and applies them when generating API test cases. Request body content is
    generated correctly for both `application/x-www-form-urlencoded` or `multipart/form-data` media types.

    Generated tests now also recognize these encodings when validating responses. Encoded response body content is correctly
    validated for the `application/x-www-form-urlencoded` media type. (Validation of `multipart/form-data` response body content
    is not yet supported.)

  * **Options for handling `writeOnly` properties in response content** [[232](https://github.com/Cornutum/tcases/issues/232)]

    According to the [OpenAPI spec](https://spec.openapis.org/oas/v3.0.2#fixed-fields-19), when response content is defined by
    an object schema, any object properties that are designated as `writeOnly` "MAY be sent as part of a request but SHOULD NOT
    be sent as part of the response." So is a response containing a value for a `writeOnly` property considered invalid?
    Normally, yes. But this release provides an option to [skip that check](tcases-openapi/Running-Api-Test-Cases.md#handle-writeonly-property-validation).

  * **`spaceDelimited` and `pipeDelimited` serialization styles supported for object values** [[230](https://github.com/Cornutum/tcases/issues/230)]

    These serialization styles are now correctly supported for object values in requests and responses.

  * **Names altered in the input model are recovered in generated test cases** [[227](https://github.com/Cornutum/tcases/issues/227)]

    Variable names in the Tcases input model generated from an OpenAPI definition are often derived from, but different than,
    names found in the API definition. In such cases, a derived Tcases-compatible identifier is used as the variable name, with
    the original name attached as an annotation so that it can be recovered in generated test cases. This release no longer
    fails to recover the original names for headers and object properties.


## 3.8.0 ##

This release introduces an important new feature for Tcases for OpenAPI: response validation. By default, generated tests will
now include code that checks the response data returned by API requests and verifies that all response requirements defined in the
OpenAPI definition are satisfied.

  * **Backward compatibility**

    Because this change alters the behavior of generated tests and introduces new runtime dependencies, it is not strictly
    compatible with previous releases. But if you want to maintain the previous behavior, you can use new command options
    to [exclude response validation](tcases-openapi/Running-Api-Test-Cases.md#example-exclude-response-validation).

  * **New test dependencies**

    Generated tests that validate response requirements (the default) now depend on classes defined in the `tcases-openapi-test`
    JAR. You must ensure that this JAR appears on the class path when compiling or executing these tests. For details,
    see [*Set up test dependencies*](tcases-openapi/Running-Api-Test-Cases.md#set-up-test-dependencies).

  * **New test resources**

    Generated tests that validate response requirements must have access to the OpenAPI response definitions. Therefore, for
    each generated test class, Tcases for OpenAPI now generates a corresponding *response definition file* that the test must
    access as a resource at runtime. For details, see
    [*Manage test resources*](tcases-openapi/Running-Api-Test-Cases.md#manage-test-resources).



## 3.7.2 ##

This release provides the following improvements to Tcases for OpenAPI.

  * **Handle empty and invalid `content` definitions**

    Various elements of an OpenAPI definition can be described by a `content` property that defines one or more data media
    types. For example a `content` property can be used to describe a request parameter, a request body, or a request
    response. Depending on the situation, the `content` must conform to certain rules. For example, a response `content` can
    define multiple media types, but a parameter `content` must define exactly one media type. Tcases for OpenAPI will now
    correctly verify conformance with all these rules.

  * **Handle `content` media types that define no schema**

    Some `content` media types represent unstructured data that can't be described in an OpenAPI definition, in which case the
    `schema` property is undefined. Tcases for OpenAPI now correctly accepts such media type definitions but reports a
    [warning](tcases-openapi/Running-Api-Test-Cases.md#content-no-schema).

## 3.7.1 ##

This release provides the following improvements to Tcases for OpenAPI.

  * **Avoid conflicting test cases for null vs. empty values** [[201](https://github.com/Cornutum/tcases/issues/201)]

    Depending on how a request parameter is serialized, a null value and an empty value can take the same form in the serialized
    request message. This is problematic if the parameter schema dictates that a null value is invalid but an empty value is OK,
    or vice versa. But Tcases for OpenAPI will now adjust the request input model to eliminate such conflicting test cases.

  * **Request parameters serialized correctly**

    The [encoders](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/openapi/testwriter/encoder/UriEncoder.html) used
    when generating executable tests to serialize parameter input values now correctly implement the rules defined
    by [the OpenAPI Specification](https://spec.openapis.org/oas/v3.0.2#style-values) for each parameter `style`. In particular,
    this entails changes to the serialization of `null` and empty values.

    
## 3.7.0 ##

This release provides the following improvements to Tcases for OpenAPI.

  * **Generated tests support request authorization** [[115](https://github.com/Cornutum/tcases/issues/115)]

    Tcases for OpenAPI now handles information in OpenAPI definitions about the [security
    schemes](https://spec.openapis.org/oas/v3.0.2#security-scheme-object) required by API requests. Some types of security
    schemes are not yet supported -- see [*OpenAPI tips*](tcases-openapi/README.md#openapi-tips) for details.

    Because security requirements are now included in generated input models and request test definitions, the
    `RestAssuredTestCaseWriter` can create tests that apply the required authorization credentials to request invocations. Note
    that values for authorization credentials do not appear in the OpenAPI definition and must be injected at runtime when
    generated tests execute. See [*Define credentials for request authorization*](tcases-openapi/Running-Api-Test-Cases.md#define-credentials-for-request-authorization)
    for details.

## 3.6.3 ##

This release provides the following improvements to Tcases for OpenAPI.

  * **More control over the target API server** [[192](https://github.com/Cornutum/tcases/issues/192)]

    In previous releases, tests generated by Tcases for OpenAPI always submit requests to the first API server listed in the
    `servers` property defined by the OpenAPI definition. But it's reasonable to expect that the `servers` list could contain
    different URIs for different test environments. In which case, it would be helpful to have a way to select a specific server
    from the list.  It's also possible that the server needed to run the generated test does not appear in the `servers` list at
    all. In which case, it would be helpful to have an option to specify the server URI directly.

    This release introduces new options for controlling the base URI for the target API server, either
    [when tests are generated](tcases-openapi/Running-Api-Test-Cases.md#example-select-the-api-server-used-by-generated-tests)
    or when [tests are executed](tcases-openapi/Running-Api-Test-Cases.md#override-the-default-api-server).

    Note: To apply these new features, test source code must be regenerated with this release.

  * **Consistent standard terminology**

    For consistency with standard terminology, all code and documentation files have been revised to replace deprecated terms
    (e.g. "OpenAPI spec", "OpenAPI document") with standard terms (e.g. "OpenAPI definition").

    
## 3.6.2 ##

This release resolves the following issues for Tcases for OpenAPI.

  * **Circular references in schemas** [[177](https://github.com/Cornutum/tcases/issues/177)]

    For any element of an OpenAPI definition, a schema can be defined by a reference to another schema definition, using the
    `$ref` keyword. Consequently, it's possible for a schema to reference itself -- either directly or indirectly -- through one
    of its components, such as one of the `properties` of an object or the `items` of an array.  Unfortunately, an API
    definition containing such circular references is big trouble for Tcases for OpenAPI. For an explanation of why this
    situation is not supported, read the comments for this issue [here](https://github.com/Cornutum/tcases/issues/177).

    In previous releases, when you try to generate tests for such an API definition, you enter an infinite loop that ultimately
    leads to a StackOverflowError. But in this release, Tcases for OpenAPI reports a failure with a more helpful description of the
    circular reference.

  * **Handle when input `media-type` contains wildcards**

    `tcases-api-test` (and, for Maven users, the corresponding `tcases:api-test` goal) can now generate tests for API requests
    when input `media-type` definitions use wild cards -- for example `text/*`, `application/*+json`, etc.

## 3.6.1 ##

This release fixes the following issues.

  * **Duplicate variable/value name defined for media types** [[178](https://github.com/Cornutum/tcases/issues/178)]

    This issue concerns the input model created by Tcases for OpenAPI for any `content` object in an OpenAPI definition. This
    portion of the input model defines variables for each of the media types for the `content`. The names of these variables and
    their values must be identifiers, which are generated from the media type name. But in some cases, the same identifier could
    be produced for two different media types.  A change to the form of the generated identifier now ensures that
    unique values are produced for each media type.

  * **Upgraded dependencies**

    Upgraded to swagger-parser 2.0.26.

## 3.6.0 ##

This release adds new options for Tcases for OpenAPI to improve the quality of generated executable tests.

  * **Test method timeout** [[171](https://github.com/Cornutum/tcases/issues/171)]

    Each test method submits a single API request, but it's possible for a faulty request to run forever.  To handle this
    situation, you can specify a timeout that defines the maximum time (in milliseconds) to complete an individual test
    method. If a method continues past this time limit, a test failure occurs. When running `tcases-api-test` from the command
    line, use the `-u` option (for details, run `tcases-api-test -help`). When using Maven to run `tcases:api-test`, use
    the `timeout` option (for details, run `mvn tcases:help -Dgoal=api-test -Ddetail=true`). 
    
  * **Generate a separate test file for each API resource path** [[168](https://github.com/Cornutum/tcases/issues/168)]

    By default, Tcases for OpenAPI creates a single test source file that contains the test cases generated for all resource
    paths defined by the OpenAPI definition. But that can be unwieldy for an extensive API that defines a large number of
    endpoints. To better deal with this situation, you have the option to generate multiple test source files, each containing
    the test cases for a single API resource path. When running `tcases-api-test` from the command
    line, use the `-S` option (for details, run `tcases-api-test -help`). When using Maven to run `tcases:api-test`, use
    the `byPath` option (for details, run `mvn tcases:help -Dgoal=api-test -Ddetail=true`). 

## 3.5.1 ##

This release provides improvements to Tcases for OpenAPI when generating API tests using the
[examples](./tcases-openapi/README.md#how-does-it-work) defined in an OpenAPI v3 definition.  Specifically, request test cases are
generated even if example data is not defined for some request input data items. When example data is not available,
request inputs are generated using the default input model source (i.e. based on schema definitions).

## 3.5.0 ##

This release introduces a new capability to Tcases for OpenAPI: generating API tests using the
[examples](./tcases-openapi/README.md#how-does-it-work) defined in an OpenAPI v3 definition.  You can do this from the command line,
using the `-X` option, or with the [Tcases Maven Plugin](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/), using the
`-Dsource=examples` option.

Other improvements:
    
  * **CLI**

    * Want to report a problem with your input model but unable to share proprietary information?
      Introducing the [Tcases Anonymizer](./How-To-Anonymize.md#how-to-anonymize-your-input-model).
    
  * **Tcases For OpenAPI**

    * Fixes a defect that produced inconsistent test cases for requests with path parameters or non-nullable query
      parameters when the value is a simple string. [[154](https://github.com/Cornutum/tcases/issues/154)]

    * Upgraded to swagger-parser 2.0.24

  * **Core**

    * Fixes a defect that caused Tcases to incorrectly claim that a required value combination is [infeasible](./Troubleshooting-FAQs.md#infeasible).
      In previous versions, this problem could arise when generating tests using high-level combination coverage
      requirements (2-tuples, 3-tuples, etc) for an input model with complex conditions.

  * **Other**

    * When a system input definition is written as an XML document, variable and value conditions are correctly represented using
      a "whenNot" attribute when possible. [[62](https://github.com/Cornutum/tcases/issues/62)]

    * Troubleshooting tips are now consolidated in a new [Troubleshooting FAQ](./Troubleshooting-FAQs.md).
  
## 3.4.2 ##

This release provides the following improvements:

  * **CLI**

    * Unix shell commands simplified by consolidating common steps using the `tcases-exec` command.

  * **Tcases for OpenAPI**

    * Request test case resolution now produces values that satisfy specified "pattern" assertions.
      [[104](https://github.com/Cornutum/tcases/issues/104)]

    * Request test case resolution now produces values that correctly satisfy specified "multipleOf" assertions.
    
    * In certain cases, request test case resolution now produces more useful test cases for "string" and "array" inputs.
      For example, for a "string" that defines a "maxLength" but no "minLength", generated success cases will supply either
      an empty string, a string with the maximum length, or a string with some random length less than the maximum.
      But, in previous versions, that random length might be 0, often producing a duplicate test case. The current
      version fixes that to ensure random lengths are non-zero. The same fix applies also to "array" values that define a
      "maxItems" but no "minItems".
    
    * At least one test case is created for each request defined, even for a request that has no parameters or request body.
      [[132](https://github.com/Cornutum/tcases/issues/132)]

    * `api-test` now produces only executable test cases that are uniquely "realizable". Previously, some test cases
      turned out to be either duplicates or infeasible because of the way inputs were serialized into HTTP messages.
      For example, a query parameter value of `null` is indistinguishable from an empty string. Similarly, a test case
      to verify a failure when an query parameter expecting a string value is given a non-string is not actually
      realizable when all inputs are serialized as simple strings. [[133](https://github.com/Cornutum/tcases/issues/133)]

    * Upgraded to latest swagger-parser version 2.0.21.

  * **Core**

    * No more bogus warnings about unused properties for properties that are referenced only by cardinality conditions.

## 3.4.1 ##

This is a patch release to fix the following problems.

  * **tcases-maven-plugin**

    * Fixes a brazen defect in how the `tcases:api-test` goal handles the `paths` and `operations` parameters, which caused the
      default configuration (all request paths, all request operations) to be incorrectly interpreted as "no paths, no operations"!
      [[124](https://github.com/Cornutum/tcases/issues/124)]

 * **CLI**

   * Shell commands now work correctly when the Tcases installation directory path contains space characters.
     [[116](https://github.com/Cornutum/tcases/issues/116)]
   
   * Shell command help:
     A new implementation of the Tcases CLI provides better command line help information. All commands now use the same interface
     (the `-help` option) to display full command line details in a consistent format on both UNIX and Windows platforms. Also, for
     each command, help information is now defined in a single location, where it is more easily kept accurate and up-to-date.
     [[122](https://github.com/Cornutum/tcases/issues/122)]

     This change is fully compatible with previous CLI versions, with one exception: when running in the Cygwin environment, UNIX shell
     commands no longer automatically convert file name arguments to the preferred `--mixed` format. If you encounter trouble with
     file names of the form `/cygdrive/c/...`, try converting them using the `cygpath --mixed` command.

## 3.4.0 ##

This release introduces an important new capability for Tcases for OpenAPI: generating an executable test directly from an OpenAPI v3 definition.
You can do this from the command line, using the new `tcases-api-test` command, or with the
[Tcases Maven Plugin](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/), using the new `tcases:api-test` goal. For this release,
these commands can automatically create Java tests, runnable with either JUnit or TestNG, that execute all requests using the REST Assured DSL.
For all the details, see [*Running API Test Cases*](tcases-openapi/Running-Api-Test-Cases.md#running-api-test-cases).

Other improvements:
  
  * **Tcases for OpenAPI**
    * For request parameters containing a `string` value, the string `format` may limit the range of valid lengths. Generated
      input models now recognize such `format` constraints, and generated test cases cover both valid and invalid lengths.

    * For request parameters containing a `string` value, input resolution now produces strings that conform to the character set restrictions
      for the location specified by the parameter `in` property.

    * Input resolution now correctly handles failure test cases for invalid `readOnly` properties in request inputs.
    
    * A new number format is used in generated identifiers. Some values in an OpenAPI definition must be converted into identifiers -- for example,
      to designate input variables in the generated input model or Java methods in generated test code. In the new identifier format,
      numeric values are converted by replacing '-' with 'm' and replacing '.' with 'd'. For example, converting "-123.45" will
      now produce the identifier "m123d45".
  
  * **Tcases Core**
    * `TestCase` now has an optional `name` property that provides a descriptive name for the test case. For `TestCase` instances
       generated by Tcases, the `name` is a description of the tuple that this test case was created to cover.


## 3.3.0 ##

This release delivers significant improvements to Tcases for OpenAPI.

  * **Boolean combinations of schemas**: For API inputs defined by boolean combinations of subschemas (using assertions like
  `allOf`, `oneOf`, etc.), defining the proper input space model has always been problematic. In previous versions, the results
  have been complicated, often inconsistent, and hobbled by several limitations. But in this release, a new implementation
  produces much better results. In particular, all restrictions on the content and nesting of `not` assertions have been
  eliminated.

  * **Generating request inputs**: Can Tcases for OpenAPI automatically generate an executable test program? No, not yet. But
  this release starts the journey and takes the first step down that road. This release adds options for generating a ["request
  test definition"](tcases-openapi/Request-Test-Definition.md#request-test-definition-guide) that
  ["resolves"](tcases-openapi/Running-Api-Test-Cases.md#generating-request-inputs) the model of each test case input into an
  actual data value.

  * **Other improvements**: Fixes for issues
  [85](https://github.com/Cornutum/tcases/issues/85),
  [96](https://github.com/Cornutum/tcases/issues/96),
  [100](https://github.com/Cornutum/tcases/issues/100)

  * **Compatibility**: In general, this release is compatible with previous versions, with one key exception. Because
  of the large-scale improvements listed above, the input models generated by Tcases for OpenAPI, while
  functionally equivalent to previous releases, are significantly different in form and structure. So, too, are
  the test models they produce. But unless your usage has dependencies on these details, you should not see any
  difference in how things work.

## 3.2.1 ##

This release enhances the interfaces for Tcases for OpenAPI by adding support for [transforming generated test cases using
XSLT](tcases-openapi/README.md#transforming-generated-test-cases), using the same command options used for basic Tcases.

## 3.2.0 ##

  * **Tcases Maven Plugin**: You can run [Tcases for OpenAPI](tcases-openapi/README.md) using the new `tcases:api` goal.
    See the [plugin documentation](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/) for details.

  * **`tcases` CLI**: Test case output transformations to JUnit (`-J` option) and HTML (`-H` option) work even when default output format would be JSON.


  * **`tcases-api` CLI**: Standard transformations to JUnit (`-J` option) or HTML (`-H` option) are supported for generated API test cases.

## 3.1.2 ##

  * **[Tcases for OpenAPI](tcases-openapi/README.md)**: For more reliable translation of OpenAPI definitions, upgraded to use
  [`swagger-parser`](https://github.com/swagger-api/swagger-parser) version 2.0.14.
  
## 3.1.1 ##

This release adds several improvements to [Tcases for OpenAPI](tcases-openapi/README.md).

  * **Support for `not` schemas**: The effects of `not` assertions are now applied to the input models and test cases generated,
  with some caveats and limitations. See [OpenAPI tips](tcases-openapi/README.md#openapi-tips) for details.

  * **`memberValidated` annotations**: For API inputs defined by boolean combinations of subschemas (using assertions like `allOf`, `oneOf`, etc.),
  test cases are generated to cover all of the cases where one or more subschemas are not validated. To clarify which subschema failed to
  validate and why, a `memberValidated: false` annotation is added to the corresponding failure value choice.

  * **Accumulated string pattern matches**: A `string` schema defined using boolean combinations of subschemas (using assertions like `allOf`, `oneOf`, etc.)
  can accumulate multiple `pattern` assertions that must all be (not) matched for validation to be successful. Tcases for Open API now generates
  input models and test cases to cover all accumulated `pattern` assertions.

  * **`tcases-api` command fixes**: Some problems handling certain input files have been repaired.
  
  
## 3.1.0 ##

This release add two new capabilities to Tcases.

  * **[Cardinality conditions](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm#cardinalityConditions)**:
  This new family of conditions allows you to define when a value or a variable can be used based on
  how many times a certain property appears in a test case.

  * **[Tcases for OpenAPI](tcases-openapi/README.md)**: Use Tcases to automatically generate test cases for your REST-ful API,
  based on an OpenAPI v3 definition.

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

Another major change is that Tcases APIs have been reorganized into multiple JARs. The improvement in modularity allows API users to
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
    * Let's show some love for unit tests, too! For most Tcases entities, there is now a `*Matcher` class that enables more powerful test
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
    see the **Compatibility** notes for details. This also changes how "not applicable" bindings are represented in the API.

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
