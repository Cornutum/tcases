# Request Test Definition Guide #

This guide explains the elements of a JSON document representing a *request test definition*. A request test definition lists
test case inputs for all of the API requests defined in an OpenAPI v3 definition. You can generate the request test
definition for an OpenAPI definition by running Tcases for OpenAPI with [options for input
resolution](Running-Api-Test-Cases.md#generating-request-inputs).

The JSON schema for a request test definition document is located at
[http://www.cornutum.org/tcases/request-cases-schema.json](http://www.cornutum.org/tcases/request-cases-schema.json).

Note: Required fields are designated by the :registered: symbol. All other fields are optional.

## `RequestTestDef` ##

A request test definition is an array of [`RequestCase`](#requestcase) objects that lists the test cases for all API requests.

## `RequestCase` ##

A `RequestCase` object defines the inputs for a test case that invokes a specific API request.

| Field         | Type |                    | Description |
| ---           | ---  | ---                | ---         |
| id            | integer | :registered:    | An id number that uniquely identifies this test case among all tests for the given path and operation |
| path          | string | :registered:     | The request path |
| operation     | string | :registered:     | The HTTP operation for this request |
| api           | string | :registered:     | The title of the API that defines this request |
| name          | string |                  | A descriptive name for this test case |
| server        | string |                  | The API server URL |
| version       | string |                  | The version of the OpenAPI definition that defines this request |
| parameters    | \[[`ParamData`](#paramdata)\] |     | The parameter values for this request |
| body          | [`MessageData`](#messagedata) |   | The body value for this request |
| auth          |\[[`AuthDef`](#authDef)\] |   | The authentication inputs required to authorize this request |
| invalidInput  | string |                  | If defined, a failure response is expected for this request and this string describes the invalid input |
| authFailure   | boolean |                 | If true, an authorization failure is expected for this request |

## `ParamData` ##

A `ParamData` object specifies the name, value, and encoding of a request parameter. A `ParamData` object is the same as a `MessageData` object,
with the following additional fields.

| Field         | Type |                            | Description |
| ---           | ---  | ---                        | ---         |
| name          | string  | :registered:            | The name of this parameter |
| in            | string | :registered:             | The location of this parameter |
| style         | string | :registered:             | Defines how this parameter value will be serialized |
| explode       | boolean             |             | For parameters of type array or object, specifies if elements will be serialized in exploded form |

## `MessageData` ##

A `MessageData` object specifies the value and encoding of a request input data item. 

| Field         | Type |                            | Description |
| ---           | ---  | ---                        | ---         |
| data          | [`DataValue`](#datavalue) | :registered:    | Defines the value of this data item |
| valid         | boolean | :registered:            | Defines if the value for this data item is valid or invalid (i.e. failure response expected) |
| mediaType     | string |                          | The media type used to represent the value of this data item |
| encodings     | [`Encodings`](#encodings) | | When `mediaType` is `application/x-www-form-urlencoded` or `multipart/form-data`, maps each `data` object property to its encoding settings |

## `DataValue` ##

A `DataValue` object specifies the type, format, and value of a request input data item.

| Field         | Type |                    | Description |
| ---           | ---  | ---                | ---         |
| type          | string  | :registered:    | The type of this data value |
| value         | JSON value | :registered: | The JSON representation of this data value |
| format        | string |                  | The format of this data value |

## `Encodings` ##

An `Encodings` object maps each `MessageData` object property to its encoding settings. Specified only for `MessageData` with
`application/x-www-form-urlencoded` or `multipart/form-data` media type.

| Field         | Type |                    | Description |
| ---           | ---  | ---                | ---         |
| *(propertyName)* | [`EncodingData`](#encodingdata) | | The encoding settings for this `MessageData` object property |

## `EncodingData` ##

An `EncodingData` object defines the encoding settings for a `MessageData` object property.

| Field         | Type |                    | Description |
| ---           | ---  | ---                | ---         |
| style | string | | For a property of an `application/x-www-form-urlencoded` object, defines how this property value will be serialized |
| explode | boolean | | For a property of an `application/x-www-form-urlencoded` object, specifies if this property value will be serialized in exploded form |
| contentType | string | | For a property of a `multipart/form-data` object, specifies the media type for this form part |
| headers | \[[`HeaderData`](#headerdata)\] | | For a property of a `multipart/form-data` object, specifies the headers for this form part |

## `HeaderData` ##

A `HeaderData` object specifies the name, value, and encoding of a request header. A `HeaderData` object is the same as a `MessageData` object,
with the following additional fields.

| Field         | Type |                            | Description |
| ---           | ---  | ---                        | ---         |
| name          | string  | :registered:            | The name of this header |
| explode       | boolean             |             | For headers of type array or object, specifies if elements will be serialized in exploded form |

## `AuthDef` ##

An `AuthDef` object describes a required authentication input: one of [`ApiKeyDef`](#apiKeyDef), [`HttpBasicDef`](#httpBasicDef), or [`HttpBearerDef`](#httpBearerDef).

## `ApiKeyDef` ##

An `ApiKeyDef` object specifies that an API key is required to authorize the request.

| Field         | Type |                            | Description |
| ---           | ---  | ---                        | ---         |
| type          | string  | :registered:            | "apiKey" |
| location      | string | :registered:             | The location of the key value |
| name          | string | :registered:             | The name of this key value |

## `HttpBasicDef` ##

An `HttpBasicDef` object specifies that an HTTP Basic authorization header is required.

| Field         | Type |                            | Description |
| ---           | ---  | ---                        | ---         |
| type          | string | :registered:             | "http" |
| scheme        | string | :registered:             | "basic" |

## `HttpBearerDef` ##

An `HttpBearerDef` object specifies that an HTTP Bearer authorization header is required.

| Field         | Type |                            | Description |
| ---           | ---  | ---                        | ---         |
| type          | string | :registered:             | "http" |
| scheme        | string | :registered:             | "bearer" |
