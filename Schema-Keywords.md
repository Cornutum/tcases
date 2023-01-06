# Schema Keywords #

## Introduction ##

You can use _schema keywords_ to define the [value classes](Tcases-Guide.md#defining-value-schemas) for a
[variable definition](Tcases-Guide.md#defining-input-variables) in your Tcases
[input model](Tcases-Guide.md#modeling-the-input-space).

Tcases uses a [subset](#compared-to-json-schema) of the standard [JSON Schema Validation vocabulary](https://json-schema.org/draft/2020-12/json-schema-validation.html),
as described in the following sections.

## Generic Keywords ##

* `type`

  The value of this keyword is a string that identifies the type of values in this value class. The `type` must one of the following values.

  * "array"
  * "boolean"
  * "integer"
  * "number"
  * "string"

  The `type` keyword is optional. If undefined, the expected value type will be derived from other schema keywords.

* `const`

  The value of this keyword is single value of any type -- array, boolean, integer, number, string -- or `null`.
  `const` identifies a value class containing exactly one member. If defined, any other type-specific keyword is ignored.

* `enum`

  The value of this keyword is an array of unique values of any type -- array, boolean, integer, number, string -- or
  `null`. All non-null values in the `enum` array must have the same type. `enum` identifies a value class containing only the
  specified members.  If defined, any other type-specific keyword is ignored.

* `format`

  The value of this keyword is a string that identifies a specific form for values in this class. The `format` keyword is
  optional and it can have any value, although certain values have standard definitions. (See [_Supported Formats_](#supported-formats).)


## String Keywords ##

The following optional keywords apply to values of type "string". If present, they imply an expected type of "string" for all
members of this value class.

  * `maxLength`

    The value of this keyword is an non-negative integer that defines an inclusive upper bound for the length of a value
    string. All members of this value class must have a length less than or equal to the given value.

  * `minLength`

    The value of this keyword is an non-negative integer that defines an inclusive lower bound for the length of a value
    string. All members of this value class must have a length greater than or equal to the given value.

  * `pattern`

    The value of this keyword is a string containing a valid regular expression (as defined by the
    [ECMAScript](https://www.ecma-international.org/ecma-262/#sec-patterns) standard). If defined, this keyword
    specifies that all members of this value class must match the given regular expression.
    

## Number Keywords ##

The following optional keywords apply to values of type "number" or "integer". If present, they imply an expected type of "number" for all
members of this value class, unless `"type": "integer"` is also specified.

  * `maximum`

    The value of this keyword is a number that defines an inclusive upper bound for members of this value class, which must be
    less than or equal to the given value.

  * `minimum`

    The value of this keyword is a number that defines an inclusive lower bound for members of this value class, which must be
    greater than or equal to the given value.

  * `exclusiveMaximum`

    The value of this keyword is a number that defines an exclusive upper bound for members of this value class, which must be
    strictly less than the given value.

  * `exclusiveMinimum`

    The value of this keyword is a number that defines an exclusive lower bound for members of this value class, which must be
    strictly greater than the given value.

  * `multipleOf`

    The value of this keyword must be a positive number. If defined, division by this number must produce an integer result for
    all members of this value class.

## Array Keywords ##

The following optional keywords apply to values of type "array". If present, they imply an expected type of "array" for all
members of this value class.

  * `maxItems`

    The value of this keyword is a non-negative integer that defines an inclusive upper bound for the size of arrays in this
    value class.

  * `minItems`

    The value of this keyword is a non-negative integer that defines an inclusive lower bound for the size of arrays in this
    value class.

  * `uniqueItems`

    The value of this keyword is a boolean. If `true`, elements must be unique for any array in this value class. Otherwise, if
    false or undefined, member arrays can contain duplicate elements.

  * `items`

    The value of this keyword is a schema -- an object containing only schema keywords. If defined, each element of an array
    in this value class must belong to the specified value class. Otherwise, array elements can be any value, including `null`.

## Supported Formats ##

Tcases provides special handling for the following values for the `format` keyword. Any other value is accepted but ignored. In any case, the
`format` value is propagated to generated test definitions in the form of a [variable binding annotation](Tcases-Guide.md#variable-binding-annotations).

  * "int32"

    This format applies to values of type "integer". If defined, a member of the value class must be a 32-bit integer.

  * "int64"

    This format applies to values of type "integer". If defined, a member of the value class must be a 64-bit integer.

  * "date-time"

    This format applies to values of type "string". If defined, a member of this class must represent a valid date-time value as
    specified in [RFC3339](https://www.rfc-editor.org/info/rfc3339).

  * "date"

    This format applies to values of type "string". If defined, a member of this class must represent a valid date value as
    specified in [RFC3339](https://www.rfc-editor.org/info/rfc3339).

  * "email"

    This format applies to values of type "string". If defined, a member of this class must represent a valid email address as
    specified in [RFC5321](https://www.rfc-editor.org/info/rfc5321).

  * "uuid"

    This format applies to values of type "string". If defined, a member of this class must represent a valid UUID as
    specified in [RFC4291](https://www.rfc-editor.org/info/rfc4291).



## Compared To JSON Schema ##

In general, Tcases schema keywords conform to the standard [JSON Schema Validation vocabulary](https://json-schema.org/draft/2020-12/json-schema-validation.html),
with the following exceptions.

  * **`object` type not supported**

    In a Tcases input model, a complex input composed of multiple parts is modeled as a [variable set](Tcases-Guide.md#defining-variable-sets).
    Schemas apply only to the values of the individual variable definitions within a variable set. Consequently, there is no place for an
    `object` schema.

  * **Keywords for applying subschemas not supported**

    Keywords that combine multiple subschemas -- such as `allOf`, `anyOf`, `not`, etc. -- or that apply subschemas conditionally
    -- such as `if`, `contains`, etc -- are not supported.

  * **No multi-type value classes**

    Although the JSON Schema standard allows a schema to validate multiple types of value instances, this is not supported in a
    Tcases schema.  The members of a value class must all have the same type. When no `type` keyword is given, the value type is
    defined implicitly by the type-specific keywords that are specified. It is an error to specify keywords that imply different
    types. For example, a schema that defines both `minLength` (which implies the "string" type) and `maxItems` (which implies
    the "array" type) is invalid.

    It's possible for a schema to contain no type-specific keywords, but this is valid only in a default schema supplied by a
    variable definition.  Ultimately, the effective schema for any value definition must specify exactly one type.

  * **`null` is a value, not a type**

    In a Tcases input model, `null` is treated much like it is in Java and other programming languages -- `null` is a special value that can
    be assigned to a value of any type. This is different than the way JSON Schema handles `null` as a distinct type. In Tcases, any input
    variable can be "nullable", depending on how it is modeled.

  * **Not all defined formats are recognized**

    Tcases recognizes [some](#supported-formats) but not all of the
    [`format` values defined by JSON Schema](https://json-schema.org/draft/2020-12/json-schema-validation.html#name-defined-formats).

