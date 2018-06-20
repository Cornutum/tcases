# tcases-annotation

Java annotation based definition of systems and generation of testcase instances.

## Implemented features

* new Maven module tcases-annotation
* Annotations `@Function`, `@VarSet`, `@Var`, `@Value`, `@Has` similar to System Definition XML elements.
* Annotations `TestCaseId`, ``IsFailure`, `OutputAnnotations` to support extra output XML elements.
* Reader/Creator classes to allow an annotated class to serve both as Function definition and as TestCase
* Support for properties and conditions (But not AllOf, AnyOf, Not yet)
* One testcase equivalent to Tcases tutorial

## TODO

* Urgent
  * Support Conditions AllOf, AnyOf, Not: Annotation vs DSL...
  * Throw exceptions for invalid Function annotation combinations
  * Allow annotating enum values for varvalue defaults
  * properly support bean getters/setters (Consider Jackson-databind?)
  * Require each non-static field to have one Tcases annotation (Var, VarSet, TestCaseId, IsFailure)?
  * Support primitives (boolean, int, char)
  * Try out SystemDef with Java inheritence and composition for maximum reuse
  * Cleanup code
  * Unit tests
  * Provide all testcase data in the output (varvalue properties)?

* Nice to have
  * Allows comma-separated String value for value properties?
  * Define Generators as JUnit TestRule (consider invocation order, or providing seed, for output matching inputs)
  * @Value annotation attribute for Enum values not to be used for a given @Var
  * Map to Junit4 / Junit5 ParametrizedTests
  * Map concepts of arg vs. env to JUnit @Before vs. @Test
  * Support special non-primitives (Time, date, ..?)
  * Consider Var ranges for numbers, times, dates, ...?
  * Support better testcase descriptions/ids than 0..n?
  * Support defining Generators of actual test values for each testcase Value
  * Apply common Java codestyle to tCases / Apply static code checkers
  * Migrate Tcases to Gradle
  * Move Ant Support  / TCases class to separate modules out of tcases-lib
