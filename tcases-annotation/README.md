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

* Minimal for PR
  * negotiate merge / handover / release
  * Feature: Support Conditions AllOf, AnyOf, Not: Annotation vs DSL...
  * Design: New @System annotation to support System output Annotations?
  * Design: Try out SystemDef with Java inheritence for maximum reuse
  * Design: For non-primitives, handle "null" value
  * Todo: Cleanup code
  * Todo: Decide on code style for new module
  * Todo: Full unit tests

* High priority
  * Feature: Allow annotating enum values for varvalue defaults
  * Feature: properly support bean getters/setters (Consider Jackson-databind?)
  * Design: Require each non-static field to have one Tcases annotation (Var, VarSet, TestCaseId, IsFailure)?
  * Feature: Support primitives (boolean, int, char)
  * Design: Provide all testcase data in the output (varvalue properties)?
  * Design: Use better not applicable constants than "NA" (Configurable?)
  * Create more test examples

* Nice to have
  * Design: Generator configuration inside FunctionInputDef (In particular for Combine)?
  * Design: ShortCut to define a Failure value with an annotation (failure = "fileNotFound")
  * Design: ShortCut annotation for Annotations (@Having("foo:bar,foo2:baz2")
  * Design: ShortCut annotation (@SimpleVar(value = "foo;bar;baz", fail = "bam;bim;bum")
  * Design: Value generator method reference for vars with many values.
  * Design: Allows comma-separated String value for value properties?
  * Feature: Define Generators as JUnit TestRule (consider invocation order, or providing seed, for output matching inputs)
  * Feature: @Value annotation attribute for Enum values not to be used for a given @Var
  * Design: Refactor TCases to allow getTests() for FunctionInputDef without SystemInputDef?
  * Feature: Junit4 / Junit5 Test Rules?
  * Feature: Support special non-primitives (Time, date, ..?)
  * Feature: Consider Var ranges for numbers, times, dates, ...?
  * Feature:Support better testcase descriptions/ids than 0..n?
  * Feature: Support defining Generators of actual test values for each testcase Value
  * Modernize: Apply common Java codestyle to tCases / Apply static code checkers
  * Modernize: Migrate Tcases to Gradle
  * Modernize: Move Ant Support  / TCases class to separate modules out of tcases-lib
  * Feature: Throw exceptions for invalid Function annotation combinations
