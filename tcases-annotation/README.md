# tcases-annotation

Java annotation based definition of systems and generation of testcase instances.

## TODO

* Support Conditions AllOf, AnyOf, Not: Annotation vs DSL...
* Define Generators as JUnit TestRule
* Map to Junit4 / Junit5 ParametrizedTests
* Throw exceptions for invalid Function annotation combinations
* Allow annotating enum values for varvalue defaults
* Allows comma-separated String value for value properties?
* properly support bean getters/setters (Consider Jackson-databind?)
* Require each non-static field to have one Tcases annotation (Var, VarSet, TestCaseId, IsFailure)?
* Support primitives (boolean, int, char)
* Support special non-primitives (Time, date, ..?)
* Consider multiple fail cases (E.g. http 400, 404)?
* Consider Var ranges for numbers, times, dates, ...?
* Support better testcase descriptions/ids than 0..n?