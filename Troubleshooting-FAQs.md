# Troubleshooting FAQs #

  * **Tcases keeps running and never finishes! WTF?**

    You have probably over-constrained your input model, making it difficult or impossible to generate test
    cases that satisfy all of the constraints. But that won't stop Tcases from trying until it has eliminated
    all possible combinations. Which, in some cases, can take a *very* long time. Tcases is not frozen -- it's
    busy with a long, exhaustive, and possibly fruitless search.

    This situation can occur especially when using [higher coverage
    levels](./Tcases-Guide.md#defining-higher-coverage) (2-tuples, 3-tuples, etc), in
    which case some [infeasible combinations](#infeasible) may be expected.

    To fix this, check the conditions and properties in your model until you find the problem. You may find it
    helpful to [set the logging level](#logging) to `DEBUG` or `TRACE`.

    If you're using higher coverage levels, try running a quick check using only the [default
    coverage](#defaultCoverage). If there is an infeasible value, this check can sometimes show you the
    error. You can also try adjusting the variables included in a combination group to avoid infeasible
    combinations.


  * **How can I find out which value combinations are infeasible?**<A name="infeasible"/>

    If you've defined constraints that make some intended value combination impossible, we say that
    combination is "infeasible".

    For "combinations" of a single variable (the default coverage level), this is an error, and you must fix
    the offending constraints before Tcases can continue. Tcases prints an error message identifying the
    infeasible value.
    
    For combinations of 2 or more variables (2-tuples, 3-tuples, etc.), some infeasible combinations may be
    expected, so Tcases will simply [log a warning](#logging) and keep going. Here's what the warning for an infeasible
    combination looks like:

    ```
    11:48:16.685 WARN  o.c.t.generator.TupleGenerator - Can't create test case for tuple=Tuple[[patterns=none, patternsInLine=one]]
    ```


  * **How can I see more details about what Tcases is doing?**<A name="logging"/>

    Tcases uses the [Logback](http://logback.qos.ch/) system for producing a log of its actions. By default,
    log messages are written to a file named `tcases.log` in the current working directory, although you can
    redirect them to standard output by using the `-l stdout` option.

    The default logging level is `INFO`, which shows only basic progress and error information.  To see more
    details, change the logging level to `DEBUG` using the `-L` option.  To see even more details, change the
    logging level to `TRACE`.


  * **How can I run a quick check using only default coverage?**<A name="defaultCoverage"/>

    ```
    # Generate tests using only the default coverage, reading the input definition from
    # standard input and printing log messages to standard output
    tcases -l stdout < myProject-Input.xml
    ```

    ```
    # Same thing, reading the input definition in JSON form from standard input
    tcases -l stdout -T json < myProject-Input.json
    ```


  * **This is just wrong. How do I report an issue?**

    Please add a new issue to the [Tcases issue list](https://github.com/Cornutum/tcases/issues). Be sure to
    attach the input model files needed to reproduce the problem. If you're concerned about sharing
    proprietary information, use the [Tcases Anonymizer](./How-To-Anonymize.md#how-to-anonymize-your-input-model).
    
