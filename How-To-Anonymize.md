# How To Anonymize Your Input Model #

## Why anonymize? ##

When reporting a problem with Tcases, it's helpful to include the input model files needed to reproduce the problem.
But those files might contain proprietary information that you are unable to share publicly. What to do?

Just run the Tcases Anonymizer. This program converts your system input definition (as well as any
corresponding generator definition) into an equivalent model, using anonymous names for all functions,
variables, values, and properties. All output annotations are excluded from the anonymized model. You can then
report the problem using the results of running this anonymous version of your input model.

## How to anonymize ##

There are two different ways to run the Tcases Anonymizer: [from the command line](#from-the-command-line) or [using Maven](#using-maven).

### From the command line ###

You can run the Tcases Anonymizer directly from your shell command line. If you use `bash` or a similar UNIX shell, you can run
the `tcases-anon` command. Or if you are using a Windows command line, you can run the `tcases-anon.bat` command file,
using exactly the same syntax.  For details about `tcases-anon` command syntax, run `tcases-anon -help`.

`tcases-anon` is included in the Tcases binary distribution file. For instructions on how to download and install it, see
[*Tcases: The Complete Guide*](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm#install).

Some examples:

```bash
# Write an anonymous version of the "find" system input definition to standard output
tcases-anon docs/examples/xml/find-Input.xml
```

```bash
# Write an anonymous version of "my-Input.xml" in JSON form to "results/anon-Input.json".
# If "my-Generators.xml" exists, write an anonymous version of it to "results/anon-Generators.json".
tcases-anon -f results/anon-Input.json my-Input.xml
```

```bash
# Write an anonymous version of "my-Input.xml" in XML form to "results/my-Anon".
# Read corresponding XML generators from "stdGenerators" and write anonymous generators to "results/my-Anon-Generators.xml".
tcases-anon -f results/my-Anon -g stdGenerators my-Input.xml
```


### Using Maven ###

You can also run the Tcases Anonymizer with the Tcases Maven Plugin
using the [`tcases:anon`](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/anon-mojo.html) goal.

Some examples:

```bash
# Write an anonymous version of each Tcases project in src/test/tcases to a correponding
# "anon-*" file in target/tcases.
mvn tcases:anon 
```

```bash
# Write an anonymous version of each Tcases project in src/test-models to a correponding
# "the-anonymous-*.xml" file in target/test-models
mvn tcases:anon -DinDir=src/test-models -DoutFile='the-anonymous-*.xml' -DoutDir=test-models
```

```bash
# Write an anonymous version of the "myProject" input definition in src/test/tcases to a correponding
# "anon-*" file in target/tcases. Read the project generator definition from the
# "myProjectGenDef" file and write an anonymized version to the "anon-myProject-Generators.*" file.
mvn tcases:anon -Dproject=myProject -DgenDef='*GenDef'
```
