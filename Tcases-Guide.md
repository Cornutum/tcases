# Tcases: The Complete Guide #

## Contents ##

  * [Introduction](#introduction)
    * [What Does It Do?](#what-does-it-do)
    * [How Does It Work?](#how-does-it-work)
  * [Getting Started](#getting-started)
    * [About This Guide](#about-this-guide)
    * [Installing The Tcases Maven Plugin](#installing-the-tcases-maven-plugin)
    * [Installing The Tcases Distribution](#installing-the-tcases-distribution)
    * [JSON? Or XML?](#json-or-xml)
    * [Running From the Command Line](#running-from-the-command-line)
    * [Understanding Tcases Results](#understanding-tcases-results)
    * [Troubleshooting FAQs](#troubleshooting-faqs)
  * [Modeling The Input Space](#modeling-the-input-space)
    * [An Example: The find Command](#an-example-the-find-command)
    * [Defining System Functions](#defining-system-functions)
    * [Defining Input Variables](#defining-input-variables)
    * [Defining Input Values](#defining-input-values)
    * [Defining Value Schemas](#defining-value-schemas)
    * [Defining Variable Schemas](#defining-variable-schemas)
    * [Defining Variable Sets](#defining-variable-sets)
    * [Defining Constraints: Properties and Conditions](#defining-constraints-properties-and-conditions)
      * [Value properties](#value-properties)
      * [Value conditions](#value-conditions)
      * [Failure values are different!](#failure-values-are-different)
      * [Condition expressions](#condition-expressions)
      * [Variable conditions](#variable-conditions)
      * [Cardinality conditions](#cardinality-conditions)
      * [But be careful!](#but-be-careful)
  * [Defining Input Coverage](#defining-input-coverage)
    * [Combinatorial Testing Basics](#combinatorial-testing-basics)
    * [Failure Cases Are Different!](#failure-cases-are-different)
    * [Default Coverage](#default-coverage)
    * [Defining Higher Coverage](#defining-higher-coverage)
    * [Defining Multiple Levels Of Coverage](#defining-multiple-levels-of-coverage)
  * [Managing A Tcases Project](#managing-a-tcases-project)
    * [Managing Project Files](#managing-project-files)
    * [Copying A Tcases Project](#copying-a-tcases-project)
    * [Reusing Previous Test Cases](#reusing-previous-test-cases)
    * [Mix It Up: Random Combinations](#mix-it-up-random-combinations)
    * [Reducing Test Cases: A Random Walk](#reducing-test-cases-a-random-walk)
    * [Avoiding Unneeded Combinations](#avoiding-unneeded-combinations)
    * [Simple Generator Definitions](#simple-generator-definitions)
      * [Defining A Random Seed](#defining-a-random-seed)
      * [Defining The Default Coverage Level](#defining-the-default-coverage-level)
  * [Transforming Test Cases](#transforming-test-cases)
    * [Creating An HTML Report](#creating-an-html-report)
    * [Creating JUnit/TestNG Tests](#creating-junittestng-tests)
    * [Using Output Annotations](#using-output-annotations)
      * [How it works](#how-it-works)
      * [Property annotations](#property-annotations)
  * [Further Reference](#further-reference)

## Introduction ##
### What Does It Do? ###

Tcases is a tool for designing tests. It doesn't matter what kind of system you are testing. Nor does it matter what level of
the system you are testing -- unit, subsystem, or full system. You can use Tcases to design your tests in any of these
situations. With Tcases, you define the input space for your system-under-test and the level of coverage that you want. Then
Tcases generates a minimal set of test cases that meets your requirements.

Tcases is primarily a tool for [black-box test design](http://en.wikipedia.org/wiki/Black-box_testing). For such tests, the
concept of "coverage" is different from structural testing criteria such as line coverage, branch coverage, etc. Instead, Tcases
is guided by coverage of the input space of your system.

What is the "input space" of the system? The simplest way to look at it is this: the set of all (combinations of) input values
that could possibly be applied. Easy to say, but hard to do! For all but the simplest systems, such a set is enormous, perhaps
even infinite. You could never afford to build and run all those test cases.  Instead, you have to select test cases from a
small sample of the input space. But how? If your sample is too big, you'll run out of time before you finish. But if your
sample is too small -- or, worse, if it's the _wrong_ subset -- you'll miss lots of defects.

That is the test design problem: given a limited amount of testing effort, how can you minimize the risk of defects?  And Tcases
is the tool for the job. Tcases gives you a way to define the input space for your system in a form that is concise but
comprehensive.  Then Tcases allows you to control the number of test cases in your sample subset by specifying the level of
coverage you want. You can start with a basic level of coverage, and Tcases will generate a small set of test cases that touches
every significant element of the input space. Then you can improve your tests by selectively adding coverage in specific
high-risk areas.  For example, you can specify [pairwise coverage](http://en.wikipedia.org/wiki/All-pairs_testing) or
higher-order combinations of selected input variables.

### How Does It Work? ###

First, you create a [system input definition](#defining-system-functions), a document that defines your system as a set of
[functions](#defining-system-functions). For each system function, the system input definition defines the
[variables](#defining-input-variables) that characterize the function input space.

Then, you can create a [generator definition](#defining-higher-coverage). That's another document that defines the coverage you
want for each system function. The generator definition is optional. You can skip this step and still get a basic level of
coverage.

Finally, you run Tcases. Tcases is a Java program that you can run from the command line or from your favorite IDE.  Tcases
comes with built-in support for running using a shell script.  You can also run Tcases with Maven using the [Tcases Maven
Plugin](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/index.html).  Using your input definition and your generator
definition, Tcases generates a [system test definition](#understanding-tcases-results).  The system test definition is a
document that lists, for each system function, a set of test cases that provides the specified level of coverage. Each test case
defines a specific value for every function input variable. Tcases generates not only valid input values that define successful
test cases but also invalid values for the tests cases that are needed to verify expected error handling.

Of course, the system test definition is not something you can execute directly. But it follows a well-defined schema, which
means you can use a variety of transformation tools to convert it into a form that is suitable for testing your system. For
example, Tcases comes with a built-in transformer that converts a system test definition into a Java source code template for a
[JUnit or TestNG test class](#creating-junittestng-tests).  You can also automatically transform a system test definition into a
simple [HTML report](#creating-an-html-report).



## Getting Started ##

### About This Guide ###

This guide explains everything about how Tcases works. And when it comes to examples, this guide shows how to do things when
running Tcases as a shell command. If you run Tcases using the [Tcases Maven
Plugin](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/index.html), the command line details will be slightly
different, but all of the concepts remain the same.

### Installing The Tcases Maven Plugin ###

To get dependency info for the Tcases Maven Plugin, visit the
plugin [documentation site](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/dependency-info.html).

### Installing The Tcases Distribution ###

To get the command line version of Tcases, download the Tcases binary distribution file from the Maven Central Repository, using
the following procedure.

  1. Visit the [Central Repository page](https://central.sonatype.com/artifact/org.cornutum.tcases/tcases-shell/4.0.1/versions) for `tcases-shell`.
  1. Find the entry for the latest version and click on "Browse".
  1. To download the distribution ZIP file, click on "tcases-shell-_${version}_.zip". If you prefer a compressed TAR file, click on "tcases-shell-_${version}_.tar.gz".

Extract the contents of the distribution file to any directory you like -- this is now your _"Tcases home directory"_. Unpacking
the distribution file will create a _"Tcases release directory"_ -- a subdirectory of the form `tcases-`_m_._n_._r_ -- that
contains all the files for this release of Tcases. The release directory contains the following subdirectories.

  * `bin`: Executable shell scripts used to run Tcases 
  * `docs`: User guide, examples, and Javadoc 
  * `lib`: All JAR files needed to run Tcases 


One more step and you're ready to go: add the path to the `bin` subdirectory to the `PATH` environment variable for your system.


### JSON? Or XML? ###

The preferred form for all Tcases documents is JSON, which is capable of expressing all Tcases features and which is used for
all of the examples in this guide.

But the original version of Tcases used XML for all documents, and XML is still supported for older documents. You can find all
the details about using Tcases with XML in the [original version of this guide](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm),
including [how to convert an existing XML project into JSON](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm#json).

### Running From the Command Line ###

You can run Tcases directly from your shell command line. If you use `bash` or a similar UNIX shell, you can run the `tcases`
command.  Or if you are using a Windows command line, you can run Tcases with the `tcases.bat` command file, using exactly the
same syntax.

For example, for a quick check, you can run one of the examples that comes with Tcases, using the following commands.

```bash
cd ${tcases-release-dir}
cd docs/examples/json 
tcases < find-Input.json 
```

For details about the interface to the `tcases` command (and the `tcases.bat` command, too), see the Javadoc for the
[`TcasesCommand.Options`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/TcasesCommand.Options.html) class.  To get
help at the command line, run `tcases -help`.

### Understanding Tcases Results ###

What happens when you run Tcases? Tcases reads a [system input definition](#defining-system-functions), a document that defines
the "input space" of the system function to be tested. From this, Tcases produces a different document called a 
_system test definition_, which describes a set of test cases.


Try running Tcases on one of the example system input definitions. The following commands will generate
test cases for the `find` command [example](#an-example-the-find-command), which is
explained in [full detail](#modeling-the-input-space) later in this guide.

```bash
cd ${tcases-release-dir}
cd docs/examples/json 
tcases < find-Input.json 
```


The resulting system test definition is written to standard output. Here's what it looks like: for
the `find` [function](#defining-system-functions), a list of test case definitions, each of which defines values for all of
the function's input [variables](#defining-input-variables).

```json
{
  "system": "Examples",
  "find": {
    "testCases": [
      {
        "id": 0,
        "name": "pattern='empty'",
        "has": {
          "properties": "fileExists,fileName,patternEmpty"
        },
        "arg": {
          "pattern": {
            "value": "",
            "source": "empty"
          },
          "fileName": {
            "value": "defined"
          }
        },
        "env": {
          "file.exists": {
            "value": true
          },
          "file.contents.linesLongerThanPattern": {
            "NA": true
          },
          "file.contents.patternMatches": {
            "NA": true
          },
          "file.contents.patternsInLine": {
            "NA": true
          }
        }
      },
      ...
    ]
  }
}
```

### Troubleshooting FAQs ###

Something going wrong? See the [Troubleshooting FAQs](./Troubleshooting-FAQs.md#troubleshooting-faqs) for assistance.

## Modeling The Input Space ##

Tcases creates test definitions based on a _system input definition_ that you create. But how do you do that? That's what this
section aims to explain.

A system input definition is a document that models the "input space" of the system-under-test (SUT). We say it "models"
system inputs because it doesn't literally itemize all possible input values. Instead, a system input definition lists all the
important aspects of system inputs that affect system results. Think of this as describing the "dimensions of variation" in the
"input space" of your system. Some dimensions of variation are obvious. If you are testing the `add` function, you know there
are at least two dimensions of variation -- the two different numbers being added. But to find all of the key dimensions, you
may have to take a deeper look.

For example, consider how you might test a simple "list files" command, like the `ls` command in `UNIX`. (And to keep it simple,
let's assume there are no command options or switches to worry about.) Clearly, one dimension of variation is the number of file
names given. `ls` should handle not just one file name but also a list of many file names. And if no file names are given, `ls`
is expected to have a completely different result. But what about each file name itself? `ls` will produce a different result,
depending on whether the name identifies a simple file or a directory. So, the type of the file identified by each file name is
an additional dimension of variation. But that's not all! Some file names could identify actual files, but others could be bogus
names for files that don't exist, and this difference has a big effect of what `ls` is expected to do. So, here's another
dimension of variation that has nothing to do with the file names themselves but instead concerns the state of the environment
in which `ls` runs.

You can see that modeling the input space demands careful thought about the SUT. That's a job that no tool can do for you. But
Tcases gives you a way to capture that knowledge and to translate it into effective test cases.

### An Example: The `find` Command ###

To understand input modeling with Tcases, it helps to see an example in action. In this guide, we're
going to explain how Tcases works by showing how we can use it to test a hypothetical `find`
command. The complete input definition for `find` is included with this guide -- you can
see it [here](http://www.cornutum.org/tcases/docs/examples/json/find-Input.json).


Take a look at the `find` specification below. What test cases would you use to test it?

> Usage: `find` _pattern_ _file_
>
> Locates one or more instances of a given pattern in a text file.
> 
> All lines in the file that contain the pattern are written to standard output. A
> line containing the pattern is written only once, regardless of the number of
> times the pattern occurs in it.
> 
> The pattern is any sequence of characters whose length does not exceed the
> maximum length of a line in the file. To include a blank in the pattern, the
> entire pattern must be enclosed in quotes (`"`). To include a quotation mark in the
> pattern, two quotes in a row (`""`) must be used.

### Defining System Functions ###

A system input definition describes a specific system-under-test, so the root object of the document looks like this:

```json
{
  "system": "Examples",
  ...
}
```


In general, the SUT has one or more operations or "functions" to be tested. Accordingly, the system input definition contains a
function definition object for each of them.  For our example, we'll build an input definition for a system named "Examples"
which has only one function named "find".


```json
{
  "system": "Examples",
  "find": {
    ...
  }
}
```


Obviously, what constitutes a "system" or a "function" depends entirely on what you're testing. If your "system" is a Java class, then your
"functions" might be its methods. If your "system" is an application, then your "functions" might be use cases. If your "system" is a Web site,
then your "functions" might be pages. In any case, the process of input modeling is exactly the same.


### Defining Input Variables ###

For each function to be tested, you need to define all of the dimensions of variation in its input space. For simplicity, Tcases
refers to each such dimension as a "variable" and each basic variable is represented by a variable definition object.

In addition, a function definition object groups input variables by type.  The `find` command has two different types of input
variables. There are direct input arguments, such as the file name, which have input type `arg`. There are also other factors,
such as the state of the file, which act as indirect "environmental" input variables and are given input type `env`.  (More
details about these are shown in a [later section](#defining-variable-sets).)

```json
{
  "system": "Examples",
  "find": {
    "arg": {
      "pattern": {
        ...
      },
      "fileName": {
        ...
      }
    },
    "env": {
      ...
    }
  }
}
```


Actually, the input type name used to group input variables is just a tag that can be any value you want.  There is no limit to
the number of different input type groups that you can define.

### Defining Input Values ###

For Tcases to create a test case, it must choose values for all of the input variables. How can it do that? Because we describe
all of the possible values for each input variable using one or more value definition objects.

By default, a value definition describes a valid value, one that the function-under-test is expected to accept. But we can use
the optional `failure` property to identify an value that is invalid and expected to cause the function to produce some kind of
failure response.  Tcases uses these input values to generate two types of test cases -- "success" cases, which use only valid
values for all variables, and "failure" cases, which use a `failure` value for exactly one variable.

For example, we can define two possible values for the `fileName` argument to `find`.

```json
{
  "system": "Examples",
  "find": {
    "arg": {
      "fileName": {
        "values": {
          "defined": {
          },
          "missing": {
            "failure": true
          }
        }
      }
    ...
    }
  }
}
```


That's it? Your only choices for the file name are "defined" or "missing"? Good question! What's happening here is a very
important part of input space modeling. It would be silly to list every possible file name as a value here.  Why? Because it
just doesn't matter. At least for this particular function, the letters and format of the file name have no bearing on the
behavior of the function. Instead, what's needed is a _model_ of the value domain for this variable that characterizes the
_types_ of values that are significant to the test. This is a well-known test design technique known as
[equivalence class partitioning](http://en.wikipedia.org/wiki/Equivalence_partitioning). You use each value definition to
identify a _class_ of values. By definition, all specific values in this class are test-equivalent. We don't need to test them
all -- any one of them will do.


In the case of the `fileName` variable, we've decided that the significance of file name itself is whether it is present or not,
and we've chosen to identify those two variations as "defined" and "missing".  But the name you use to identify each value
class is entirely up to you -- it is part of the input model you design to describe your tests and it appears in the test case
definitions that Tcases generates, to guide your test implementation.

### Defining Value Schemas ###

If you like, you have the option to define the class of values represented by a value definition more concretely, using a value _schema_.

For example, for the `pattern` variable, one interesting class of values is an unquoted string containing multiple characters. Of course,
a pattern string that is unquoted can't contain any spaces or quotation marks. You could define this `unquotedMany` value class using
the following schema keywords, which describe a string of at least 2 chars matching a certain regular expression.

```json
...
"pattern": {
  "values": {
    "unquotedMany": {
      "type": "string",
      "pattern": "^[^\\s\"]+$",
      "minLength": 2
    },
    ...
  }
}
...
```

What happens when the `pattern` is an empty string? That's another important value to add to your test cases. Here's how you could use a
schema keyword to define this `empty` value.

```json
...
"pattern": {
  "values": {
    "empty": {
      "const": ""
    },
    ...
  }
}
...
```

To define a value schema, you can use a subset of the standard [JSON Schema Validation vocabulary](https://json-schema.org/draft/2020-12/json-schema-validation.html).
For full details, see [_Schema Keywords_](Schema-Keywords.md).

Why define a value schema? The difference lies in the test case definitions that Tcases generates. If your input model defines a
value class using only a name, then any test case that uses this value definition can show only this name. To translate this
test case into executable form, you must substitute an appropriate concrete value that belongs to this class. However, if you
define the value class using a schema, Tcases will automatically generate a random member of the value class for you. Morever, a
different concrete value can be generated for every test case that uses this value definition.  Not only does this save you 
some effort, but also it can improve your test cases by adding more ["gratuitous variety"](#mix-it-up-random-combinations).

### Defining Variable Schemas ###

Did you know that you can also add schema keywords to a variable definition? You do now.

Why is this useful? For starters, you can use a variable schema to define defaults for all of its value schemas. For example,
you can specify that the default type for `pattern` variable is "string". If so, you don't need to add a "type" keyword to each
individual value definition.

```json
...
"pattern": {
  "type": "string",
  "values": {
    "empty": {
      "const": ""
    },
    "unquotedSingle": {
      "pattern": "^[^\\s\"]$"
    },
    "unquotedMany": {
      "pattern": "^[^\\s\"]+$",
      "minLength": 2
    },
    "quoted": {
      "pattern": "^\"[^\\s\"]+\"$",
      "minLength": 2
    },
    ...
  }
}
...
```

Note that schema keywords in a value definition will override any defaults given in a variable schema. A value definition can define a completely different value for "type"
or any other schema keyword, including no schema keywords at all.

But in some cases, a variable schema is all you need. Tcases can use this to generate all of the value definitions automatically. For example, suppose your input model
contained a variable for an email address. You could define such a variable using only a schema, like this:

```json
{
  ...
  "user-email-address": {
    "format": "email",
    "minLength": 8,
    "maxLength": 32
  },
  ...
}
```

Using this schema, Tcases will generate an _effective input model_ for the `user-email-address` variable, including both valid and invalid value
classes, as shown below. This effective input model is then what Tcases will use to generate test cases. 

```json
{
  ...
  "user-email-address": {
    "values": {
      "minimumLength": {
        "format": "email",
        "minLength": 8,
        "maxLength": 8
      },
      "maximumLength": {
        "format": "email",
        "minLength": 32,
        "maxLength": 32
      },
      "tooShort": {
        "failure": true,
        "format": "email",
        "maxLength": 7
      },
      "tooLong": {
        "failure": true,
        "format": "email",
        "minLength": 33
      },
      "wrongFormat": {
        "failure": true,
        "format": "date-time"
      }
    }
  }
  ...
}
```

Tip: if you want to see the effective input model that Tcases is using, run `tcases` with the `-I` option.

```bash
# Instead of creating test cases, just print the effective input model for the "find" project to the /tmp/test directory
tcases -I -o /tmp/test find
```

### Defining Variable Sets ###

It's common to find that a single logical input actually has lots of different characteristics, each of which creates a
different "dimension of variation" in the input space. For example, consider the file that is searched by the `find`
command. Does it even exist? Maybe yes, maybe no -- that's one dimension of variation that the tests must cover. And what about
its contents? Of course, you'd like to test the case where the file contains lines that match the pattern, as well the case
where there are no matches. So, that's another dimension of variation. The spec says that each matching line is printed exactly
once, even when it contain multiple matches. Wouldn't you want to test a file that has lines with different numbers of matches?
Well, there's yet another dimension of variation. One file -- so many dimensions!


You can model this complex sort of input as a "variable set", using the `members` property.  With a variable set, you can
describe a single logical input as a set of multiple variable definitions.  A variable set can even contain another variable
set, creating a hierarchy of logical inputs that can be extended to any number of levels.


For example, the single `file` input to the `find` command can modeled by the following variable set
definition.

```json
 {
  "system": "Examples",
  "find": {
    ...
    "env": {
      "file": {
        "members": {
          "exists": {
            "values": {
              ...
            }
          },
          "contents": {
            "members": {
              "linesLongerThanPattern": {
                "values": {
                  ...
                }
              },
              "patternMatches": {
                "values": {
                  ...
                }
              },
              "patternsInLine": {
                "values": {
                  ...
                }
              }
            }
          }
        }
      }
    }
  }
}
```


Isn't this hierarchy really just the same as four variable definitions, something like the following?

```json
{
  "system": "Examples",
  "find": {
    ...
    "env": {
      "file.exists": {
        "values": {
          ...
        }
      },
      "file.contents.linesLongerThanPattern": {
        "values": {
          ...
        }
      },
      "file.contents.patternMatches": {
        "values": {
          ...
        }
      },
      "file.contents.patternsInLine": {
        "values": {
          ...
        }
      }
    }
  }
}
```


Yes, and when generating test cases, that's essentially how Tcases handles it. But defining a complex input as a variable set
makes the input model simpler to create, read, and maintain. Also, it allows you to apply constraints to an entire tree of
variables at once, at you'll see in the next section.

### Defining Constraints: Properties and Conditions ###

We've seen how to define the value choices for all of the input variables of each function-under-test, including complex input
variables with multiple dimensions. That's enough for us to complete a system input definition for the `find` command that looks
something like the following.

```json
{
  "system": "Examples",
  "find": {
    "arg": {
      "pattern": {
        "type": "string",
        "maxLength": 16,
        "values": {
          "empty":          {"const": ""},
          "unquotedSingle": {"pattern": "^[^\\s\"]$"},
          "unquotedMany":   {"pattern": "^[^\\s\"]+$"},
          "quoted":         {"pattern": "^\"[^\\s\"]+\"$"},
          "quotedEmpty":    {"const": "\"\""},
          "quotedBlanks":   {"pattern": "^\"[^\\s\"]*( +[^\\s\"]*)+\"$"},
          "quotedQuotes":   {"pattern": "^\"[^\\s\"]*(\"{2}[^\\s\"]*)+\"$"}
        }
      },
      "fileName": {
        "type": "string",
        "values": {
          "defined": {},
          "missing": {"failure": true}
        }
      }
    },
    "env": {
      "file": {
        "members": {
          "exists": {
            "type": "boolean",
            "values": {
              "true":  {},
              "false": {"failure": true}
            }
          },
          "contents": {
            "members": {
              "linesLongerThanPattern": {
                "type": "integer",
                "format": "int32",
                "values": {
                  "1":    {},
                  "many": {"minimum": 2, "maximum": 32},
                  "0":    {"failure": true}
                }
              },
              "patternMatches": {
                "type": "integer",
                "format": "int32",
                "values": {
                  "0":    {},
                  "1":    {},
                  "many": {"minimum": 2, "maximum": 16}
                }
              },
              "patternsInLine": {
                "type": "integer",
                "format": "int32",
                "values": {
                  "1": {},
                  "many": {"minimum": 2, "maximum": 4}
                }
              }
            }
          }
        }
      }
    }
  }
}
```


When we run Tcases with this input document, we'll get a list of test case definitions like this:

```json
{
  "system": "Examples",
  "find": {
    "testCases": [
      {
        "id": 0,
        "name": "pattern='empty'",
        "arg": {
          "pattern":  {"value": "", "source": "empty"},
          "fileName": {"value": "defined"}
        },
        "env": {
          "file.exists":                          {"value": true},
          "file.contents.linesLongerThanPattern": {"value": 1},
          "file.contents.patternMatches":         {"value": 0},
          "file.contents.patternsInLine":         {"value": 1}
        }
      },
      {
        "id": 1,
        "name": "pattern='unquotedSingle'",
        "arg": {
          "pattern":  {"value": "}", "source": "unquotedSingle"},
          "fileName": {"value": "defined"}
        },
        "env": {
          "file.exists":                          {"value": true},
          "file.contents.linesLongerThanPattern": {"value": 26, "source": "many"},
          "file.contents.patternMatches":         {"value": 1},
          "file.contents.patternsInLine":         {"value": 4, "source": "many"}
        }
      },
      ...
    ]
  }
}
```


But wait up a second -- something doesn't look right here. Take a closer look at test case 0 above. It's telling us to try a
test case using a file that contains no instances of the test pattern. Oh, and at the same time, the file should contain a line
that has one match for the test pattern. Not only that, but the pattern to match is the null pattern -- an empty string. That
pattern will match any line multiple times! In short, this test case seems sort of ... well, impossible.

And look at test case 1 above. It looks equally problematic. For this test case, there should be exactly one match in the file.
Which must also contain a line that matches the pattern 4 times. No way!

What's happening here? Clearly, some of the "dimensions of variation" described by these variable definitions are not entirely
independent of each other. Instead, there are relationships among these variables that _constrain_ which combinations of values
are feasible. We need a way to define those relationships so that infeasible combinations can be excluded from our test cases.

With Tcases, you can do that using _properties_ and _conditions_. The following sections explain how, including some tips about
how to avoid certain [problems that constraints can introduce](#but-be-careful).

#### Value properties ####

A value definition can declare a `properties` list that specifies one or more "properties" for this value. For example:

```json
...
"patternMatches": {
  "type": "integer",
  "format": "int32",
  "values": {
    "0": {
    },
    "1": {
      "properties": ["match"]
    },
    "many": {
      "properties": ["match", "matchMany"]
    }
  }
}
...
```

A property is just a name that you invent for yourself to identify an important characteristic of this value. The concept is that
when this value is included in a test case, it contributes all of its properties -- these now become properties of the test case itself.
That makes it possible for us to later define "conditions" on the properties that a test case must (or must not!) have for certain values
to be included.

For example, the definition above for the `file.contents.patternMatches` variable says that when we choose the value `1` for a
test case, the test case acquires a property named `match`.  But if we choose the value `many`, the test case acquires two
properties -- `match` and `matchMany`.  And if we choose the value `0`, no new properties are added to the test case. Note that
the correspondence between these particular names of the values and properties is not exactly accidental -- it helps us
understand what these elements mean -- but it has no special significance. We could have named any of them differently if we
wanted to.

But note that all of this applies _only_ to valid value definitions, not to failure value definitions that specify `"failure":
true`. Why? Because [failure values are different!](#failure-values-are-different).


#### Value conditions ####

We can define the conditions required for a value to be included in a test case using the `when` property, which defines a
"condition" object.  Adding a condition object means "for this value to be included in a test case, the properties of the test
case must satisfy this condition.


For example, consider the conditions for the values of the `file.contents.patternsInLine` variable.

```json
...
"patternsInLine": {
  "type": "integer",
  "format": "int32",
  "values": {
    "1": {
      "when": {"allOf": [ {"hasAll": ["matchable", "match"]}, {"hasNone": ["patternEmpty"]}]}
    },
    "many": {
      "when": {"allOf": [ {"hasAll": ["matchable", "matchMany"]}, {"hasNone": ["patternEmpty"]}]},
      "minimum": 2,
      "maximum": 4
    }
  }
}
...
```

This defines _constraints_ on the values of the `file.contents.patternsInLine` variable. We want to have a test case in which
the value for this variable is `1`, i.e. some line contains exactly one substring that matches the pattern. If so, this must be
a test case in which the file contains at least one matching line. In other words, the test case must have the `match` property,
which appears only if the value of `file.contents.patternMatches` is non-zero.

We also want a test case in which the value of `file.contents.patternsInLine` is `many`. For this value, the required conditions
are much the same, except that this can't be a test case in which the file contains only one instance of a pattern match.
Therefore, the test case must have the `matchMany` property, as shown below.


```json
...
"patternMatches": {
  "type": "integer",
  "format": "int32",
  "values": {
    "0": {
    },
    "1": {
      "properties": ["match"]
    },
    "many": {
      "minimum": 2,
      "maximum": 16,
      "properties": ["match", "matchMany"]
    }
  }
},
...
```

Also, this must be a test case in which the file contains at least one line longer than the pattern. Otherwise, no matches are
possible.  In other words, the test case must have the `matchable` property, which appears only if the value of
`file.contents.linesLongerThanPattern` is non-zero, as shown below.

```json
...
"linesLongerThanPattern": {
  "type": "integer",
  "format": "int32",
  "values": {
    "1": {
      "properties": ["matchable"]
    },
    "many": {
      "minimum": 2,
      "maximum": 32,
      "properties": ["matchable"]
    },
    "0": {
      "failure": true
    }
  }
},
...
```

Moreover, this must be a test case in which the pattern is not empty. Otherwise, the question of how many matches occur in any
line is irrelevant. In other words, the test case must _not_ have the `patternEmpty` property, which appears the value of the
`pattern` variable is an empty string. But is that combination of inputs is really infeasible? Strictly speaking, no. But it's
certainly not very useful. This demonstrates another way for a smart tester to use properties and conditions: to steer toward
test cases with more potent combinations and away from combinations that add little defect-fighting power.

It's important to note that there are many values with no conditions attached.  A test case can use these value in combination
with any others. And that's a good thing.  We want to model the reality of the input space for the function, without eliminating
any test cases that are actually feasible. Otherwise, our tests will have a blind spot that could allow defects to slip by
undetected. Rule of thumb: Use conditions sparingly and only when necessary to avoid infeasible or unproductive test cases.

#### Failure values are different! ####

Different? Yes, because failure value definitions -- i.e. those that specify `"failure": true` -- _cannot_ define properties.

If you think about it, you can see that there is a fundamental reason why this is so.  Suppose you declare that some value=V
defines a property=P. Why would you do that? There really is only one reason: so that some other value=X can require combination
with V (or, to be precise, with any value that defines P). But if V declares `"failure": true`, that doesn't make sense. If the
other value X is valid, it can't demand combination with a failure value -- otherwise, X could never appear in a success
case. And if X is a failure value itself, it can't demand combination with a different failure value -- at most one failure
value can appear in a [failure case](#failure-cases-are-different).

But note that a failure value _can_ define a condition.  In other words, it can demand combination with specific values from
other variables. By working from this direction, you can control the other values used in a failure case.
For example, the `find` command recognizes an error when there are no lines in the file long enough to match the pattern.
In other words, for the variable `file.contents.linesLongerThanPattern`, the value `0` is a failure value. But in this case,
the pattern ought to be longer than just 1 character. With suitable definitions of properties and conditions, we can ensure
this combination is formed properly.

```json
...
"pattern": {
  "values": {
    ...
    "unquotedMany": {
      "pattern": "^[^\\s\"]+$",
      "minLength": 2,
      "maxLength": 16,
      "properties": ["patternMany"]
    },
    "quoted": {
      "pattern": "^\"[^\\s\"]+\"$",
      "minLength": 2,
      "maxLength": 16,
      "properties": ["patternMany"]
    },
    ...
  }
},
...
"file": {
  "members": {
    ...
    "contents": {
      "members": {
        "linesLongerThanPattern": {
          "values": {
            ...
            "0": {
              "when": { "hasAll": ["patternMany"]},
              "failure": true
            }
          }
        },
        ...
      }
    }
  }
}
...
```

#### Condition expressions ####

The value of a `when` property is a condition expression object. A condition expression is a JSON object with a single property, which must be
one of the following.

* `"hasAll"`
  * Contains a list of value property names
  * Satisfied if the test case has all of the listed properties

* `"hasAny"`
  * Contains a list of value property names
  * Satisfied if the test case has at least one of the listed properties

* `"hasNone"`
  * Contains a list of value property names
  * Equivalent to `"not": { "hasAny": [...]}`

* `"allOf"`
  * A logical AND expression
  * Contains a list of condition expression objects
  * Satisfied if all of the listed expressions are satisfied

* `"anyOf"`
  * A logical OR expression
  * Contains a list of condition expression objects
  * Satisfied if at least one of the listed expressions is satisfied

* `"not"`
  * A logical negation expression
  * Contains a single condition expression object
  * Satisfied if this expression is _not_ satisfied

* `"lessThan"`
  * A [cardinality condition](#cardinality-conditions) (see [examples](http://www.cornutum.org/tcases/docs/examples/json/Ice-Cream-Input.json))
  * Satisfied when the given `property` occurs less than the given `max` times

* `"notLessThan"`
  * A [cardinality condition](#cardinality-conditions) (see [examples](http://www.cornutum.org/tcases/docs/examples/json/Ice-Cream-Input.json))
  * Satisfied when the given `property` occurs greater than or equal to the given `min` times

* `"moreThan"`
  * A [cardinality condition](#cardinality-conditions) (see [examples](http://www.cornutum.org/tcases/docs/examples/json/Ice-Cream-Input.json))
  * Satisfied when the given `property` occurs more than the given `min` times

* `"notMoreThan"`
  * A [cardinality condition](#cardinality-conditions) (see [examples](http://www.cornutum.org/tcases/docs/examples/json/Ice-Cream-Input.json))
  * Satisfied when the given `property` occurs less than or equal to the given `max` times

* `"between"`
  * A [cardinality condition](#cardinality-conditions) (see [examples](http://www.cornutum.org/tcases/docs/examples/json/Ice-Cream-Input.json))
  * Satisfied when occurrences of the given `property` are both greater than or equal to the given
    `min` and less than or equal to the given `max`. If you want to specify a strictly greater/less than relationship,
    specify an `exclusiveMin` or `exclusiveMax` property instead.

* `"equals"`
  * A [cardinality condition](#cardinality-conditions) (see [examples](http://www.cornutum.org/tcases/docs/examples/json/Ice-Cream-Input.json))
  * Satisfied when the given `property` occurs exactly the given `count` times


#### Variable conditions ####

You may find that, under certain conditions, an input variable becomes irrelevant. It doesn't matter which value you choose --
none of them make a difference in function behavior. It's easy to model this situation -- just define a condition on the
variable definition itself.


For example, when we're testing the `find` command, we want to try all of the values defined for every dimension of the
`file.contents.patternMatches` variable. But, in the case when the file contains no lines long enough to match the pattern, the
question of how many matches it contains is pointless.  In this case, the `file.contents.patternMatches` variable is irrelevant.
Similarly, when we test a file that contains no matches at all, the `file.contents.patternsInLine` variable is meaningless. We
can capture these facts about the input space by adding `when` conditions to these variables, as shown below. Notice
how these variable conditions make it simpler or unnecessary to define conditions for individual values.

```json
...
"patternMatches": {
  "when": { "hasAll": ["matchable"]},
  "type": "integer",
  "format": "int32",
  "values": {
    "0": {
    },
    "1": {
      "properties": ["match"]
    },
    "many": {
      "minimum": 2,
      "maximum": 16,
      "properties": ["match", "matchMany"]
    }
  }
},

"patternsInLine": {
  "when": { "hasAll": ["match"]},
  "type": "integer",
  "format": "int32",
  "values": {
    "1": {
    },
    "many": {
      "when": { "hasAll": ["matchMany"]},
      "minimum": 2,
      "maximum": 4
    }
  }
}
...
```

You can define variable conditions at any level of a variable set hierarchy. For example, you can see in the example below that
a condition is defined for the entire `file.contents` variable set. This condition models the fact that the file contents are
irrelevant when the file specified to search doesn't even exist or when the pattern to match is empty.

```json
...
"contents": {
  "when": { "allOf": [ { "hasAll": ["fileExists"]}, { "hasNone": ["patternEmpty"]}]},
  "members": {
    ...
  }
}
...
```

How does a variable condition affect the test cases generated by Tcases? In a test case where a variable is irrelevant, it is
not given a `value` but instead is designated as `NA`, meaning "not applicable".  For example, test case 0 below shows how
testing an empty pattern causes `file.contents` to be irrelevant. Similarly, test case 8 shows how
testing with a non-existent file makes nearly every other variable irrelevant.

```json
"testCases": [
  {
    "id": 0,
    "name": "pattern='empty'",
    "has": {"properties": "fileExists,fileName,patternEmpty"},
    "arg": {
      "pattern":  {"value": "", "source": "empty"},
      "fileName": {"value": "defined"}
    },
    "env": {
      "file.exists":                          {"value": true},
      "file.contents.linesLongerThanPattern": {"NA": true},
      "file.contents.patternMatches":         {"NA": true},
      "file.contents.patternsInLine":         {"NA": true}
    }
  },
  ...
  {
    "id": 8,
    "name": "file.exists='false'",
    "has": {"properties": "fileName"},
    "arg": {
      "pattern":  {"NA": true},
      "fileName": {"value": "defined"}
    },
    "env": {
      "file.exists":                          {"failure": true, "value": false},
      "file.contents.linesLongerThanPattern": {"NA": true},
      "file.contents.patternMatches":         {"NA": true},
      "file.contents.patternsInLine":         {"NA": true}
    }
  }
]
...
```

#### Cardinality conditions ####

The basic conditions are concerned only with the presence (or absence) of certain properties in a test case. But, of course, a
test case can accumulate multiple instances of a property, if two or more of the values used in the test case contribute the
same property. And in some situations, the number of occurrences of a property is a significant constraint on the input
space. You can model these situations using _cardinality conditions_, which check if the number of property occurrences is
greater than, less than, or equal to a specific value.


For example, consider the case of an ice cream shop that sells different types of ice cream cones. These yummy products come
only in specific combinations, and the price depends on the combination of scoops and toppings added. But how to test that? The
input model for these cones might look like the one below. (You can find the full example
[here](http://www.cornutum.org/tcases/docs/examples/json/Ice-Cream-Input.json).)

```json
{
  "system": "Ice-Cream",
  "Cones": {
    "arg": {
      "Cone": {
        "values": {
          "Empty": {...},
          "Plain": {...},
          "Plenty": {...},
          "Grande": {...},
          "Too-Much": {...}
        }
      },
      "Flavors": {
        "members": {
          "Vanilla":    {"values": {"Yes": {...}, "No": {}}},
          "Chocolate":  {"values": {"Yes": {...}, "No": {}}},
          "Strawberry": {"values": {"Yes": {...}, "No": {}}},
          ...
        }
      },
      "Toppings": {
        "members": {
          "Sprinkles": {"values": {"Yes": {...}, "No": {}}},
          "Pecans":    {"values": {"Yes": {...}, "No": {}}},
          "Oreos":     {"values": {"Yes": {...}, "No": {}}},
          ...
        }
      }
    }
  }
}
```


To build a cone, you can add a scoop of any flavor and any of the given toppings. To keep track, each of these choices
contributes either a `scoop` or a `topping` property to our cone test cases.

```json
...
"Flavors": {
  "members": {
    "Vanilla":    {"values": {"Yes": {"properties": ["scoop"]}, "No": {}}},
    "Chocolate":  {"values": {"Yes": {"properties": ["scoop"]}, "No": {}}},
    "Strawberry": {"values": {"Yes": {"properties": ["scoop"]}, "No": {}}},
    ...
  }
},
"Toppings": {
  "members": {
    "Sprinkles": {"values": {"Yes": {"properties": ["topping"]}, "No": {}}},
    "Pecans":    {"values": {"Yes": {"properties": ["topping"]}, "No": {}}},
    "Oreos":     {"values": {"Yes": {"properties": ["topping"]}, "No": {}}},
    ...
  }
}
```


Then we can define specific cone products based on the number of scoops and toppings, using cardinality conditions like
`lessThan`, `equals`, and `between`.

```json
...
"Cone": {
  "values": {
    "Empty": {
      "failure": true,
      "when": {"lessThan": {"property": "scoop", "max": 1}}
    },
    "Plain": {
      "when": {
        "allOf": [
          {"equals": {"property": "scoop", "count": 1}},
          {"notMoreThan": {"property": "topping", "max": 1}}
        ]
      }
    },
    "Plenty": {
      "when": {
        "allOf": [
          {"between": {"property": "scoop", "min": 1, "max": 2}},
          {"notMoreThan": {"property": "topping", "max": 2}}
        ]
      }
    },
    "Grande": {
      "when": {
        "allOf": [
          {"between": {"property": "scoop", "exclusiveMin": 0, "exclusiveMax": 4}},
          {"between": {"property": "topping", "min": 1, "max": 3}}
        ]
      }
    },
    "Too-Much": {
      "failure": true,
      "when": {
        "anyOf": [
          {"moreThan": {"property": "scoop", "min": 3}},
          {"notLessThan": {"property": "topping", "min": 4}}
        ]
      }
    }
  }
}
...
```

#### But be careful! ####

With the constraints defined by properties and conditions comes great power. Use it carefully! It's possible to define
constraints that make it very difficult or even impossible for Tcases to generate the test cases you want.  If it looks to you
like Tcases is frozen, that's probably what's going on.  Tcases is not frozen -- it's busy with a very long and perhaps
fruitless search for a combination of values that will satisfy your constraints.

The following sections describe some of the situations to watch out for.

##### Infeasible combinations #####

Tcases always generates test cases that include specific combinations of values, based on the [coverage
level](#defining-input-coverage) you've specified.  But what if you've defined constraints that make some intended value
combination impossible? If so, we say that this combination is "infeasible". For combinations of 2 or more variables (2-tuples,
3-tuples, etc.), this may be expected, so Tcases will simply [log](./Troubleshooting-FAQs.md#troubleshooting-faqs) a warning and
keep going. For "combinations" of a single variable (the default coverage level), this is an error, and you must fix the
offending constraints before Tcases can continue.

> Tips:
> * To help find the bad constraint that's giving you grief, try [changing the logging level](./Troubleshooting-FAQs.md#troubleshooting-faqs) to `DEBUG` or `TRACE`.
> 
> * Are you using [higher coverage levels](#defining-higher-coverage) (2-tuples, 3-tuples, etc.)? If so, try running a quick check using
>   only the default coverage. An easy way to do that is to run Tcases like this: `tcases < ${myInputModelFile}`.
>   If there is an infeasible value, this check can sometimes show you the error.


Usually, Tcases can quickly report when combinations are infeasible. But in some cases, Tcases can find the problem only after
trying and eliminating all possibilities.  If it looks to you like Tcases is frozen, that's probably what's going on.  Tcases is
not frozen -- it's busy with a long, exhaustive, and ultimately fruitless search.


To avoid such problems, it helps to remember this simple rule: every "success" test case must define a valid value for all
variables. For any individual variable `V`, no matter which values are chosen for the other variables in a success test case,
there must be at least one valid value of `V` that is compatible with them.

For example, the following variable definitions are infeasible. There is no way to complete a success test case containing
`Shape=Square` because there is no valid value for `Color` that is compatible with it.

```json
...
"Shape": {
  "values": {
    "Square": {"properties": ["quadrilateral"]},
    "Circle": {}
  }
},
"Color": {
  "values": {
    "Red":   {"when": {"hasNone": ["quadrilateral"]}},
    "Green": {"when": {"hasNone": ["quadrilateral"]}},
    "Blue":  {"when": {"hasNone": ["quadrilateral"]}},
    "None":  {"failure": true}
  }
}
...
```

The only exception is for conditions in which a variable is defined to be entirely [irrelevant](#variable-conditions).  For example,
the following definitions are OK, because they explicitly declare that `Color` is incompatible with `Shape=Square`.

```json
...
"Shape": {
  "values": {
    "Square": {"properties": ["quadrilateral"]},
    "Circle": {}
  }
},
"Color": {
  "when": {"hasNone": ["quadrilateral"]},
  "values": {
    "Red":   {},
    "Green": {},
    "Blue":  {},
    "None":  {"failure": true}
  }
}
...
```

##### Large `anyOf` conditions #####

You can use an [`anyOf` condition](#condition-expressions) to define a logical "OR" expression. But beware an `anyOf` that contains
a large number of subexpressions. When Tcases is looking for value combinations to satisfy such a condition, it must evaluate a
large number of possibilities. As the number of subexpressions increases, the number of possibilities increases exponentially!
This can quickly get out of hand, even when a satisfying combination exists. And things go from bad to worse if this `anyOf`
makes an intended test case infeasible.  If it looks to you like Tcases is slow or frozen, that may be what's going on.

If you face this situation, you should try to find a way simplify the large `anyOf` condition. For example, you may be able to
eliminate subexpressions by assigning special properties that produce an equivalent result.


## Defining Input Coverage ##

Tcases generates test case definitions by creating combinations of values for all input variables. But how does it come up with
these combinations? And why these particular combinations and not others? And just how good are these test cases? Can you rely
on them to test your system thoroughly?

Good questions. And here's the basic answer: Tcases generates the minimum number of test cases needed to meet the coverage
requirements that you specify. But to understand what that means, you need to understand how Tcases measures coverage.

### Combinatorial Testing Basics ###

Tcases is concerned with input space coverage -- how many of the feasible combinations of input values are tested. To measure
input space coverage, Tcases is guided by concepts from the field of
[combinatorial testing](http://csrc.nist.gov/groups/SNS/acts/index.html). As testers, we're looking for combinations of input
values that will trigger a _failure_, thus exposing a _defect_ in the SUT. But, in general, we can't afford the effort to test
every combination. We have to settle for some subset. But how?

Suppose we tried the following approach. For the first test case, just pick a valid value for every input variable. Then, for
the next test case, pick a different valid value for every variable. Continue this until we've used every valid value of every
variable at least once. Of course, as we do this, we'll skip over any infeasible combinations that don't satisfy our
constraints. The result will be a fairly small number of test cases. In fact, assuming there are no constraints on input values,
the number of "success" cases created by this procedure will be S, where S is the maximum number of valid values defined for any
one variable. For the failure cases, we can do something similar by creating a new test case for each invalid value,
substituting it into an otherwise-valid combination of other values. That gives us F more test cases, where F is the total
number of invalid values for all variables. So that's S+F tests cases, a pretty small test suite that ought to be quite
doable. But what is the coverage? Well, we've guaranteed that every value of every variable is used at least once. That is what
is known as "1-way coverage" or "1-tuple coverage" -- all "combinations" of 1 variable. (This is also known as "each choice
coverage".)


But is that good enough? Experience and [research](https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=8960929) teach us
that many failures are triggered by the interaction of two or more variables. So maybe we should aim for a higher level of input
coverage. We could iterate over every pair of input variables and consider every combination of their values.

For example, the `file.contents.linesLongerThanPattern` variable has 2 valid values and the `file.contents.patternMatches`
variable has 2 valid values. That makes 4 combinations for this pair of variables. For each pair, create a test case and fill it
out with values for all of the other variables. In the end, we'll have a test suite that uses every such pair at least once --
that's "2-way coverage" or "2-tuple coverage" (also known as "pairwise coverage"). This is a much stronger test suite -- more
likely to find many defects -- but it's also a larger number of test cases.

We can extend the same approach to even higher levels of combinatorial coverage -- 3-way coverage, 4-way coverage, etc. With
each higher level, our tests become more powerful. But the price is that the number of test cases increases rapidly with each
additional level. At some point, the gain is not worth the pain. In fact, research indicates that very few failures are caused
by the interaction of 4 or more variables, and failures that require an interaction of 6 or more variables are virtually
unknown. Most failures appear to be triggered by 1- or 2-way interactions. But that doesn't necessarily mean you should stop at
2-way coverage.  Every system has its own unique risks. Also, not all variables interact equally -- you may have some sets of
variables that need a higher level of coverage than the rest.

Note that the number of test cases required to meet a specific level of coverage depends on many factors. Naturally, the number
of test cases needed for N-way coverage increases for larger values of N. Also, variables that have a large number of values
create more combinations to be covered, which may demand more test cases. Also, when there are constraints among input values,
the number of test cases tends to increase. For example, a [variable condition](#variable-conditions) means that some test cases must
use `"NA": true` for that variable, which means that additional test cases are needed to cover the real values.

### Failure Cases Are Different! ###

Notice that when we talk about the various levels of N-way variable combinations, we are careful to apply these combinations
only to _valid_ values of these variables. Why? Because failures cases are different!

Clearly, for every variable, each invalid value (i.e. with `"failure": true`) deserves its own test case.  A "failure" case like
this should have an invalid value for exactly one variable and valid values for all of the other variables.  That's the only
sure way to verify that an expected failure can be attributed solely to this invalid value. Consequently, it should be
understood that _the number of failure cases generated by Tcases will always be equal to the number of invalid values_,
regardless of the combinatorial coverage level used.

Of course, for any given level of N-way valid combinations, the set of failure cases will nearly always include some of those
combinations. But that doesn't count! For true N-tuple coverage, a test set must include every valid combination in at least one
"success" case.  Again, the reason for this should be clear. That's the only sure way to verify the expectation that this
combination leads to a valid result.

### Default Coverage ###

For the record, the default for Tcases is 1-tuple coverage. In other words, unless you specify otherwise, Tcases will translate
your system input definition into a minimal set of test case definitions that uses every value of every variable -- every
"1-tuple" -- at least once, while satisfying all constraints.

Is that good enough? Maybe. If you've built your system input definition carefully, you're likely to find that a 1-tuple
coverage test suite also achieves upward of 75% basic block (line) coverage of the SUT. In fact, using Tcases in tandem with a
structural coverage tool like Emma or Cobertura can be very effective. Tip: Use Tcases to create a 1-tuple coverage test suite,
then measure structural coverage to identify any gaps in the system input definition. You can repeat this process to quickly
reach a small but powerful set of test cases.

But to get the tests you need faster, you may need to selectively apply 2-tuple coverage or higher. The next section explains
how.

### Defining Higher Coverage ###

For higher levels of coverage, you need to create a _generator definition_ that specifies your coverage
requirements in detail. A generator definition, which is another document that Tcases applies to your system input definition,
defines a set of "generators".

Generators definitions are optional -- if omitted, [the default coverage generator](#default-coverage) is used for each
function. By convention, generator definitions appear in a file named `${myProjectName}-Generators.json`.  But, if you prefer,
you can specify a different file name using the `-g` option of the
[`tcases` command](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/TcasesCommand.Options.html).


The simplest possible generator definition is equivalent to the default: for all functions, 1-tuple coverage for all variables. It looks like this:

```json
{ 
  "*": {} 
} 
```


To require 2-tuple coverage for all variables of the `find` function, you would create a generator definition like this:

```json
{ 
  "find": {
    "tuples": 2
  } 
} 
```


To require 3-tuple coverage only for the function named `F`, while generating 2-tuple coverage for all other functions, you
would create a generator definition like the one below. Notice that you can explicitly identify "all functions" using the
special function name `*`.

```json
{ 
  "*": {
    "tuples": 2
  },
  "F": {
    "tuples": 3
  }
} 
```

### Defining Multiple Levels Of Coverage ###

When you look carefully at the functions of your system-under-test, you may well find that some of them call for more intense
testing than others. That's what a generator definition allows you to do. In fact, when you look carefully at a single function,
you may be more concerned about the interactions between certain specific variables. You may even want to test every possible
permutation for a small subset of key variables. Is it possible to get high coverage in a few areas and basic coverage
everywhere else? Why, yes, you can. This section explains how.

You've already seen how you can specify different levels of coverage for different functions. For finer control, you can use one
or more _combiner definitions_.  A combiner definition is an object that defines the level of coverage generated for a
specific subset of variable definitions. You specify which variables to combine using a variable "path pattern".

For example, here is a generator definition for the `find` function that specifies 2-tuple coverage for all variables in the
`file.contents` variable set, with 1-tuple coverage (the default) for other variables

```json
{
  "find": {
    "combiners": [
      {
        "tuples": 2,
        "include": [ "file.contents.**" ]
      }
    ]
  }
}
```

A variable path pattern describes a path to a specific variable definition, possibly nested within a variable set
hierarchy. Wildcards allow you to match all immediate children (`*`) or all descendants (`**`) of a variable set. Note that a
pattern can contain at most one wildcard, which can appear only at the end of the path.


You can use a combination of `include` and `exclude` properties to concisely describe exactly which variables to combine. 
For example, here is a generator for the `find` function that generates 1-tuple coverage (the default) for all variables in the
`file.contents` variable set, _except_ for `file.contents.patternsInLine`, with 2-tuple coverage for all other variables.

```json
{
  "find": {
    "tuples": 2,
    "combiners": [
      {
        "include": [ "file.contents.*" ],
        "exclude": [ "file.contents.patternsInLine" ]
      }
    ]
  }
}
```


A combiner definition that specifies the special value `"tuples": 0` generates test cases that include all possible value
permutations of the included variables.  Obviously, this setting has the potential to create a huge number of test cases, so it
should be used sparingly and only for small sets of variables. 


Each combiner definition describes how to combine a specific set of variables. But what about the variables that are not
included in any combiner? For these, Tcases automatically creates a default combiner definition, using the default `tuples`
defined for the function.


## Managing A Tcases Project ##

Using Tcases to design a test suite means:

* Learning about the expected behavior of the SUT
* Creating an initial system input definition
* Generating, evaluating, and improving test case definitions
* Evaluating and improving coverage requirements
* Changing input definitions to handle new cases

You might finish all these tasks very quickly. Or this effort might extend over a significant period of time. Either way, that's
a project.  This section offers some tips to help you complete your Tcases project more effectively.

### Managing Project Files ###

A Tcases project must deal with several closely-related files: a system input definition, zero or more generator definitions,
and the test case definition document that is generated from them (possibly in multiple forms). The `tcases` command implements
some conventions that make it easier to keep these files organized.

The `tcases` command allows you to refer to all of the files for a project named `${myProjectName}` using the following
conventions.

* `${myProjectName}-Input.json`: the system input definition file
* `${myProjectName}-Generators.json`: the generator definition file
* `${myProjectName}-Test.json`: the test case definition file

For example, here's a simple command to run Tcases.

```bash
tcases ${myProjectName} 
```

This command performs the following actions.


1. Reads the system input definition from `${myProjectName}-Input.json`
1. Reads the generator definition from `${myProjectName}-Generators.json`, if it exists
1. Writes test case definitions to `${myProjectName}-Test.json`

Of course, you can use various options for the `tcases` command to customize this default pattern. For details, see the
[`TcasesCommand.Options`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/TcasesCommand.Options.html) class,
or run `tcases -help`.

### Copying A Tcases Project ###

You can copy all the files in a Tcases project to a different directory or to a different project name or even to a different
content type, using the `tcases-copy` command (or, if using Maven, the `tcases:copy` goal). For complete details, run `tcases-copy -help`.

For example:

```bash
# Copy all of the files for the "find" project to /home/tcases/projects
tcases-copy --toDir /home/tcases/projects find

# Copy all of the files for the "find" project to a project named "myProject" in /home/tcases/projects
tcases-copy --toDir /home/tcases/projects --toName myProject find

# Convert all of the files for "myProject" to JSON
tcases-copy --toType json myProject-Input.xml
```

### Reusing Previous Test Cases ###

You know the feeling. You've spent days figuring out a minimal set of test cases that covers all test requirements. Then the
developer walks up with the great news: they've decided to add a new feature with some new parameters. And they've changed their
minds about some things. You know that required parameter? Well, it's optional now -- leaving it blank is no longer an
error. Sometimes is seems they're doing this just to torture you. But, honestly, most of the time it's just the normal
progression of a development project. After a few iterations, you've gained more knowledge that you need to apply to the system
you're building. Or after a release or two, it's time to make the system do new tricks.

Either way, it's back to the ol' test drawing board. Or is it? You're not changing everything. Why can't you just tweak the test
cases you already have? Funny you should ask. Because that's exactly what Tcases can do. In fact, it's the default way of
working. Remember that simple `tcases` command line?

```bash
tcases ${myProjectName} 
```

Here's what it _really_ does:


1. Reads the system input definition from `${myProjectName}-Input.json`
1. Reads the generator definition from `${myProjectName}-Generators.json`, if it exists
1. _And reads previous test cases_  from `${myProjectName}-Test.json`, if it exists
1. _Then writes new test case definitions_ to `${myProjectName}-Test.json` which
   reuse as much of the previous test cases as possible, extending or modifying them as needed

You might prefer to ignore previous test cases and just create new ones from scratch. That's especially true in the early stages
of your project while you're still working out the details of the system input definition. If so, you can use the `-n` option to
always create new tests cases, ignoring any previous ones.

```bash
tcases -n ${myProjectName} 
```

### Mix It Up: Random Combinations ###

By default, Tcases creates combinations of input variables by marching through the system input definition top-to-bottom,
picking things up in the order in which it finds them. You might try to exploit that natural order, although satisfying
constraints can take things off a predictable sequence. That's why you really shouldn't care too much about which combinations
Tcases comes up with.  Even better? Ask Tcases to randomize its combination procedure.

You can define random combinations in your generator definition by using the `seed` attribute -- see the example below.  This
integer value acts as the seed for a random number generator that controls the combination process. Alternatively, you can
(re)define the seed value using [command line options](#simple-generator-definitions) described in later sections.  By specifying the seed
explicitly, you ensure that _exactly the same_ random combinations will be used every time you run Tcases with this generator
definition.

```json
{
  "find": {
    "seed": 200712190644
  }
}
```

The results of random combination can be very interesting. First, you can end up with test cases that you might not have
considered, even though they are perfectly valid and produce the same coverage. Sometimes that's just enough to expose a defect
that might otherwise have been overlooked, simply because no one thought to try that case. This is an application of the
principle of "gratuitous variety" to improve your tests. This also produces another benefit -- sometimes an unusual combination
can demonstrate a flaw in your test design. If a combination just doesn't make sense, then it's likely that a constraint is
missing or incorrect.

Finally, random combinations can occasionally reduce the number of test cases needed to meet your coverage requirements. That's
because some combinations may "consume" variable tuples more efficiently than other equally-valid combinations. Tcases does not
attempt to spend the enormous effort needed to guarantee an optimally minimal set of test cases. It simply starts at the
beginning and does its best to get quickly to the end. But a random walk through the combinations may lead Tcases to a more
efficient path. If you're concerned about the size of your test suite, try the [Tcases Reducer](#reducing-test-cases-a-random-walk).

### Reducing Test Cases: A Random Walk ###

A random walk through the combinations may lead Tcases to a smaller set of test cases. So you could try repeatedly altering your
generator definition with a bunch of different `seed` values, searching for one that minimizes the size of the generated test
definition file. Sounds tedious, huh? So, don't do that -- use the Tcases Reducer instead.

Here how to do it, using the `tcases-reducer` command.

```bash
cd ${tcases-release-dir}
cd docs/examples/json 
tcases-reducer find-Input.json
```

And the result? Now there is a `find-Generators.json` file that looks something like this:
a generator definition that uses a random seed for each function.

```json
{
  "find": {
    "seed": 1909310132352748544
  }
}
```

But why this seed value? For a detailed view, look at the resulting `tcases-reducer.log` file (see example below). First, the
Reducer generates test cases without using a random seed, producing 10 test cases.  Then, the Reducer tries again, and it
reduces the results to 9 test cases.  Then, the Reducer tries several more time, each time using a different random
seed. Finally, the Reducer cannot find a greater reduction, so it terminates.

```
INFO  org.cornutum.tcases.Reducer - Reading system input definition=find-Input.json
INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[find]: generating test cases
...
INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[find]: completed 10 test cases
INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[find]: generating test cases
...
INFO  o.c.t.generator.TupleGenerator - FunctionInputDef[find]: completed 9 test cases
INFO  org.cornutum.tcases.Reducer - Round 1: after 2 samples, reached 9 test cases
...
INFO  org.cornutum.tcases.Reducer - Round 2: after 10 samples, terminating
INFO  org.cornutum.tcases.Reducer - Updating generator definition=find-Generators.json
```


The Reducer handles all the work of searching for the best random seed, without overwriting any existing test definition files.
Here's how it works.  The reducing process operates as a sequence of "rounds". Each round consists of a series of test case
generations called "samples". Each sample uses a new random seed to generate test cases for a specified function (or, by
default, all functions) in an attempt to find a seed that produces the fewest test cases. If all samples in a round complete
without reducing the current minimum test case count, the reducing process terminates. Otherwise, as soon as a new minimum is
reached, a new round begins. The number of samples in each subsequent round is determined using a "resample factor". At the end
of the reducing process, the generator definition file for the given system input definition is updated with the random seed
value that produces the minimum test case count.

Even though the Reducer produces a random seed that minimizes test cases, you still have to consider if these test cases are
satisfactory. You might wonder if a different seed might produce an equally small but more interesting set of test cases. If so,
try using the `-R` option. This tells the Reducer to ignore any previous random seed in the generator definition and to search
for a new seed value.

For details about all the options for the `tcases-reducer` command (and its Windows counterpart `tcases-reducer.bat`), see the
Javadoc for the [`ReducerCommand.Options`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/ReducerCommand.Options.html) class.
To get help at the command line, run `tcases-reducer -help`.

### Avoiding Unneeded Combinations ###

Even when Tcases is generating test cases for the default 1-tuple coverage, it's typical to see some input values used many
times. This is most likely for those variable definition that contain only a few value definitions. Even after these values have
been used, Tcases will continue to reuse them to fill out the remaining test cases needed to complete the test suite. In some
situations, this can be a bit of a pain. Sometimes there is a value definition that you need to test at least once, but for
various reasons, including it multiple times adds complexity without really increasing the likelihood of finding new failures.
In this case, you can use the `once` property as a hint to avoiding reusing a value more than once.

For example, the `find` command requires that the `pattern` must not exceed the maximum length of a line in the file. Even one
line longer than the pattern would be enough to avoid this error condition. In fact, the principles of boundary value testing
suggest that it's a good idea to have a test case that has _exactly_ one line longer. Therefore:

```json
...
"linesLongerThanPattern": {
  "type": "integer",
  "format": "int32",
  "values": {
    "1": {
      "properties": ["matchable"]
    },
    "many": {
      "minimum": 2,
      "maximum": 32,
      "properties": ["matchable"]
    },
    "0": {
      "failure": true
    }
  }
},
...
```

But this is a corner case that doesn't bear repeating. It's a chore to create a test file that meets this special condition, and
it's complicated to stretch such a file to meet additional conditions. Moreover, it's unlikely that this special condition will
have higher-order interactions with other variable combinations. So let's add `"once": true` to request Tcases to include this
value in only one test case.

```json
...
"linesLongerThanPattern": {
  "type": "integer",
  "format": "int32",
  "values": {
    "1": {
      "properties": ["matchable"],
      "once": true
    },
    "many": {
      "minimum": 2,
      "maximum": 32,
      "properties": ["matchable"]
    },
    "0": {
      "failure": true
    }
  }
},
...
```

Nice! But keep in mind that the `once` hint may not always be respected.  Even when `"once": true`, a value may be used more
than once if it is needed to satisfy a constraint in remaining test cases.

The `once` hint is actually a shortcut that applies only to a 1-tuple for a single variable value. If
the [generator definition](#defining-higher-coverage) includes this variable in higher-order tuples, `once` has no effect. But the same situation
can occur with higher-order combinations, too. For example, although you may want pairwise coverage for a certain set of
variables, one or more of these 2-tuples may be special cases that should be used at most once. To define such exceptions you
can add one or more `once` tuples to the combiners in your generator definition. 
For example, the following generator definition for the `find` function specifies 2-tuple coverage for all variables but tells
Tcases to create only one test case that uses a certain 2-tuple combination.: a line containing multiple matches of a pattern
containing quotation marks.

```json
{ 
  "find": { 
    "combiners": [ 
      { 
        "tuples": 2, 
        "once": [ 
          { 
            "pattern": "quotedQuotes", 
            "file.contents.patternsInLine": "many" 
          } 
        ]
      } 
    ] 
  } 
} 
```

### Simple Generator Definitions ###

Tcases provides some options to make it easier to create and update a simple [generator definition](#defining-higher-coverage)
document.

#### Defining A Random Seed ####

To define a random combination seed, use the `-r` option. For example, the following command generates test cases with a default
generator that uses the specified seed value.

```bash
tcases -r 299293214 ${myProjectName} 
```

If you already have a `${myProjectName}-Generators.json` file, this command will update the file by adding or changing the
default `seed` value, as shown below. If no `${myProjectName}-Generators.json` file exists, it will create one.

```json
{
  "find": {
    "seed": 299293214
  }
}
```

If you'd like to randomize combinations but you're not particular about the seed value, use the `-R` option, and Tcases will
choose a random seed value for you. This option can be handy when you want to see if a different seed value might produce more
interesting test case combinations.

#### Defining The Default Coverage Level ####

To define the default coverage level for all functions, use the `-c` option. For example, the following command generates test
cases with a default generator that uses the specified coverage level.

```bash
tcases -c 2 ${myProjectName} 
```

If you already have a `${myProjectName}-Generators.json` file, this command will
update the file by adding or changing the default `tuples` value, as shown
below. If no `${myProjectName}-Generators.json` file exists, it will create one.

```json
{
  "find": {
    "tuples": 2
  }
}
```

## Transforming Test Cases ##

The test case definitions that Tcases produces are not directly executable. Their purpose is to specify and guide the
construction of actual tests. But because test case definitions can appear in a well-defined JSON document, it's not hard to
transform them into a more concrete form. This section describes the options Tcases offers for output transformations.

### Creating An HTML Report ###

The JSON form for test case definitions is pretty simple. But let's face it -- reading a long JSON document can be tedious.
It's not necessarily what you'd want to hand someone for guidance during manual testing. So how about looking at the same
information in a nice Web page on your browser?  To do that, just add the `-H` option to your `tcases` command, and Tcases will
automatically write test case definitions in the form of an HTML file.

Here's a simple example. Try out these commands:

```bash
cd ${tcases-release-dir}
cd docs/examples/json 
tcases -H find 
```


This runs Tcases on the input definitions in `find-Input.json` and produces a file named `find-Test.htm`.  Open this file with
your browser and you'll see something like [this simple HTML report](http://www.cornutum.org/tcases/docs/find-Test-Html.png).
This report allows you to browse through all of the test cases and look at each one of them in detail. You'll see all of the
input values needed for the selected test case (omitting any input variables that are [irrelevant](#variable-conditions) for this test
case).

Don't particularly care for this report format? You can define and apply your own presentation format using the
[`TestDefToHtmlFilter`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/io/TestDefToHtmlFilter.html) class.

### Creating JUnit/TestNG Tests ###

Transforming test cases into JUnit or TestNG code is a capability that is built into the `tcases` command. How does it work?
Just add the `-J` option, and Tcases will automatically writes test case definitions in the form of Java code for a JUnit
test. The same code works for TestNG, too.

Here's a simple example. Try out these commands:

```bash
cd ${tcases-release-dir}
cd docs/examples/json 
tcases -J < find-Input.json 
```

Here's what you'll see printed to standard output: each test case definition has been transformed into a `@Test` method. The
name of the method is based on the function name. And the Javadoc comments describe the input values for this test case.
Similarly, all input value assignments are shown in the body of the method. Otherwise, the body of the method is empty, waiting
for the implementation to be filled in by you. For failure test cases, the Javadoc highlights the single invalid value that
defines the case.


```java
  /**
   * Tests {@link Examples#find find()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. find (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> pattern </TD> <TD>  </TD> </TR>
   * <TR><TD> fileName </TD> <TD> defined </TD> </TR>
   * <TR><TD> file.exists </TD> <TD> true </TD> </TR>
   * <TR><TD> file.contents.linesLongerThanPattern </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> file.contents.patternMatches </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> file.contents.patternsInLine </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void find_0()
    {
    // properties = fileExists,fileName,patternEmpty

    // Given...
    //
    //   pattern = 
    //
    //   fileName = defined
    //
    //   file.exists = true
    //
    //   file.contents.linesLongerThanPattern = (not applicable)
    //
    //   file.contents.patternMatches = (not applicable)
    //
    //   file.contents.patternsInLine = (not applicable)
    
    // When...

    // Then...
    }
...    
```

The `-J` option is most useful when the `System` corresponds to a class and each `Function` corresponds to a class method to be
tested. Accordingly, the Javadoc includes an `@link` to the method-under-test, as shown above. You can customize the form of
this `@link` by defining either the `class` parameter or the `system` parameter. For example, if you use the options `-J -p
class=MyClass`, then the output looks like this:

```java
  /**
   * Tests {@link MyClass#find find()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. find (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> pattern </TD> <TD>  </TD> </TR>
   * <TR><TD> fileName </TD> <TD> defined </TD> </TR>
   * <TR><TD> file.exists </TD> <TD> true </TD> </TR>
   * <TR><TD> file.contents.linesLongerThanPattern </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> file.contents.patternMatches </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> file.contents.patternsInLine </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void find_0()
    {
    ...
```

Alternatively, if you use the options "`-J -p system=MySystem`", then the output looks like this:

```java
  /**
   * Tests MySystem using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. find (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> pattern </TD> <TD>  </TD> </TR>
   * <TR><TD> fileName </TD> <TD> defined </TD> </TR>
   * <TR><TD> file.exists </TD> <TD> true </TD> </TR>
   * <TR><TD> file.contents.linesLongerThanPattern </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> file.contents.patternMatches </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> file.contents.patternsInLine </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void find_0()
    {
    ...
```


And, if you'd rather not have input value assignments shown in the test method body, you can exclude them by adding the option
`-p values=false`.


Using the `-J` option also changes the default output file for the `tcases` command.  Normally, when you're working with a
Tcases [project](#managing-project-files), generated test case definitions are written by default to a file named
`${myProjectName}-Test.json`. But with `-J`, the generated `@Test` methods are written by default to a file named
`${myProjectName}Test.java`. Exception: if your `${myProjectName}` is not a valid Java class identifier, a slightly modified
form of the project name is used instead.

For example, the following command will write generated test definitions in the form of `@Test` methods to a file named
`findTest.java`

```bash
tcases -J find 
```

### Using Output Annotations ###

For a transformation to produce concrete test cases, sometimes the basic information in the input model -- functions, variables,
and values -- is not enough. You need to add extra information that is not important for generating the test cases but is
necessary to form the final output. That's what _output annotations_ are for.

An output annotation is a special property setting -- a name-value pair -- that you can add to various
elements of a system input definition.  It has no effect on test cases that Tcases generates. But Tcases will
accumulate output annotations and attach them to the resulting system test definition document.
In addition, Tcases will automatically attach [output annotations listing the properties of each test case](#property-annotations).
From there, any output transform can use these output annotations to complete tests cases in their final form.

#### How it works ####


We can add the following kinds of output annotations. (See examples [here](http://www.cornutum.org/tcases/docs/examples/json/annotations-Input.json).)


##### System annotations #####

System annotations are created by adding a `has` object to the top-level system definition object. Each system annotation is
transferred to the output document by applying it to the top-level test definition object.  In addition, each system annotation is
added to all function and test case objects in the system test definition.

For example, given the following system annotation...

```json
{
  "system": "Things",
  "has": {
    "systemType": "Graphics"
  },
  ...
}
```

... the resulting test definition looks like this:

```json
{
  "system": "Things",
  "has": {
    "systemType": "Graphics"
  },
  "Make": {
    "has": {
      "systemType": "Graphics"
    },
    "testCases": [
      {
        "id": 0,
        "name": "Color.Hue='Red'",
        "has": {
          "systemType": "Graphics"
        },
        ...
      },
      ...
    ]
  }
}
```
          
##### Function annotations #####

Function annotations are created by adding a `has` object to a function input definition. Each function annotation is
transferred to the output document by applying it to the corresponding output function defitino.  In addition, each function
annotation is added to all test cases within its scope. Annotations for a function override any annotations of the same name
defined for the system.

For example, given the following funtion annotation...

```json
{
  "system": "Things",
  "has": {
    "systemType": "Graphics"
  },
  "Make": {
    "has": {
      "measurement": "None"
    }
    ...
  }
}
```

... the resulting test definition looks like this:

```json
{
  "system": "Things",
  "has": {
    "systemType": "Graphics"
  },
  "Make": {
    "has": {
      "systemType": "Graphics",
      "measurement": "None"
    },
    "testCases": [
      {
        "id": 0,
        "name": "Color.Hue='Red'",
        "has": {
          "systemType": "Graphics",
          "measurement": "None"
        },
        ...
      },
      ...
    ]
  }
}
```

          
##### Variable binding annotations #####

Variable binding annotations are created by adding a `has` object to a variable or a variable set definition. You can also
create a variable binding annotation that applies to a group of variables by adding a `has` object to an input type group. Or
you can create a variable binding annotation that is value-specific by adding a `has` object to a value definition. Each
variable binding annotation is transferred to the output document by applying it to all variable binding objects within its
scope.

Annotations for a value override any annotations of the same name defined for a variable, which override annotations for a
variable set, which override annotations for an input type group.

For example, given the following variable binding annotations...

```json
{
  "system": "Things",
  "has": {
    "systemType": "Graphics"
  },
  "Make": {
    "has": {
      "measurement": "None"
    },
    "arg": {
      "has": {
        "valueType": "Valid"
      },
      "Color": {
        "has": {
          "measurement": "Ordinal"
        },
        "members": {
          "Hue": {
            "has": {
              "measurement": "Nominal"
            },
            "values": {
              "Red": {},
              "Green": {},
              "Blue": {}
            }
          },
          ...
          "Saturation": {
            "values": {
              "Pale": {},
              "Even": {},
              "Intense": {},
              "Undefined": {
                "has": {
                  "valueType": "Invalid"
                },
                "failure": true
              }
            }
          }
        }
      },
      ...
    }
  }
}
```

... the resulting test definition looks like this:

```json
{
  "system": "Things",
  "has": {
    "systemType": "Graphics"
  },
  "Make": {
    "has": {
      "systemType": "Graphics",
      "measurement": "None"
    },
    "testCases": [
      ...
      {
        "id": 6,
        "name": "Color.Saturation='Undefined'",
        "has": {
          "systemType": "Graphics",
          "measurement": "None"
        },
        "arg": {
          "Color.Hue": {
            "has": {
              "valueType": "Valid",
              "measurement": "Nominal"
            },
            "value": "Red"
          },
          ...
          "Color.Saturation": {
            "has": {
              "valueType": "Invalid",
              "measurement": "Ordinal"
            },
            "failure": true,
            "value": "Undefined"
          },
          ...
        }
      }
    ]
  }
}
```


##### Multi-level output annotations #####

Why are system and function annotations also copied to all of the associated test case definitions? Because such annotations can
have multiple purposes. In some cases, these annotations can be used to form the corresponding level of the transformed output
document. In other cases, these annotations can be used to define system- or function-wide defaults for values used to form
individual concrete test cases.

#### Property annotations ####

The [value properties](#value-properties) that characterize a test case can be useful meta-data for further transformations of test case data.
For this reason, Tcases automatically attaches them to each generated test case using a special output annotation
named "properties".


## Further Reference ##

Want more technical details about Tcases? Here are links to some additional information.


* The [`find` command example](http://www.cornutum.org/tcases/docs/examples/json/find-Input.json)

* The `tcases` [command line](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/TcasesCommand.Options.html)

* Document schemas

  * [System input definition](http://www.cornutum.org/tcases/system-input-schema.json)
  * [Generator definitions](http://www.cornutum.org/tcases/generators-schema.json)
  * [System test definition](http://www.cornutum.org/tcases/system-test-schema.json)


* Related testing techniques

  * [Black-box test design](http://en.wikipedia.org/wiki/Black-box_testing)
  * [Equivalence class partitioning](http://en.wikipedia.org/wiki/Equivalence_partitioning)
  * [Pairwise testing](https://en.wikipedia.org/wiki/All-pairs_testing)
  * [Combinatorial testing](http://csrc.nist.gov/groups/SNS/acts/index.html)

