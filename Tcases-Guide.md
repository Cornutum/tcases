# Tcases: The Complete Guide #

## Contents ##

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

First, you create a [system input definition](#systemInputDef), a document that defines your system as a set of
[functions](#functionInputDef). For each system function, the system input definition defines the [variables](#varDef) that
characterize the function input space.

Then, you can create a [generator definition](#genDef). That's another document that defines the coverage you want for each
system function. The generator definition is optional. You can skip this step and still get a basic level of coverage.

Finally, you run Tcases. Tcases is a Java program that you can run from the command line or from your favorite IDE.  Tcases
comes with built-in support for running using a shell script.  You can also run Tcases with Maven using the
[Tcases Maven Plugin](http://www.cornutum.org/tcases/docs/tcases-maven-plugin/index.html).  Using your input definition and your
generator definition, Tcases generates a [system test definition](#systemTestDef).  The system test definition is a document
that lists, for each system function, a set of test cases that provides the specified level of coverage. Each test case defines
a specific value for every function input variable. Tcases generates not only valid input values that define successful test
cases but also invalid values for the tests cases that are needed to verify expected error handling.

Of course, the system test definition is not something you can execute directly. But it follows a well-defined schema, which
means you can use a variety of transformation tools to convert it into a form that is suitable for testing your system. For
example, Tcases comes with a built-in transformer that converts a system test definition into a Java source code template
for a [JUnit or TestNG test class](#junit).  You can also automatically transform a system test definition into a simple
[HTML report](#html).



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

  1. Visit the [Central Repository search page](https://search.maven.org/search?q=tcases-shell). 
  1. Search for "tcases-shell".
  1. You will see the most recent release of "tcases-shell". (To see all N previous versions, select "(N)" under "Latest Version".).
  1. Use the downward arrow button to select the type of file you want to download. Choose either as a ZIP file or a compressed `tar` file (`tar.gz`).

Extract the contents of the distribution file to any directory you like -- this is now your _"Tcases home directory"_. Unpacking
the distribution file will create a _"Tcases release directory"_ -- a subdirectory of the form `tcases-`_m_._n_._r_ -- that
contains all the files for this release of Tcases. The release directory contains the following subdirectories.

  * `bin`: Executable shell scripts used to run Tcases 
  * `docs`: User guide, examples, and Javadoc 
  * `lib`: All JAR files needed to run Tcases 


One more step and you're ready to go: add the path to the `bin` subdirectory to the `PATH` environment variable for your system.


### JSON? Or XML? ###

The preferred form for all Tcases documents is JSON, which is capable of expressing all Tcases features and which is used for
all of the examples in this guide.  But the original version of Tcases used XML for all documents, and XML is still supported
for older documents. You can find all the details about using Tcases with XML in the [original version of this
guide](http://www.cornutum.org/tcases/docs/Tcases-Guide.htm).

### Running From the Command Line ###

You can run Tcases directly from your shell command line. If you use `bash` or a similar UNIX shell, you can run the `tcases`
command.  Or if you are using a Windows command line, you can run Tcases with the `tcases.bat` command file, using exactly the
same syntax.

For example, for a quick check, you can run one of the examples that comes with Tcases, using the following commands.

```
cd ${tcases-release-dir}
cd docs/examples/json 
tcases < find-Input.json 
```

For details about the interface to the `tcases` command (and the `tcases.bat` command, too), see the Javadoc for the
[`TcasesCommand.Options`](http://www.cornutum.org/tcases/docs/api/org/cornutum/tcases/TcasesCommand.Options.html) class.  To get
help at the command line, run `tcases -help`.

### Understanding Tcases Results ###

What happens when you run Tcases? Tcases reads a [system input definition](#systemInputDef), a document
that defines the "input space" of the system function to be tested. From this, Tcases produces a different document
called a <A name="systemTestDef">_system test definition_</A>, which describes a set of test cases.


Try running Tcases on one of the example system input definitions. The following commands will generate
test cases for the `find` command [example](#exampleFind), which is
explained in [full detail](#input) later in this guide.

```
cd ${tcases-release-dir}
cd docs/examples/json 
tcases < find-Input.json 
```


The resulting system test definition is written to standard output. Here's what it looks like: for
the "find" [function](#functions), a list of test case definitions, each of which defines values for all of
the function's input [variables](#vars).

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

## Modeling The Input Space ##

Tcases creates test definitions based on a _system input definition_ that you create. But how do you do that? That's what this
section aims to explain.

A _system input definition_ is a document that models the "input space" of the system-under-test (SUT). We say it "models"
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

### An Example: The find Command ###

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

A <A name="systemInputDef">system input definition</A> describes a specific system-under-test, so the root object of the document looks like this:

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
details about these are shown in a [later section](#exampleEnv).)

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
[equivalence class partitioning](http://en.wikipedia.org/wiki/Equivalence_partitioning). You use each value definitino to
identify a _class_ of values. By definition, all specific values in this class are test-equivalent. We don't need to test them
all -- any one of them will do.


In the case of the `fileName` variable, we've decided that the significance of file name itself is whether it is present or not,
and we've chosen to identify those two variations as "defined" and "missing".  But the name you use to identify each value
class is entirely up to you -- it is part of the input model you design to describe your tests and it appears in the test case
definitions that Tcases generates, to guide your test implementation.

### Defining Value Schemas ###

TBD

### Defining Variable Sets ###

It's common to find that a single logical input actually has lots of different characteristics, each of which creates a
different "dimension of variation" in the input space. For example, consider the file that is searched by the `find`
command. Does it even exist? Maybe yes, maybe no -- that's one dimension of variation that the tests must cover. And what about
its contents? Of course, you'd like to test the case where the file contains lines that match the pattern, as well the case
where there are no matches. So, that's another dimension of variation. The spec says that each matching line is printed exactly
once, even when it contain multiple matches. Wouldn't you want to test a file that has lines with different numbers of matches?
Well, there's yet another dimension of variation. One file -- so many dimensions!


You can model this complex sort of input as a "variable set" (or "varSet"), using the `members` property.  With a varSet, you
can describe a single logical input as a set of multiple variable definitions.  A varSet can even contain another varSet, creating
a hierarchy of logical inputs that can be extended to any number of levels.


For example, the single `file` input to the `find` command can modeled by the following <A name="exampleEnv">variable set
definition</A>.

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


Yes, and when generating test cases, that's essentially how Tcases handles it. But defining a complex
input as a varSet makes the input model simpler to create, read, and maintain. Also,
it allows you to apply constraints to an entire tree of variables at once, at you'll see in the next section.

### Defining Constraints: Properties and Conditions ###
#### Value properties ####
#### Value conditions ####
#### Failure values are different! ####
#### Variable conditions ####
#### Complex conditions ####
#### Cardinality conditions ####
#### But be careful! ####


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
coverage. We could iterate over every pair of input variables and consider every combination of their values. For example, the
`pattern.size` variable has 3 valid values and the `pattern.quoted` variable has 2 valid values. That makes 6 combinations for
this pair of variables (ignoring constraints). For each pair, create a test case and fill it out with values for all of the
other variables. In the end, we'll have a test suite that uses every such pair at least once -- that's "2-way coverage" or
"2-tuple coverage" (also known as "pairwise coverage"). This is a much stronger test suite -- more likely to find many defects
-- but it's also a larger number of test cases.

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
the number of test cases tends to increase. For example, a [variable condition](#varConditions) means that some test cases must
use `"NA": true` for that variable, which means that additional test cases are needed to cover the real values.

### Failure Cases Are Different! ###

Notice that when we talk about the various levels of N-way variable combinations, we are careful to apply these combinations
only to _valid_ values of these variables. Why? Because failures cases are different!

Clearly, for every variable, each invalid value (i.e. with `"failure": true`) deserves its own test case.  A "failure" case like
this should have an invalid value for exactly one variable and valid values for all of the other variables.  That's the only
sure way to verify that an expected failure can be attributed to solely to this invalid value. Consequently, it should be
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
### Defining Multiple Levels Of Coverage ###


## Managing A Tcases Project ##
### Managing Project Files ###
### Reusing Previous Test Cases ###
### Mix It Up: Random Combinations ###
### Reducing Test Cases: A Random Walk ###
### Avoiding Unneeded Combinations ###
### Simple Generator Definitions ###
### Troubleshooting FAQs ###


## Transforming Test Cases ##
### Creating An HTML Report ###
### Creating JUnit/TestNG Tests ###
### Using XSLT Transforms ###
### Using Output Annotations ###
#### Example: Generating test code ####
#### How it works ####
#### Property annotations ####


## Further Reference ##
