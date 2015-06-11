---
id: infer-workflow
title: Infer workflow
layout: docs
permalink: /docs/infer-workflow.html
section: User Guide
section_order: 01
order: 01
---

This page documents several ways of running Infer, that you can adapt
to your own project.

**tl; dr**:

1. Make sure your project is clean when you first run Infer on it
  (with `make clean`, or `gradle clean`, or ...).
2. When running Infer several times in a row, either clean your
  project as in step 1 in-between Infer runs, or add `--incremental`
  to the `infer` command.
3. These steps are not needed if you are not using an incremental
  build system, for instance if you are analyzing single files with
  `infer -- javac Hello.java`.
4. After a successful Infer run, you can explore Infer's reports in
  more details by running `inferTraceBugs` from the same directory.

## The two phases of an Infer run

Regardless of the input language (Java, Objective-C, or C), there are
two main phases in an Infer run:

### 1. The capture phase

Compilation commands are captured by Infer to translate the files to
be analyzed into Infer's own internal intermediate language.

This translation is similar to compilation, so Infer takes information
from the compilation process to perform its own translation. This is
why we call infer with a compilation command: `infer -- javac
File.java` or `infer -- clang -c file.c`. What happens is that the
files get compiled as usual, and they also get translated by Infer to
be analyzed in the second phase. In particular, if no file gets
compiled, also no file will be analyzed.

Infer stores the intermediate files in the results directory which by
default is created in the folder where the `infer` command is invoked,
and is called `infer-out/`.  You can change the name of the results
directory with the option `-o`, e.g.

```bash
infer -o /tmp/out -- javac Test.java
```

### 2. The analysis phase

In this phase, the files in `infer-out/` are analyzed by Infer.  Infer
analyzes each function and method separately. If Infer encounters an
error when analyzing a method or function, it stops there for that
method or function, but will continue the analysis of other methods
and functions. So, a possible workflow would be to run Infer on your
code, fix the errors generated, and run it again to find possibly more
errors or to check that all the errors have been fixed.

The errors will be displayed in the standard output and also in a file
`infer-out/bugs.txt`. We filter the bugs and show the ones that are
most likely to be real. In the results directory (`infer-out/`),
however, we also save a file `report.csv` that contains all the
errors, warnings and infos reported by Infer in csv format.


## Incremental and non-incremental workflows

By default, running Infer will delete the previous `infer-out/`
directory if it exists. This leads to a *non-incremental*
workflow. Passing `--incremental` (or `-i`) to Infer prevents it from
deleting `infer-out/`, leading to an *incremental* workflow.

There are exceptions to this. In particular, you can run only one of
the phases above. For instance, `infer -- javac Hello.java` is
equivalent to running these two commands:

```bash
infer -a capture -- javac Hello.java
infer -- analyze
```

Notice that the second command does not erase `infer-out/`, as the
files it needs to analyze live there!

You can learn more about the various modes of operations of Infer by
running `infer --help`.

Let us highlight when you may need non-incremental and incremental
workflows.


### Non-incremental workflow

Non-incremental workflow is well suited to running Infer repeatedly
with a single compiler command, e.g.

```bash
infer -- javac Hello.java
edit Hello.java
# make some changes to Hello.java, e.g. fix a bug reported by Infer
infer -- javac Hello.java
```

To start a fresh analysis, you have to

1. delete the results directory:

    ```bash
    rm -fr infer-out
    ```

2. clean the build products, for instance with `make clean` for a make-based project.


### Incremental workflow

Software projects such as mobile apps use *incremental* build systems.
Infer understands several such build systems, detailed in the [next
section](docs/analyzing-apps-or-projects.html). To analyze your
project using Infer, it has to use one of these build systems.

Running Infer on your project is as simple as running `infer -- <your
build command here>` where the build command is the one you would
normally use to compile your source code. Infer should be first run on
a *clean* version of the project, to capture all the compilation
commands in its capture phase.

For instance, for a project compiled using gradle,

```bash
gradle clean
infer -- gradle build
```

Next, if you change some files in your project, for instance in
response to an Infer report, you can either repeat the commands above,
that is clean and reanalyze the entire project, or else tell Infer
that you are using an incremental toolchain:

```bash
edit some/File.java
# make some changes to some/File.java
infer --incremental -- gradle build
```

Note that you can run Infer with the `--incremental` flag the first
time around as well.


## Exploring Infer reports

You can get more information about the reports generated by Infer by
running `inferTraceBugs` in the same directory. For instance

```bash
infer -- gradle build
inferTraceBugs
```

This tool allows you to see error traces leading to each bug reported
by Infer, which can be helpful in tracking down the precise cause of
each bug. See the output of `inferTraceBugs --help` for more
information.
