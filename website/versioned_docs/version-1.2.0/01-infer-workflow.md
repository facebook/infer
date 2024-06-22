---
id: infer-workflow
title: Infer workflow
---

This page documents several ways of running Infer, that you can adapt to your
own project.

**tl; dr**:

1. Make sure your project is clean when you first run Infer on it (with
   `make clean`, or `gradle clean`, or ...).
2. When running Infer several times in a row, either clean your project as in
   step 1 in-between Infer runs, or add `--reactive` to the `infer` command.
3. These steps are not needed if you are not using an incremental build system,
   for instance if you are analyzing single files with
   `infer run -- javac Hello.java`.
4. After a successful Infer run, you can explore Infer's reports in more details
   by running `infer explore` from the same directory.

## The two phases of an Infer run

Regardless of the input language (Java, Objective-C, or C), there are two main
phases in an Infer run:

### 1. The capture phase

Compilation commands are captured by Infer to translate the files to be analyzed
into Infer's own internal intermediate language.

This translation is similar to compilation, so Infer takes information from the
compilation process to perform its own translation. This is why we call infer
with a compilation command: `infer run -- javac File.java` or
`infer run -- clang -c file.c`. What happens is that the files get compiled as
usual, and they also get translated by Infer to be analyzed in the second phase.
In particular, if no file gets compiled, also no file will be analyzed.

Infer stores the intermediate files in the results directory which by default is
created in the folder where the `infer` command is invoked, and is called
`infer-out/`. You can change the name of the results directory with the option
`-o`, e.g.

```bash
infer run -o /tmp/out -- javac Test.java
```

You can run just the capture phase using the `capture` subcommand instead of the
`run` subcommand:

```bash
infer capture -- javac Test.java
```

### 2. The analysis phase

In this phase, the files in `infer-out/` are analyzed by Infer. Infer analyzes
each function and method separately. If Infer encounters an error when analyzing
a method or function, it stops there for that method or function, but will
continue the analysis of other methods and functions. So, a possible workflow
would be to run Infer on your code, fix the errors generated, and run it again
to find possibly more errors or to check that all the errors have been fixed.

The errors will be displayed in the standard output and also in a file
`infer-out/report.txt`. We filter the bugs and show the ones that are most
likely to be real.

## Global (default) and differential workflows

By default, running Infer will delete the previous `infer-out/` directory if it
exists. This leads to a _default_ workflow where the entire project is analyzed
every time. Passing `--reactive` (or `-r`) to Infer prevents it from deleting
`infer-out/`, leading to a _differential_ workflow.

There are exceptions to this. In particular, you can run only one of the phases
above. For instance, `infer run -- javac Hello.java` is equivalent to running
these two commands:

```bash
infer capture -- javac Hello.java
infer analyze
```

Notice that the second command does not erase `infer-out/`, as the files it
needs to analyze live there!

You can learn more about the subcommands supported by Infer by running
`infer --help`, `infer capture --help`, or more generally
`infer <subcommand> --help`.

Let us highlight when you may need global and differential workflows.

### Global workflow

The global workflow is well suited to running Infer on all the files in a
project, e.g., for a Gradle-based project that compiles using the `gradle build`
command:

```bash
infer run -- gradle build
```

In general, running Infer on your project is as simple as running
`infer run -- <your build command here>` where the build command is the one you
would normally use to compile your source code.

To start a fresh analysis and be sure to analyze all the files in your project,
you have to clean the build products, for instance with `make clean` for a
make-based project, `gradle clean` for Gradle, etc.

### Differential workflow

Software projects such as mobile apps use _incremental_ build systems, where
code evolves as a sequence of code changes. For these projects, it can often
make sense to analyze only the current changes in the project, instead of
analyzing the whole project every time. It is possible to analyze only what's
changed using Infer's _reactive mode_.

Infer should first be run on a _clean_ version of the project, to capture all
the compilation commands in its capture phase.

For instance, for a project compiled using Gradle,

```bash
gradle clean
infer capture -- gradle build
```

Note that the above command does not perform an expensive analysis, but captures
all the compilation commands and stores the results in Infer's internal format.

Next, if you change some files in your project, for instance in response to an
Infer report, or as part of normal development, you can either clean and
reanalyze the entire project (as in the [global workflow](#global-workflow)
above), or else tell Infer that you are interested in the effects of the code
change. The second option can be significantly faster, as only a subset of the
project needs to be analyzed: the modified files/procedures and their
dependencies.

```bash
edit some/File.java
# make some changes to some/File.java
infer run --reactive -- gradle build
```

Note that you can run Infer with the `--reactive` flag the first time around as
well.

To control the granularity of the changes to be analyzed, it is possible to tell
Infer to combine several changes into one before the analysis. This is done with
the `--continue` option.

For example:

```bash
edit some/File1.java
# make some changes to some/File1.java
infer run --reactive -- gradle build
edit some/File2.java
# make some changes to some/File2.java
infer run --reactive --continue -- gradle build
```

After the first invocation, Infer will analyze the results of the first change.
After the second invocation, Infer will analyze the results of both changes. If
the `--continue` option were omitted, it would only analyze the results of the
second change.

Finally, it is always possible to perform an analysis of the current changes in
isolation:

```bash
infer run --reactive --continue -- analyze
```

The list of build systems supported by Infer is detailed in the
[next section](analyzing-apps-or-projects).

## Exploring Infer reports

You can get more information about the reports generated by Infer by running
`infer explore` in the same directory. For instance

```bash
infer run -- gradle build
infer explore
```

This tool allows you to see error traces leading to each bug reported by Infer,
which can be helpful in tracking down the precise cause of each bug. See the
output of `infer explore --help` for more information.
