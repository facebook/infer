---
docid: analyzing-apps-or-projects
title: Analyzing apps or projects
layout: docs
permalink: /docs/analyzing-apps-or-projects.html
---

To analyze files with Infer you can use the compilers `javac` and `clang`. You can also use Infer with `gcc`, however, internally Infer will use `clang` to compile your code. So, it may not work if your code does not compile with `clang`.

Moreover, you can run Infer with a variety of build systems. Notice that you can run infer faster by running the compilation command in parallel, e.g. `infer run -- make -j8`.
Please also take into account that if you wish to analyze a project, you should probably do `clean` beforehand so that the compiler compiles all the files and so Infer also analyses all the files (see the [previous section](docs/infer-workflow.html)).

Here is an overview of the build systems supported by Infer. You can
get more information about how a particular build system is supported
by looking at the SYNOPSIS section of the infer-capture manual:
`infer capture --help`.

### Gradle

```bash
infer run -- gradle <gradle task, e.g. "build">
infer run -- ./gradlew <gradle task, e.g. "build">
```

### Buck

Running:
```bash
infer run -- buck <buck target>
```
will compute the list of Infer warnings in the targets passed as argument.

Running:
```bash
infer run -- buck --deep <buck target>
```
will compute the list of Infer warnings in the targets passed as argument and all the transitive dependencies.

The distinction between `--deep` and the normal Buck complation mode is only supported for Java projects. For the other kinds of projects, the `--deep` option has no effect.

### Maven
```bash
infer run -- mvn <maven target>
```

### Xcodebuild

Infer can analyze apps built using `xcodebuild`. For instance, for an iOS app:

```bash
infer run -- xcodebuild -target <target name> -configuration <build configuration> -sdk iphonesimulator
```

### Make

Infer can analyze projects that compile with `make`. If there are C++ files in the project, they will be ignored.

```bash
infer run -- make <make target>
```


### Using a compilation database

Many build systems like cmake, Xcode or Buck generate compilation databases. infer is able to use this database directly, simplifying its usage.

```bash
infer --compilation-database compile_commands.json
```
