---
id: analyzing-apps-or-projects
title: Analyzing apps or projects
layout: docs
permalink: /docs/analyzing-apps-or-projects.html
section: User Guide
section_order: 01
order: 02
---

To analyze files with Infer you can use the compilers `javac` and `clang`. You can also use Infer with `gcc`, however, internally Infer will use `clang` to compile your code. So, it may not work if your code does not compile with `clang`. 

Moreover, you can run Infer with a variety of build systems. Notice that you can run infer faster by running the compilation command in parallel, e.g. `infer -- make -j8`.
Please also take into account that if you wish to analyze a project, you should probably do `clean` beforehand so that the compiler compiles all the files and so Infer also analyses all the files (see the [previous section](docs/infer-workflow.html)).

Here is an overview of the build systems supported by Infer. You can
get more information about how a particular build system is supported
by running `infer --help -- <build system>`, for instance `infer
--help -- gradle`.

### Gradle

```bash
infer -- gradle <gradle task, e.g. "build">
infer -- ./gradlew <gradle task, e.g. "build">
```

### Buck

```bash
infer -- buck <buck target>
```

### Maven
```bash
infer -- mvn <maven target>
```

### Xcodebuild

Infer can analyze apps built using `xcodebuild`. Only `.m` and `.c`
files will be analyzed; `.cpp`, `.cc` and `.mm` files will be
ignored. For instance, for an iOS app:

```bash
infer -- xcodebuild -target <target name> -configuration <build configuration> -sdk iphonesimulator
```

### Make

Infer can analyze projects that compile with `make`. If there are C++ files in the project, they will be ignored.

```bash
infer -- make <make target>
```
