---
id: analyzing-apps-or-projects
title: Analyzing apps or projects
---

To analyze files with Infer you can use the compilers `javac` and `clang`. You
can also use Infer with `gcc`, however, internally Infer will use `clang` to
compile your code. So, it may not work if your code does not compile with
`clang`.

Moreover, you can run Infer with a variety of build systems. Notice that you can
run infer faster by running the compilation command in parallel, e.g.
`infer run -- make -j8`. Please also take into account that if you wish to
analyze a project, you should probably do `clean` beforehand so that the
compiler compiles all the files and so Infer also analyses all the files (see
the [previous section](infer-workflow)).

Here is an overview of the build systems supported by Infer. You can get more
information about how a particular build system is supported by looking at the
SYNOPSIS section of the infer-capture manual: `infer capture --help`.

### ant

```bash
infer run -- ant
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

will compute the list of Infer warnings in the targets passed as argument and
all the transitive dependencies.

The distinction between `--deep` and the normal Buck complation mode is only
supported for Java projects. For the other kinds of projects, the `--deep`
option has no effect.

### cmake

The most robust way is to have `cmake` generate a compilation database that can
be then processed by Infer:

```bash
cd build
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..
cd ..
infer run --compilation-database build/compile_commands.json
```

Alternatively, one can trick `cmake` into using infer instead of the system's
compilers:

```bash
cd build
infer compile -- cmake ..
infer run -- make -j 4
```

### Gradle

```bash
infer run -- gradle <gradle task, e.g. "build">
infer run -- ./gradlew <gradle task, e.g. "build">
```

### Make

Infer can analyze projects that compile with `make` by switching the compilers
(for C/C++/Objective-C or Java) called by `make` with infer wrappers. This
doesn't always work, for instance if the Makefiles hardcode the absolute paths
to the compilers (eg, if `make` calls `/usr/bin/gcc` instead of `gcc`). This is
because this integration works by modifying `PATH` under the hood.

```bash
infer run -- make <make target>
```

### Maven

```bash
infer run -- mvn <maven target>
```

### Xcodebuild

The most robust way is to generate a compilation database, then pass that
database to Infer:

```bash
xcodebuild <your build options> | tee xcodebuild.log
xcpretty -r json-compilation-database -o compile_commands.json < xcodebuild.log > /dev/null
infer run --skip-analysis-in-path Pods --clang-compilation-db-files-escaped compile_commands.json
```

See also
[this comment on GitHub](https://github.com/facebook/infer/issues/9#issuecomment-280121791).

Infer also provides a direct integration to xcodebuild that swaps the compiler
used by xcodebuild under the hood. For instance, for an iOS app:

```bash
infer run -- xcodebuild -target <target name> -configuration <build configuration> -sdk iphonesimulator
```

There is an alternative xcodebuild integration that uses `xcpretty` under the
hood; use it by passing `--xcpretty` to infer.

### xctool

Use `xctool` to generate a compilation database then pass it to infer:

```bash
xctool.sh <your build options> -reporter json-compilation-database:compile_commands.json
infer run --skip-analysis-in-path Pods --clang-compilation-db-files-escaped compile_commands.json
```

See also
[this comment on GitHub](https://github.com/facebook/infer/issues/9#issuecomment-280121791).

### Using a compilation database

Many build systems like cmake, Xcode or Buck generate compilation databases.
infer is able to use this database directly, simplifying its usage.

```bash
infer --compilation-database compile_commands.json
```

### Other build systems

If infer doesn't recognize your build system, you will get an error like this:

```console
$ infer run -- foo
Usage Error: Unsupported build command foo
```

If your build system behaves like one of the above, you can tell infer to use
the same integration with `--force-integration`. For instance this will proceed
as if `foo` was working the same way as `make`:

```bash
infer run --force-integration make -- foo
```

If your build system is more exotic, and it doesn't support outputting
compilation databases, please let us know by
[opening an issue](https://github.com/facebook/infer/issues/new).
