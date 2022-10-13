# Contribution Guidelines


## Reporting Issues

If you encounter a problem when using infer or if you have any questions, please open a
[GitHub issue](https://github.com/facebook/infer/issues/).

## Hacking on the Code

We welcome contributions via [pull requests on GitHub](https://github.com/facebook/infer/pulls).

### Development Dependencies

You'll want to install a few more dependencies to comfortably hack on the infer codebase;
in order to do this run `./build_infer.sh` which will allow you to then run:
```sh
make devsetup
```

### Building Infer for Development

- The default build mode ("dev") makes all build warnings *fatal*. If you want the build to ignore
  warnings, for example to be able to test an infer executable before polishing the code to remove
  warnings, you can build in "dev-noerror" mode with `make BUILD_MODE=dev-noerror`.

- Faster edit/build cycle when working on OCaml code inside infer/src/: build inside infer/src/
  (skips building the models after infer has been built), and build only what is needed for type
  checking with `make -j -C infer/src check`. You need to have run `make -j` at some point before.

- Alternatively, if you want to test your changes on a small example, build in bytecode mode:
  `make -j -C infer/src byte`.

- In general, `make` commands from the root of the repository make sure that dependencies are in a
  consistent and up-to-date state (e.g., they rebuild infer and the models before running steps that
  use infer), while running `make` commands from within subdirectories generally assumes that
  dependencies are already up-to-date.

  For instance, running `make direct_java_biabduction_test` will rebuild infer and the models if
  necessary before running the test, but running `make -C infer/tests/codetoanalyze/java/biabduction/ test`
  will just execute the test.


### Debugging OCaml Code

- Printf-debug using `Logging.debug_dev`. It comes with a warning so
  that you don't accidentally push code with calls to `debug_dev` to
  the repo.

- Browse the documentation of OCaml modules in your browser with `make doc`

- When using `ocamldebug`, and in particular when setting break points
  with `break @ <module> <line>` don't forget that an infer module `M`
  is in reality called `InferModules__M`, or `InferBase__M`, or
  ... See the html documentation of the OCaml modules from `make doc`
  if you're unsure of a module name.

```console
$ ledit ocamldebug infer/bin/infer.bc.exe
(ocd) break @ InferModules__InferAnalyze 100
Breakpoint 1 at 9409684: file backend/InferAnalyze.ml, line 99, characters 18-78
```

- To test the infer OCaml code you can use the OCaml toplevel. To
  build the OCaml toplevel with the infer modules pre-loaded, run
  `make toplevel` and follow the instructions.

  To pass infer options to the toplevel, use `INFER_ARGS`, for
  instance: `INFER_ARGS=--debug^-o^infer-out-foo`.

  Many operations require the results directory and database to be
  initialized with `ResultsDir.assert_results_dir ""`.


## Contributor License Agreement

We require contributors to sign our Contributor License Agreement. In
order for us to review and merge your code, please sign up at
https://code.facebook.com/cla. If you have any questions, please drop
us a line at cla@fb.com.

You are also expected to follow the [Code of Conduct](CODE_OF_CONDUCT.md), so please read that if you are a new contributor.

Thanks!

## Coding Style

### All Languages

- Indent with spaces, not tabs.

- Line width limit is 100 characters.

- In general, follow the style of surrounding code.

### OCaml

- The module IStd (infer/src/istd/IStd.ml) is automatically opened in every file. Beware that this
  can cause weird errors such as:
```
$ pwd
/somewhere/infer/infer/src
$ cat base/toto.ml
let b = List.mem true [true; false]
$ make
[...]
File "base/toto.ml", line 1, characters 17-21:
Error: This variant expression is expected to have type 'a list
       The constructor true does not belong to type list
```

- All modules open `IStd` using `open! IStd`. This is to make that fact more explicit (there's also
  the compilation flag mentioned above), and also it helps merlin find the right types. In
  particular this also opens `Core.Std`.

- Do not add anything to `IStd` unless you have a compelling reason to do so, for instance if you
  find some utility function is missing and is not provided by
  [`Core`](https://ocaml.janestreet.com/ocaml-core/latest/doc/core/).

- Polymorphic equality is disabled; use type-specific equality instead, even for primitive types
  (e.g., `Int.equal`). However, if your module uses a lot of polymorphic variants with no arguments
  you may safely `open PolyVariantEqual`.

  If you try and use polymorphic equality `=` in your code you will get a compilation error, such as:
```
Error: This expression has type int but an expression was expected of type
         [ `no_polymorphic_compare ]
```

- Alias and use `module L = Logging` for all your logging needs. Refer to its API in Logging.mli for
  documentation.

- Check that your code compiles without warnings with `make -j test_build` (this also runs as part
  of `make test`).

- Apart from `IStd` and `PolyVariantEqual`, refrain from globally `open`ing modules. Using
  local open instead when it improves readability: `let open MyModule in ...`.

- Avoid the use of module aliases, except for the following commonly-aliased modules. Use
  module aliases consistently (e.g., do not alias `L` to a module other than `Logging`).
```OCaml
module CLOpt = CommandLineOption
module F = Format
module L = Logging
module MF = MarkupFormatter
```

- Use `[@@deriving compare, equal]` to write comparison/equality functions whenever possible.
  Watch out for [this issue](https://github.com/ocaml-ppx/ppx_deriving/issues/116) when writing
  `type nonrec t = t [@@deriving compare]`.

- Use named arguments whenever the purpose of the argument is not immediately obvious. In
  particular, use named arguments for boolean and integer parameters unless the name of the function
  mentions them explicitly. Also use named arguments to disambiguate between several arguments of
  the same type.

- Use named arguments for functions taken as argument; it is common to name a function argument
  `f`. For instance: `List.map : 'a list -> f:('a -> 'b) -> 'b list`.

- In modules defining a type `t`, functions that take an argument of that type should generally have
  that argument come first, except for optional arguments: `val f : ?optional:bool -> t -> ...`.

- Use the `_hum` suffix to flag functions that output human-readable strings.

- Format code with [ocamlformat](https://github.com/ocaml-ppx/ocamlformat).

### C/C++/Objective-C

Follow `clang-format` (see ".clang-format" at the root of the repository).

## Testing your Changes

- Make sure infer builds: `make -j test_build`. Refer to the [installation
  document](https://github.com/facebook/infer/blob/main/INSTALL.md) for details.

- Run the tests: `make -j 4 test` (adjust 4 to the number of cores available of your machine). The
  tests (almost) all consist of the same three ingredients:
  1. Some source code to run infer on.
  2. An "issues.exp" file where each line represents one item of output of the test. For most tests,
     one line is one issue reported by infer.
  3. A `Makefile` that orchestrates the test, for instance running infer on the source code and
     comparing the results with issues.exp using `diff`.

- If your changes modified some of the expected outputs and if the changes make sense, you can
  update the expected test results by running `make test-replace`.

- If relevant, add a test for your change.

- To add a test that infer finds (or does not find) a particular issue, add your test in
  "infer/tests/codetoanalyze/{language}/{analyzer}/". Look at the `Makefile` in that directory and
  make sure it runs your test. "{analyzer}" is often an infer analyzer (as in
  `infer -a {analyzer}`), with some special cases:
  - "errors" is "infer"
  - "frontend" is a mode where the expected output is the result of the translation of the program
     by infer's clang frontend into infer's intermediate representation.

  Name the procedures in your test following these conventions:
  - Test procedures where the analyzer should report an error should end with the suffix `Bad`.
  - Test procedures where the analyzer should not report an error should end with the suffix `Ok`.
  - Test procedures documenting current limitations of the analyzer should have the prefix `FP_`
    (for "false positive") or `FN_` (for "false negative") and a comment explaining why the analyzer
    gets the wrong answer.


- To add a test that a certain build system integration or a command-line option works in a certain
  way, add a test in "infer/tests/build_systems/".

- If you created a new Makefile for your test, add it to the root "Makefile", either to the
  `DIRECT_TESTS` (first case) or to the `BUILD_SYSTEMS_TESTS` variable (second case). Gate the
  test appropriately if it depends on Java or Clang or Xcode (see how other tests do it).

- It can be useful to look at the debug HTML output of infer to see the detail of the symbolic
  execution. For instance:
```sh
$ infer --debug -- clang -c examples/hello.c
$ firefox infer-out/captured/hello.c.*.html
```

## Updating infer.opam and infer.opam.locked

`opam/infer.opam.locked` records fixed versions of the opam
dependencies known to work with infer and to respect the constraints
in opam. This prevents unpredictable breakages of infer or its
dependencies, especially for infer releases, for which it is more
difficult to change their package constraints after the fact.

To add an opam package or update its version constraints, edit
`opam/infer.opam` then run `make` in directory `opam/`.
