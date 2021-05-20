Facebook Clang Plugins
======================

This folder contains the Infer frontend plugin to the [clang compiler](http://clang.llvm.org/) used to process the syntax of source files and export AST information to the Infer frontend.

Structure of the repository
---------------------------

- libtooling : frontend plugins (currently a clang-to-json AST exporter),

- clang-ocaml : OCaml libraries to process the JSON output of frontend plugins,


Quick start
-----------

The plugin requires recent version of the clang compiler, re-compiled from source.

To compile and use the required version of clang, please run ./clang/setup.sh.

Caveat:
- Because of the nature of C++, clang and the plugins need to be compiled with the exact same C++ libraries.
- Also, the default stripping command of clang in release mode breaks plugins.

Once the target compiler is installed, `make test` should run the unit tests.

OCaml users may also run:
```
make -C clang-ocaml test  #requires proper ocaml libraries, see included clang-ocaml/README
```

Additional configuration options are available in `Makefile.config`.

Licence
-------

Infer is MIT-licensed.
