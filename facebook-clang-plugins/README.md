Facebook Clang Plugins
======================

This [repository](https://github.com/facebook/facebook-clang-plugins) aims to share some useful clang plugins developed at Facebook. They are mostly used by [infer](https://github.com/facebook/infer).

It contains frontend plugins to the [clang compiler](http://clang.llvm.org/) to process the syntax of source files directly to accomplish more general tasks; specifically, we have developed a clang-to-ocaml bridge to make code analyses easier.

Structure of the repository
---------------------------

- [`libtooling`](https://github.com/facebook/facebook-clang-plugins/tree/master/libtooling) : frontend plugins (currently a clang-to-json AST exporter),

- [`clang-ocaml`](https://github.com/facebook/facebook-clang-plugins/tree/master/clang-ocaml) : OCaml libraries to process the JSON output of frontend plugins,


Quick start
-----------

The current version of the plugins requires recent version of the clang compiler, re-compiled from source. Clang source which is used by this project can be found in `clang/src/`

General instructions to compile clang can be found here: http://clang.llvm.org/get_started.html

To compile and use the required version of clang, please run ./clang/setup.sh.
Using this script should make the variable CLANG_PREFIX unnecessary to compile the plugin.

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

The plugins are MIT-licensed.
