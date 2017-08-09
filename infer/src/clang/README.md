# Clang Front-End

This is the front-end for the clang compiler.

The main entry point is [`ClangWrapper`](ClangWrapper.ml).

## General structure
1. Given a compilation command from the build system, sanitize it, attach `ASTExporter` clang plugin to the relevant commands and run it.
2. Parse the AST from Biniou format to OCaml data structure.
3. (optional) Invoke translation to `SIL` via [`CFrontend`](cFrontend.ml).
4. (optional) Invoke linters via [`CFrontend_checkers_main`](CFrontend_checkers_main.ml). [More on linters](http://fbinfer.com/docs/linters.html)

## Format of the AST
OCaml data structure is defined in `atd` format. The `clang_ast_t.atd` file is generated from comments in [ASTExporter.h](https://github.com/facebook/facebook-clang-plugins/blob/master/libtooling/ASTExporter.h) file.

For more information, refer to the relevant documentation in `facebook-clang-plugins`:
- [libtooling/ATD_GUIDELINES.md](https://github.com/facebook/facebook-clang-plugins/blob/master/libtooling/ATD_GUIDELINES.md)
- [clang-ocaml/README.md](https://github.com/facebook/facebook-clang-plugins/blob/master/clang-ocaml/README.md)

## Hacking on `.atd` file
1. Create a simple example (`example.cpp`) source file with construct that needs to be exported. The smaller the better.
2. Export extra information by changing code in [`libtooling/ASTExporter.h`](https://github.com/facebook/facebook-clang-plugins/blob/master/libtooling/ASTExporter.h). For more information, refer to the [ATD_GUIDELINES](https://github.com/facebook/facebook-clang-plugins/blob/master/libtooling/ATD_GUIDELINES.md).
3. Compile Infer with the new version of `facebook-clang-plugins`. Running `make all` from top level of Infer repository will do that. Sometimes there may be compilation errors due to `.atd` file changes - they need to be fixed.
4. Use newly exported information information in the frontend.

Tips & Tricks:
- To view the AST in a human readable version, Infer can generate `.bdump` file: `infer -g -- clang -c example.cpp && sh example.cpp.ast.sh`. Then open `example.cpp.ast.bdump`
- To inspect ast dump visually: `clang -c example.cpp -Xclang -ast-dump`. It doesn't include all the information that Infer sees, but it's pretty concise
- If running `bdump` is failing (it happens on huge sources sometimes), there is a way to view it in "Yojson" format. To do that, replace all occurrences of `BiniouASTExporter` with `YojsonASTExporter` in `.ast.sh` script.
