
Front end plugins for Clang
---------------------------

Assuming that the current dir is the root of the git repository and CLANG_PREFIX=/usr/local, you may compile and run tests with
```
export CLANG_PREFIX=/usr/local
make -C libtooling test
```

More information:
- [`ATD_GUIDELINES`](https://github.com/facebook/infer/blob/main/facebook-clang-plugins/libtooling/ATD_GUIDELINES.md) for documentation about ASTExporter.
- http://clang.llvm.org/docs/ClangPlugins.html for general documentation about clang plugins
