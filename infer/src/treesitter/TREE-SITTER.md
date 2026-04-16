# Tree-sitter Integration into Infer

## Why

The clang frontend is Infer's most mature C/C++ integration, but it requires the full clang toolchain — header files, compilation database, and a working build system. This creates friction in several scenarios:

- **IDE integration**: editors need instant feedback on the file being edited — tree-sitter was designed for this. It parses incrementally, tolerates syntax errors in incomplete code, and produces a usable CST even while the user is mid-keystroke. This makes it possible to run Infer analysis on save or on keypress without waiting for a full build.
- **Broken/incomplete files**: tree-sitter gracefully handles parse errors — missing semicolons, unclosed braces, incomplete expressions. The clang frontend rejects these outright. Tree-sitter produces `ERROR` nodes in the CST but still parses the surrounding valid code, so analysis can proceed on the parts that parse.
- **Quick triage**: you have a `.c` file and want to check it for bugs without setting up a build.
- **CI on partial checkouts**: the full source tree isn't available, but individual files can still be analyzed.
- **Embedded/cross-compilation**: the target toolchain isn't clang, and setting up a clang cross-compilation environment is costly.
- **New platform bootstrap**: getting clang + Infer working on a new platform takes significant effort.
- **Educational use**: students and new users want `infer file.c` without build system prerequisites.

The tree-sitter frontend addresses these by parsing C directly from source text with zero external dependencies. It trades precision for accessibility — it won't match clang's full type-aware analysis, but it finds real bugs (null derefs, use-after-free, memory leaks) with no setup.

### How it complements clang

| | Clang frontend | Tree-sitter frontend |
|---|---|---|
| **Dependencies** | clang toolchain, headers, compilation database | None (bundled in Infer binary) |
| **Setup** | `infer -- make` or `infer -- clang -c file.c` | `infer --capture-tree-sitter file.c` |
| **Type information** | Full (from clang AST) | Partial (inferred from syntax) |
| **Macro expansion** | Yes (clang preprocessor) | Optional (`--tree-sitter-preprocessor`) |
| **Broken files** | Rejects files with syntax errors | Parses around errors, analyzes valid code |
| **Speed** | Requires full compilation | Direct FFI parse, sub-millisecond |
| **Issue coverage** | ~217 issues on c/pulse test suite | ~221 issues, 91 exact matches with clang |
| **Best for** | Production analysis with full build | IDE integration, quick checks, incomplete code |

The two frontends are complementary: use clang when you have a build system, tree-sitter when you don't — or when you need instant feedback on code that may not even compile yet.

## Status

**Working MVP.** Bundled tree-sitter runtime and C grammar compiled into the Infer binary. On the c/pulse test suite: 49/55 files captured (89%), 221 issues found (clang finds 217), 91 exact matches.

### Why the results differ from clang

Tree-sitter finds 221 issues vs clang's 217 — a similar total but not the same set. Of these, 91 match exactly (same function, same issue type). The differences:

- **False positives** (~130 tree-sitter-only): tree-sitter lacks full type information, so Pulse sometimes explores paths that clang prunes. For example, ternary expressions (`x ? a : b`) produce `Exp.If` which Pulse doesn't split on, causing ~10 false positives where clang's CFG-level branching is precise. Struct fields not tracked through compound initializers also cause spurious reports.
- **False negatives** (~126 clang-only): without type information, pointer aliasing and interprocedural value tracking are less precise. 6 files don't capture at all due to complex patterns. Some issues require type-aware reasoning (e.g. unsigned value comparisons) that tree-sitter can't provide.
- **Over-approximation finds real bugs**: tree-sitter's simpler model sometimes over-approximates paths that clang prunes with type info, which occasionally finds real bugs that clang misses.

Despite these differences, the overlap is strong: the total issue counts are comparable, and tree-sitter finds the same categories of bugs (null derefs, use-after-free, memory leaks, uninitialized values).

### Usage

```sh
# Capture and analyze (bundled grammar, no extra flags needed)
infer --capture-tree-sitter file.c --pulse

# With preprocessor (resolves #include, expands macros)
infer --capture-tree-sitter file.c \
      --tree-sitter-preprocessor 'cc -E' \
      --pulse

# Capture with textual stored in capture.db for debugging
infer --capture-tree-sitter file.c --pulse --store-textual

# Export stored textual SIL from capture.db
infer debug --export-textual output_dir/
```

Only C (.c, .h) files are currently supported. Other file extensions will produce an error.

## Architecture

```
C source file
    |
    +-- [optional] --tree-sitter-preprocessor 'cc -E'
    |   runs preprocessor, writes to temp file
    |
    v
TreeSitterFFI.parse_file              (bundled C stubs + grammar)
    |  calls tree-sitter C API directly via OCaml FFI
    |  no subprocess, no XML serialization
    v
cst_node (OCaml record)
    |
    v
TreeSitterCTranslator.translate_cst   (infer/src/treesitter/)
    |  CST -> Textual.Module.t
    |  - build Textual.Exp.t, Textual.Instr.t, Textual.Node.t directly
    |  - flatten control flow to CFG blocks
    |  - use Binop/Unop types for operators
    v
Textual.Module.t
    |
    +-> TextualVerification.verify_keep_going
    +-> TextualTransform.run_exn
    +-> TextualSil.module_to_sil
    |
    v
cfg + tenv -> capture.db -> Pulse analysis
```

## Bundled Grammar

The tree-sitter runtime and C grammar are compiled directly into the Infer binary via dune `foreign_stubs`. No external tree-sitter CLI or grammar download is needed at runtime.

### Vendored sources

The following are checked into the Infer repo under `infer/src/treesitter/vendor/`:

| Directory | Source | Version | License | Files |
|-----------|--------|---------|---------|-------|
| `vendor/tree-sitter/` | [tree-sitter runtime](https://github.com/tree-sitter/tree-sitter) | 0.26.7 | MIT | `src/lib.c` (unity build) + headers, `include/tree_sitter/api.h` |
| `vendor/tree-sitter-c/` | [tree-sitter C grammar](https://github.com/tree-sitter/tree-sitter-c) | latest | MIT | `src/parser.c` + `src/tree_sitter/parser.h` |

Both vendored projects are MIT-licensed (same as Infer). LICENSE files are included in each vendor directory for attribution.

### Updating the vendored sources

To update the tree-sitter runtime or C grammar:

**1. Update tree-sitter runtime:**

```sh
# Clone or update tree-sitter
git clone https://github.com/tree-sitter/tree-sitter.git /tmp/tree-sitter
cd /tmp/tree-sitter && git checkout v0.26.7  # or desired version

# Copy runtime sources
cp src/*.c src/*.h infer/src/treesitter/vendor/tree-sitter/src/
cp src/unicode.h infer/src/treesitter/vendor/tree-sitter/src/
mkdir -p infer/src/treesitter/vendor/tree-sitter/src/portable
cp src/portable/endian.h infer/src/treesitter/vendor/tree-sitter/src/portable/

# Copy public API header
cp include/tree_sitter/api.h infer/src/treesitter/vendor/tree-sitter/include/tree_sitter/
```

**2. Update tree-sitter C grammar:**

```sh
# Clone or update tree-sitter-c
git clone https://github.com/tree-sitter/tree-sitter-c.git /tmp/tree-sitter-c

# Generate the parser (requires tree-sitter CLI or npm)
cd /tmp/tree-sitter-c && tree-sitter generate

# Copy generated parser
cp src/parser.c infer/src/treesitter/vendor/tree-sitter-c/src/
cp src/tree_sitter/parser.h infer/src/treesitter/vendor/tree-sitter-c/src/tree_sitter/
```

**3. Build and test:**

```sh
make -j -C infer/src check    # type-check
make -j                       # full build
make ocaml_unit_test           # unit tests
make direct_c_treesitter_test  # integration tests
```

### Adding a new language grammar

To add support for a new language (e.g. Go):

1. Vendor the grammar parser: copy `parser.c` and `tree_sitter/parser.h` to `vendor/tree-sitter-go/src/`
2. Include the grammar in `tree_sitter_stubs.c`: add `#include "vendor/tree-sitter-go/src/parser.c"` and declare `extern const TSLanguage *tree_sitter_go(void);`
3. Add a new FFI function in `tree_sitter_stubs.c` that calls `tree_sitter_go()` instead of `tree_sitter_c()`
4. Add `TreeSitterFFI.parse_go_file` OCaml external
5. Write a new translator (e.g. `TreeSitterGoTranslator.ml`)
6. Extend `lang_of_file` in `TreeSitter.ml` to handle `.go` files
7. Update dune `-I` flags to include the new vendor directory

### How it's compiled

The C code is compiled as a unity build via `tree_sitter_stubs.c`:
- `#include "vendor/tree-sitter/src/lib.c"` — tree-sitter runtime
- `#include "vendor/tree-sitter-c/src/parser.c"` — C grammar tables

Dune compiles this with `foreign_stubs` and links it into the Infer binary. The `-fPIC` flag is required to avoid linker issues with large object files.

## Files

### Tree-sitter frontend (`infer/src/treesitter/`)
- `TreeSitterCTranslator.ml` — C CST → `Textual.Module.t` translator
- `TreeSitterCTranslator.mli` — Interface (`translate_cst`, `translate_xml`, `cst_node` type)
- `TreeSitterFFI.ml` — OCaml FFI binding to `caml_tree_sitter_parse_file`
- `tree_sitter_stubs.c` — C stubs: unity build of runtime + grammar + OCaml bridge
- `vendor/` — Vendored tree-sitter runtime and C grammar sources
- `dune` — Build config with `foreign_stubs`
- `unit/TreeSitterCTranslatorTest.ml` — OCaml expect tests
- `unit/dune` — Unit test build config

### Integration (`infer/src/integration/`)
- `TreeSitter.ml` — Orchestrates capture: FFI parse → translate → verify → transform → to_sil → capture.db
- `TreeSitter.mli` — Interface (`capture ~files`)

### Tests (`infer/tests/codetoanalyze/c/treesitter/`)
- `Makefile` — Integration test harness + `make compare` target
- `nullptr.c`, `memory.c`, `control_flow.c` — C source files with `_Bad`/`_Ok` naming
- `issues.exp` — Expected Pulse analysis output

### Modified files
- `Config.ml` / `Config.mli` — `--capture-tree-sitter` and `--tree-sitter-preprocessor` options
- `Driver.ml` / `Driver.mli` — `TreeSitter` mode variant
- `Makefile` — `c_treesitter` in `DIRECT_TESTS`
- `integration/dune.in` — Added `TreeSitterFrontend` library dependency

## Testing

**Unit tests** (`make ocaml_unit_test`): Feed XML strings directly to `translate_xml`, verify and transform the output, compare pretty-printed textual against expected output using `ppx_expect`. These always run — no external dependencies.

**Integration tests** (`make direct_c_treesitter_test`): End-to-end capture and Pulse analysis of C source files via the bundled tree-sitter. Always runs.

**Comparison** (`make compare` in the treesitter test dir): Runs both tree-sitter and clang on the full `c/pulse` test suite (55 files) and diffs results with line numbers stripped. Current results: 49/55 files captured (89%), 221 issues found (clang finds 217), 91 exact matches.

### Preprocessor tradeoffs

The `--tree-sitter-preprocessor 'cc -E'` flag runs a C preprocessor before tree-sitter parsing, which resolves `#include` directives and expands macros. However, **the comparison test runs without the preprocessor** because:

- **Without preprocessor**: 49/55 files captured (89%), 221 issues. The auto-declaration feature handles missing stdlib functions (e.g. `malloc`, `free`) by emitting `declare name(...) : *void` for any called-but-undeclared function. Most test files use explicit forward declarations or simple includes that tree-sitter can handle.

- **With preprocessor**: 43/55 files captured (78%), 81 issues. Preprocessing expands system headers into thousands of lines of complex C (anonymous structs, inline functions with different arity, `__attribute__` extensions), which introduces more verification failures than it solves. Source line numbers also shift to the preprocessed file.

The preprocessor is most useful for real-world code that heavily depends on system headers with complex types. For analysis-focused code with explicit declarations, the bundled tree-sitter without preprocessing gives better results.

## What works

### C constructs translated
- Function definitions with parameters and return types
- Function declarations (prototypes) → `Textual.Module.Procdecl`
- Local variable declarations with initializers
- Bare declarations without initializers (`int a, b;`)
- Global variables → `Textual.Module.Global`
- If/else → `Textual.Terminator.If` with `BoolExp`
- Short-circuit `&&`/`||` in conditions → `BoolExp.And`/`BoolExp.Or` (lowered to prune blocks by TextualTransform)
- Short-circuit `&&`/`||` in expressions → `Textual.Exp.If` (prevents eager evaluation of RHS)
- For loops → init/cond/update/body CFG blocks
- While loops → cond/body CFG blocks
- Do-while loops → body-first CFG with condition at end
- Switch/case/default → dispatch chain with `Eq` prune
- Break/continue → jump to tracked loop exit/continue labels
- Binary expressions → `Binop.PlusA`, `Binop.Mult`, `Binop.Gt`, etc.
- Unary expressions → `Unop.Neg`, `Unop.LNot`, `Unop.BNot`
- Compound assignment (`+=`, `-=`, `*=`, `/=`, `%=`)
- Increment/decrement (`i++`, `i--`)
- Function calls → `Textual.Exp.call_non_virtual`
- Pointer dereference (`*p`) → Load/Store through pointer
- Arrow field access (`p->field`) and dot access (`obj.field`) → `Textual.Exp.Field` with struct type tracking
- Nested field access (`o->inner->val`) → chained loads with graceful fallback
- Array subscript (`arr[i]`) → pointer arithmetic via `PlusA` + Load/Store
- Struct type definitions with pointer fields → `Textual.Module.Struct` (e.g. `struct Node* next` emits `Ptr(Struct(...))`)
- Nested pointer declarations (`int** pp`) → multiple levels of `Ptr` wrapping
- Ternary operator (`a ? b : c`) → materialized `Textual.Exp.If` via `Let`, handles nesting
- Struct initializer lists (`struct S s = {1, 2}`) → field-by-field stores using `struct_fields_registry`
- Comma expressions (`a, b`) → evaluate left for side effects, return right
- Address-of (`&x`) → `Textual.Exp.Lvar`
- Chained assignment (`a = b = c = 0`)
- Pointer arithmetic (`arr + len`, `arr++`)
- Logical negation in conditions (`if (!p)`, `if (p)`)
- Type qualifiers handled: `const`, `unsigned int`, `long long`, `unsigned long`, `signed`, `enum`
- Typedef names (`type_identifier`) → opaque `Struct` type
- Cast expressions including pointer casts (`(int*)p`)
- Sizeof (emitted as constant `1`)
- String literals → `Textual.Const.Str`

### Type tracking

The translator maintains a `var_struct_types` table mapping variable names to their struct type names. This is populated from:
- Local declarations: `struct Node* n = ...` records `n → Node`
- Function parameters: `void f(struct Node* n)` records `n → Node`

This enables proper field access: `n->value` generates `load n0.Node.value` with the correct `enclosing_class`, which is required by `TextualSil` for C (wildcard field names are only supported for Hack/Python).

When the struct type is unknown (e.g. nested access `o->inner->val` where `inner`'s type isn't tracked), the translator falls back to a plain pointer load instead of generating an invalid `Field` expression.

### Auto-declaration of undeclared functions

The translator collects all called function names and checks against the already-emitted declarations list. Any function that is called but not declared gets an auto-generated `declare name(...) : *void` with unknown arity (`formals_types=None`). This handles both explicit declarations missing from the source and functions from system headers that tree-sitter can't resolve.

### Error recovery

The capture pipeline is best-effort at every stage:
- **Verification**: uses `verify_keep_going` so type errors and missing field declarations produce debug warnings rather than blocking capture
- **Verification failure**: if verification fails entirely, the file is skipped with a warning
- **Transform/SIL conversion**: wrapped in `try/with` so any crash in `TextualTransform.run_exn` or `TextualSil.module_to_sil` skips the file instead of aborting the entire capture

### Expression materialization

Ternary expressions (`a ? b : c`) and return values are always materialized into `Let` instructions before being used in terminators or nested in other expressions. This ensures `TextualTransform` can lower `Textual.Exp.If` nodes into prune blocks — without this, `TextualSil` would crash on `If` expressions that survive past the transform phase.

## Gaps and TODO

### Translation quality

- **Ternary in expression position** — `x ? &d : 0` produces `Exp.If` which Pulse doesn't split on; both null and non-null paths explored. Would need CFG-level branching (like clang) instead of expression-level `If`. Causes ~10 false positives on `c/pulse/ternary.c`.
- **Compound literals** — `(struct S){1, 2}` not handled
- **Union types** — not translated, causes false uninitialized-value reports
- **Assert/abort modelling** — `assert()` not recognized as path terminator

### Verification

- **BasicVerification arity mismatches** — 19/55 files fail because system header functions have different arity than call sites (e.g. `random()` called with 0 args but declared with 1 param). Would need `BasicVerification` errors to be non-fatal in `verify_keep_going`.

### Low priority / future

- **Preprocessor line mapping** — when using `--tree-sitter-preprocessor`, line numbers in reports reference the preprocessed file rather than the original source. Could parse `# line` directives to map back
- **Function pointers** — calls through function pointer variables treated as regular calls; auto-declared but indirect dispatch not modelled
- **Goto/labels** — not handled (would need label tracking)
- **Variadic functions** — `printf` etc.
- **Multi-file capture** — currently processes files independently, no cross-file symbol resolution
- **Parallel capture** — current implementation is sequential; could use `ProcessPool` like Hack/Python frontends
- **Other languages** — Go, JavaScript, TypeScript are natural next targets (see "Adding a new language grammar" above)
- **Taint annotations** — marking sources/sinks for taint analysis
- **Models** — built-in models for standard library functions (beyond what Pulse already knows about `malloc`/`free`)
