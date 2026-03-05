# AGENTS.md

This file provides guidance to Open Code / Codex / Claude Code (claude.ai/code) when working with code in this repository.

## What is Infer?

Infer is a multi-language static analysis tool developed by Meta. It detects bugs in Java, C, C++, Objective-C, Erlang, Hack, Python, Rust, Swift, and C# code. The primary implementation language is OCaml.

## Build Commands

All build commands run from the repository root. You must have run `./build-infer.sh` and `make devsetup` initially.

| Command                          | Purpose                                                              |
| -------------------------------- | -------------------------------------------------------------------- |
| `make -j`                        | Full native build (default `dev` mode, warnings are fatal)           |
| `make -j BUILD_MODE=dev-noerror` | Build ignoring warnings                                              |
| `make -j -C infer/src check`     | Type-check only (fastest iteration cycle, requires prior full build) |
| `make -j -C infer/src byte`      | Bytecode build (for testing changes quickly)                         |
| `make opt`                       | Optimized build with flambda                                         |
| `make fmt`                       | Format changed OCaml files with ocamlformat                          |
| `make fmt_all`                   | Format all OCaml files                                               |

## Testing

| Command                                                     | Purpose                                                |
| ----------------------------------------------------------- | ------------------------------------------------------ |
| `make -j 4 test`                                            | Run all tests (adjust parallelism to your cores)       |
| `make -j test_build`                                        | Verify build has no warnings                           |
| `make test-replace`                                         | Update expected test outputs after intentional changes |
| `make direct_{lang}_{analyzer}_test`                        | Run a single test suite (rebuilds infer first)         |
| `make -C infer/tests/codetoanalyze/{lang}/{analyzer}/ test` | Run a single test suite (assumes infer is up-to-date)  |
| `make ocaml_unit_test`                                      | Run OCaml unit tests only                              |
| `make swift_tests`                                          | Run Swift tests (separate from main `test` target)     |

Direct test names follow the pattern `{lang}_{analyzer}`, e.g., `direct_java_pulse_test`, `direct_cpp_pulse_test`, `direct_c_frontend_test`. These are defined in the `DIRECT_TESTS` variable in the root Makefile.

### Test structure

Tests live in `infer/tests/codetoanalyze/{language}/{analyzer}/`. Each test directory has:

- Source files to analyze
- `issues.exp` with expected output (one issue per line)
- `Makefile` that runs infer and diffs output against `issues.exp`

Test procedure naming conventions:

- `*Bad` — should report an error
- `*Ok` — should not report an error
- `FP_*` — false positive (known limitation, include explanatory comment)
- `FN_*` — false negative (known limitation, include explanatory comment)

### Debugging analysis

```sh
infer --debug -- clang -c examples/hello.c
firefox infer-out/captured/hello.c.*.html
```

## Architecture

### Frontends (language → SIL translation)

Each supported language has a frontend that translates source code into SIL (Separation Logic Intermediate Language), defined in `infer/src/IR/`:

| Directory  | Language          | Mechanism                                          |
| ---------- | ----------------- | -------------------------------------------------- |
| `clang/`   | C/C++/Objective-C | Custom clang plugin (`facebook-clang-plugins/`)    |
| `java/`    | Java              | javalib/Sawja bytecode parsing                     |
| `erlang/`  | Erlang            | Erlang compiler integration                        |
| `python/`  | Python            | PyML-based frontend                                |
| `rust/`    | Rust              | Charon IR translation                              |
| `llvm/`    | LLVM bitcode      | LLVM OCaml bindings (used by Swift)                |
| `textual/` | Textual SIL       | Text-based SIL representation (Hack, Python, LLVM) |

### Intermediate Representation (`infer/src/IR/`)

Core types: `Sil.ml` (instructions), `Procdesc.ml` (procedure descriptors), `Typ.ml` (types), `Procname.ml` (procedure names), `Tenv.ml` (type environments).

### Analysis engines

- **Pulse** (`pulse/`, ~150+ files) — The main abstract domain. Handles memory safety, taint tracking, null safety, resource leaks via abductive reasoning.
- **Abstract interpretation framework** (`absint/`) — `AbstractDomain.ml`, `AbstractInterpreter.ml` define the fixpoint computation infrastructure.
- **Buffer overrun** (`bufferoverrun/`) — Array bounds analysis.
- **Concurrency** (`concurrency/`) — Race condition detection (RacerD).
- **Cost** (`cost/`) — Computational cost analysis.
- **TOPL** (`topl/`) — Temporal property checking.
- **Checkers** (`checkers/`) — Miscellaneous checkers (liveness, purity, annotations, etc.).

### Backend (`infer/src/backend/`)

Analysis scheduling (`AnalysisDependencyGraph.ml`), call graph construction (`CallGraph.ml`), checker registration (`callbacks.ml`), reporting.

### Configuration (`infer/src/base/Config.ml`)

Very large file (~189KB) defining all command-line options. Uses `CommandLineOption` module (aliased `CLOpt`).

## OCaml Coding Conventions

- **100-character line limit.** Format with `ocamlformat`.
- **`IStd` is auto-opened** in every file. Always write `open! IStd` explicitly at the top.
- **Polymorphic equality is disabled.** Use type-specific equality (e.g., `Int.equal`, `String.equal`). Use `PolyVariantEqual` only for modules heavy on argument-free polymorphic variants.
- **Standard module aliases** (use consistently, do not alias these names to other modules):
  ```ocaml
  module CLOpt = CommandLineOption
  module F = Format
  module L = Logging
  module MF = MarkupFormatter
  ```
- **Printf debugging:** Use `Logging.debug_dev` (triggers a warning to prevent accidental commits).
- Use `[@@deriving compare, equal]` for comparison/equality derivations.
- Use named arguments when purpose isn't obvious, especially for bool/int params and function-typed params (conventionally named `f`).
- Avoid globally opening modules other than `IStd` and `PolyVariantEqual`.
- **OCaml debugger:** Module `M` is really `InferModules__M` (or `InferBase__M`, etc.) when setting breakpoints in `ocamldebug`.

## Key Files

- `infer/src/base/Config.ml` — All CLI options
- `infer/src/IR/Sil.ml` — SIL instruction definitions
- `infer/src/absint/AbstractInterpreter.ml` — Fixpoint computation
- `infer/src/pulse/PulseAbductiveDomain.ml` — Pulse's core abstract state
- `infer/src/dune` — Main dune build config (generated from `dune.common` + `dune.in`)
- `Makefile` — Test targets and build orchestration
