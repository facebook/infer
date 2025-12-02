# Usage

To run Charon, you should run the Charon binary from *within* the crate that you
want to compile, as if you wanted to build the crate with `cargo build`. The
Charon executable is located at `bin/charon`.

Charon will build the crate and its dependencies, then extract the AST. Charon
provides various options and flags to tweak its behaviour: you can display a
detailed documentation with `--help`.
In particular, you can pretty-print the translated crate with both `--print-ullbc` and `--print-llbc`, depending on the Charon intermediate representation you wish to use.

If there is a `Charon.toml` file at the root of your project, `charon` will also take options from it.
The file supports the same options at the cli interface, except for the options that relate to
input/output like `--print-llbc`. Example `Charon.toml`:
```toml
[charon]
extract_opaque_bodies = true
[rustc]
flags = ["--cfg", "abc"]
```

**Remark**: because Charon is compiled with Rust nightly (this is a requirement to implement a rustc
driver), it will build your crate with Rust nightly. You can find the nightly version pinned for
Charon in [`rust-toolchain.template`](rust-toolchain.template).

## Names

Each item (function, trait decl, type decl etc) in Charon has a unique name, that looks like a Rust
path (e.g., `std::boxed::Box`). This name can be used to uniquely identify items using patterns.

These patterns are used most notably for the `--include`/`--exclude` cli options, and made available
in Rust and OCaml with NameMatcher. Caveat: the Rust and OCaml name matchers differ in input syntax
and behavior; this is tracked in https://github.com/AeneasVerif/charon/issues/319.

TODO: explain syntax and behavior.

Note: for well-known built-in items, you can often use `ItemMeta.lang_item` instead; it contains an
identifier meant for this purpose, documented
[here](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_hir/lang_items/enum.LangItem.html).

## CLI options

TODO: explain the main options, and LLBC vs ULLBC

In short: LLBC is the name we give to the output of Charon. It stands for Low-Level Borrow Calculus
and is the name used in the formalisations of the Aeneas project. Charon was initially created as
part of the Aeneas project, and therefore inherits that name. ULLBC means Unstructured LLBC, i.e.,
LLBC without the control-flow reconstruction.

ULLBC is a slightly simplified MIR, where we try to remove as much redundancies
as possible. For instance, we drastically simplify the representation of constants coming
from the Rust compiler.

LLBC is ULLBC where we restructured the control-flow with loops, `if
... then ... else ...`, etc. instead of gotos. Consequently, we merge MIR
statements and terminators into a single LLBC statement type.

**Remark**: most of the transformations which transform the MIR to ULLBC then LLBC are implemented
by means of micro-passes. Depending on the need, we could make them optional and control them with
flags. If you want to know more about the details, see `transformation_passes` in
`src/bin/charon-driver/driver.rs`, which applies the micro-passes one after the other.

