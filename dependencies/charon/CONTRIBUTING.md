# Contributing to Charon

First off, thanks for taking the time to contribute!

All types of contributions are encouraged. See the [Table of Contents](#table-of-contents) for different ways to help. Please make sure to read the relevant section before making your contribution. We look forward to your contributions!

## Table of Contents

- [I Have a Question](#i-have-a-question)
- [How To Contribute](#how-to-contribute)
- [Reporting Bugs](#reporting-bugs)
- [Suggesting Enhancements](#suggesting-enhancements)
- [Contributing Code](#contributing-code)


## I Have a Question

<!--
> If you want to ask a question, we assume that you have read the available [Documentation]().
-->

Before you ask a question, it is best to search for existing [Issues](https://github.com/AeneasVerif/charon/issues) that might help you. In case you have found a suitable issue and still need clarification, you can write your question in this issue.

If you then still feel the need to ask a question and need clarification, you can either:
- ask your question on our [Zulip](https://aeneas-verif.zulipchat.com/);
- or open an [Issue](https://github.com/AeneasVerif/charon/issues/new) (see [below](#reporting-bugs) for how to report a bug).

## How To Contribute

### Reporting Bugs

#### Before Submitting a Bug Report

To make our lives as maintainers as easy as possible, we ask you to investigate carefully to isolate the cause of your issue. You may want to check:

- Make sure that you are using the latest version of Charon;
- Ensure you're using the same version of Charon to produce and consume llbc files;
- Clean up cached files and recompile (inside the project, this can be done with `make clean; make`);
- If relevant, recompile the project you use to consume llbc files (e.g. Aeneas of Eurydice);
- Clean up leftover llbc files and regenerate them (inside the project, this can be done with `make clean-generated`);
- Check if a similar issue has already been reported in the [issue tracker][].

#### Reporting a Bug

To be able to help you, we need the following information:

- The version of Charon you are using (you can get the checked-out commit with `git rev-parse HEAD`);
- Your explanation of the behavior you expected to happen and the actual behavior that happened.
- The exact `charon` command you executed;
- The rust input that caused the issue;
   - As much as possible, try to isolate the problematic code and minimize the issue;
- The error message you got, in its entirety, if any;
   - If the error is "Incompatible version of charon: this program supports llbc emitted by charon v..." you are mixing up Charon versions; please redo the steps in the previous section;
   - If this is a panic, re-run with `RUST_BACKTRACE=1 bin/charon ...` to get the full backtrace;
- If the bug didn't happen on a previous version of Charon, tell us which version worked.
- Any other steps that might be relevant to reproduce the issue.

You can the submit an issue to our [issue tracker][] with all this information.

### Suggesting Enhancements

You may also suggest features and enhancements to Charon. You can submit these to our [issue tracker][] as well. As for bugs, ensure there isn't already an issue for this feature.

We may close the issue if we feel this idea does not fit within the scope of the project. Keep in mind that we want features that will be useful to the majority of our users. If you're just targeting a minority of users, consider writing an separate library (support for external rust consumers of Charon is tracked [here](https://github.com/AeneasVerif/charon/issues/178). You can also make an OCaml library by using the Charon ML library (in the folder `charon-ml/`).

### Contributing Code

Note: this section is incomplete.

#### (De)serialization and Versions

Charon is structured into two projects:

- `charon/` contains rust code that builds the `charon` and `charon-driver` binaries. These take rust code and generate a `<name>.llbc` file, which is a json serialization of the extracted crate contents;
- `charon-ml/` contains an OCaml library that deserializes the `.llbc` files. It mostly follows the structure of the corresponding rust types.

Most of `charon-ml` is generated automatically from the Rust definitions. This is done by the
`generate-ml` binary, which runs `charon` on its own source, then emits OCaml type definitions and
json deserialization functions. To update the generated code, run `make generate-ml`. In some rare
cases you will need to update `generate-ml/main.rs` yourself.

Any change to the json serialization must also increment the version in `charon/Cargo.toml`. Both
Rust and OCaml deserializers check the versions before attempting to deserialize, which greatly
improves error messages. After incrementing the version number, run `make test`; this will copy that
version number to `CharonVersoin.ml` which is how `charon-ml` gets informed of it.

#### Tests

Any non-trivial change to the project must add relevant tests. Tests reside in `charon/tests` (the `tests/` folder is legacy and will soon be moved). There are a few kinds:
- UI tests in `tests/ui`: this is the most common kind. Just add a `<file>.rs` file in the folder and it will be tested by `cargo test`. Tests support special comments, for instance to specify negative tests; look at other tests for examples or at `ui.rs` for documentation. Most tests generate a `<file>.out` file which must also be committed;
- Cargo tests in `tests/cargo`: tests that require running Charon via `cargo`, e.g. because they require dependencies or multiple crates. Note that ui tests support a limited form of multi-crate tests using the `//@ aux-crate` comment.
- Crate data tests in `tests/crate_data.rs`: these verify details of the llbc output that don't show up in the ui output.

Moreover, all the passing ui tests are run through the OCaml deserialization library. To run that part of the test suite, run `make charon-ml-tests` in the project root.

To run the complete test suite, run `make test` in the project root.

#### Continuous Integration

Charon runs checks in CI for each pull request. There are two kinds:
- The `nix` check is the normal Charon CI that builds charon and runs the full test suite;
- The `aeneas`, `eurydice` and `kyber` checks correspond to projects that use Charon that we endeavor to break as little as possible.

Sometimes breakage is unavoidable however. In this case, we must make fixes for the downstream
projects too. Once the fix is ready in a PR to aeneas/eurydice/libcrux, add `ci: use
https://github.com/url/of/the/pr` in the top-level description of the charon PR. In the next CI run,
CI will run the tests with these PRs' commits. In this way we make sure to only merge a PR in charon
if we're sure that there's a fix available for the downstream projects.

Our responsibility on the charon side is to keep downstream projects working; we don't have to
implement the features fully. E.g. if the charon change adds a new operation, the aeneas and
eurydice changes only need to handle that new case by raising an error.

## Attribution

This guide is based on **contributing-gen**. [Make your own](https://github.com/bttger/contributing-gen)!

[issue tracker]: https://github.com/AeneasVerif/charon/issues
[Aeneas]: https://github.com/AeneasVerif/aeneas
