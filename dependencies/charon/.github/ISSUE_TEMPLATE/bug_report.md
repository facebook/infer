---
name: Bug report
about: Report a bug in Charon
title: 'Bug: <describe the bug>'
labels: C-bug
---

<!--
Thank you for submitting a bug report! Please check this isn't already reported in another
issue.

If this is about a part of the rust language that we don't support, please open an "Unsupported
language feature" issue instead.

Please provide as much information as you can to help us fix the problem faster. What's most helpful
is small reproducible examples and clear explanations of expected behavior.
-->

**Code snippet to reproduce the bug**:

<!--
Provide the rust code you ran Charon on. Make it as small as possible; the majority of bugs can be
reproduced with a dozen lines of rust.
-->

```rust
// add your code here

```

**Charon version**: (the commit of Charon you are using)

**Charon command**: (the `charon` command line you ran)

**Charon output**:

<!--
Provide the output of Charon on your code:
- If there was an error, run `charon` with `RUST_BACKTRACE=1` and provide the full error message;
- If there was no error, run `charon` with the `--pretty-llbc` flag and provide the full output (or the relevant parts).
-->

```
// paste the output here

```

**Any other steps needed to reproduce**:


**Explain the bug**: (why is this behavior incorrect, and what do you think Charon should do instead)
