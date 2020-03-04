---
title: OCamlFormat open-source released
author: Josh Berdine
---

We are pleased to announce the first public release of OCamlFormat.

OCamlFormat is a tool to automatically format [OCaml](https://ocaml.org/) code.
It follows the same basic design as refmt for
[Reason](https://reasonml.github.io/) code, but for OCaml. In particular, it
works by parsing source code using the OCaml compiler's standard parser,
deciding where to place comments in the parsetree, and printing the parsetree
and comments in a uniform style.

At Facebook, we currently use this for the OCaml code of
[Infer](https://github.com/facebook/infer) to enable developers to stop thinking
about line breaking, indentation, parenthesization, etc., to minimize stylistic
nit-picking during code review, and to make it as visually obvious as possible
when the parser's interpretation of code does not match the programmer's. We use
this both with integration with editors as well as a pre-commit hook.

Development is taking place on
[github](http://github.com/ocaml-ppx/ocamlformat). License is MIT.

See the [github page](http://github.com/ocaml-ppx/ocamlformat) for more info on
installation, documentation, contributing, etc.
