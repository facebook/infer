---
title: "Impurity"
description: "Detects functions with potential side-effects. Same as \"purity\", but implemented on top of Pulse."
---

Detects functions with potential side-effects. Same as "purity", but implemented on top of Pulse.

Activate with `--impurity`.

Supported languages:
- C/C++/ObjC: Experimental
- C#/.Net: Experimental
- Erlang: Experimental
- Java: Experimental

This is an experimental inter-procedural analysis that detects impure functions. It is meant to be an improvement over the [purity](/docs/next/checker-purity) analysis with a negation on the issue types. For each function, impurity analysis keeps track of not only the impurity of the function but also some additional information such as which parameters/globals the function modifies. It models functions with no summary/model as impure. The analysis relies on [Pulse](/docs/next/checker-pulse) summaries to determine impurity.


## List of Issue Types

The following issue types are reported by this checker:
- [IMPURE_FUNCTION](/docs/next/all-issue-types#impure_function)
- [MODIFIES_IMMUTABLE](/docs/next/all-issue-types#modifies_immutable)
