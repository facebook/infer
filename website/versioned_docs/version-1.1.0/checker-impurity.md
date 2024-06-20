---
title: "Impurity"
description: "Detects functions with potential side-effects. Same as \"purity\", but implemented on top of Pulse."
---

Detects functions with potential side-effects. Same as "purity", but implemented on top of Pulse.

Activate with `--impurity`.

Supported languages:
- C/C++/ObjC: Experimental
- Java: Experimental
- C#/.Net: Experimental

This is an experimental inter-procedural analysis that detects impure functions. It is meant to be an improvement over the [purity](/docs/1.1.0/checker-purity) analysis with a negation on the issue types. For each function, impurity analysis keeps track of not only the impurity of the function but also some additional information such as which parameters/globals the function modifies. It models functions with no summary/model as impure. The analysis relies on [Pulse](/docs/1.1.0/checker-pulse) summaries to determine impurity.


## List of Issue Types

The following issue types are reported by this checker:
- [IMPURE_FUNCTION](/docs/1.1.0/all-issue-types#impure_function)
- [MODIFIES_IMMUTABLE](/docs/1.1.0/all-issue-types#modifies_immutable)
