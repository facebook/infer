---
title: "Config Checks between Markers"
description: "[EXPERIMENTAL] Collects config checks between marker start and end."
---

[EXPERIMENTAL] Collects config checks between marker start and end.

Activate with `--config-checks-between-markers`.

Supported languages:
- C/C++/ObjC: Experimental
- C#/.Net: Experimental
- Erlang: Experimental
- Java: Experimental

This checker collects config checkings in some program regions determined by pairs of marker-starts and marker-ends. The set of config checking functions, marker-start functions, and marker-end functions is hardcoded and empty by default for now, so to use this checker, please modify the code directly in [FbGKInteraction.ml](https://github.com/facebook/infer/tree/master/infer/src/opensource).

## List of Issue Types

The following issue types are reported by this checker:
- [CONFIG_CHECKS_BETWEEN_MARKERS](/docs/next/all-issue-types#config_checks_between_markers)
