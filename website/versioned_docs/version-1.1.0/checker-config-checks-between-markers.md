---
title: "Config Checks between Markers"
description: "[EXPERIMENTAL] Collects config checks between marker start and end."
---

[EXPERIMENTAL] Collects config checks between marker start and end.

Activate with `--config-checks-between-markers`.

Supported languages:
- C/C++/ObjC: Experimental
- Java: Experimental
- C#/.Net: Experimental

This checker collects config checkings in some program regions determined by pairs of marker-starts and marker-ends. The set of config checking functions, marker-start functions, and marker-end functions is hardcoded and empty by default for now, so to use this checker, please modify the code directly in [FbGKInteraction.ml](https://github.com/facebook/infer/tree/main/infer/src/opensource).

## List of Issue Types

The following issue types are reported by this checker:
- [CONFIG_CHECKS_BETWEEN_MARKERS](/docs/1.1.0/all-issue-types#config_checks_between_markers)
