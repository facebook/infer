---
title: "Config Impact Analysis"
description: "[EXPERIMENTAL] Collects function that are called without config checks."
---

[EXPERIMENTAL] Collects function that are called without config checks.

Activate with `--config-impact-analysis`.

Supported languages:
- C/C++/ObjC: Experimental
- C#/.Net: Experimental
- Erlang: Experimental
- Java: Experimental

This checker collects functions whose execution isn't gated by certain pre-defined gating functions. The set of gating functions is hardcoded and empty by default for now, so to use this checker, please modify the code directly in [FbGKInteraction.ml](https://github.com/facebook/infer/tree/master/infer/src/opensource).

## List of Issue Types

The following issue types are reported by this checker:
- [CONFIG_IMPACT](/docs/next/all-issue-types#config_impact)
