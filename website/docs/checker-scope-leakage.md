---
title: "Scope Leakage"
description: "The Java/Kotlin checker takes into account a set of \"scope\" annotations and a must-not-hold relation over the scopes. The checker raises an alarm if there exists a field access path from object A to object B, with respective scopes SA and SB, such that must-not-hold(SA, SB)."
---

The Java/Kotlin checker takes into account a set of "scope" annotations and a must-not-hold relation over the scopes. The checker raises an alarm if there exists a field access path from object A to object B, with respective scopes SA and SB, such that must-not-hold(SA, SB).

Activate with `--scope-leakage`.

Supported languages:
- C/C++/ObjC: No
- C#/.Net: No
- Erlang: No
- Hack: No
- Java: Yes
- Python: No



## List of Issue Types

The following issue types are reported by this checker:
- [SCOPE_LEAKAGE](/docs/next/all-issue-types#scope_leakage)
