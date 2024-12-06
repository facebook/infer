---
title: "Static Constructor Stall Checker"
description: "Detect if dispatch_once is called from a static constructor."
---

Detect if dispatch_once is called from a static constructor.

Activate with `--static-constructor-stall-checker`.

Supported languages:
- C/C++/ObjC: Yes
- C#/.Net: No
- Erlang: No
- Hack: No
- Java: No
- Python: No



## List of Issue Types

The following issue types are reported by this checker:
- [STATIC_CONSTRUCTOR_STALL](/docs/next/all-issue-types#static_constructor_stall)
