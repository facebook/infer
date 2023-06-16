---
title: "`printf()` Argument Types"
description: "Detect mismatches between the Java `printf` format strings and the argument types For example, this checker will warn about the type error in `printf(\"Hello %d\", \"world\")`"
---

Detect mismatches between the Java `printf` format strings and the argument types For example, this checker will warn about the type error in `printf("Hello %d", "world")`

**\*\*\*DEPRECATED\*\*\*** Unmaintained.

Activate with `--printf-args`.

Supported languages:
- C/C++/ObjC: No
- C#/.Net: No
- Erlang: No
- Hack: No
- Java: Yes
- Python: No



## List of Issue Types

The following issue types are reported by this checker:
- [CHECKERS_PRINTF_ARGS](/docs/next/all-issue-types#checkers_printf_args)
