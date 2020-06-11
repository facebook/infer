---
title: "Biabduction"
description: "This analysis deals with a range of issues, many linked to memory safety."
---

This analysis deals with a range of issues, many linked to memory safety.

Activate with `--biabduction`.

Supported languages:
- C/C++/ObjC: Yes
- Java: Yes

Read more about its foundations in the [Separation Logic and Biabduction page](separation-logic-and-bi-abduction).

## List of Issue Types

The following issue types are reported by this checker:
- [EMPTY_VECTOR_ACCESS](all-issue-types.md#empty_vector_access)
- [IVAR_NOT_NULL_CHECKED](all-issue-types.md#ivar_not_null_checked)
- [MEMORY_LEAK](all-issue-types.md#memory_leak)
- [NULL_DEREFERENCE](all-issue-types.md#null_dereference)
- [PARAMETER_NOT_NULL_CHECKED](all-issue-types.md#parameter_not_null_checked)
- [PREMATURE_NIL_TERMINATION_ARGUMENT](all-issue-types.md#premature_nil_termination_argument)
- [RESOURCE_LEAK](all-issue-types.md#resource_leak)
- [RETAIN_CYCLE](all-issue-types.md#retain_cycle)
