---
title: "Biabduction"
description: "This analysis deals with a range of issues, many linked to memory safety."
---

This analysis deals with a range of issues, many linked to memory safety.

Activate with `--biabduction`.

Supported languages:
- C/C++/ObjC: Yes
- Java: Yes
- C#/.Net: Yes

Read more about its foundations in the [Separation Logic and Biabduction page](separation-logic-and-bi-abduction).

## List of Issue Types

The following issue types are reported by this checker:
- [BIABDUCTION_MEMORY_LEAK](/docs/1.1.0/all-issue-types#biabduction_memory_leak)
- [DANGLING_POINTER_DEREFERENCE](/docs/1.1.0/all-issue-types#dangling_pointer_dereference)
- [DIVIDE_BY_ZERO](/docs/1.1.0/all-issue-types#divide_by_zero)
- [EMPTY_VECTOR_ACCESS](/docs/1.1.0/all-issue-types#empty_vector_access)
- [IVAR_NOT_NULL_CHECKED](/docs/1.1.0/all-issue-types#ivar_not_null_checked)
- [NULL_DEREFERENCE](/docs/1.1.0/all-issue-types#null_dereference)
- [PARAMETER_NOT_NULL_CHECKED](/docs/1.1.0/all-issue-types#parameter_not_null_checked)
- [PREMATURE_NIL_TERMINATION_ARGUMENT](/docs/1.1.0/all-issue-types#premature_nil_termination_argument)
- [RESOURCE_LEAK](/docs/1.1.0/all-issue-types#resource_leak)
- [RETAIN_CYCLE](/docs/1.1.0/all-issue-types#retain_cycle)
