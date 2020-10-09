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
- [BIABDUCTION_MEMORY_LEAK](/docs/all-issue-types#biabduction_memory_leak)
- [DANGLING_POINTER_DEREFERENCE](/docs/all-issue-types#dangling_pointer_dereference)
- [DIVIDE_BY_ZERO](/docs/all-issue-types#divide_by_zero)
- [EMPTY_VECTOR_ACCESS](/docs/all-issue-types#empty_vector_access)
- [IVAR_NOT_NULL_CHECKED](/docs/all-issue-types#ivar_not_null_checked)
- [NULL_DEREFERENCE](/docs/all-issue-types#null_dereference)
- [PARAMETER_NOT_NULL_CHECKED](/docs/all-issue-types#parameter_not_null_checked)
- [PREMATURE_NIL_TERMINATION_ARGUMENT](/docs/all-issue-types#premature_nil_termination_argument)
- [RESOURCE_LEAK](/docs/all-issue-types#resource_leak)
- [RETAIN_CYCLE](/docs/all-issue-types#retain_cycle)
