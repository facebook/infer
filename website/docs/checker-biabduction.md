---
title: "Biabduction"
description: "This analysis deals with a range of issues, many linked to memory safety."
---

This analysis deals with a range of issues, many linked to memory safety.

Activate with `--biabduction`.

Supported languages:
- C/C++/ObjC: Yes
- C#/.Net: Yes
- Erlang: No
- Java: Yes

Read more about its foundations in the [Separation Logic and Biabduction page](separation-logic-and-bi-abduction).

## List of Issue Types

The following issue types are reported by this checker:
- [BIABDUCTION_MEMORY_LEAK](/docs/next/all-issue-types#biabduction_memory_leak)
- [BIABDUCTION_RETAIN_CYCLE](/docs/next/all-issue-types#biabduction_retain_cycle)
- [DANGLING_POINTER_DEREFERENCE](/docs/next/all-issue-types#dangling_pointer_dereference)
- [DIVIDE_BY_ZERO](/docs/next/all-issue-types#divide_by_zero)
- [EMPTY_VECTOR_ACCESS](/docs/next/all-issue-types#empty_vector_access)
- [NULL_DEREFERENCE](/docs/next/all-issue-types#null_dereference)
- [PREMATURE_NIL_TERMINATION_ARGUMENT](/docs/next/all-issue-types#premature_nil_termination_argument)
- [RESOURCE_LEAK](/docs/next/all-issue-types#resource_leak)
