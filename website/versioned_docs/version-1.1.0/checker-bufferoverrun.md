---
title: "Buffer Overrun Analysis (InferBO)"
description: "InferBO is a detector for out-of-bounds array accesses."
---

InferBO is a detector for out-of-bounds array accesses.

Activate with `--bufferoverrun`.

Supported languages:
- C/C++/ObjC: Yes
- Java: Yes
- C#/.Net: Yes

You can read about its origins in this [blog post](https://research.fb.com/inferbo-infer-based-buffer-overrun-analyzer/).

## List of Issue Types

The following issue types are reported by this checker:
- [BUFFER_OVERRUN_L1](/docs/1.1.0/all-issue-types#buffer_overrun_l1)
- [BUFFER_OVERRUN_L2](/docs/1.1.0/all-issue-types#buffer_overrun_l2)
- [BUFFER_OVERRUN_L3](/docs/1.1.0/all-issue-types#buffer_overrun_l3)
- [BUFFER_OVERRUN_L4](/docs/1.1.0/all-issue-types#buffer_overrun_l4)
- [BUFFER_OVERRUN_L5](/docs/1.1.0/all-issue-types#buffer_overrun_l5)
- [BUFFER_OVERRUN_S2](/docs/1.1.0/all-issue-types#buffer_overrun_s2)
- [BUFFER_OVERRUN_U5](/docs/1.1.0/all-issue-types#buffer_overrun_u5)
- [CONDITION_ALWAYS_FALSE](/docs/1.1.0/all-issue-types#condition_always_false)
- [CONDITION_ALWAYS_TRUE](/docs/1.1.0/all-issue-types#condition_always_true)
- [INFERBO_ALLOC_IS_BIG](/docs/1.1.0/all-issue-types#inferbo_alloc_is_big)
- [INFERBO_ALLOC_IS_NEGATIVE](/docs/1.1.0/all-issue-types#inferbo_alloc_is_negative)
- [INFERBO_ALLOC_IS_ZERO](/docs/1.1.0/all-issue-types#inferbo_alloc_is_zero)
- [INFERBO_ALLOC_MAY_BE_BIG](/docs/1.1.0/all-issue-types#inferbo_alloc_may_be_big)
- [INFERBO_ALLOC_MAY_BE_NEGATIVE](/docs/1.1.0/all-issue-types#inferbo_alloc_may_be_negative)
- [INTEGER_OVERFLOW_L1](/docs/1.1.0/all-issue-types#integer_overflow_l1)
- [INTEGER_OVERFLOW_L2](/docs/1.1.0/all-issue-types#integer_overflow_l2)
- [INTEGER_OVERFLOW_L5](/docs/1.1.0/all-issue-types#integer_overflow_l5)
- [INTEGER_OVERFLOW_U5](/docs/1.1.0/all-issue-types#integer_overflow_u5)
- [UNREACHABLE_CODE](/docs/1.1.0/all-issue-types#unreachable_code)
