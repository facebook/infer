---
title: "Loop Hoisting"
description: "Detect opportunities to hoist function calls that are invariant outside of loop bodies for efficiency."
---

Detect opportunities to hoist function calls that are invariant outside of loop bodies for efficiency.

Activate with `--loop-hoisting`.

Supported languages:
- C/C++/ObjC: Yes
- C#/.Net: No
- Erlang: No
- Hack: No
- Java: Yes
- Python: No

This checker detects opportunities to hoist function calls that are invariant to outside of loop bodies. The hoisting analysis relies on [purity](/docs/checker-purity) analysis to determine whether a function is pure or not.

It has an additional mode that reports [loop-invariant functions that are expensive](/docs/all-issue-types#expensive_loop_invariant_call) (i.e. at least linear). This is enabled by the flag `--hoisting-report-only-expensive`.


## List of Issue Types

The following issue types are reported by this checker:
- [EXPENSIVE_LOOP_INVARIANT_CALL](/docs/all-issue-types#expensive_loop_invariant_call)
- [INVARIANT_CALL](/docs/all-issue-types#invariant_call)
