---
title: "Self in Block"
description: "An Objective-C-specific analysis to detect when a block captures `self`."
---

An Objective-C-specific analysis to detect when a block captures `self`.

Activate with `--self-in-block`.

Supported languages:
- C/C++/ObjC: Yes
- C#/.Net: No
- Erlang: No
- Hack: No
- Java: No
- Python: No



## List of Issue Types

The following issue types are reported by this checker:
- [CAPTURED_STRONG_SELF](/docs/next/all-issue-types#captured_strong_self)
- [CXX_REF_CAPTURED_IN_BLOCK](/docs/next/all-issue-types#cxx_ref_captured_in_block)
- [MIXED_SELF_WEAKSELF](/docs/next/all-issue-types#mixed_self_weakself)
- [MULTIPLE_WEAKSELF](/docs/next/all-issue-types#multiple_weakself)
- [SELF_IN_BLOCK_PASSED_TO_INIT](/docs/next/all-issue-types#self_in_block_passed_to_init)
- [STRONG_SELF_NOT_CHECKED](/docs/next/all-issue-types#strong_self_not_checked)
- [WEAK_SELF_IN_NO_ESCAPE_BLOCK](/docs/next/all-issue-types#weak_self_in_no_escape_block)
