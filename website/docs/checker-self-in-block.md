---
title: "Self in Block"
description: "An Objective-C-specific analysis to detect when a block captures `self`."
---

An Objective-C-specific analysis to detect when a block captures `self`.

Activate with `--self-in-block`.

Supported languages:
- C/C++/ObjC: Yes
- Java: No



## List of Issue Types

The following issue types are reported by this checker:
- [CAPTURED_STRONG_SELF](all-issue-types.md#captured_strong_self)
- [MIXED_SELF_WEAKSELF](all-issue-types.md#mixed_self_weakself)
- [MULTIPLE_WEAKSELF](all-issue-types.md#multiple_weakself)
- [STRONG_SELF_NOT_CHECKED](all-issue-types.md#strong_self_not_checked)
- [WEAK_SELF_IN_NO_ESCAPE_BLOCK](all-issue-types.md#weak_self_in_no_escape_block)
