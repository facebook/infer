---
title: "Self in Block"
description: "An Objective-C-specific analysis to detect when a block captures `self`."
---

An Objective-C-specific analysis to detect when a block captures `self`.

Activate with `--self-in-block`.

Supported languages:
- C/C++/ObjC: Yes
- Java: No
- C#/.Net: No



## List of Issue Types

The following issue types are reported by this checker:
- [CAPTURED_STRONG_SELF](/docs/1.1.0/all-issue-types#captured_strong_self)
- [MIXED_SELF_WEAKSELF](/docs/1.1.0/all-issue-types#mixed_self_weakself)
- [MULTIPLE_WEAKSELF](/docs/1.1.0/all-issue-types#multiple_weakself)
- [STRONG_SELF_NOT_CHECKED](/docs/1.1.0/all-issue-types#strong_self_not_checked)
- [WEAK_SELF_IN_NO_ESCAPE_BLOCK](/docs/1.1.0/all-issue-types#weak_self_in_no_escape_block)
