---
title: "Parameter Not Null Checked"
description: "An Objective-C-specific analysis to detect when a block parameter is used before being checked for null first."
---

An Objective-C-specific analysis to detect when a block parameter is used before being checked for null first.

Activate with `--parameter-not-null-checked`.

Supported languages:
- C/C++/ObjC: Yes
- C#/.Net: No
- Erlang: No
- Hack: No
- Java: No
- Python: No

This checker checks that an Objective-C block that is passed as a parameter
to a function or method is checked for `nil` before it's being executed.


## List of Issue Types

The following issue types are reported by this checker:
- [BLOCK_PARAMETER_NOT_NULL_CHECKED](/docs/all-issue-types#block_parameter_not_null_checked)
