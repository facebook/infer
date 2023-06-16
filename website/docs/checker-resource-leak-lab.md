---
title: "Resource Leak Lab Exercise"
description: "Toy checker for the \"resource leak\" write-your-own-checker exercise."
---

Toy checker for the "resource leak" write-your-own-checker exercise.

Activate with `--resource-leak-lab`.

Supported languages:
- C/C++/ObjC: No
- C#/.Net: Yes
- Erlang: No
- Hack: No
- Java: Yes
- Python: No

This toy checker does nothing by default. Hack on it to make it report resource leaks! See the [lab instructions](https://github.com/facebook/infer/blob/main/infer/src/labs/README.md).

## List of Issue Types

The following issue types are reported by this checker:
- [LAB_RESOURCE_LEAK](/docs/next/all-issue-types#lab_resource_leak)
