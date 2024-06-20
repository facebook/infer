---
title: "Starvation"
description: "Detect various kinds of situations when no progress is being made because of concurrency errors."
---

Detect various kinds of situations when no progress is being made because of concurrency errors.

Activate with `--starvation`.

Supported languages:
- C/C++/ObjC: Yes
- C#/.Net: No
- Erlang: No
- Hack: No
- Java: Yes
- Python: No

Detect several kinds of "starvation" problems:
- deadlocks
- violations of `@Lockless` annotations
- violations of [Android's "strict mode"](https://developer.android.com/reference/android/os/StrictMode)
- doing expensive operations on the Android UI thread


## List of Issue Types

The following issue types are reported by this checker:
- [ARBITRARY_CODE_EXECUTION_UNDER_LOCK](/docs/all-issue-types#arbitrary_code_execution_under_lock)
- [DEADLOCK](/docs/all-issue-types#deadlock)
- [IPC_ON_UI_THREAD](/docs/all-issue-types#ipc_on_ui_thread)
- [LOCKLESS_VIOLATION](/docs/all-issue-types#lockless_violation)
- [REGEX_OP_ON_UI_THREAD](/docs/all-issue-types#regex_op_on_ui_thread)
- [STARVATION](/docs/all-issue-types#starvation)
- [STRICT_MODE_VIOLATION](/docs/all-issue-types#strict_mode_violation)
