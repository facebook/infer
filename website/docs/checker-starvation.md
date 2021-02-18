---
title: "Starvation"
description: "Detect various kinds of situations when no progress is being made because of concurrency errors."
---

Detect various kinds of situations when no progress is being made because of concurrency errors.

Activate with `--starvation`.

Supported languages:
- C/C++/ObjC: Yes
- Java: Yes
- C#/.Net: Yes

Detect several kinds of "starvation" problems:
- deadlocks
- violations of `@Lockless` annotations
- violations of [Android's "strict mode"](https://developer.android.com/reference/android/os/StrictMode)
- doing expensive operations on the Android UI thread


## List of Issue Types

The following issue types are reported by this checker:
- [ARBITRARY_CODE_EXECUTION_UNDER_LOCK](/docs/next/all-issue-types#arbitrary_code_execution_under_lock)
- [DEADLOCK](/docs/next/all-issue-types#deadlock)
- [LOCKLESS_VIOLATION](/docs/next/all-issue-types#lockless_violation)
- [STARVATION](/docs/next/all-issue-types#starvation)
- [STRICT_MODE_VIOLATION](/docs/next/all-issue-types#strict_mode_violation)
