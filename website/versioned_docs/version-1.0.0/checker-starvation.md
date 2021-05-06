---
title: "Starvation"
description: "Detect various kinds of situations when no progress is being made because of concurrency errors."
---

Detect various kinds of situations when no progress is being made because of concurrency errors.

Activate with `--starvation`.

Supported languages:
- C/C++/ObjC: Yes
- Java: Yes

Detect several kinds of "starvation" problems:
- deadlocks
- violations of `@Lockless` annotations
- violations of [Android's "strict mode"](https://developer.android.com/reference/android/os/StrictMode)
- doing expensive operations on the Android UI thread


## List of Issue Types

The following issue types are reported by this checker:
- [DEADLOCK](/docs/1.0.0/all-issue-types#deadlock)
- [LOCKLESS_VIOLATION](/docs/1.0.0/all-issue-types#lockless_violation)
- [STARVATION](/docs/1.0.0/all-issue-types#starvation)
- [STRICT_MODE_VIOLATION](/docs/1.0.0/all-issue-types#strict_mode_violation)
