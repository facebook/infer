---
title: "Immutable Cast"
description: "Detection of object cast from immutable types to mutable types. For instance, it will detect casts from `ImmutableList` to `List`, `ImmutableMap` to `Map`, and `ImmutableSet` to `Set`."
---

Detection of object cast from immutable types to mutable types. For instance, it will detect casts from `ImmutableList` to `List`, `ImmutableMap` to `Map`, and `ImmutableSet` to `Set`.

**\*\*\*DEPRECATED\*\*\*** Unmaintained due to poor actionability of the reports.

Activate with `--immutable-cast`.

Supported languages:
- C/C++/ObjC: No
- Java: Yes
- C#/.Net: Yes

Casts flagged by this checker are unsafe because calling mutation operations on the cast objects will fail at runtime.

## List of Issue Types

The following issue types are reported by this checker:
- [CHECKERS_IMMUTABLE_CAST](/docs/1.1.0/all-issue-types#checkers_immutable_cast)
