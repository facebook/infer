---
title: "Annotation Reachability"
description: "Given a pair of source and sink annotation, e.g. `@PerformanceCritical` and `@Expensive`, this checker will warn whenever some method annotated with `@PerformanceCritical` calls, directly or indirectly, another method annotated with `@Expensive`"
---

Given a pair of source and sink annotation, e.g. `@PerformanceCritical` and `@Expensive`, this checker will warn whenever some method annotated with `@PerformanceCritical` calls, directly or indirectly, another method annotated with `@Expensive`

Activate with `--annotation-reachability`.

Supported languages:
- C/C++/ObjC: Yes
- C#/.Net: Yes
- Erlang: Yes
- Java: Yes



## List of Issue Types

The following issue types are reported by this checker:
- [CHECKERS_ALLOCATES_MEMORY](/docs/next/all-issue-types#checkers_allocates_memory)
- [CHECKERS_ANNOTATION_REACHABILITY_ERROR](/docs/next/all-issue-types#checkers_annotation_reachability_error)
- [CHECKERS_CALLS_EXPENSIVE_METHOD](/docs/next/all-issue-types#checkers_calls_expensive_method)
- [CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED](/docs/next/all-issue-types#checkers_expensive_overrides_unannotated)
