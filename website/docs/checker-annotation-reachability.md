---
title: "Annotation Reachability"
description: "Given pairs of source and sink annotations, e.g. `@A` and `@B`, this checker will warn whenever some method annotated with `@A` calls, directly or indirectly, another method annotated with `@B`. Besides the custom pairs, it is also possible to enable some built-in checks, such as `@PerformanceCritical` reaching `@Expensive` or `@NoAllocation` reaching `new`. See flags starting with `--annotation-reachability`."
---

Given pairs of source and sink annotations, e.g. `@A` and `@B`, this checker will warn whenever some method annotated with `@A` calls, directly or indirectly, another method annotated with `@B`. Besides the custom pairs, it is also possible to enable some built-in checks, such as `@PerformanceCritical` reaching `@Expensive` or `@NoAllocation` reaching `new`. See flags starting with `--annotation-reachability`.

Activate with `--annotation-reachability`.

Supported languages:
- C/C++/ObjC: No
- C#/.Net: No
- Erlang: No
- Hack: No
- Java: Yes
- Python: No



## List of Issue Types

The following issue types are reported by this checker:
- [CHECKERS_ALLOCATES_MEMORY](/docs/next/all-issue-types#checkers_allocates_memory)
- [CHECKERS_ANNOTATION_REACHABILITY_ERROR](/docs/next/all-issue-types#checkers_annotation_reachability_error)
- [CHECKERS_CALLS_EXPENSIVE_METHOD](/docs/next/all-issue-types#checkers_calls_expensive_method)
- [CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED](/docs/next/all-issue-types#checkers_expensive_overrides_unannotated)
