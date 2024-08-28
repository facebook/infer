---
id: suppressions
title: Suppression of Infer Issues
---

Infer includes a language-agnostic method to suppress Infer issues in source files.

When an Infer issue is suppressed, it will not appear in the text report (infer-out/report.txt) after an Infer run. Suppressed issues will still appear in the JSON report file (infer-out/report.json) but with the added key `"suppressed": true`.

Two keywords are supported:

* `@infer-ignore ISSUE_TYPE, ISSUE_TYPE2, ...`

  Ignore matching Infer issues on the next or current line. Comma `,` is used to separate multiple issue types on the same line.

* `@infer-ignore-every ISSUE_TYPE, ISSUE_TYPE2, ...`

  Ignore all matching Infer issues in the current file. This keyword can be added anywhere in the source file.

These two keywords are meant to be added as comment lines in the source files you run Infer on.

The full name of the issue type is given as an argument to the @ignore keywords. For example, use `DEAD_STORE` and not 'Dead Store' which is the name used in the text report file.

### Wildcards

Wildcards can be used as arguments to the @ignore keywords to suppress a larger set of Infer issue types without the need to list all of them.

For example, `@infer-ignore PULSE_UNNECESSARY_COPY*` will suppress all unnecessary copy issues, including `PULSE_UNNECESSARY_COPY_ASSIGNMENT_CONST` and `PULSE_UNNECESSARY_COPY_INTERMEDIATE`, etc.

### Examples

* Suppress a single issue

```java
// @infer-ignore THREAD_SAFETY_VIOLATION
if (ConfigUtil.getBoolean(context)) {
    ...
}

// or

if (ConfigUtil.getBoolean(context)) { // @infer-ignore THREAD_SAFETY_VIOLATION
    ...
}

```

* Suppress all issues in a file

```php
% @infer-ignore-every TOPL_ERROR, TOPL_ERROR_LATENT
-module(hello).
-export([hello_world/0]).
hello_world() -> io:fwrite("hello\n").
```

* A line without an @ignore keyword cannot be placed between the @ignore line and the line with the issues.

```java
// @infer-ignore THREAD_SAFETY_VIOLATION
// some more context
if (ConfigUtil.getBoolean(context)) {
    ...
}
```

In this case, the issue on the if statement line is not suppressed since a line without a @infer-ignore is added between the suppression and the line with the issue.

#### Accumulation

When using the `@infer-ignore` keyword, all issue types in a contiguous block of comment lines containing the `@infer-ignore` keyword are accumulated and applied to the next line without a @ignore keyword. This way, long lines can be avoided, and extra context can be given.

For example:

```c
free(x);
// a really good explanation why we are suppressing this warning
// @infer-ignore USE_AFTER_FREE
// also this one @infer-ignore DEAD_STORE
return *x;
```

In the example above, on the `return` line, both the issue types `USE_AFTER_FREE` and `DEAD_STORE` are suppressed.

Equivalent to the example above:

```c
free(x);
// a really good explanation why we are suppressing this warning
// @infer-ignore USE_AFTER_FREE
return *x; // @infer-ignore DEAD_STORE
```

An alternative way to achieve the same result would be:

```c
free(x);
// a really good explanation why we are suppressing this warning
// @infer-ignore USE_AFTER_FREE, DEAD_STORE
return *x;
```
