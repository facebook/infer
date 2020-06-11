---
title: "Quandary"
description: "The Quandary taint analysis detects flows of values between sources and sinks, except if the value went through a \"sanitizer\". In addition to some defaults, users can specify their own sources, sinks, and sanitizers functions."
---

The Quandary taint analysis detects flows of values between sources and sinks, except if the value went through a "sanitizer". In addition to some defaults, users can specify their own sources, sinks, and sanitizers functions.

Activate with `--quandary`.

Supported languages:
- C/C++/ObjC: Yes
- Java: Yes

Quandary is a static taint analyzer that identifies a variety of unsafe
information flows. It has a small list of built-in
[sources](https://github.com/facebook/infer/blob/master/infer/src/quandary/JavaTrace.ml#L36)
and
[sinks](https://github.com/facebook/infer/blob/master/infer/src/quandary/JavaTrace.ml#L178),
and you can define custom sources and sinks in your `.inferconfig` file (see
example
[here](https://github.com/facebook/infer/blob/master/infer/tests/codetoanalyze/java/quandary/.inferconfig)).


## List of Issue Types

The following issue types are reported by this checker:
