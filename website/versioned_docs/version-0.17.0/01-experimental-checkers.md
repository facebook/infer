---
id: experimental-checkers
title: "Infer : Experimental Checkers"
---

Infer contains a number of experimental checkers that can be run using just like
the normal infer analysis
`infer -a checkers --<checker_name> -- <your build command>`. `checker_name` can
be `bufferoverrun`, `siof`, or `quandary`. We'll explain the capabilities of
each experimental checker, its level of maturity (on a scale including "in
development", "medium", and "probably deployable"), and the language(s) it
targets.

# Inferbo

- Languages: C (but should be easy to adapt to Objective-C/C++, and possibly
  Java.)
- Maturity: Medium

Inferbo is a detector for out-of-bounds array accesses. You can read all about
it in this blog
[post](https://research.fb.com/inferbo-infer-based-buffer-overrun-analyzer/). It
has been tuned for C, but we are planning to adapt it to other languages in the
near future.

# Quandary

- Languages: Java, C/C++
- Maturity: Medium

Quandary is a static taint analyzer that identifies a variety of unsafe
information flows. It has a small list of built-in
[sources](https://github.com/facebook/infer/blob/master/infer/src/quandary/JavaTrace.ml#L36)
and
[sinks](https://github.com/facebook/infer/blob/master/infer/src/quandary/JavaTrace.ml#L178),
and you can define custom sources and sinks in your `.inferconfig` file (see
example
[here](https://github.com/facebook/infer/blob/master/infer/tests/codetoanalyze/java/quandary/.inferconfig)).
