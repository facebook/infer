---
docid: experimental-checkers
title: "Infer : Experimental Checkers"
layout: docs
permalink: /docs/experimental-checkers.html
---

Infer contains a number of experimental checkers that can be run using just like the normal infer analysis `infer -a <checker name> -- <your build command>`. 
`checker_name` can be `threadsafety`, `siof`, `bufferoverrun`, or `quandary`. We'll explain the capabilities of each experimental checker, its level of maturity (on a scale including "in development", "medium", and "probably deployable"), and the language(s) it targets.

# Thread-safety
- Languages: Java
- Maturity: Probably deployable

When a class is marked `@ThreadSafe` or a method is marked [`@ThreadSafeMethod`](https://github.com/facebook/infer/blob/master/infer/annotations/com/facebook/infer/annotation/ThreadSafeMethod.java), this checker will complain if any methods in the class/the marked method writes to a field outside of sychronization.
The idea here is that any race condition involves two concurrent accesses where at least one is a write. 
This checker aims to identify the writes that may be involved in a race.

There are several specialized annotations like `@ThreadConfined` and `@AssumeThreadSafe` that can be used to suppress warnings for the thread-safety analysis; you can read more about what these do in the source [files](https://github.com/facebook/infer/tree/master/infer/annotations/com/facebook/infer/annotation) for the annotations.

# SIOF (static initialization order fiasco)
- Languages: C++
- Maturity: Probably deployable

This checker identifies a class of nondeterministic order-of-initialization [issues](https://isocpp.org/wiki/faq/ctors#static-init-order) in C++.

# Inferbo
- Languages: C (but should be easy to adapt to Objective-C/C++, and possibly Java.)
- Maturity: Medium

Inferbo is a detector for out-of-bounds array accesses. You can read all about it in this blog [post](https://research.fb.com/inferbo-infer-based-buffer-overrun-analyzer/).
It has been tuned for C, but we are planning to adapt it to other languages in the near future.

# Quandary
- Languages: Java (but should be easy to adapt to Objective-C/C++/C)
- Maturity: Medium

Quandary is a static taint analyzer that identifies a variety of unsafe information flows. 
It has a small list of built-in [sources](https://github.com/facebook/infer/blob/master/infer/src/quandary/JavaTrace.ml#L36) and [sinks](https://github.com/facebook/infer/blob/master/infer/src/quandary/JavaTrace.ml#L178), and you can define custom sources and sinks in your `.inferconfig` file (see example [here](https://github.com/facebook/infer/blob/master/infer/tests/codetoanalyze/java/quandary/.inferconfig)).
