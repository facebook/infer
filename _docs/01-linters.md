---
docid: linters
title: "Infer : Linters"
layout: docs
permalink: /docs/linters.html
---

For iOS apps, we provide a linters framework. These are checks about the syntax of the program; it could be about a property, or about code inside one method, or that a class or method have certain properties. We provide [a few checks](/docs/linters-bug-types.html) and the framework is also easy to extend. Soon we will have a domain specific language (DSL) to make it even easier to write checks.


The linters are run by default when you run Infer. However, there is a way of running only the linters, which is faster than also running Infer. This is by adding the option `-a linters` to the analysis command as in this example:

```bash
infer -a linters -- clang -c Test.m
```
