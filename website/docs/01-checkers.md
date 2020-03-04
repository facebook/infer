---
id: checkers
title: "Infer : AI"
---

Infer.AI is a collection of program analyses which range from simple checks to
sophisticated inter-procedural analysis. Infer.AI is so named because it is
based on Abstract Interpretation.

Current Infer.AI's which are in production include ThreadSafety,
AnnotationReachability (e.g., can an allocation be reached from a
@PerformanceCritical method), and
[immutable cast](checkers-bug-types#CHECKERS_IMMUTABLE_CAST) for Java,
as well as Static Initialization Order Fiasco for C++.

The current checkers can be run by adding the option `-a checkers` to the
analysis command as in this example:

```bash
infer run -a checkers -- javac Test.java
```

In addition, we are working on experimental AI's which target security
properties (Quandary) and buffer overruns (Inferbo). The infer commandline man
page (`infer --help`) explains how to run experimental AI's, or how to select
certain AI's and not others.
