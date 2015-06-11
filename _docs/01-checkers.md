---
id: checkers
title: "Infer : Checkers"
layout: docs
permalink: /docs/checkers.html
section: User Guide
section_order: 01
order: 03
---

The Infer analyzer performs sophisticated interprocedural static
analysis. When this power is not needed, such as for analyses of the
kind usually found in so-called *linters*, we have a framework called
Infer:Checkers.

Infer:Checkers can check a given property in each method of a given
project, but *intra-procedurally*, not inter-procedurally.

The checkers can be run by adding the option `-a checkers` to the analysis command as in this example:

```bash
infer -a checkers -- javac Test.java
```

At the moment, we have the checker 
[immutable cast](docs/checkers-bug-types.html#CHECKERS_IMMUTABLE_CAST). 
