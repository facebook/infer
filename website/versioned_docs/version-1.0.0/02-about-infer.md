---
id: about-Infer
title: About Infer
---

Infer is a static program analyzer for Java, C, and Objective-C, written in
[OCaml](https://ocaml.org/). Infer is deployed within Facebook and it is running
continuously to verify select properties of every code modification for the main
Facebook apps for Android and iOS, Facebook Messenger, Instagram, and other
apps. It can be used for other code too: Infer can also analyze C code, and Java
code that is not Android. At present Infer is tracking problems caused by null
pointer dereferences and resource and memory leaks, which cause some of the more
important problems on mobile.

Infer came to Facebook with the acquisition of the verification startup
Monoidics in 2013. Monoidics was itself based on recent academic research,
particularly on separation logic and bi-abduction.

We have broadened Infer's scope within the past few years. We now refer to the
original separation logic analysis as Infer.SL. We now also have Infer.AI, a
general analysis framework which is an interface to the modular analysis engine
which can be used by other kinds of program analyses (technically, called
''abstract interpretations'', hence the AI monicker). This added generality has
been used to develop instantiations of Infer.AI for security, concurrency and in
other domains. Additionally, we have Infer linters for describing shallow
syntactic analyses, using the AL language, because sometimes linters are just
what you need.
