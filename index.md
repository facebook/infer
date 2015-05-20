---
layout: home
title: Infer | A static analyser for mobile apps
id: home
---

## What is Infer?

Infer is a static analyzer based on recent research
in program verification that we are developing inside Facebook. 
We deploy it as part of our development process, where we use it to improve the quality of our mobile apps.
Infer runs on
of code diffs submitted to our mobile code bases, including the main Facebook apps for Android and iOS, Facebook Messenger, Instagram, and other apps which are used by over a billion people in total.
Each month, hundreds of potential bugs are reported by Infer and fixed by Facebook developers,
before they are committed and shipped to peoples' phones.

If you give Infer an input program, it produces a list of potential bugs.

![static/images/Infer-landing.jpg](static/images/Infer-landing.jpg)


We are releasing binaries of Infer for use with Linux or MacOS. You can run it on your local machine to view the results. We are also releasing the source code for people who wish to contribute to or study it.

At present, Infer reports null pointer exceptions and resource leaks for Android and Java code, and additionally memory leaks for iOS and C. These are some of the more important error-types on mobile apps. 
We will be extending Infer's capabilities as we develop it further.


##Using Infer

Start with our <a href="docs/getting-started.html">Getting Started</a> guide to download and try Infer yourself. Infer is open-source, so you can also start with the code on the <a href="https://github.com/facebook/Infer">GitHub repo</a>.

Infer is still evolving, and we want to continue to develop it in the open. 

We hope it will be useful for other projects, so please try it out or contribute to it, join the community and give us feedback!
