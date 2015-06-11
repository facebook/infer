---
id: about-Infer
title: About Infer
layout: docs
permalink: /docs/about-Infer.html
section: Foundations
section_order: 02
order: 01
---

Infer is a static program analyzer for Java, C, and Objective-C, written in [OCaml](https://ocaml.org/).  
Infer is deployed within Facebook and it is running continuously to verify select properties of every code modification for the main Facebook apps for Android and iOS, Facebook Messenger, Instagram, and other apps.
It can be used for other code too: Infer can analyze also C code and Java code that is not Android.
At present Infer is tracking problems caused by null pointer dereferences and resource and memory leaks, which cause some of the more important problems on mobile. 


Infer came to Facebook with the acquisition of the verification startup Monoidics in 2013. 
Monoidics was itself based on recent
academic research, particularly on separation logic and bi-abduction.
Within Facebook, Infer has been undergoing iterative development and changing in response to feedback from developers.
We are continuing development of Infer as open-source so that others can benefit from using it, and so that we
can partner with the community on a journey aimed at making program verification technology more broadly practical.  

