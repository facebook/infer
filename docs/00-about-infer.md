---
id: about-infer
title: About Infer
layout: docs
permalink: /docs/about-infer.html
next: getting-started.html
---

INFER is a static program analyzer for Java, C, and Objective-C.  A static analyzer analyzes source code at compile time helping identifying vulnerabilities in code before it is run and therefore allowing to ship high quality code.

INFER is deployed within Facebook and it is running continuously to verify select properties of every code modification for the main Facebook apps for Android and iOS, Facebook Messenger, Instagram, and other apps.
INFER is used at Facebook to catch bugs before mobile code is shipped. But it can be used for other code too. INFER can analyze also C code and Java code that is not Android;

We have trained INFER on Facebook's mobile apps and at present it is tracking problems caused by null pointer dereferences and resource and memory leaks, which cause some of the more important problems on mobile. Just as other Facebook products are, INFER itself is undergoing iterative development and changing in response to feedback from people who use it (in this case, developers). We are continuing development of INFER as open-source so that others can benefit from using  it on their apps. We 
want to partner with the community on a journey aimed at making program verification technology practical.  
 

**INFER's History.**
INFER  is based on recent academic research in automatic program analysis using a novel logical inference technique called bi-abduction, which applied a relatively recent development in the theoretical area of logics of programs, called separation logic. 
The following papers give a detailed technical background on these topics:

- [Paper on Separation Logic] 
- [JACM 2011 paper]
- [NASA 2015 paper for the way how infer is used at FB]
- [MORE???????....]


We founded a startup company, Monoidics, based on this research in 2009, and joined Facebook in 2013 to continue our journey towards deploying program verification to the world.  

 Program verification is an area with an active research community and amazing, promising technology. But there is much more that remains to be done. At Facebook we have a saying that this journey is 1% finished, and we look forward to a future in which, with your help,  verification technology can prove more and more useful in helping programmers develop reliable code, fast.

To find out more about INFER, download it, and try it out, check out fb-infer.org.


