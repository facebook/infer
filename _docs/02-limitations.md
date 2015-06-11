---
id: limitations
title: Limitations, etc
layout: docs
permalink: /docs/limitations.html
section: Foundations
section_order: 02
order: 03
---



## Expectations <a name="expectations"></a>

We want to be clear that if you run Infer on your project you might get very good results, but it is also possible that you don't.
Although we have had good fix rates working with Facebook mobile codebases,
we are not making strong claims about rates of false alarms or similar when applied to arbitrary
codebases. For example, we have had some success [getting bugs fixed 
in the DuckDuckGo Android App](blog/2015/05/22/Infer-on-open-source-android-apps.html), but we encountered many false alarms when running Infer on GNU coreutils.
It typical of program verification and static analysis tools that their results vary,
and that is to be expected, e.g., because
they are tackling undecidable problems and because different codebases they are applied to will have been coded differently.



The good thing, though, is that you might get useful results! And, where the results are imperfect,
this can be taken as input for improvement.


Apart from these general remarks, Infer has a number of specific technical limitations, which we describe in terms
of bug types and language features.



## Bug types <a name="bugtypes"></a>

At present Infer is reporting on a restricted collection of
[bug types](/docs/infer-bug-types.html),
typically involving null pointers and memory or resource leaks.
The intitial set of bug types
Infer has focused on was driven by the most pressing needs for serving the Facebook
mobile developers. Our approach has been to report less initially, to iterate with developers and provide value to them,
and gradually expand what we can do while still providing value.


Some bug types we don't report as of yet include

- Array bounds errors
- Cast exceptions
- Leaking of tainted data
- Concurrency race conditions

and more.  In the first three cases we have partial treatments inside of Infer, but we have not surfaced these capabilities yet; the reports are not of sufficient quality to present to developers. 
For example, Infer can
find some potential array bounds errors, but many of its reports are false alarms and it misses still more.

Put another way: there is more work to do!


## Language Features <a name="languagefeatures"></a>



A different dimension in which Infer is limited concerns language features.
Infer either does not understand or has a weak treatment of

- Concurrency, including Java's Concurrency Utilities and iOS's Grand Central Dispatch
- Dynamic dispatch
- Reflection
- Android lifecycles
- Arithmetic
- and more

Some of these problems are fundamental, largely open, problems in program analysis
(especially concurrency), while for others there is much prior and successful work to draw upon
(e.g., arithmetic)
and are simply on our todo list awaiting work.


Thus,
Infer's core algorithms can be understood as being sound with
respect to an idealized model (that is
all soundness can ever be), but this idealized model is some distance from
real execution models for programs where Infer is deployed.
One consequence of this is that we cannot claim that Infer reasons about <i> all </i> flows through an application,
but only <i> some </i> flows.

In approaching these limitations going forward we must consider solutions that take into account our use case: to comment in minutes on
modifications to large codebases. Methods based on whole program analysis are challenging to consider
when approaching these problems for our deployment model.


These limitations can be seen positively as opportunities for improvement,
to do more static analysis and program verification for the benefit of programmers everywhere!
We will
be delighted if people from the static analysis and program verification communities
join us in working on these problems.
