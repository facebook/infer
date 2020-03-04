---
docid: absint-framework
title: Building checkers with the Infer.AI framework
layout: docs
permalink: /docs/absint-framework.html
---

Infer.AI is a framework for quickly developing abstract interpretation-based checkers (intraprocedural or interprocedural). You define only:

(1) An abstract domain (type of abstract state plus `<=`, `join`, and `widen` operations)

(2) Transfer functions (a transformer that takes an abstract state as input and produces an abstract state as output)

and then you have an analysis that can run on all of the languages Infer supports (C, Obj-C, C++, and Java)!

This guide covers how to use the framework. For background on why we built the framework and how it works, check out these [slides](http://fbinfer.com/downloads/pldi17-infer-ai-tutorial.pdf) from a PLDI 2017 tutorial and this [talk](https://atscaleconference.com/videos/getting-the-most-out-of-static-analyzers) from @Scale2016. 

**If you feel like coding instead of reading, a great way to get started with Infer.AI is to go through the lab exercise [here](https://github.com/facebook/infer/blob/master/infer/src/labs/lab.md).**

## By example: intraprocedural analysis

This section helps you get started ASAP if you already understand [abstract interpretation](http://www.di.ens.fr/~cousot/AI/IntroAbsInt.html) (or don't, but are feeling bold).

Take a look at [liveness.ml](https://github.com/facebook/infer/blob/master/infer/src/checkers/liveness.ml). This code is performing a compilers-101 style liveness analysis over [SIL](#ir-basics-sil-cfgs-tenvs-procdescs-and-procnames), Infer's intermediate language. Since this code is fairly small and you should already understand what it's trying to do, it's a fairly good place to look in order to understand both how to use the abstract interpretation framework and what SIL is.

There are basically three important bits here: defining the domain, defining the transfer functions, and then passing the pieces to the framework to create an an analysis. Let's break down the third bit:

```
module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Backward(ProcCfg.Exceptional))
    (TransferFunctions)
```

The `ProcCfg.Backward(ProcCfg.Exceptional)` part says: "I want the direction of iteration to be backward" (since liveness is a backward analysis), and "I want to the analysis to follow exceptional edges". For a forward analysis that ignores exceptional edges, you would do `ProcCfg.Normal` instead (and many other combinations are possible; take a look at [ProcCfg.mli](https://github.com/facebook/infer/blob/master/infer/src/absint/ProcCfg.mli) for more). And finally, the `TransferFunctions` part says "Use the transfer functions I defined above".

Now you have an `Analyzer` module that exposes useful functions like [`compute_post`](https://github.com/facebook/infer/blob/master/infer/src/absint/AbstractInterpreter.mli#L30) (take a procedure as input and compute a postcondition) and [`exec_pdesc`](https://github.com/facebook/infer/blob/master/infer/src/absint/AbstractInterpreter.mli#L36) (take a procedure and compute an invariant map from node id's to the pre/post at each node). The next step is to hook your checker up to the Infer CLI. For the liveness analysis, you would do this by exposing a function for running the checker on a single procedure:

```
let checker { Callbacks.proc_desc; tenv; } =
  match Analyzer.compute_post (ProcData.make_default proc_desc tenv) with
  | Some post -> Logging.progress "Computed post %a for %a" Analyzer.Domain.pp post Typ.Procname.pp (Procdesc.get_proc_name proc_desc);
  | None -> ()
```

and then adding `Liveness.checker, checkers_enabled` to the list of registered checkers [here](https://github.com/facebook/infer/blob/master/infer/src/checkers/registerCheckers.ml#L42).

you can then run `infer run -a checkers -- <your_build_command>` to run your checker on real code. See [here](http://fbinfer.com/docs/analyzing-apps-or-projects.html) for more details on the build systems supported by Infer.

Other examples of simple intraprocedural checkers are [addressTaken.ml](https://github.com/facebook/infer/blob/master/infer/src/checkers/addressTaken.ml) and [copyPropagation.ml](https://github.com/facebook/infer/blob/master/infer/src/checkers/copyPropagation.ml).

## Basic error reporting

Useful analyses have output. Basic printing to stderr or stderr is good for debugging, but to report a programmer-readable error that is tied to a source code location, you'll want to use `Reporting.log_error`. Some examples of error-logging code: [1](https://github.com/facebook/infer/blob/master/infer/src/concurrency/RacerD.ml#L166), [2](https://github.com/facebook/infer/blob/master/infer/src/checkers/annotationReachability.ml#L224), or [3](https://github.com/facebook/infer/blob/master/infer/src/quandary/TaintAnalysis.ml#L186).

## By example: interprocedural analysis

Let's assume you have already read and understood the "intraprocedural analysis" section and have an intraprocedural checker. The abstract interpretation framework makes it easy to convert your intraprocedural analysis into a *modular* interprocedural analysis. Let me emphasize the *modular* point once more; global analyses cannot be expressed in this framework.

To make your checker interprocedural, you need to:

(1) Define the type of procedure summaries for your analysis and add some boilerplate for storing your data alongside the summaries for other analyses

(2) Add logic for (a) using summaries in your transfer functions and (b) converting your intraprocedural abstract state to a summary.

A good example to look at here is [siof.ml](https://github.com/facebook/infer/blob/master/infer/src/checkers/Siof.ml). Step (1) is just:

```
module Summary = Summary.Make (struct
    type summary = SiofDomain.astate

    let update_payload astate payload =
      { payload with Specs.siof = Some astate }

    let read_from_payload payload =
      payload.Specs.siof
  end)
```

along with adding the `Specs.siof` [field](https://github.com/facebook/infer/blob/master/infer/src/backend/specs.ml#L329) to the `Specs.payload` record [type](https://github.com/facebook/infer/blob/master/infer/src/backend/specs.ml#L321). Here, the type of the abstract state and the type of the summary are the same, which makes things easier for us (no logic to convert an abstract state to a summary).

Part (2a) is [here](https://github.com/facebook/infer/blob/master/infer/src/checkers/Siof.ml#L65):

```
match Summary.read_summary pdesc callee_pname with
```

This says: "read the summary for `callee_pname` from procedure `pdesc` with type environment `tenv`". You must then add logic for applying the summary to the current abstract state (often, this is as simple as doing a join).

Because our summary type is the same as the abstract state, part (2b) can be done for us by making use of the convenient `AbstractInterpreter.Interprocedural` [functor](https://github.com/facebook/infer/blob/master/infer/src/absint/AbstractInterpreter.mli#L19) (for an example of what to do when the types are different, take a look at [Quandary](https://github.com/facebook/infer/blob/master/infer/src/quandary/TaintAnalysis.ml#L540)):

```
module Interprocedural = Analyzer.Interprocedural (Summary)
```

This `Interprocedural` module will automatically do the work of computing and storing the summary for us. All we need to do is change the exposed `checker` function registered in `registerCheckers.ml` to call `Interprocedural.checker` instead:

```
let checker callback =
  ignore(Interprocedural.checker callback ProcData.empty_extras in)
```

That's it! We now have an interprocedural analysis.

One very important note here: a current (and soon-to-be-lifted) limitation prevents us from running multiple interprocedural checkers at the same time. If you register an interprocedural checker, be sure to unregister the other other ones. Otherwise, there's a risk that the checkers will clobber each other's results.

## Relevant code

Some pointers to useful code for building new analyses, and to the implementation of the framework for the interested:

Domain combinators:
- `AbstractDomain.BottomLifted`, `AbstractDomain.FiniteSet`, `AbstractDomain.Map`, `AbstractDomain.Pair` (all in [AbstractDomain](https://github.com/facebook/infer/blob/master/infer/src/checkers/AbstractDomain.mli))

Domains and domain building blocks:
- [AccessPath](https://github.com/facebook/infer/blob/master/infer/src/checkers/accessPath.mli)
- [AccessPathDomains](https://github.com/facebook/infer/blob/master/infer/src/checkers/accessPathDomains.mli)
- [AccessTree](https://github.com/facebook/infer/blob/master/infer/src/checkers/accessTree.ml)

Reporting errors with interprocedural traces:
- Examples: [`SiofTrace.ml`](https://github.com/facebook/infer/blob/master/infer/src/checkers/SiofTrace.ml), [`JavaTrace.ml`](https://github.com/facebook/infer/blob/master/infer/src/quandary/JavaTrace.ml), [`CppTrace.ml`](https://github.com/facebook/infer/blob/master/infer/src/quandary/CppTrace.ml).
- Implementation: [`Trace`](https://github.com/facebook/infer/blob/master/infer/src/checkers/Trace.mli)

Implementation:
- [`AbstractDomain`](https://github.com/facebook/infer/blob/master/infer/src/absint/AbstractDomain.ml)
- [`TransferFunctions`](https://github.com/facebook/infer/blob/master/infer/src/absint/AbstractInterpreter.mli)
- [`AbstractInterpreter`](https://github.com/facebook/infer/blob/master/infer/src/absint/AbstractInterpreter.mli)
- [`ProcCFG`](https://github.com/facebook/infer/blob/master/infer/src/absint/ProcCfg.mli)
- [`Summary`](https://github.com/facebook/infer/blob/master/infer/src/absint/Summary.ml)
- [`Scheduler`](https://github.com/facebook/infer/blob/master/infer/src/absint/Scheduler.ml)

## IR basics: SIL, CFG's, `tenv`'s, `procdesc`'s, and `procname`'s

All of the languages analyzed by Infer are converted into a common intermediate representation. A program is represented as a control-flow graph ([CFG](https://github.com/facebook/infer/blob/master/infer/src/IR/Cfg.rei)) whose nodes contain lists of instructions in the SIL language. SIL is a small low-level language that has some similarities with C, LLVM [IR](http://llvm.org/docs/LangRef.html), and [Boogie](https://research.microsoft.com/en-us/um/people/leino/papers/krml178.pdf).

[Expressions](https://github.com/facebook/infer/blob/master/infer/src/IR/Exp.rei#L25) are literals, program variables (`Pvar`'s), temporary variables (`Ident`'s), a field offset from a struct (OO features like objects are lowered into struct's), or an index offset from an array.

There are four interesting kinds of [instructions](https://github.com/facebook/infer/blob/master/infer/src/IR/Sil.rei#L38): `Load` for reading into a temporary variable, `Store` for writing to a program variable, field of a struct, or an array, `Prune e` (often called `assume` in other PL formalisms) blocks execution unless the expression `e` evaluates to true, and `Call` represents function calls.

Instructions and expressions have [types](https://github.com/facebook/infer/blob/master/infer/src/IR/Typ.rei#L76). A `Tstruct` (think: object) type has a [`Typename`](https://github.com/facebook/infer/blob/master/infer/src/IR/Typename.rei#L13), and it is often useful to look up metadata about the type (what fields does it have, what methods does it declare, what is its superclass, etc.) in the type environment, or [`tenv`](https://github.com/facebook/infer/blob/master/infer/src/IR/Tenv.rei#L37).

A procedure description or [`procdesc`](https://github.com/facebook/infer/blob/master/infer/src/IR/Procdesc.rei) (sometimes abbreviated `pdesc`) is an abstraction of a procedure declaration: it stores the CFG of the procedure, its signature, its annotations, and so on.

A procedure name or [`procname`](https://github.com/facebook/infer/blob/master/infer/src/IR/Procname.rei) (sometimes abbreviated `pname`) is an abstraction of a called procedure name. One procname may correspond to multiple (or zero) `procdesc`'s after resolution.

## Framework-specific IR: `ProcCFG`, `ProcData`, and `extras`

The abstract interpretation framework has a few additional constructs that are worth explaining.

A [`ProcCfG`](https://github.com/facebook/infer/blob/master/infer/src/absint/procCfg.mli) represents the CFG of a *single* procedure whereas (perhaps confusingly) a [`Cfg`](https://github.com/facebook/infer/blob/master/infer/src/IR/Cfg.rei) is the CFG for an entire file. A `ProcCfg` is really a customizable view of the underlying procedure CFG; we can get a view the CFG with its edges  backward (`ProcCfg.Backward`), with or without exceptional edges (`Normal`/`Exceptional`, respectively), or with each node holding at most one instruction (`OneInstrPerNode`).

[`ProcData`](https://github.com/facebook/infer/blob/master/infer/src/absint/procData.mli) is a container that holds all of the read-only information required to analyze a single procedure: procedure description, and `extras`. The `extras` are custom read-only data that are computed before analysis begins, and can be accessed from the transfer functions. Most often, no extras are required for analysis (`ProcData.empty_extras`), but it can be useful to stash information like a map from a formal to its [index](https://github.com/facebook/infer/blob/master/infer/src/quandary/TaintAnalysis.ml#L88) or an invariant [map](https://github.com/facebook/infer/blob/master/infer/src/backend/preanal.ml#L115) from a prior analysis in the extras.

## How it works

Coming soon.

## Intro: abstract interpretation

Coming soon.

## How do I make an analysis compositional?

Coming soon.
