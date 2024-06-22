---
id: absint-framework
title: Building checkers with the Infer.AI framework
---

Infer.AI is a framework for quickly developing abstract interpretation-based
checkers (intraprocedural or interprocedural). You define only:

1. An abstract domain (type of abstract state plus `<=`, `join`, and `widen`
operations)
2. Transfer functions (a transformer that takes an abstract state as input and
produces an abstract state as output)

What you get in exchange is an analysis that can run on all of the
languages Infer supports (C, Objective-C, C++, and Java)!

This guide covers how to use the framework. For background on why we built the
framework and how it works, check out these
[slides](/downloads/pldi17-infer-ai-tutorial.pdf) from a PLDI
2017 tutorial and this
[talk](https://atscaleconference.com/videos/getting-the-most-out-of-static-analyzers)
from @Scale2016.

**If you feel like coding instead of reading, a great way to get started with
Infer.AI is to go through the lab exercise
[here](https://github.com/facebook/infer/blob/main/infer/src/labs/README.md).**

## By example: intraprocedural analysis

This section helps you get started ASAP if you already understand
[abstract interpretation](http://www.di.ens.fr/~cousot/AI/IntroAbsInt.html) (or
don't, but are feeling bold).

Take a look at
[liveness.ml](https://github.com/facebook/infer/blob/main/infer/src/checkers/liveness.ml).
This code is performing a compilers-101 style liveness analysis over
SIL, Infer's intermediate
language. Since this code is fairly small and you should already understand what
it's trying to do, it's a fairly good place to look in order to understand both
how to use the abstract interpretation framework and what SIL is.

There are basically three important bits here: defining the domain, defining the
transfer functions, and then passing the pieces to the framework to create an
analysis. Let's break down the third bit:

```OCaml
module CFG = ProcCfg.OneInstrPerNode (ProcCfg.Backward (ProcCfg.Exceptional))
module CheckerAnalyzer =
  AbstractInterpreter.MakeRPO (TransferFunctions (CheckerMode) (CFG))
```

The `ProcCfg.Backward (ProcCfg.Exceptional)` part says: "I want the direction of
iteration to be backward" (since liveness is a backward analysis), and "I want
the analysis to follow exceptional edges". For a forward analysis that ignores
exceptional edges, you would do `ProcCfg.Normal` instead (and many other
combinations are possible; take a look at
[ProcCfg.mli](https://github.com/facebook/infer/blob/main/infer/src/absint/ProcCfg.mli)
for more). And finally, the `TransferFunctions` part says "Use the transfer
functions I defined above".

Now you have a `CheckerAnalyzer` module that exposes useful functions
like
[`compute_post`](https://github.com/facebook/infer/blob/main/infer/src/absint/AbstractInterpreter.mli#L30)
(take a procedure as input and compute a postcondition) and
[`exec_pdesc`](https://github.com/facebook/infer/blob/main/infer/src/absint/AbstractInterpreter.mli#L36)
(take a procedure and compute an invariant map from node id's to the
pre/post at each node). The next step is to hook your checker up to
the Infer command-line interface (CLI). For the liveness analysis, you
would do this by exposing a function for running the checker on a
single procedure:

```OCaml
let checker ({IntraproceduralAnalysis.proc_desc; err_log} as analysis_data) =
  match Analyzer.compute_post analysis_data ~initial:Domain.empty with
  | Some post ->
      Logging.progress "Computed post %a for %a"
        Domain.pp post Procname.pp (Procdesc.get_proc_name proc_desc);
  | None -> ()
```

and then adding `Liveness.checker` to the list of registered checkers
in
[registerCheckers.ml](https://github.com/facebook/infer/blob/main/infer/src/backend/registerCheckers.ml)
(search for "Liveness").

you can then run `infer run --liveness-only -- <your_build_command>` to run your
checker on real code. See [here](/docs/next/analyzing-apps-or-projects) for more
details on the build systems supported by Infer.

Other examples of simple intraprocedural checkers are
[addressTaken.ml](https://github.com/facebook/infer/blob/main/infer/src/checkers/addressTaken.ml)
and
[Siof.ml](https://github.com/facebook/infer/blob/main/infer/src/checkers/Siof.ml).

## Error reporting

Useful analyses have output. Basic printing to stderr or stderr is
good for debugging, but to report a programmer-readable error that is
tied to a source code location, you'll want to use
[`Reporting.log_issue`](pathname:///odoc/next/infer/Absint/Reporting/index.html#val-log_issue).

## By example: interprocedural analysis

Let's assume you have already read and understood the "intraprocedural analysis"
section and have an intraprocedural checker. The abstract interpretation
framework makes it easy to convert your intraprocedural analysis into a
_modular_ interprocedural analysis. Let me emphasize the _modular_ point once
more; global analyses cannot be expressed in this framework.

To make your checker interprocedural, you need to:

1. Define the type of procedure summaries for your analysis and let
registerCheckers.ml know that your checker is interprocedural

2. Add logic for (a) using summaries in your transfer functions and (b)
converting your intraprocedural abstract state to a summary.

A good example to look at here is
[Siof.ml](https://github.com/facebook/infer/blob/main/infer/src/checkers/Siof.ml).
Step (1) is just:

```OCaml
(* in src/checkers/SiofDomain.ml *)
(* note that as a result the type of summaries is the same as the type of domain
   elements *)
module Summary = ...
include Summary


(* in src/backend/Payloads.ml: register the payload of the analyzer *)
type t =
  { ...
  ; siof: SiofDomain.Summary.t option
  ... }


(* in src/backend/registerCheckers.ml *)
let all_checkers = [ ...
  ; {checker= SIOF; callbacks= [(interprocedural Payloads.Fields.siof Siof.checker, Clang)]}
  ... ]
```

Here, the type of the abstract state and the type of the summary are the same,
which makes things easier for us (no logic to convert an abstract state to a
summary).

Part (2a) is
[here](https://github.com/facebook/infer/blob/be4ddc48f6330b7b788d899ce12ca51b4d673530/infer/src/checkers/Siof.ml#L168)
and uses the `analyze_dependency` callback provided by the framework:

```
match analyze_dependency callee_pname with
```

This says: "read the summary for `callee_pname`, possibly computing it
first". You must then add logic for applying the summary to the
current abstract state (often, this is as simple as doing a join).

Because our summary type is the same as the abstract state, part (2b)
here simply consists in return the post computed by the analysis as
the procedure's summary, using `Analyzer.compute_post`.

That's it! We now have an interprocedural analysis.

To go deeper, jump to the [lab
exercise](https://github.com/facebook/infer/blob/main/infer/src/labs/README.md)
and to the [API documentation](internal-API/), e.g. for the
[Absint](pathname:///odoc/next/infer/Absint.html) and
[IR](pathname:///odoc/next/infer/IR.html) modules.
