---
docid: adding-checkers
title: Simple intraprocedural checkers
layout: docs
permalink: /docs/adding-checkers.html
---

## How can I create my own checkers?

Infer Checkers provide a framework to perform intra-procedural static analyses.
Since this is an open source project, everyone is welcome to contribute with new great checkers.
In this page, we will create a very basic checker - a detector for every time the output method `java.io.PrintStream.println` is called.
This should be enough to get you started.

## Before you start

Make sure you are able to successfully build Infer and your developer environment is set up:

```
./build-infer.sh
make devsetup
```

Get familiar with Infer checkers and run Infer with some examples:

```
infer run -- javac Hello.java
```

In addition, get familiar with the Control Flow Graph (CFG) that Infer generates for you:

```
infer run -g -- javac Hello.java
dot -Tpdf infer-out/captured/Hello.java*/icfg.dot -o icfg.pdf
open icfg.pdf
```
This will give you further information about the analysis that is being done, including the CFG in dot format.
It is important that you understand the generated CFG since this is the abstraction of code that Checkers will analyze.

Infer is built with [OCaml](https://ocaml.org).
This is a programming language that combines both functional and imperative programming.
If you are not familiar with OCaml, it might be hard at the beginning to understand the code.
Take your time to review the [basics](https://ocaml.org/learn/tutorials/basics.html) and do some [exercises](https://ocaml.org/learn/tutorials/99problems.html).


## Let's go

The directory `infer/src/absint` contains utilities for the abstract interpretation framework that checkers are based on.

Looking into `infer/src/checkers` we can find some simple checkers. Most of them are implemented as a module created from a `TransferFunctions` module that is turned into an analyzer by applying one of the `AbstractInterpreter.Make*` functors, together with a `checker` function that calls into it. You can start by copying the code for one of these and modify it (eg checkers/SimpleChecker.ml). For example:

```ocaml
module TransferFunctions = struct
  ...
  let exec_instr astate proc_data cfg_node (instr : Sil.instr) =
    match instr with
    | pattern ->
        ST.report_error
          proc_name
          proc_desc
          "CHECKERS_MY_SIMPLE_CHECKER"
          location
          "A description of my simple checker"
    | _ -> astate
end

module Analyzer = AbstractInterpreter.Make (TransferFunctions)

let checker {Callbacks.exe_env; summary; get_procs_in_file} : Summary.t =
  let proc_name = Summary.get_proc_name summary in
  let tenv = Exe_env.get_tenv exe_env proc_name in
  let proc_data = ProcData.make_default summary tenv in
  ignore (Analyzer.compute_post proc_data ~initial) ;
  summary
```

Checkers implement a function that detects a given pattern for our specific checker and then calls `AbstractInterpreter.Make` to iterate over all the nodes of the CFG.

So now we need to know how to create our pattern.
As an example, consider the following:

```ocaml
Sil.Call (_, Sil.Const (Sil.Cfun pn), _, loc, _)
```

This pattern matches every function call. In our code, it would look like:

```ocaml
  let exec_instr astate proc_data cfg_node (instr : Sil.instr) =
    match instr with
    | Call (_, Const (Cfun pn), _, loc, _) ->
        ST.report_error
          proc_name
          proc_desc
          "CHECKERS_MY_SIMPLE_CHECKER"
          location
          "A description of my simple checker"
    | _ -> astate
```
The `absint/PatternMatch.ml` module contains the `java_proc_name_with_class_method` function which we can use for matching the required pattern.

Each node is represented using the type `instr` from the Smallfoot Intermediate Language (SIL). Take a look at `IR/Sil.mli` to get familiar with all the types. All source code languages supported by Infer are converted to this representation.

In this particular example, `Sil.Call` has the following information:

```ocaml
Sil.Call (
	list_of_return_values,
	Sil.Const (Const.Cfun name_of_function),
	list_of_arguments,
	location,
	call_flags
)
```

I hope this looks straight forward. Argument `call_flags` holds information about the function, such as whether it is virtual or not. Again, this is specified in the file `Sil.mli`.

The Checker we have written so far is able to detect every single function call. Now, we have to detect whether a specific function call is actually calling `java.io.PrintStream.println`.

Let's try this:

```ocaml
  let is_println pln = match pln with
    | Procname.Java pn_java ->
        PatternMatch.java_proc_name_with_class_method
          pn_java "java.io.PrintStream" "println"
    | _ ->
        false in

  let exec_instr astate proc_data cfg_node (instr : Sil.instr) =
    match instr with
    | Call (_, Const (Cfun pn), _, loc, _) when is_println pn ->
        ST.report_error
          proc_name
          proc_desc
          "CHECKERS_MY_SIMPLE_CHECKER"
          location
          "A description of my simple checker"
    | _ -> astate

```

Can you spot the difference? A new restriction was added to our pattern -- `is_println` expression helps us to check whether the current method is a `java.io.PrintStream.println` method or not.

So our implementation is done.
Now we have to register it as an enabled Checker in `checkers/registerCheckers.ml`.

Assuming the code is in SimpleCheckers.ml, you would register your checker as a _java\_checker_ in `checkers/registerCheckers.ml` by adding it to the `all_checkers` list:

```ocaml
let all_checkers =
  [ { name= "my simple checker"
    ; active= true
    ; callbacks= [(Procedure SimpleChecker.checker, Language.Java)] }
  ; (* the rest of the list as it was there *)
    ... ]
```

Build Infer with `./build-infer.sh` and your first Checker is ready!

If you want you can try with this java example:

```java
/*Hello.java*/
class Hello {
	int println(){
		return 0;
	}
	int test() {
		String s = "Hello World";
		System.out.println(s);
		s = null;
		println();
		return s.length();
	}
}
```

Notice that only `System.out.println` is being detected.

All set! You are ready to create your own Checkers!
Infer is an open source project and you are more than welcome to contribute. Take a look at the  [Github](https://github.com/facebook/infer/) page and feel free to fork or even open an issue if you're facing any trouble.
