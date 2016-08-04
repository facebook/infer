---
docid: adding-checkers
title: Adding checkers
layout: docs
permalink: /docs/adding-checkers.html
---

## How can I create my own checkers?

Infer Checkers provide a framework to perform intra-procedural static analyses.
Since this is an open source project, everyone is welcome to contribute with new great checkers.
In this page, we will create a very basic checker - a detector for every time the output method ```java.io.PrintStream.println``` is called.
This should be enough to get you started.

## Before you start

Make sure you are able to successfully build Infer:

```
./build-infer.sh
```

Get familiar with Infer checkers and run Infer with some examples:

```
infer -a checkers -- javac Hello.java
```

In addition, get familiar with the Control Flow Graph (CFG) that Infer generates for you:

```
infer -g -a checkers -- javac Hello.java
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

Directory ```infer/src/checkers``` has all the source code related with Infer:Checkers.

Looking into ```checkers.ml``` we can find some simple checkers.
They share this basic structure:

```ocaml
let callback_my_simple_checker { Callbacks.proc_desc; proc_name } =
  let do_instr node = function
    | pattern ->
        ST.report_error
          proc_name
          proc_desc
          "CHECKERS_MY_SIMPLE_CHECKER"
          location
          "A description of my simple checker"
    | _ -> () in
  Cfg.Procdesc.iter_instrs do_instr proc_desc

```

Checkers implement a function that detects a given pattern for our specific checker and then calls ```Cfg.Procdesc.iter_instrs``` to iterate over all the nodes of the CFG.

So now we need to know how to create our pattern.
As an example, consider the following:

```ocaml
Sil.Call (_, Sil.Const (Sil.Cfun pn), _, loc, _)
```

This pattern matches every function call. In our code, it would look like:

```ocaml
let callback_my_simple_checker { Callbacks.proc_desc; proc_name } =
  let do_instr node = function
    | Sil.Call (_, Sil.Const (Sil.Cfun pn), _, loc, _) ->
        ST.report_error
          proc_name
          proc_desc
          "CHECKERS_MY_SIMPLE_CHECKER"
          location
          "A description of my simple checker"
    | _ -> () in
  Cfg.Procdesc.iter_instrs do_instr proc_desc

```
The `checkers/PatternMatch.ml` module contains the `java_proc_name_with_class_method` function which we can use for matching the required pattern.

Each node is represented using the type ```instr``` from the Smallfoot Intermediate Language (SIL). Take a look at ```IR/sil.rei``` to get familiar with all the types. All source code languages supported by Infer are converted to this representation.

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

I hope this looks straight forward. Argument ```call_flags``` holds information about the function, such as whether it is virtual or not. Again, this is specified in the file ```sil.rei```.

The Checker we have written so far is able to detect every single function call. Now, we have to detect whether a specific function call is actually calling ```java.io.PrintStream.println```.

Let's try this:

```ocaml
(** Simple Checker *)
let callback_my_simple_checker { Callbacks.proc_desc; proc_name } =
  let is_println pln = match pln with
    | Procname.Java pn_java ->
        PatternMatch.java_proc_name_with_class_method
          pn_java "java.io.PrintStream" "println"
    | _ ->
        false in
  let do_instr node = function
    | Sil.Call (_, Sil.Const (Const.Cfun pn), _, loc, _) when is_println pn->
        L.err "Sample Log Message %a@." Procname.pp proc_name;
        ST.report_error
          proc_name
          proc_desc
          "CHECKERS_MY_SIMPLE_CHECKER"
          loc
          "A description of my simple checker"
    | _ -> () in
  Cfg.Procdesc.iter_instrs do_instr proc_desc

```

Can you spot the difference? A new restriction was added to our pattern -- ```is_println``` expression helps us to check whether the current method is a ```java.io.PrintStream.println``` method or not.

So our implementation is done.
Now we have to register it as an enabled Checker.

First we have to make ```callback_my_simple_checker``` available outside module ```Checkers``` by adding its signature to ```checkers.mli```:

```ocaml
(**checkers.mli**)
val callback_my_simple_checker : Callbacks.proc_callback_t
```

And in the file ```registerCheckers.mli``` register our Checker as a _java\_checker_:

```ocaml
  let java_checkers =
    let l =
      [
		  (**...**)
		  Checkers.callback_my_simple_checker, checkers_enabled;
		  (**...**)
      ] in
    IList.map (fun (x, y) -> (x, y, Some Config.Java)) l in

```

Build Infer with ```./build-infer.sh``` and your first Checker is ready!

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
		s= null;
		println();
		return s.length();
	}
}
```

Notice that only ```System.out.println``` is being detected.

All set! You are ready to create your own Checkers!
Infer is an open source project and you are more than welcome to contribute. Take a look at the  [Github](https://github.com/facebook/infer/) page and feel free to fork or even open an issue if you're facing any trouble.
