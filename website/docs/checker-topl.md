---
title: "Topl"
description: "Detect errors based on user-provided state machines describing temporal properties over multiple objects."
---

Detect errors based on user-provided state machines describing temporal properties over multiple objects.

Activate with `--topl`.

Supported languages:
- C/C++/ObjC: Experimental
- C#/.Net: No
- Erlang: Experimental
- Hack: No
- Java: Experimental
- Python: No

# Topl

## What is it?

Topl is an analysis framework, built on top of Infer, for statically finding violations of temporal properties. Many analyses can be encoded as temporal properties supported by Topl, such as taint analysis. As a simple example, suppose that we don't want a value returned by method `source()` to be sent as an argument to a method `sink()`. This can be specified as follows:

```
property Taint
  prefix "Main"
  start -> start: *
  start -> tracking: source(Ret) => x := Ret
  tracking -> error: sink(Arg, VoidRet) when x == Arg
```

This specifies an automaton called `Taint` that has three states (`start`, `tracking`, `error`). Two of those states (`start` and `error`) have special meaning; other states (`tracking`) can have any names. The first transition (`start → tracking`) is taken when a method called `source()` is called, and its return value is stored in a register called `x`; the second transition (`tracking → error`) is taken when a method called `sink()` is called, but only if its argument equals what was previously saved in register `x`.
This property is violated in the following Java code:

```
public class Main {
  static void f() { g(tito(source())); }
  static void g(Object x) { h(x); }
  static void h(Object x) { sink(x); }
  static Object tito(Object x) { return x; }
  static Object source() { return "dirty"; }
  static void sink(Object x) {}
}
```

Note that `source()` and `sink()` are not called from the same method, and that the “dirty” object is passed around a few times before finally reaching the sink. Assuming that the property is in a file `taint.topl` and the Java code in a file `Main.java`, you can invoke Infer with the following command:

```
infer --topl --topl-properties taint.topl  -- javac Main.java
```

It will display the following error:

```
Main.java:2: error: Topl Error
  property Taint reaches state error.
  1.   public class Main {
  2. >   static void f() { g(tito(source())); }
  3.     static void g(Object x) { h(x); }
  4.     static void h(Object x) { sink(x); }
```

To get a full trace, use the command

```
infer explore
```

## Specifying Properties

A property is a nondeterministic automaton that can remember values in registers. An execution that drives the automaton from the start state to the error state will make Infer report an issue, and the trace that it produces will indicate which parts of the program drive which transitions of the automaton.

The general form of a property is the following:

```
property Name
  message "Optional error message" // This line can be missing
  prefix "Prefix" // There can be zero, one, or more prefix declarations
  sourceState -> targetState: Pattern(Arg1,...,ArgN,Ret) when Condition => Action
```

The property name and the optional error message are used for reporting issues. The prefix declarations are used to simplify Patterns. The core of the property is the list of transitions.

Each transition has a source state and a target state. The special transition label * means that the transition is always taken. Typically, there is a transition

```
  start -> start: *
```

meaning that the property can start anywhere, not just at the beginning of a method.

Otherwise, the label on a transition contains:

* a *Pattern*, which indicates what kind of instruction in the program drives this transition;
* a list of transition variable bindings (above named Arg1, ..., but any identifier starting with uppercase letters works);
* possibly a boolean Condition, which can refer to transition variables, registers and fields;
* possibly and Action, which is a list sequence of assignments of the form *register* := *TransitionVariable* (registers do not need to be declared, and any identifier starting with a lowercase letter works).

There are two types of patterns:

* a regex that matches method names
    * if the regex uses non-letters (such as dots) it must be within double-quotes; otherwise, double quotes are optional
    * the prefix declarations are used to add potential prefixes to the regex. The combine regex is essentially “(prefix_regex_a | prefix_regex_b) transition_pattern_regex“
    * for a method with n arguments, there must be n+1 transition variables to get a match. The first n transition variables get bound to the argument values, and the last transition variable gets bound to the return value. *This is true even for the case in which the return type is void*.
* the special keyword **#ArrayWrite**. In that case, there should be two transition variables like “(Array, Index)” — Array gets bound to the array object, and Index gets bound to the index at which the write happens.

The condition supports the following kinds of expressions:
* Referring to identifiers: transition variables and registers
* Field access over objects in the form `Identifier:Type.FieldName`, e.g. `X:MyClass.myField` (this is currently only supported for Erlang)
* Integer literals
* The usual comparison operators (`==`, `!=`, `<`, `>`, `>=`, `<=`) and conjunctions (`&&`)
* Reachability predicates of the form `Ident1 ~~> Ident2` meaning that `Ident2` can be reached via pointers/fields from `Ident1` in the heap

For several examples, see https://github.com/facebook/infer/tree/main/infer/tests/codetoanalyze/java/topl

## Limitations

* By design, some problems may be missed. Topl is built on Pulse, which attempts to minimize false positives, at the cost of sometimes having false negatives.
* Analysis time increases exponentially with the number of registers used in properties.
    * In theory, there should be no significant slowdown if registers belong to different properties, but the implementation is not yet optimized.
    * If there are many registers within the same property, then the slowdown is unavoidable (without some significant breakthrough). However, the maximum number of registers we ever used for one practical property was 3.


## List of Issue Types

The following issue types are reported by this checker:
- [TOPL_ERROR](/docs/next/all-issue-types#topl_error)
- [TOPL_ERROR_LATENT](/docs/next/all-issue-types#topl_error_latent)
