---
title: List of all checkers
hide_table_of_contents: true
---

Here is an overview of the checkers currently available in Infer.

## Annotation Reachability

Given a pair of source and sink annotation, e.g. `@PerformanceCritical` and `@Expensive`, this checker will warn whenever some method annotated with `@PerformanceCritical` calls, directly or indirectly, another method annotated with `@Expensive`

[Visit here for more information.](/docs/next/checker-annotation-reachability)

## Biabduction

This analysis deals with a range of issues, many linked to memory safety.

[Visit here for more information.](/docs/next/checker-biabduction)

## Buffer Overrun Analysis (InferBO)

InferBO is a detector for out-of-bounds array accesses.

[Visit here for more information.](/docs/next/checker-bufferoverrun)

## Config Impact Analysis

[EXPERIMENTAL] Collects function that are called without config checks.

[Visit here for more information.](/docs/next/checker-config-impact-analysis)

## Cost: Complexity Analysis

Computes the asymptotic complexity of functions with respect to execution cost or other user defined resources. Can be used to detect changes in the complexity with `infer reportdiff`.

[Visit here for more information.](/docs/next/checker-cost)

## Eradicate

The eradicate `@Nullable` checker for Java annotations.

[Visit here for more information.](/docs/next/checker-eradicate)

## Fragment Retains View

Detects when Android fragments are not explicitly nullified before becoming unreachable.

**\*\*\*DEPRECATED\*\*\*** Unmaintained due to poor precision.

[Visit here for more information.](/docs/next/checker-fragment-retains-view)

## Immutable Cast

Detection of object cast from immutable types to mutable types. For instance, it will detect casts from `ImmutableList` to `List`, `ImmutableMap` to `Map`, and `ImmutableSet` to `Set`.

**\*\*\*DEPRECATED\*\*\*** Unmaintained due to poor actionability of the reports.

[Visit here for more information.](/docs/next/checker-immutable-cast)

## Impurity

Detects functions with potential side-effects. Same as "purity", but implemented on top of Pulse.

[Visit here for more information.](/docs/next/checker-impurity)

## Inefficient keySet Iterator

Check for inefficient uses of iterators that iterate on keys then lookup their values, instead of iterating on key-value pairs directly.

[Visit here for more information.](/docs/next/checker-inefficient-keyset-iterator)

## AST Language (AL)

Declarative linting framework over the Clang AST.

**\*\*\*DEPRECATED\*\*\*** On end-of-life support, may be removed in the future.

[Visit here for more information.](/docs/next/checker-linters)

## Litho "Required Props"

Checks that all non-optional `@Prop`s have been specified when constructing Litho components.

[Visit here for more information.](/docs/next/checker-litho-required-props)

## Liveness

Detection of dead stores and unused variables.

[Visit here for more information.](/docs/next/checker-liveness)

## Loop Hoisting

Detect opportunities to hoist function calls that are invariant outside of loop bodies for efficiency.

[Visit here for more information.](/docs/next/checker-loop-hoisting)

## Parameter Not Null Checked

An Objective-C-specific analysis to detect when a block parameter is used before being checked for null first.

[Visit here for more information.](/docs/next/checker-parameter-not-null-checked)

## `printf()` Argument Types

Detect mismatches between the Java `printf` format strings and the argument types For example, this checker will warn about the type error in `printf("Hello %d", "world")`

**\*\*\*DEPRECATED\*\*\*** Unmaintained.

[Visit here for more information.](/docs/next/checker-printf-args)

## Pulse

Memory and lifetime analysis.

[Visit here for more information.](/docs/next/checker-pulse)

## Purity

Detects pure (side-effect-free) functions. A different implementation of "impurity".

[Visit here for more information.](/docs/next/checker-purity)

## Quandary

The Quandary taint analysis detects flows of values between sources and sinks, except if the value went through a "sanitizer". In addition to some defaults, users can specify their own sources, sinks, and sanitizers functions.

[Visit here for more information.](/docs/next/checker-quandary)

## RacerD

Thread safety analysis.

[Visit here for more information.](/docs/next/checker-racerd)

## Resource Leak Lab Exercise

Toy checker for the "resource leak" write-your-own-checker exercise.

[Visit here for more information.](/docs/next/checker-resource-leak-lab)

## Resource Leak checker for .NET

"resource leak" checker for .NET.

[Visit here for more information.](/docs/next/checker-dotnet-resource-leak)

## Static Initialization Order Fiasco

Catches Static Initialization Order Fiascos in C++, that can lead to subtle, compiler-version-dependent errors.

[Visit here for more information.](/docs/next/checker-siof)

## Simple Lineage

Computes a dataflow graph

[Visit here for more information.](/docs/next/checker-simple-lineage)

## Self in Block

An Objective-C-specific analysis to detect when a block captures `self`.

[Visit here for more information.](/docs/next/checker-self-in-block)

## Starvation

Detect various kinds of situations when no progress is being made because of concurrency errors.

[Visit here for more information.](/docs/next/checker-starvation)

## Topl

Detect errors based on user-provided state machines describing temporal properties over multiple objects.

[Visit here for more information.](/docs/next/checker-topl)

## Uninitialized Value

Warns when values are used before having been initialized.

**\*\*\*DEPRECATED\*\*\*** Uninitialized value checking has moved to Pulse.

[Visit here for more information.](/docs/next/checker-uninit)

