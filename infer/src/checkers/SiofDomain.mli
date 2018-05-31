(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module VarNames : module type of AbstractDomain.FiniteSet (String)

module Summary :
    module type of AbstractDomain.Pair (AbstractDomain.BottomLifted (SiofTrace)) (VarNames)

(* The domain for the analysis is:

   - On the one hand, sets of global variables if an initialization is needed at runtime, or Bottom
   if no initialization is needed. For instance, `int x = 32; int y = x * 52;` gives a summary of
   Bottom for both initializers corresponding to these globals, but `int x = foo();` gives a summary
   of at least "NonBottom {}" for x's initializer since x will need runtime initialization.

   The encoding in terms of a BottomLifted domain is an efficiency hack to represent two pieces of
   information: whether a global variable (via its initializer function) requires runtime
   initialization, and which globals requiring initialization a given function (transitively)
   accesses.

   - On the other hand, the set of variables that are guaranteed to be initialized when the function
   terminates (even before main() has started). For instance, this is the case for
   std::ios_base::Init::Init(). *)

include module type of Summary
