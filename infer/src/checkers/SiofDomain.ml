(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(* The domain for the analysis is sets of global variables if an initialization is needed at
   runtime, or Bottom if no initialization is needed. For instance, `int x = 32; int y = x * 52;`
   gives a summary of Bottom for both initializers corresponding to these globals, but `int x =
   foo();` gives a summary of at least "NonBottom {}" for x's initializer since x will need runtime
   initialization.

   The encoding in terms of a BottomLifted domain is an efficiency hack to represent two pieces of
   information: whether a global variable (via its initializer function) requires runtime
   initialization, and which globals requiring initialization a given function (transitively)
   accesses. *)
include AbstractDomain.BottomLifted(SiofTrace)
