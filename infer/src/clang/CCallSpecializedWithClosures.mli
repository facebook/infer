(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This preanalysis aims to create specialized clones of methods that have blocks as arguments and
    that are called with concrete closures, and then it calls these clone methods instead of the
    original ones. One complication is with the captured variables in the closure: we add them to
    the formals of the cloned method and pass them through to the concrete blocks. We do this
    transformation in two steps:

    1. Go through all the callers of methods with blocks as parameters, and create the clone
    methods. In this preanalysis we only create the attributes for the new method, not the code. We
    also update the call instructions in the callers to represent a call to the cloned method with
    updated arguments: we don't need to pass closure's arguments anymore, we instead pass the
    captured variables as new arguments.

    2. (In ClosureSubstSpecializedMethod.ml) We add the corresponding code to the newly created
    clones: this means swapping the call to the block variable with a call to the corresponding
    block. Moreover, we add some of the new formals (that correspond to the captured variables) to
    the arguments of the call. *)

open! IStd

val process : Cfg.t -> unit
