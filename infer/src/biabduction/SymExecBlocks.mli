(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

val resolve_method_with_block_args_and_analyze :
  caller_pdesc:Procdesc.t -> Typ.Procname.t -> (Exp.t * Typ.t) list
  -> (Specs.summary * (Exp.t * Typ.t) list) option
(* [resolve_method_with_block_args_and_analyze caller_pdesc pname args]
  create a copy of the method pname if it is defined and it's called with
  the correct number of arguments, and some arguments are block closures.
  The copy is created by adding extra formals for each captured variable,
  and by swapping the calls to the block arguments to the calls to the concrete
  blocks.
  The new procedure is analyzed and the possibly computed summary is returned
  together with the list of arguments where the closures where swapped by their
  captured variables. *)
