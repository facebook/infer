(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val map_args_captured_vars : f:(Exp.closure -> 'a list) -> (Exp.t * 'b) list -> 'a list

val process_closure_param : Procdesc.t -> unit

val process_closure_call : Procdesc.t -> unit
