(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let dispatch_models =
  [ "_dispatch_once"
  ; "dispatch_async"
  ; "dispatch_after"
  ; "dispatch_group_async"
  ; "dispatch_barrier_async"
  ; "dispatch_group_notify" ]


let is_model proc_name = List.mem dispatch_models ~equal:String.equal (Procname.to_string proc_name)

let get_dispatch_closure_opt actual_params =
  List.find_map actual_params ~f:(fun (exp, _) ->
      match exp with
      | Exp.Closure c when Procname.is_objc_block c.name ->
          (* We assume that for these modelled functions, the block passed as parameter doesn't
             have arguments, so we only pass the captured variables. *)
          Some (c.name, exp, [])
      | _ ->
          None )
