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
